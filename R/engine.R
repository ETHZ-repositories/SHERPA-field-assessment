# SHERPA v1.8 engine --------------------------------------------------------

load_sherpa_config <- function(base_dir=".") {
  list(
    metals = read.csv(file.path(base_dir,"config","heavy_metals.csv"),
                      stringsAsFactors=FALSE, check.names=FALSE),
    crops = read.csv(file.path(base_dir,"config","crop_n_yield.csv"),
                     stringsAsFactors=FALSE, check.names=FALSE),
    pli = read.csv(file.path(base_dir,"config","pesticide_pli_thresholds.csv"),
                   stringsAsFactors=FALSE, check.names=FALSE),
    bulk_density = read.csv(file.path(base_dir,"config","bulk_density_texture.csv"),
                            stringsAsFactors=FALSE, check.names=FALSE),
    applicability = transform(
      read.csv(file.path(base_dir,"config","part2_applicability.csv"),
               stringsAsFactors=FALSE, check.names=FALSE),
      full_scheme = toupper(full_scheme) == "TRUE",
      published_eu = toupper(published_eu) == "TRUE"
    ),
    lithology = read.csv(file.path(base_dir,"config","forest_lithology_s2_4.csv"),
                         stringsAsFactors=FALSE, check.names=FALSE),
    proxy_manifest = read.csv(file.path(base_dir,"config","proxy_manifest.csv"),
                              stringsAsFactors=FALSE, check.names=FALSE)
  )
}

make_source_map <- function(record) {
  out <- setNames(rep("MISSING", length(record)), names(record))
  for (nm in names(record)) {
    x <- record[[nm]]
    present <- FALSE
    if (is.logical(x) && length(x) > 0 && !is.na(x[1])) present <- TRUE
    if (is.numeric(x) && length(x) > 0 && !is.na(x[1])) present <- TRUE
    if (is.character(x) && length(x) > 0 && !is.na(x[1]) && nzchar(x[1]) && x[1] != "unknown") present <- TRUE
    if (present) out[[nm]] <- "USER"
  }
  out
}

calculate_sherpa <- function(record, cfg, proxy_root="data/proxies") {
  sources <- make_source_map(record)
  proxy_notes <- character(0)
  proxy_log <- NULL

  if (isTRUE(record$use_proxy)) {
    resolved <- resolve_optional_proxies(record, sources, cfg$proxy_manifest, proxy_root)
    record <- resolved$record
    sources <- resolved$sources
    proxy_notes <- resolved$notes
    proxy_log <- resolved$log
  }

  p1 <- calculate_part1(record)
  route <- p1$route

  if (route %in% c("not_considered","grassland_unresolved","unsupported")) {
    return(list(
      record=record, sources=sources, route=route,
      part1=p1, part2=NULL, final=NA_real_,
      proxy_log=proxy_log,
      notes=unique(c(p1$notes, proxy_notes))
    ))
  }

  p2 <- calculate_part2(record, route, cfg$metals, cfg$crops, cfg$pli,
                        cfg$bulk_density, cfg$applicability, sources=sources)

  final <- if (!is_na_scalar(p1$score) && !is_na_scalar(p2$mean)) p1$score + p2$mean else NA_real_

  notes <- unique(c(p1$notes, p2$notes, proxy_notes))

  # Published theoretical range based on Part 1=1..10 and mean Part 2=0..-9.
  if (!is_na_scalar(final) && (final < -8 - 1e-9 || final > 10 + 1e-9)) {
    notes <- c(notes, "Computed result falls outside the theoretical SHERPA range (-8 to 10); inspect inputs/scoring.")
  }

  list(
    record=record,
    sources=sources,
    route=route,
    part1=p1,
    part2=p2,
    final=final,
    proxy_log=proxy_log,
    notes=unique(notes)
  )
}


# Human-readable labels for the 19 SHERPA Part 2 components used by the
# application, validation interface, and regression tests.
part2_component_label <- function(component) {
  labels <- c(
    erosion = "Soil erosion",
    landslide = "Landslide density",
    metal_cu = "Heavy metal - Cu",
    metal_hg = "Heavy metal - Hg",
    metal_zn = "Heavy metal - Zn",
    metal_cd = "Heavy metal - Cd",
    metal_ni = "Heavy metal - Ni",
    metal_pb = "Heavy metal - Pb",
    metal_sb = "Heavy metal - Sb",
    metal_as = "Heavy metal - As",
    metal_cr = "Heavy metal - Cr",
    metal_co = "Heavy metal - Co",
    nitrogen = "Nitrogen surplus / load",
    p_excess = "Phosphorus excess",
    p_mining = "Phosphorus mining",
    pesticide = "Pesticide pressure",
    salinisation = "Salinisation",
    compaction = "Compaction",
    soc = "SOC assessment"
  )
  if (length(component) == 0 || is.na(component[1])) return("")
  key <- as.character(component[1])
  if (key %in% names(labels)) unname(labels[[key]]) else key
}


# Validation-only pathway -------------------------------------------------
# This bypasses the raw measurement -> severity-class functions and feeds
# already scored Part 2 severities directly into the SAME aggregation helper
# used by the scientific engine. It is intended for:
#   (a) manual consistency checks using pre-scored SHERPA classes; and
#   (b) legacy regression cases.
#
# It is NOT a substitute for normal SHERPA assessment from field/raw inputs.
calculate_sherpa_prescored <- function(part1_score,
                                       route,
                                       severity_map,
                                       applicability_mode = "full_scheme",
                                       aggregation_mode = "available_mean",
                                       app_cfg,
                                       field_id = "Validation_case",
                                       source_label = "PRE_SCORED") {

  p1 <- suppressWarnings(as.numeric(part1_score[1]))
  if (length(p1) == 0 || is.na(p1) || p1 < 1 || p1 > 10) p1 <- NA_real_

  route <- as.character(route[1])
  if (!route %in% c("cropland", "grassland", "forest")) {
    stop("Pre-scored validation route must be cropland, grassland, or forest.")
  }

  rr <- app_cfg[app_cfg$effective_land_use == route, , drop = FALSE]
  if (nrow(rr) == 0) stop("No Part 2 applicability rows found for validation route.")

  rows <- lapply(seq_len(nrow(rr)), function(i) {
    comp <- rr$component[i]

    sev <- NA_real_
    if (!is.null(severity_map[[comp]])) {
      sev <- suppressWarnings(as.numeric(severity_map[[comp]][1]))
      if (length(sev) == 0 || is.na(sev) || sev < 0 || sev > 9) sev <- NA_real_
    }

    applicable <- if (identical(applicability_mode, "published_eu")) {
      isTRUE(rr$published_eu[i])
    } else if (safe_eq(rr$s2_1_status[i], "na")) {
      # v1.7: SI2 'na' is included in Full scheme only when a local/pre-scored
      # value is actually available. 'nr' remains excluded through full_scheme=FALSE.
      isTRUE(rr$full_scheme[i]) && !is.na(sev)
    } else {
      isTRUE(rr$full_scheme[i])
    }

    data.frame(
      component = comp,
      label = part2_component_label(comp),
      applicable = applicable,
      s2_1_status = rr$s2_1_status[i],
      severity = if (applicable) sev else NA_real_,
      negative_score = if (applicable && !is.na(sev)) -sev else NA_real_,
      detail = if (applicable && !is.na(sev)) paste0("pre-scored severity=", format(sev)) else "",
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, rows)
  applicable_df <- df[df$applicable, , drop = FALSE]

  agg <- aggregate_part2_negative_scores(
    applicable_df$negative_score,
    aggregation_mode = aggregation_mode,
    n_expected = nrow(applicable_df)
  )

  final <- if (!is.na(p1) && !is.na(agg$mean_negative)) {
    p1 + agg$mean_negative
  } else {
    NA_real_
  }

  p1_obj <- list(
    score = p1,
    route = route,
    sub_scores = data.frame(
      component = "Part 1 - pre-scored validation value",
      score = p1,
      stringsAsFactors = FALSE
    ),
    notes = paste0("Part 1 supplied directly in ", source_label, " validation mode.")
  )

  missing_components <- applicable_df$label[is.na(applicable_df$negative_score)]

  p2_obj <- list(
    table = df,
    mean = agg$mean_negative,
    severity_magnitude = agg$severity_magnitude,
    n_expected = agg$n_expected,
    n_available = agg$n_available,
    completeness = agg$completeness,
    missing_components = unique(missing_components),
    notes = paste0(
      "Part 2 severities were supplied directly in ", source_label,
      " validation mode; raw-input threshold functions were intentionally bypassed."
    ),
    compaction = NULL,
    soc = NULL,
    p_balance = NA_real_,
    nitrogen = NULL
  )

  rec <- list(
    field_id = field_id,
    land_use = route,
    applicability_mode = applicability_mode,
    aggregation_mode = aggregation_mode,
    validation_source = source_label
  )

  sources <- c(
    field_id = source_label,
    land_use = source_label,
    applicability_mode = source_label,
    aggregation_mode = source_label,
    validation_source = source_label
  )

  list(
    record = rec,
    sources = sources,
    route = route,
    part1 = p1_obj,
    part2 = p2_obj,
    final = final,
    notes = unique(c(p1_obj$notes, p2_obj$notes))
  )
}
