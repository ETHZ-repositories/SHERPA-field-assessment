# SHERPA v1.8 optional pan-European proxy extraction ----------------------
#
# Data hierarchy: USER -> PROXY_EU -> MISSING.
# User values always have priority. WGS84 lon/lat are transformed internally
# to each raster's native CRS. NoData cells are reported as gaps; no
# neighbouring-cell substitution or interpolation is used for missing cells.

.proxy_raster_cache <- new.env(parent=emptyenv())
.proxy_lookup_cache <- new.env(parent=emptyenv())

load_lithology_crosswalk <- function(path="config/lithology_crosswalk_PROVISIONAL_v0_1.csv") {
  key <- normalizePath(path, winslash="/", mustWork=FALSE)
  if (exists(key, envir=.proxy_lookup_cache, inherits=FALSE)) {
    return(get(key, envir=.proxy_lookup_cache, inherits=FALSE))
  }
  if (!file.exists(path)) return(NULL)
  x <- read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  assign(key, x, envir=.proxy_lookup_cache)
  x
}

load_usda_texture_crosswalk <- function(path="config/usda_texture_crosswalk_v1_8_10.csv") {
  key <- normalizePath(path, winslash="/", mustWork=FALSE)
  if (exists(key, envir=.proxy_lookup_cache, inherits=FALSE)) {
    return(get(key, envir=.proxy_lookup_cache, inherits=FALSE))
  }
  if (!file.exists(path)) return(NULL)
  x <- read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  assign(key, x, envir=.proxy_lookup_cache)
  x
}

load_proxy_manifest <- function(path="config/proxy_manifest.csv") {
  x <- read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  if ("auto_fill" %in% names(x)) x$auto_fill <- toupper(as.character(x$auto_fill)) == "TRUE"
  x
}

proxy_root_normalize <- function(root_dir) {
  root_dir <- char_or_na(root_dir)
  if (is.na(root_dir) || !nzchar(trimws(root_dir))) return("data/proxies")
  path.expand(trimws(root_dir))
}

proxy_row_auto_fill <- function(rr) {
  if (is.null(rr) || nrow(rr) != 1 || !"auto_fill" %in% names(rr)) return(FALSE)
  isTRUE(rr$auto_fill[1]) || identical(toupper(as.character(rr$auto_fill[1])), "TRUE")
}

proxy_root_for_section <- function(root_dir, section) {
  if (is.list(root_dir) || (length(root_dir) > 1 && !is.null(names(root_dir)))) {
    sec <- toupper(as.character(section)[1])
    x <- root_dir[[sec]]
    if (is.null(x) || length(x)==0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) {
      return(NA_character_)
    }
    return(proxy_root_normalize(as.character(x[1])))
  }
  proxy_root_normalize(root_dir)
}

proxy_candidate_names <- function(row) {
  out <- character(0)
  if ("file_name" %in% names(row) && !is.na(row$file_name[1]) && nzchar(row$file_name[1])) {
    out <- c(out, row$file_name[1])
  }
  if ("aliases" %in% names(row) && !is.na(row$aliases[1]) && nzchar(row$aliases[1])) {
    out <- c(out, strsplit(row$aliases[1], "|", fixed=TRUE)[[1]])
  }
  more <- unlist(lapply(out, function(z) {
    if (grepl("\\.(tif|tiff|vrt)$", z, ignore.case=TRUE)) character(0)
    else c(paste0(z, ".tif"), paste0(z, ".tiff"), paste0(z, ".vrt"))
  }))
  unique(c(out, more))
}

proxy_name_key <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  tolower(gsub("[^a-z0-9]", "", x))
}

find_proxy_file <- function(row, root_dir) {
  section <- if ("section" %in% names(row)) row$section[1] else ""
  root <- proxy_root_for_section(root_dir, section)
  if (is.na(root) || !dir.exists(root)) return(NA_character_)

  candidates <- proxy_candidate_names(row)

  for (nm in candidates) {
    p <- file.path(root, nm)
    if (file.exists(p)) return(normalizePath(p, winslash="/", mustWork=TRUE))
  }

  all_files <- list.files(root, recursive=TRUE, full.names=TRUE,
                          include.dirs=FALSE, all.files=FALSE)
  if (length(all_files) == 0) return(NA_character_)

  base <- basename(all_files)
  for (nm in candidates) {
    hit <- which(tolower(base) == tolower(nm))
    if (length(hit) > 0) {
      return(normalizePath(all_files[hit[1]], winslash="/", mustWork=TRUE))
    }
  }

  file_keys <- vapply(base, proxy_name_key, character(1))
  cand_keys <- unique(vapply(candidates, proxy_name_key, character(1)))
  hits <- integer(0)
  for (ck in cand_keys[nzchar(cand_keys)]) {
    hits <- c(hits, which(startsWith(file_keys, ck) | startsWith(ck, file_keys)))
  }
  hits <- unique(hits)
  if (length(hits) > 0) {
    h <- hits[which.min(nchar(base[hits]))]
    return(normalizePath(all_files[h], winslash="/", mustWork=TRUE))
  }

  NA_character_
}

proxy_manifest_row <- function(variable, manifest) {
  rr <- manifest[manifest$variable == variable, , drop=FALSE]
  if (nrow(rr) != 1) return(NULL)
  rr
}

proxy_inventory <- function(manifest, root_dir) {
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    rr <- manifest[i,,drop=FALSE]
    p <- find_proxy_file(rr, root_dir)
    ready <- proxy_row_auto_fill(rr)
    data.frame(
      section=rr$section[1],
      variable=rr$variable[1],
      record_field=rr$record_field[1],
      file=if (is.na(p)) rr$file_name[1] else basename(p),
      found=!is.na(p),
      auto_fill=ready,
      mapping=rr$mapping[1],
      units=rr$units[1],
      notes=rr$notes[1],
      status=if (is.na(p)) "FILE MISSING" else if (ready) "READY" else "REVIEW REQUIRED",
      stringsAsFactors=FALSE
    )
  })
  do.call(rbind, rows)
}

get_cached_proxy_raster <- function(path) {
  if (!requireNamespace("terra", quietly=TRUE)) return(NULL)
  key <- gsub("[^A-Za-z0-9_]", "_", normalizePath(path, winslash="/", mustWork=TRUE))
  if (!exists(key, envir=.proxy_raster_cache, inherits=FALSE)) {
    assign(key, terra::rast(path), envir=.proxy_raster_cache)
  }
  get(key, envir=.proxy_raster_cache, inherits=FALSE)
}

sherpa_lithology_display <- function(x) {
  x <- char_or_na(x)
  if (is.na(x)) return("MISSING")
  labels <- c(
    base_rich="Calcareous / carbonate-bearing / siliceous base-rich",
    medium_base="Siliceous medium-base-content",
    high_sandy="Expected sand >85% / high-sandy class"
  )
  if (x %in% names(labels)) unname(labels[[x]]) else x
}

map_proxy_category <- function(raw_value, mapping) {
  raw <- suppressWarnings(as.numeric(raw_value[1]))
  if (length(raw)==0 || is.na(raw)) {
    return(list(value=NA, status="PROXY_EU_GAP", detail="No category value."))
  }

  if (safe_eq(mapping, "koppen_geiger_original_30class")) {
    # Beck et al. Köppen-Geiger V3 original 30-class legend.
    # The source class is preserved; SHERPA routing is applied only at runtime.
    kg <- c(
      "1"="Af","2"="Am","3"="Aw","4"="BWh","5"="BWk","6"="BSh","7"="BSk",
      "8"="Csa","9"="Csb","10"="Csc","11"="Cwa","12"="Cwb","13"="Cwc",
      "14"="Cfa","15"="Cfb","16"="Cfc","17"="Dsa","18"="Dsb","19"="Dsc",
      "20"="Dsd","21"="Dwa","22"="Dwb","23"="Dwc","24"="Dwd",
      "25"="Dfa","26"="Dfb","27"="Dfc","28"="Dfd","29"="ET","30"="EF"
    )
    sym <- unname(kg[as.character(as.integer(raw))])
    if (length(sym)==0 || is.na(sym)) {
      return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                  detail=paste0("Unknown Köppen-Geiger raster code ", raw, ".")))
    }
    if (sym %in% c("Csa","Cfb","Dfb","Dfc")) {
      return(list(value=sym, status="OK",
                  detail=paste0("Original Köppen-Geiger class ", sym,
                                " -> published SHERPA normal forest climate route.")))
    }
    if (sym == "ET") {
      return(list(value="ET", status="OK",
                  detail="Original Köppen-Geiger class ET -> published SHERPA high-sandy/high-altitude route."))
    }
    return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                detail=paste0("Original Köppen-Geiger class ", sym,
                              " is not explicitly assigned by the published SHERPA forest decision tree.")))
  }

  if (safe_eq(mapping, "sherpa_climate_strict")) {
    # Backward compatibility with the earlier derived 0/1/2 climate-routing raster.
    if (safe_eq(raw, 1)) return(list(value="SHERPA_NORMAL", status="OK",
                                     detail="Strict SHERPA climate class: normal Csa/Cfb/Dfb/Dfc route."))
    if (safe_eq(raw, 2)) return(list(value="ET", status="OK",
                                     detail="Strict SHERPA climate class: ET route."))
    return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                detail=paste0("Strict climate raster code ", raw, " is unresolved/unsupported.")))
  }

  if (safe_eq(mapping, "isiklik_original_123class_provisional")) {
    # Original detailed Isik/EGDI lithology code is retained in the raster.
    # SHERPA grouping is performed at runtime using the transparent crosswalk CSV.
    cw <- load_lithology_crosswalk()
    if (is.null(cw)) {
      return(list(value=NA, status="PROXY_REVIEW_REQUIRED",
                  detail="Lithology crosswalk CSV is missing from config/."))
    }

    code <- as.integer(round(raw))
    rr <- cw[cw$source_code == code, , drop=FALSE]
    if (nrow(rr) != 1) {
      return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                  detail=paste0("Original lithology code ", code,
                                " has no unique SHERPA crosswalk entry.")))
    }

    label <- rr$source_lithology[1]
    cls <- rr$sherpa_proxy_class[1]
    conf <- rr$confidence[1]

    if (safe_eq(cls, "base_rich")) {
      return(list(value="base_rich", status="OK",
                  detail=paste0("Original lithology: ", label, " [code ", code,
                                "] -> SHERPA base-rich/calcareous; confidence ", conf, ".")))
    }
    if (safe_eq(cls, "medium_base")) {
      return(list(value="medium_base", status="OK",
                  detail=paste0("Original lithology: ", label, " [code ", code,
                                "] -> SHERPA siliceous medium-base; confidence ", conf, ".")))
    }
    if (safe_eq(cls, "sandy_hint")) {
      return(list(value=NA, status="PROXY_EU_SANDY_HINT",
                  detail=paste0("Original lithology: ", label, " [code ", code,
                                "] -> sandy lithology hint; independent sand % controls the >=85% SHERPA route.")))
    }

    return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                detail=paste0("Original lithology: ", label, " [code ", code,
                              "] is unresolved for automatic SHERPA routing; confidence ", conf, ".")))
  }

  if (safe_eq(mapping, "sherpa_lithology_proxy")) {
    # Backward compatibility with the earlier derived 0/1/2/3 lithology raster.
    if (safe_eq(raw, 1)) return(list(value="base_rich", status="OK",
                                     detail="SHERPA lithology proxy: base-rich/calcareous."))
    if (safe_eq(raw, 2)) return(list(value="medium_base", status="OK",
                                     detail="SHERPA lithology proxy: siliceous medium-base."))
    if (safe_eq(raw, 3)) return(list(value=NA, status="PROXY_EU_SANDY_HINT",
                                     detail="Lithology indicates sandy material; independent sand % controls the >=85% SHERPA route."))
    return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                detail=paste0("Lithology raster code ", raw, " is unresolved.")))
  }

  if (safe_eq(mapping, "usda_texture_lookup_v1_8_10")) {
    cw <- load_usda_texture_crosswalk()
    if (is.null(cw)) {
      return(list(value=NA, status="PROXY_REVIEW_REQUIRED",
                  detail="USDA texture crosswalk CSV is missing from config/."))
    }
    code <- as.integer(round(raw))
    rr <- cw[cw$value == code, , drop=FALSE]
    if (nrow(rr) != 1) {
      return(list(value=NA, status="PROXY_EU_UNSUPPORTED_CLASS",
                  detail=paste0("USDA texture code ", code, " is not present in the supplied 1-12 legend.")))
    }
    return(list(value=rr$sherpa_texture_group[1], status="OK",
                detail=paste0("USDA texture ", rr$usda_name[1], " [code ", code,
                              "] -> SHERPA group: ", rr$sherpa_texture_group[1],
                              "; mapping status: ", rr$mapping_status[1], ".")))
  }

  if (safe_eq(mapping, "usda_texture_lookup")) {
    return(list(value=NA, status="PROXY_REVIEW_REQUIRED",
                detail="Legacy USDA texture mapping is not active; use v1.8.10 crosswalk."))
  }

  list(value=NA, status="PROXY_REVIEW_REQUIRED",
       detail=paste0("No categorical mapping is implemented for '", mapping, "'."))
}

extract_proxy_value <- function(variable, lon, lat, manifest, root_dir,
                                permit_review=FALSE) {
  lon <- num_or_na(lon)
  lat <- num_or_na(lat)
  rr <- proxy_manifest_row(variable, manifest)

  make_log <- function(status, note, path=NA_character_, raw=NA, value=NA, source=status) {
    data.frame(
      variable=variable,
      record_field=if (is.null(rr)) "" else rr$record_field[1],
      status=status,
      source=source,
      raw_value=if (length(raw)==0 || is.na(raw[1])) "" else as.character(raw[1]),
      resolved_value=if (length(value)==0 || is.na(value[1])) "" else as.character(value[1]),
      file=if (is.na(path)) "" else basename(path),
      note=note,
      stringsAsFactors=FALSE
    )
  }

  if (is.null(rr)) {
    lg <- make_log("MANIFEST_MISSING", paste0(variable, ": no unique manifest entry."), source="MISSING")
    return(list(value=NA, source="MISSING", log=lg))
  }
  if (is.na(lon) || is.na(lat)) {
    lg <- make_log("COORDINATES_MISSING", paste0(variable, ": WGS84 longitude/latitude missing."), source="MISSING")
    return(list(value=NA, source="MISSING", log=lg))
  }
  if (!proxy_row_auto_fill(rr) && !isTRUE(permit_review)) {
    lg <- make_log("PROXY_REVIEW_REQUIRED",
                   paste0(variable, ": raster inventoried but not auto-used. ", rr$notes[1]),
                   source="PROXY_REVIEW_REQUIRED")
    return(list(value=NA, source="PROXY_REVIEW_REQUIRED", log=lg))
  }

  path <- find_proxy_file(rr, root_dir)
  if (is.na(path)) {
    section_root <- proxy_root_for_section(root_dir, rr$section[1])
    lg <- make_log("PROXY_FILE_MISSING",
                   paste0(variable, ": raster not found under ",
                          ifelse(is.na(section_root), "the selected folder", section_root),
                          ". Expected e.g. ", rr$file_name[1], "."),
                   source="PROXY_FILE_MISSING")
    return(list(value=NA, source="PROXY_FILE_MISSING", log=lg))
  }
  if (!requireNamespace("terra", quietly=TRUE)) {
    lg <- make_log("TERRA_NOT_INSTALLED",
                   paste0(variable, ": install R package 'terra' to use raster proxies."),
                   path, source="PROXY_NOT_AVAILABLE")
    return(list(value=NA, source="PROXY_NOT_AVAILABLE", log=lg))
  }

  r <- get_cached_proxy_raster(path)
  if (is.null(r)) {
    lg <- make_log("RASTER_OPEN_FAILED", paste0(variable, ": raster could not be opened."),
                   path, source="PROXY_NOT_AVAILABLE")
    return(list(value=NA, source="PROXY_NOT_AVAILABLE", log=lg))
  }

  pt <- terra::vect(data.frame(lon=lon, lat=lat), geom=c("lon","lat"), crs="EPSG:4326")
  pt <- tryCatch(terra::project(pt, terra::crs(r)), error=function(e) NULL)
  if (is.null(pt)) {
    lg <- make_log("CRS_TRANSFORM_FAILED",
                   paste0(variable, ": WGS84 point could not be transformed to raster CRS."),
                   path, source="PROXY_NOT_AVAILABLE")
    return(list(value=NA, source="PROXY_NOT_AVAILABLE", log=lg))
  }

  ex <- tryCatch(terra::extract(r, pt, method="simple"), error=function(e) NULL)
  if (is.null(ex) || nrow(ex)<1 || ncol(ex)<2 || is.na(ex[1,2])) {
    lg <- make_log("PROXY_EU_GAP",
                   paste0(variable, ": raster exists but has no value at the supplied location. No neighbouring-cell filling was applied."),
                   path, source="PROXY_EU_GAP")
    return(list(value=NA, source="PROXY_EU_GAP", log=lg))
  }

  raw <- ex[1,2]
  provenance <- if ("provenance" %in% names(rr) && !is.na(rr$provenance[1]) && nzchar(rr$provenance[1])) rr$provenance[1] else "PROXY_EU"

  if (safe_eq(rr$value_type[1], "numeric")) {
    value <- suppressWarnings(as.numeric(raw))
    if (is.na(value)) {
      lg <- make_log("PROXY_EU_GAP", paste0(variable, ": extracted value is not numeric."),
                     path, raw, source="PROXY_EU_GAP")
      return(list(value=NA, source="PROXY_EU_GAP", log=lg))
    }
    note <- paste0(variable, ": ", rr$reference[1],
                   if (!is.na(rr$resolution[1]) && nzchar(rr$resolution[1])) paste0(" (",rr$resolution[1],")") else "", ".")
    lg <- make_log("OK", note, path, raw, value, provenance)
    return(list(value=value, source=provenance, log=lg))
  }

  mapped <- map_proxy_category(raw, rr$mapping[1])
  if (!safe_eq(mapped$status, "OK")) {
    lg <- make_log(mapped$status, paste0(variable, ": ", mapped$detail), path, raw,
                   source=mapped$status)
    return(list(value=NA, source=mapped$status, log=lg))
  }
  lg <- make_log("OK", paste0(variable, ": ", mapped$detail), path, raw, mapped$value, provenance)
  list(value=mapped$value, source=provenance, log=lg)
}

field_present <- function(x) {
  if (is.null(x) || length(x)==0 || is.na(x[1])) return(FALSE)
  if (is.character(x)) return(nzchar(x[1]) && x[1] != "unknown")
  TRUE
}

fill_proxy_field <- function(record, sources, record_field, manifest_variable,
                             lon, lat, manifest, root_dir) {
  if (field_present(record[[record_field]])) {
    return(list(record=record, sources=sources, log=NULL))
  }
  ex <- extract_proxy_value(manifest_variable, lon, lat, manifest, root_dir)
  if (field_present(ex$value)) {
    record[[record_field]] <- ex$value
    sources[[record_field]] <- ex$source
  } else {
    sources[[record_field]] <- ex$source
  }
  list(record=record, sources=sources, log=ex$log)
}

resolve_forest_routing_proxies <- function(record, sources, manifest, root_dir) {
  logs <- list()
  if (!isTRUE(record$use_proxy)) return(list(record=record,sources=sources,log=NULL))
  do_fill <- function(field, variable) {
    z <- fill_proxy_field(record, sources, field, variable,
                          record$lon, record$lat, manifest, root_dir)
    record <<- z$record
    sources <<- z$sources
    if (!is.null(z$log)) logs[[length(logs)+1]] <<- z$log
  }
  do_fill("forest_climate", "climate_class")
  do_fill("altitude_m", "altitude_m")
  do_fill("sand_pct", "sand_pct")
  do_fill("bedrock_class", "lithology_class")
  list(record=record, sources=sources,
       log=if (length(logs)) do.call(rbind,logs) else NULL)
}

resolve_optional_proxies <- function(record, sources, manifest, root_dir) {
  if (!isTRUE(record$use_proxy)) return(list(record=record,sources=sources,notes=character(0),log=NULL))

  logs <- list()
  notes <- character(0)
  route <- route_land_use(record)
  do_fill <- function(field, variable) {
    z <- fill_proxy_field(record, sources, field, variable,
                          record$lon, record$lat, manifest, root_dir)
    record <<- z$record
    sources <<- z$sources
    if (!is.null(z$log)) logs[[length(logs)+1]] <<- z$log
  }

  # Part 1 proxies: forest routing only.
  if (route == "forest") {
    do_fill("forest_climate", "climate_class")
    do_fill("altitude_m", "altitude_m")
    do_fill("sand_pct", "sand_pct")
    do_fill("bedrock_class", "lithology_class")
  }

  # Erosion. Raster 2 is all erosion processes; it is not a cropland-only map.
  # SHERPA nevertheless uses the cumulative/all-process input for cropland and
  # water-only erosion for permanent grassland and forest.
  if (route == "cropland" && !field_present(record$erosion_total)) {
    do_fill("erosion_total", "erosion_all")
    if (field_present(record$erosion_total)) record$erosion_method <- "direct"
  }
  if (route %in% c("grassland","forest") && !field_present(record$erosion_water)) {
    do_fill("erosion_water", "erosion_water")
    if (field_present(record$erosion_water)) record$erosion_method <- "components"
  }

  # Landslide inventory is currently review-required.
  if (!field_present(record$landslide_density)) {
    do_fill("landslide_density", "landslide_density")
    if (field_present(record$landslide_density)) record$landslide_method <- "detailed_density"
  }

  # Heavy metals: representative/bulk proxy values only.
  for (m in c("Cu","Hg","Zn","Cd","Ni","Pb","Sb","As","Cr","Co")) {
    field <- paste0(m,"_bulk")
    if (!field_present(record[[field]])) do_fill(field,m)
  }
  if (safe_eq(record$metal_input_mode, "depth_resolved")) {
    notes <- c(notes, "EU heavy-metal proxies are representative/bulk values; depth-resolved USER observations retain priority.")
  }

  # Nitrogen proxy only when the local route cannot be calculated.
  local_n_available <- FALSE
  if (route == "forest") local_n_available <- field_present(record$n_atmospheric_5y)
  if (route == "grassland") local_n_available <- field_present(record$n_total_inputs_5y)
  if (route == "cropland") {
    harvest_ok <- field_present(record$n_harvest_export) || field_present(record$crop_name)
    balance_ok <- all(vapply(list(record$n_atmospheric_annual,record$n_mineral_fertilizer,record$n_organic_fertilizer),field_present,logical(1))) && harvest_ok
    local_n_available <- field_present(record$n_surplus_direct) || balance_ok
  }
  if (!local_n_available && !field_present(record$n_surplus_batool)) {
    do_fill("n_surplus_batool", "n_surplus_batool")
    if (field_present(record$n_surplus_batool)) record$n_method <- "batool_proxy"
  }

  # Phosphorus proxy uses the original SI1 unit: P balance in kg P ha-1 yr-1.
  if (!field_present(record$p_balance_direct)) do_fill("p_balance_direct", "p_balance")

  # Pesticide development proxy: if the user did not choose a raw pesticide
  # method, use a pre-classified SHERPA severity raster (0-9). This is explicitly
  # provisional and recorded as PROXY_EU_IMPUTED.
  pesticide_user <- field_present(record$pesticide_method) && !safe_eq(record$pesticide_method,"none")
  if (!pesticide_user && !field_present(record$pesticide_proxy_score)) {
    do_fill("pesticide_proxy_score", "pesticide")
    if (field_present(record$pesticide_proxy_score)) record$pesticide_method <- "proxy_score"
  }

  # Salinisation proxy supplies EC only. Natural/anthropogenic context remains
  # a USER decision because SI1 Table S1.2.6.1 routes natural/geogenic cases out.
  if (!field_present(record$ec)) do_fill("ec", "salinisation")

  # SOC loss only; agricultural SOC % and clay % remain USER-required.
  if (!field_present(record$soc_loss_9y)) do_fill("soc_loss_9y", "soc_loss_9y")

  # Agricultural bulk-density proxy. USDA texture codes 1-12 are mapped
  # using the user-supplied legend and the v1.8.10 SHERPA crosswalk.
  if (route %in% c("cropland","grassland")) {
    if (!field_present(record$bulk_density)) do_fill("bulk_density", "bulk_density")
    if (!field_present(record$bulk_density_texture_group)) do_fill("bulk_density_texture_group", "soil_texture")
    if (field_present(record$bulk_density) && field_present(record$bulk_density_texture_group) && !field_present(record$compaction_method)) {
      record$compaction_method <- "bulk_density_proxy"
    }
  }

  logdf <- if (length(logs)) do.call(rbind,logs) else NULL
  if (!is.null(logdf)) {
    issue <- logdf$status %in% c("PROXY_EU_GAP","PROXY_EU_UNSUPPORTED_CLASS","PROXY_EU_SANDY_HINT","PROXY_FILE_MISSING","PROXY_REVIEW_REQUIRED")
    if (any(issue)) notes <- c(notes,"One or more requested EU proxy values were unavailable, unsupported, absent from the raster folder, or held for metadata review. See the EU proxy extraction log in Results.")
  }

  list(record=record,sources=sources,notes=unique(notes),log=logdf)
}
