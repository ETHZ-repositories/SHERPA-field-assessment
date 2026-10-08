# SHERPA Part 2: degradation-process assessment ----------------------------

score_erosion_severity <- function(rate) {
  if (is.na(rate) || rate < 0) return(NA_real_)
  severity_from_upper_bounds(rate, c(0.5,1,2,3,4,5,6,8,10))
}

erosion_rate_from_record <- function(record, route) {
  method <- char_or_na(record$erosion_method)
  if (is.na(method)) return(NA_real_)

  if (method == "direct") {
    return(record$erosion_total)
  }

  if (method == "components") {
    if (route == "cropland") {
      req <- c(record$erosion_water, record$erosion_wind,
               record$erosion_tillage, record$erosion_harvest)
      if (any(is.na(req))) return(NA_real_)
      total <- sum(req)
      if (isTRUE(record$include_postfire_erosion)) {
        if (is.na(record$erosion_postfire)) return(NA_real_)
        total <- total + record$erosion_postfire
      }
      return(total)
    }
    if (route %in% c("grassland","forest")) {
      return(record$erosion_water)
    }
  }
  NA_real_
}

score_landslide_detailed <- function(x) {
  if (is.na(x) || x < 0) return(NA_real_)
  if (x == 0) return(0)
  if (x <= 1) return(1)
  if (x <= 2) return(2)
  if (x <= 4) return(3)
  if (x <= 6) return(4)
  if (x <= 10) return(5)
  if (x <= 25) return(6)
  if (x <= 30) return(7)
  if (x <= 35) return(8)
  9
}

score_landslide_coarse <- function(x) {
  if (is.na(x)) return(NA_real_)
  map <- c(none=0, low=1, medium=4, high=6)
  if (!x %in% names(map)) return(NA_real_)
  unname(map[[x]])
}

score_heavy_metal <- function(metal, values, metal_cfg) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]
  if (length(values) == 0 || any(values < 0)) return(NA_real_)
  row <- metal_cfg[metal_cfg$metal == metal, , drop=FALSE]
  if (nrow(row) != 1) return(NA_real_)
  thresholds <- as.numeric(row[1, paste0("max_score_",0:8)])
  # SI1: negative score applies if concentration exceeds class boundary in
  # any upper surface/main rooting layer. For depth-resolved input, use worst layer.
  max(vapply(values, function(v) severity_from_upper_bounds(v, thresholds), numeric(1)))
}

score_n_common <- function(x) {
  if (is.na(x)) return(NA_real_)
  if (x < 0) return(NA_real_)
  # SI1 forest/Batool tables print "<2" then ">2"; v1.1 closes the exact-2 gap as score 0.
  if (x <= 2) return(0)
  if (x <= 3) return(1)
  if (x <= 4) return(2)
  if (x <= 5) return(3)
  if (x <= 10) return(4)
  if (x <= 15) return(5)
  if (x <= 20) return(6)
  if (x <= 25) return(7)
  if (x <= 30) return(8)
  9
}

score_n_grassland_local <- function(x) {
  if (is.na(x) || x < 0) return(NA_real_)
  if (x <= 2) return(0)
  if (x <= 3) return(1)
  if (x <= 5) return(2)
  if (x <= 10) return(3)
  if (x <= 15) return(4)
  if (x <= 20) return(5)
  if (x <= 30) return(6)
  if (x <= 40) return(7)
  if (x <= 50) return(8)
  9
}

calculate_n_value <- function(record, route, crop_cfg) {
  method <- char_or_na(record$n_method)
  if (is.na(method)) return(list(value=NA_real_, severity=NA_real_, note="Nitrogen method not selected."))

  if (method == "batool_proxy") {
    x <- record$n_surplus_batool
    return(list(value=x, severity=score_n_common(x),
                note="Table S1.2.3.4 / Batool et al. 5-year nitrogen-surplus route."))
  }

  if (method == "local") {
    if (route == "forest") {
      x <- record$n_atmospheric_5y
      return(list(value=x, severity=score_n_common(x),
                  note="Forest local route: 5-year mean total atmospheric N deposition (Table S1.2.3.1.1)."))
    }
    if (route == "grassland") {
      x <- record$n_total_inputs_5y
      return(list(value=x, severity=score_n_grassland_local(x),
                  note="Permanent-grassland local route: 5-year mean deposition + mineral and organic fertiliser (Table S1.2.3.2)."))
    }
    if (route == "cropland") {
      x <- record$n_surplus_direct
      used_lookup <- FALSE
      if (is.na(x)) {
        harvest <- record$n_harvest_export
        if (is.na(harvest) && !is.na(record$crop_name)) {
          rr <- crop_cfg[crop_cfg$crop == record$crop_name, , drop=FALSE]
          if (nrow(rr) == 1) {
            harvest <- rr$n_yield_kgN_ha_yr[1]
            used_lookup <- TRUE
          }
        }
        vals <- c(record$n_atmospheric_annual, record$n_mineral_fertilizer,
                  record$n_organic_fertilizer, harvest)
        if (!any(is.na(vals))) {
          x <- vals[1] + vals[2] + vals[3] - vals[4]
        }
      }
      note <- "Cropland local route: deposition + mineral/organic fertiliser - harvested N (Table S1.2.3.3)."
      if (used_lookup) note <- paste(note, "Harvested N was taken from Table S1.2.3.3A crop lookup.")
      return(list(value=x, severity=score_n_common(x), note=note))
    }
  }

  list(value=NA_real_, severity=NA_real_, note="Nitrogen route unresolved.")
}

calculate_p_balance <- function(record) {
  if (!is.na(record$p_balance_direct)) return(record$p_balance_direct)
  vals <- c(record$p_mineral_input, record$p_organic_input, record$p_harvest_export)
  if (any(is.na(vals))) return(NA_real_)
  vals[1] + vals[2] - vals[3]
}

score_p_excess <- function(balance) {
  if (is.na(balance)) return(NA_real_)
  if (balance <= 1) return(0)
  if (balance <= 2) return(1)
  if (balance <= 3) return(2)
  if (balance <= 4) return(3)
  if (balance <= 5) return(4)
  if (balance <= 6) return(5)
  if (balance <= 7) return(6)
  if (balance <= 8) return(7)
  if (balance <= 9) return(8)
  9
}

score_p_mining <- function(balance) {
  if (is.na(balance)) return(NA_real_)
  if (balance >= -0.5) return(0)
  if (balance >= -1) return(1)
  if (balance >= -2) return(2)
  if (balance >= -3) return(3)
  if (balance >= -4) return(4)
  if (balance >= -5) return(5)
  if (balance >= -6) return(6)
  if (balance >= -7) return(7)
  if (balance >= -8) return(8)
  9
}

score_pli <- function(x, method, pli_cfg) {
  if (is.na(x) || x < 0 || is.na(method) || !method %in% names(pli_cfg)[-1]) return(NA_real_)
  thresholds <- as.numeric(pli_cfg[[method]])

  # AI4SoilHealth v1.7 convention agreed during scientific review:
  # negligible PLI values <= 0.01 score 0. Above 0.01, each printed SI1
  # score/value anchor is treated as the inclusive upper boundary of its class.
  # The same interval logic is applied to PLIFate, PLITotal and PLIUK.
  if (x <= 0.01) return(0)
  for (i in 2:length(thresholds)) {
    if (x <= thresholds[i]) return(i - 1)
  }
  9
}

score_pesticide_tang <- function(rs, ai) {
  if (is.na(rs) || is.na(ai) || ai < 0) return(NA_real_)
  if (rs <= 0 && ai == 0) return(0)
  if (rs <= 0 && ai > 0) return(1)
  if (rs > 0 && rs <= 1 && ai > 0 && ai <= 10) return(2)
  if (rs > 1 && rs <= 2 && ai > 1 && ai <= 10) return(3)
  if (rs > 1 && rs <= 2 && ai > 10 && ai <= 20) return(4)
  if (rs > 2 && rs <= 3 && ai > 1 && ai <= 10) return(5)
  if (rs > 2 && rs <= 3 && ai > 10 && ai <= 20) return(6)
  if (rs > 3 && rs <= 4 && ai > 1 && ai <= 10) return(7)
  if (rs > 3 && rs <= 4 && ai > 10 && ai <= 20) return(8)
  if (rs > 4 && ai > 10) return(9)
  NA_real_
}

score_pesticide <- function(record, pli_cfg) {
  method <- char_or_na(record$pesticide_method)
  if (!is.na(method) && identical(method, "Tang_RS_AI")) {
    return(score_pesticide_tang(record$pesticide_rs, record$pesticide_ai))
  }
  if (!is.na(method) && method %in% c("PLIFate","PLITotal","PLIUK")) {
    return(score_pli(record$pesticide_pli_value, method, pli_cfg))
  }
  if (!is.na(method) && identical(method, "proxy_score")) {
    x <- num_or_na(record$pesticide_proxy_score)
    if (is.na(x) || x < 0 || x > 9) return(NA_real_)
    return(x)
  }
  NA_real_
}

score_salinity <- function(context, ec) {
  if (is.na(context)) return(NA_real_)
  if (context %in% c("calcareous_natural","marine_influence","semiarid_nonirrigated")) {
    # SI1 Table S1.2.6.1 routes natural/geogenic salinity to Part 1 rather than
    # assigning a Part 2 salinisation severity. Represent this as NA and route
    # the component out of the Part 2 denominator in calculate_part2().
    return(NA_real_)
  }
  if (context != "anthropogenic_irrigated") return(NA_real_)
  if (is.na(ec) || ec < 0) return(NA_real_)
  if (ec < 2) return(0)
  if (ec < 2.2) return(1)
  if (ec < 2.4) return(2)
  if (ec < 2.6) return(3)
  if (ec < 2.9) return(4)
  if (ec < 3.1) return(5)
  if (ec < 3.4) return(6)
  if (ec < 3.7) return(7)
  if (ec < 4.0) return(8)
  9
}

score_compaction_forest_surface <- function(pct) {
  if (is.na(pct) || pct < 0 || pct > 100) return(NA_real_)
  if (pct == 0) return(0)
  if (pct <= 1) return(1)
  if (pct < 4) return(2)
  if (pct < 7) return(3)
  if (pct < 10) return(4)
  if (pct < 13) return(5)
  if (pct < 16) return(6)
  if (pct < 19) return(7)
  if (pct < 22) return(8)
  9
}

score_compaction_ag_surface <- function(pct) {
  if (is.na(pct) || pct < 0 || pct > 100) return(NA_real_)
  if (pct == 0) return(0)
  if (pct <= 5) return(1)
  if (pct <= 10) return(2)
  if (pct <= 15) return(3)
  if (pct <= 20) return(4)
  if (pct <= 30) return(5)
  if (pct <= 40) return(6)
  if (pct <= 50) return(7)
  if (pct <= 60) return(8)
  9
}

score_compaction_subsoil <- function(class_score) {
  if (is.na(class_score)) return(NA_real_)
  if (class_score < 0 || class_score > 9) return(NA_real_)
  as.numeric(class_score)
}

score_bulk_density_proxy <- function(bd, texture_group, bd_cfg) {
  if (is.na(bd) || bd < 0 || is.na(texture_group)) return(NA_real_)
  row <- bd_cfg[bd_cfg$source_group == texture_group, , drop=FALSE]
  if (nrow(row) != 1) return(NA_real_)
  suitable <- as.numeric(row$suitable_lt[1])
  restrictive <- as.numeric(row$restricts_gt[1])
  # Cross-check with the R repository linked by the paper:
  # 0 below suitable threshold; 3 from suitable through restrictive; 9 above restrictive.
  if (bd < suitable) return(0)
  if (bd <= restrictive) return(3)
  9
}

calculate_compaction <- function(record, route, bd_cfg) {
  method <- char_or_na(record$compaction_method)
  if (is.na(method)) return(list(severity=NA_real_, sub_surface=NA_real_, sub_subsoil=NA_real_, note="Compaction method not selected."))

  if (method == "field") {
    if (route == "forest") {
      s <- score_compaction_forest_surface(record$compaction_surface_pct)
      return(list(severity=s, sub_surface=s, sub_subsoil=NA_real_,
                  note="Forest field compaction: Table S1.2.7.1."))
    }
    if (route %in% c("cropland","grassland")) {
      s1 <- score_compaction_ag_surface(record$compaction_surface_pct)
      s2 <- score_compaction_subsoil(record$compaction_subsoil_class)
      # v1.7 scientific decision: the agricultural compaction process is the
      # arithmetic mean of the surface and subsoil scores. Both sub-scores are
      # required; if either is missing, compaction is reported as missing.
      sev <- if (!is.na(s1) && !is.na(s2)) mean(c(s1, s2)) else NA_real_
      return(list(severity=sev, sub_surface=s1, sub_subsoil=s2,
                  note="Agricultural field compaction: mean(surface Table S1.2.7.2, subsoil Table S1.2.7.3); both sub-scores are required."))
    }
  }

  if (method == "bulk_density_proxy") {
    if (!route %in% c("cropland","grassland")) {
      return(list(severity=NA_real_, sub_surface=NA_real_, sub_subsoil=NA_real_,
                  note="Bulk-density proxy is only used for cropland/grassland in the published European implementation."))
    }
    s <- score_bulk_density_proxy(record$bulk_density, record$bulk_density_texture_group, bd_cfg)
    return(list(severity=s, sub_surface=NA_real_, sub_subsoil=NA_real_,
                note="European compaction proxy: SI1 Table S1.2.7.4 (bulk density by texture)."))
  }

  list(severity=NA_real_, sub_surface=NA_real_, sub_subsoil=NA_real_, note="Compaction route unresolved.")
}

score_soc_content <- function(soc) {
  if (is.na(soc) || soc < 0) return(NA_real_)
  # SI1 Table S1.2.8.3 contains overlapping printed intervals.
  # Boundaries below follow the non-overlapping implementation in the repository
  # linked by the paper and the ordered score sequence.
  if (soc > 5) return(0)
  if (soc > 4.5) return(1)
  if (soc > 4.0) return(2)
  if (soc > 3.5) return(3)
  if (soc > 3.0) return(4)
  if (soc > 2.5) return(5)
  if (soc > 2.0) return(6)
  if (soc > 1.5) return(7)
  if (soc > 0.5) return(8)
  9
}

score_soc_clay_ratio <- function(soc, clay) {
  if (is.na(soc) || is.na(clay) || soc < 0 || clay <= 0) return(NA_real_)
  r <- soc / clay
  if (r >= 1/8) return(0)
  if (r >= 1/9) return(1)
  if (r >= 1/10) return(3)
  if (r >= 1/11) return(5)
  if (r >= 1/12) return(7)
  if (r >= 1/13) return(8)
  9
}

score_soc_loss <- function(loss9y, current_soc) {
  if (is.na(loss9y) || is.na(current_soc) || current_soc < 0) return(NA_real_)

  # AI4SoilHealth extension for the missing SI1 h1 class (<0.5% SOC):
  # no loss/positive change -> 0; small loss down to -0.5 -> 8;
  # any loss below -0.5 -> 9.
  if (current_soc < 0.5) {
    if (loss9y >= 0) return(0)
    if (loss9y >= -0.5) return(8)
    return(9)
  }

  col <- if (current_soc >= 5) 1 else
    if (current_soc >= 2.5) 2 else
      if (current_soc >= 1) 3 else 4

  mat <- rbind(
    c(0,0,0,0),
    c(1,2,4,6),
    c(2,3,5,7),
    c(3,4,6,8),
    c(4,5,7,9),
    c(5,6,8,9),
    c(6,7,9,9),
    c(7,8,9,9),
    c(8,9,9,9),
    c(9,9,9,9)
  )

  row <- if (loss9y >= 0) 1 else
    if (loss9y >= -0.5) 2 else
      if (loss9y >= -1.5) 3 else
        if (loss9y >= -3) 4 else
          if (loss9y >= -4) 5 else
            if (loss9y >= -5) 6 else
              if (loss9y >= -6) 7 else
                if (loss9y >= -7) 8 else
                  if (loss9y >= -8) 9 else 10
  mat[row,col]
}

calculate_soc <- function(record, route) {
  loss <- score_soc_loss(record$soc_loss_9y, record$soc_pct)

  if (route %in% c("cropland","grassland")) {
    content <- score_soc_content(record$soc_pct)
    ratio <- score_soc_clay_ratio(record$soc_pct, record$clay_pct)
    # v1.7 project decision: local assessment requires all three agricultural
    # SOC sub-indicators. If SOC loss, SOC content or clay is missing, the whole
    # SOC degradation component is NA and is reported in the Part 2 missing-data note.
    if (is.na(loss) || is.na(content) || is.na(ratio)) {
      return(list(severity=NA_real_, loss=loss, ratio=ratio, content=content,
                  note="Agricultural SOC requires SOC loss, SOC content and clay content in v1.7; if any sub-indicator is missing, the SOC component is not scored."))
    }
    return(list(severity=mean(c(loss,ratio,content)), loss=loss, ratio=ratio, content=content,
                note="Agricultural SOC: mean(SOC loss, SOC/clay ratio, SOC content), using all three required sub-scores."))
  }

  if (route == "forest") {
    return(list(severity=loss, loss=loss, ratio=NA_real_, content=NA_real_,
                note="Forest/semi-natural route: SOC loss only (SI1 Table S1.2.8)."))
  }

  list(severity=NA_real_, loss=NA_real_, ratio=NA_real_, content=NA_real_, note="SOC not applicable.")
}

part2_applicable <- function(route, component, applicability_mode, app_cfg) {
  rr <- app_cfg[app_cfg$effective_land_use == route & app_cfg$component == component, , drop=FALSE]
  if (nrow(rr) != 1) return(FALSE)
  if (!is.na(applicability_mode) && identical(applicability_mode, "published_eu")) return(isTRUE(rr$published_eu[1]))
  isTRUE(rr$full_scheme[1])
}

part2_source_status <- function(route, component, app_cfg) {
  rr <- app_cfg[app_cfg$effective_land_use == route & app_cfg$component == component, , drop=FALSE]
  if (nrow(rr) != 1) return(NA_character_)
  rr$s2_1_status[1]
}

source_is_user <- function(sources, field) {
  if (is.null(sources) || is.null(sources[[field]]) || length(sources[[field]])==0) return(FALSE)
  identical(as.character(sources[[field]][1]), "USER")
}

part2_record_applicable <- function(route, component, applicability_mode, app_cfg,
                                    severity = NA_real_, routed_out = FALSE,
                                    local_available = TRUE) {
  if (isTRUE(routed_out)) return(FALSE)
  base <- part2_applicable(route, component, applicability_mode, app_cfg)
  if (!base) return(FALSE)

  status <- part2_source_status(route, component, app_cfg)
  # In the full local-data scheme, SI2 'na' means unavailable in the European
  # evaluation, not irrelevant. Include such a process only when valid local
  # information actually yields a severity; otherwise exclude it from the denominator.
  if (!safe_eq(applicability_mode, "published_eu") && safe_eq(status, "na")) {
    # Approved SHERPA/AI4SoilHealth rule: 'na' may be included only when
    # valid LOCAL/USER information exists. EU proxy data alone do not activate
    # a process that was unavailable in the published European evaluation.
    return(!is.na(severity) && isTRUE(local_available))
  }
  TRUE
}

calculate_part2 <- function(record, route, metal_cfg, crop_cfg, pli_cfg, bd_cfg, app_cfg, sources=NULL) {
  notes <- character(0)
  detail_rows <- list()

  add_component <- function(component, label, severity, detail="", note="", routed_out=FALSE,
                              local_available=TRUE) {
    status <- part2_source_status(route, component, app_cfg)
    applicable <- part2_record_applicable(
      route, component, record$applicability_mode, app_cfg,
      severity = severity, routed_out = routed_out,
      local_available = local_available
    )
    detail_rows[[length(detail_rows)+1]] <<- data.frame(
      component=component,
      label=label,
      applicable=applicable,
      s2_1_status=status,
      severity=if (applicable) severity else NA_real_,
      negative_score=if (applicable) negative_score(severity) else NA_real_,
      detail=if (applicable) detail else "",
      stringsAsFactors=FALSE
    )
    if (!is.null(note) && length(note)>0 && !is.na(note[1]) && nzchar(note[1])) notes <<- c(notes, note[1])
  }

  # Erosion
  erate <- erosion_rate_from_record(record, route)
  add_component("erosion","Soil erosion",score_erosion_severity(erate),
                detail=ifelse(is.na(erate),"",paste0("rate=",format(erate)," t ha-1 yr-1")))

  # Landslide
  lmethod <- char_or_na(record$landslide_method)
  lsev <- if (!is.na(lmethod) && identical(lmethod, "detailed_density")) {
    score_landslide_detailed(record$landslide_density)
  } else if (!is.na(lmethod) && identical(lmethod, "hollis_class")) {
    score_landslide_coarse(record$landslide_class)
  } else NA_real_
  add_component("landslide","Landslide density",lsev)

  # Heavy metals - each is an individual Part 2 component.
  for (m in metal_cfg$metal) {
    vals <- if (safe_eq(record$metal_input_mode, "depth_resolved")) {
      depth_vals <- c(record[[paste0(m,"_0_10")]], record[[paste0(m,"_10_20")]], record[[paste0(m,"_20_30")]])
      # Field depth-resolved observations take precedence. If none are supplied,
      # permit the bulk/reference field (including an EU proxy) as fallback.
      if (all(is.na(depth_vals))) c(record[[paste0(m,"_bulk")]]) else depth_vals
    } else {
      c(record[[paste0(m,"_bulk")]])
    }
    sev <- score_heavy_metal(m, vals, metal_cfg)
    add_component(paste0("metal_",tolower(m)), paste0("Heavy metal - ",m), sev)
  }

  # Nitrogen
  nres <- calculate_n_value(record, route, crop_cfg)
  add_component("nitrogen","Nitrogen surplus / load",nres$severity,
                detail=ifelse(is.na(nres$value),"",paste0("N=",format(nres$value)," kg N ha-1 yr-1")),
                note=nres$note)

  # Phosphorus (two separately scored components in the published cropland implementation)
  pb <- calculate_p_balance(record)
  add_component("p_excess","Phosphorus excess",score_p_excess(pb),
                detail=ifelse(is.na(pb),"",paste0("P balance=",format(pb)," kg P ha-1 yr-1")))
  p_balance_local <- source_is_user(sources,"p_balance_direct") ||
    (source_is_user(sources,"p_mineral_input") && source_is_user(sources,"p_organic_input") &&
     source_is_user(sources,"p_harvest_export"))
  add_component("p_mining","Phosphorus mining",score_p_mining(pb),
                detail=ifelse(is.na(pb),"",paste0("P balance=",format(pb)," kg P ha-1 yr-1")),
                local_available=p_balance_local)

  # Pesticide
  psev <- score_pesticide(record, pli_cfg)
  add_component("pesticide","Pesticide pressure",psev)

  # Salinisation
  ssev <- score_salinity(record$salinity_context, record$ec)
  salinity_routed_out <- safe_in(
    record$salinity_context,
    c("calcareous_natural","marine_influence","semiarid_nonirrigated")
  )
  add_component("salinisation","Salinisation",ssev,
                detail=if (salinity_routed_out) "Natural/geogenic context: routed to Part 1; not included in Part 2." else "",
                routed_out=salinity_routed_out)

  # Compaction
  cres <- calculate_compaction(record, route, bd_cfg)
  compaction_local <- if (route == "forest") {
    source_is_user(sources,"compaction_surface_pct")
  } else TRUE
  add_component("compaction","Compaction",cres$severity,
                detail=paste0("surface=",ifelse(is.na(cres$sub_surface),"NA",cres$sub_surface),
                              "; subsoil=",ifelse(is.na(cres$sub_subsoil),"NA",cres$sub_subsoil)),
                note=cres$note,
                local_available=compaction_local)

  # SOC
  socres <- calculate_soc(record, route)
  soc_local <- if (route == "forest") source_is_user(sources,"soc_loss_9y") else TRUE
  add_component("soc","SOC assessment",socres$severity,
                detail=paste0("SOC-loss severity=",ifelse(is.na(socres$loss),"NA",socres$loss),
                              "; SOC/clay severity=",ifelse(is.na(socres$ratio),"NA",socres$ratio),
                              "; SOC-content severity=",ifelse(is.na(socres$content),"NA",socres$content)),
                note=socres$note,
                local_available=soc_local)

  df <- do.call(rbind, detail_rows)
  applicable_df <- df[df$applicable, , drop=FALSE]
  missing_rows <- applicable_df[is.na(applicable_df$negative_score), , drop=FALSE]
  missing_components <- unique(missing_rows$label)
  if (length(missing_components) > 0) {
    notes <- c(notes, paste0(
      "Incomplete Part 2 assessment: missing applicable component(s): ",
      paste(missing_components, collapse=", "),
      ". Missing components are excluded from the available-score mean and are not treated as zero degradation."
    ))
  }

  agg <- aggregate_part2_negative_scores(
    applicable_df$negative_score,
    aggregation_mode = record$aggregation_mode,
    n_expected = nrow(applicable_df)
  )

  if (safe_eq(record$aggregation_mode, "complete_case") && agg$n_available < agg$n_expected) {
    notes <- c(notes, paste0("Complete-case aggregation selected: ", agg$n_available,
                             " of ", agg$n_expected, " applicable Part 2 components are available. Part 2 is therefore not calculated."))
  }

  list(
    table=df,
    mean=agg$mean_negative,
    severity_magnitude=agg$severity_magnitude,
    n_expected=agg$n_expected,
    n_available=agg$n_available,
    completeness=agg$completeness,
    missing_components=missing_components,
    notes=unique(notes),
    compaction=cres,
    soc=socres,
    p_balance=pb,
    nitrogen=nres
  )
}
