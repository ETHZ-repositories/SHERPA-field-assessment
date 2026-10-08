# SHERPA v1.7 Part 2 source-threshold + approved-decision validation -------
#
# Purpose
# -------
# Compare the implemented raw-input -> Part 2 severity functions against the
# numerical/decision rules printed in Supporting Information 1 (SI1), and the
# published-European applicability flags in Supporting Information 2 (SI2).
#
# PASS = a source-defined rule, original-code rule, or approved v1.7 scientific
#        decision is reproduced.
# FAIL = an enforced rule/decision is not reproduced.
# FLAG = an unresolved scientific ambiguity only. Approved project decisions
#        are no longer flags; they are enforced regression tests.

audit_values_equal <- function(a, b, tol = 1e-10) {
  if (length(a) == 0 || length(b) == 0) return(FALSE)
  a <- a[1]
  b <- b[1]

  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)

  if (is.numeric(a) && is.numeric(b)) {
    return(abs(as.numeric(a) - as.numeric(b)) <= tol)
  }

  identical(as.character(a), as.character(b))
}

audit_value_text <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return("NA")
  if (is.numeric(x)) return(format(as.numeric(x[1]), digits = 12, trim = TRUE))
  as.character(x[1])
}

run_part2_threshold_audit <- function(cfg, tol = 1e-10) {
  rows <- list()

  add_case <- function(component,
                       source_table,
                       case_id,
                       input_value,
                       expected,
                       observed,
                       evidence = "SOURCE_DEFINED",
                       note = "") {
    enforce <- evidence %in% c("SOURCE_DEFINED", "SOURCE_ANCHOR", "ORIGINAL_CODE", "PROJECT_DECISION")
    status <- if (!enforce) {
      "FLAG"
    } else if (audit_values_equal(expected, observed, tol = tol)) {
      "PASS"
    } else {
      "FAIL"
    }

    rows[[length(rows) + 1]] <<- data.frame(
      component = component,
      source_table = source_table,
      case_id = case_id,
      input = as.character(input_value),
      expected = audit_value_text(expected),
      observed = audit_value_text(observed),
      evidence = evidence,
      status = status,
      note = note,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  eps <- 1e-7

  # -------------------------------------------------------------------
  # 2.1 Erosion: Table S1.2.1.1
  erosion_bounds <- c(0.5, 1, 2, 3, 4, 5, 6, 8, 10)
  add_case("Erosion", "S1.2.1.1", "zero", "0", 0, score_erosion_severity(0))
  for (i in seq_along(erosion_bounds)) {
    b <- erosion_bounds[i]
    add_case("Erosion", "S1.2.1.1",
             paste0("boundary_", b),
             paste0("rate=", b),
             i - 1,
             score_erosion_severity(b))
    add_case("Erosion", "S1.2.1.1",
             paste0("just_above_", b),
             paste0("rate=", b, "+eps"),
             min(i, 9),
             score_erosion_severity(b + eps))
  }
  add_case("Erosion", "S1.2.1.1", "negative_invalid", "rate=-0.1",
           NA_real_, score_erosion_severity(-0.1))

  # -------------------------------------------------------------------
  # 2.1 Landslides: Tables S1.2.1.2 and S1.2.1.2.1
  landslide_bounds <- c(1, 2, 4, 6, 10, 25, 30, 35)
  add_case("Landslide", "S1.2.1.2", "none", "density=0", 0,
           score_landslide_detailed(0))
  add_case("Landslide", "S1.2.1.2", "small_positive",
           "density=eps", 1, score_landslide_detailed(eps))
  for (i in seq_along(landslide_bounds)) {
    b <- landslide_bounds[i]
    add_case("Landslide", "S1.2.1.2",
             paste0("boundary_", b),
             paste0("density=", b),
             i,
             score_landslide_detailed(b))
    add_case("Landslide", "S1.2.1.2",
             paste0("just_above_", b),
             paste0("density=", b, "+eps"),
             min(i + 1, 9),
             score_landslide_detailed(b + eps))
  }
  coarse_expected <- c(none = 0, low = 1, medium = 4, high = 6)
  for (nm in names(coarse_expected)) {
    add_case("Landslide", "S1.2.1.2.1",
             paste0("coarse_", nm), paste0("class=", nm),
             unname(coarse_expected[[nm]]),
             score_landslide_coarse(nm))
  }

  # -------------------------------------------------------------------
  # 2.2 Heavy metals: Table S1.2.2
  for (metal in cfg$metals$metal) {
    rr <- cfg$metals[cfg$metals$metal == metal, , drop = FALSE]
    th <- as.numeric(rr[1, paste0("max_score_", 0:8)])
    units <- rr$units[1]

    for (i in 0:8) {
      b <- th[i + 1]
      add_case(
        paste0("Heavy metal ", metal), "S1.2.2",
        paste0("score_", i, "_boundary"),
        paste0(metal, "=", b, " ", units),
        i,
        score_heavy_metal(metal, b, cfg$metals)
      )
      add_case(
        paste0("Heavy metal ", metal), "S1.2.2",
        paste0("just_above_score_", i, "_boundary"),
        paste0(metal, "=", b, "+eps ", units),
        min(i + 1, 9),
        score_heavy_metal(metal, b + eps, cfg$metals)
      )
    }

    # Source specifies contamination in any upper surface/rooting layer:
    # the most severe layer therefore controls the element score.
    probe <- c(th[1] / 2, th[6] + eps, th[3])
    add_case(
      paste0("Heavy metal ", metal), "S1.2.2",
      "worst_depth_layer",
      paste0("three layers incl. one > score-5 boundary"),
      6,
      score_heavy_metal(metal, probe, cfg$metals)
    )
  }

  # -------------------------------------------------------------------
  # 2.3 Nitrogen
  # Cropland local route (S1.2.3.3) is unambiguous at <=2.
  n_common_bounds <- c(2, 3, 4, 5, 10, 15, 20, 25, 30)
  for (i in seq_along(n_common_bounds)) {
    b <- n_common_bounds[i]
    add_case("Nitrogen - cropland", "S1.2.3.3",
             paste0("boundary_", b), paste0("N=", b),
             i - 1, score_n_common(b))
    add_case("Nitrogen - cropland", "S1.2.3.3",
             paste0("just_above_", b), paste0("N=", b, "+eps"),
             min(i, 9), score_n_common(b + eps))
  }

  # Approved v1.7 boundary clarification: close the printed forest/Batool
  # exact-2 gap consistently with cropland/grassland: N <= 2 -> severity 0.
  add_case("Nitrogen - forest", "S1.2.3.1.1",
           "exact_2_project_boundary", "N=2",
           0, score_n_common(2),
           evidence = "PROJECT_DECISION",
           note = "v1.7 decision: N <= 2 -> severity 0.")
  add_case("Nitrogen - Batool route", "S1.2.3.4",
           "exact_2_project_boundary", "N=2",
           0, score_n_common(2),
           evidence = "PROJECT_DECISION",
           note = "v1.7 decision: N <= 2 -> severity 0.")

  # Check unambiguous forest/Batool values on either side of the gap.
  add_case("Nitrogen - forest", "S1.2.3.1.1", "below_2",
           "N=1.999", 0, score_n_common(1.999))
  add_case("Nitrogen - forest", "S1.2.3.1.1", "above_2",
           "N=2.001", 1, score_n_common(2.001))
  add_case("Nitrogen - Batool route", "S1.2.3.4", "above_30",
           "N=31", 9, score_n_common(31))

  # Permanent grassland local route: S1.2.3.2
  n_grass_bounds <- c(2, 3, 5, 10, 15, 20, 30, 40, 50)
  for (i in seq_along(n_grass_bounds)) {
    b <- n_grass_bounds[i]
    add_case("Nitrogen - grassland", "S1.2.3.2",
             paste0("boundary_", b), paste0("N=", b),
             i - 1, score_n_grassland_local(b))
    add_case("Nitrogen - grassland", "S1.2.3.2",
             paste0("just_above_", b), paste0("N=", b, "+eps"),
             min(i, 9), score_n_grassland_local(b + eps))
  }

  # Route/input calculations.
  nrec <- list(
    n_method = "local",
    n_surplus_batool = NA_real_,
    n_atmospheric_5y = NA_real_,
    n_total_inputs_5y = NA_real_,
    n_surplus_direct = 18,
    n_atmospheric_annual = NA_real_,
    n_mineral_fertilizer = NA_real_,
    n_organic_fertilizer = NA_real_,
    n_harvest_export = NA_real_,
    crop_name = NA_character_
  )
  add_case("Nitrogen - cropland", "S1.2.3.3",
           "direct_surplus_route", "direct surplus=18",
           6, calculate_n_value(nrec, "cropland", cfg$crops)$severity)

  nrec$n_surplus_direct <- NA_real_
  nrec$n_atmospheric_annual <- 5
  nrec$n_mineral_fertilizer <- 20
  nrec$n_organic_fertilizer <- 10
  nrec$n_harvest_export <- 17
  add_case("Nitrogen - cropland", "S1.2.3.3",
           "calculated_balance_route", "5+20+10-17=18",
           6, calculate_n_value(nrec, "cropland", cfg$crops)$severity)

  nrec$n_harvest_export <- NA_real_
  nrec$crop_name <- "Wheat"
  nrec$n_mineral_fertilizer <- 100
  nrec$n_organic_fertilizer <- 20
  n_lookup_val <- 5 + 100 + 20 - 108.76
  add_case("Nitrogen - cropland", "S1.2.3.3A",
           "wheat_harvest_lookup",
           "5+100+20-Wheat(108.76)",
           score_n_common(n_lookup_val),
           calculate_n_value(nrec, "cropland", cfg$crops)$severity)

  grec <- nrec
  grec$n_total_inputs_5y <- 4
  add_case("Nitrogen - grassland", "S1.2.3.2",
           "local_route", "5-year inputs=4",
           2, calculate_n_value(grec, "grassland", cfg$crops)$severity)

  frec <- nrec
  frec$n_atmospheric_5y <- 18
  add_case("Nitrogen - forest", "S1.2.3.1.1",
           "local_route", "5-year deposition=18",
           6, calculate_n_value(frec, "forest", cfg$crops)$severity)

  brec <- nrec
  brec$n_method <- "batool_proxy"
  brec$n_surplus_batool <- 18
  add_case("Nitrogen - Batool route", "S1.2.3.4",
           "batool_route", "5-year surplus=18",
           6, calculate_n_value(brec, "cropland", cfg$crops)$severity)

  # -------------------------------------------------------------------
  # 2.4 Phosphorus
  p_excess_bounds <- 1:9
  for (i in seq_along(p_excess_bounds)) {
    b <- p_excess_bounds[i]
    add_case("Phosphorus excess", "S1.2.4.2",
             paste0("boundary_", b), paste0("P balance=", b),
             i - 1, score_p_excess(b))
    add_case("Phosphorus excess", "S1.2.4.2",
             paste0("just_above_", b), paste0("P balance=", b, "+eps"),
             min(i, 9), score_p_excess(b + eps))
  }
  add_case("Phosphorus excess", "S1.2.4.2",
           "above_printed_upper_range", "P balance=11",
           9, score_p_excess(11),
           evidence = "ORIGINAL_CODE",
           note = "v1.7 decision: P excess >9 remains at maximum severity 9, consistent with the original SHERPA code.")

  p_mining_bounds <- c(-0.5, -1, -2, -3, -4, -5, -6, -7, -8)
  for (i in seq_along(p_mining_bounds)) {
    b <- p_mining_bounds[i]
    add_case("Phosphorus mining", "S1.2.4.1",
             paste0("boundary_", abs(b)), paste0("P balance=", b),
             i - 1, score_p_mining(b))
    add_case("Phosphorus mining", "S1.2.4.1",
             paste0("just_below_", abs(b)), paste0("P balance=", b, "-eps"),
             min(i, 9), score_p_mining(b - eps))
  }

  prec <- list(
    p_balance_direct = NA_real_,
    p_mineral_input = 8,
    p_organic_input = 4,
    p_harvest_export = 5
  )
  add_case("Phosphorus balance", "S1.2.4",
           "calculated_balance", "8+4-5",
           7, calculate_p_balance(prec))

  # -------------------------------------------------------------------
  # 2.5 Pesticides: PLI anchor values and Tang table.
  # Approved v1.7 PLI convention: x <= 0.01 -> 0; above 0.01 each printed
  # score/value anchor is the inclusive upper boundary of the class.
  for (method in c("PLIFate", "PLITotal", "PLIUK")) {
    add_case(paste0("Pesticide ", method), "S1.2.5.1",
             "project_zero_boundary", paste0(method, "=0.01"),
             0, score_pli(0.01, method, cfg$pli),
             evidence = "PROJECT_DECISION",
             note = "v1.7 decision: PLI <= 0.01 -> severity 0.")
    add_case(paste0("Pesticide ", method), "S1.2.5.1",
             "just_above_project_zero_boundary", paste0(method, "=0.01+eps"),
             1, score_pli(0.01 + eps, method, cfg$pli),
             evidence = "PROJECT_DECISION")

    for (i in seq_len(nrow(cfg$pli))) {
      x <- as.numeric(cfg$pli[[method]][i])
      score <- as.numeric(cfg$pli$score[i])
      # The printed zero anchor remains score 0; positive anchors are retained.
      add_case(
        paste0("Pesticide ", method), "S1.2.5.1",
        paste0("printed_score_", score),
        paste0(method, "=", x),
        score,
        score_pli(x, method, cfg$pli),
        evidence = "SOURCE_ANCHOR",
        note = "Printed score/value anchor."
      )
    }

    # Test every interval between successive positive printed anchors.
    pos <- as.numeric(cfg$pli[[method]][2:nrow(cfg$pli)])
    for (j in seq_len(length(pos) - 1)) {
      xmid <- mean(c(pos[j], pos[j + 1]))
      add_case(
        paste0("Pesticide ", method), "S1.2.5.1",
        paste0("project_interval_", j),
        paste0(method, "=", xmid),
        j + 1,
        score_pli(xmid, method, cfg$pli),
        evidence = "PROJECT_DECISION",
        note = "v1.7 interval convention: lower printed anchor < x <= next printed anchor."
      )
    }
  }

  tang_cases <- list(
    list(0, 0, 0, "negligible_RS_AI0"),
    list(0, 1, 1, "negligible_RS_AIpos"),
    list(0.5, 5, 2, "low_RS"),
    list(1.5, 5, 3, "medium_low_low_AI"),
    list(1.5, 15, 4, "medium_low_high_AI"),
    list(2.5, 5, 5, "medium_low_AI"),
    list(2.5, 15, 6, "medium_high_AI"),
    list(3.5, 5, 7, "high_low_AI"),
    list(3.5, 15, 8, "high_high_AI"),
    list(5, 15, 9, "very_high")
  )
  for (tc in tang_cases) {
    add_case("Pesticide Tang RS/AI", "S1.2.5.2",
             tc[[4]], paste0("RS=", tc[[1]], "; AI=", tc[[2]]),
             tc[[3]], score_pesticide_tang(tc[[1]], tc[[2]]))
  }
  add_case("Pesticide Tang RS/AI", "S1.2.5.2",
           "unprinted_combination_lowRS_highAI", "RS=0.5; AI=15",
           NA_real_, score_pesticide_tang(0.5, 15),
           evidence = "ORIGINAL_CODE",
           note = "v1.7 decision: reproduce original Tang logic exactly; unmatched combinations remain NA.")
  add_case("Pesticide Tang RS/AI", "S1.2.5.2",
           "unprinted_combination_mediumRS_AI1", "RS=1.5; AI=1",
           NA_real_, score_pesticide_tang(1.5, 1),
           evidence = "ORIGINAL_CODE",
           note = "v1.7 decision: reproduce original Tang logic exactly; unmatched combinations remain NA.")

  # -------------------------------------------------------------------
  # 2.6 Salinisation
  sal_bounds <- c(2.0, 2.2, 2.4, 2.6, 2.9, 3.1, 3.4, 3.7, 4.0)
  # Exact 2.0 is explicitly score 1.
  exact_scores <- 1:9
  for (i in seq_along(sal_bounds)) {
    b <- sal_bounds[i]
    add_case("Salinisation", "S1.2.6.2",
             paste0("boundary_", b), paste0("EC=", b),
             exact_scores[i],
             score_salinity("anthropogenic_irrigated", b))
    if (i > 1) {
      add_case("Salinisation", "S1.2.6.2",
               paste0("just_below_", b), paste0("EC=", b, "-eps"),
               exact_scores[i] - 1,
               score_salinity("anthropogenic_irrigated", b - eps))
    }
  }
  add_case("Salinisation", "S1.2.6.2",
           "below_2", "EC=1.9", 0,
           score_salinity("anthropogenic_irrigated", 1.9))

  for (ctx in c("calcareous_natural", "marine_influence", "semiarid_nonirrigated")) {
    add_case("Salinisation routing", "S1.2.6.1",
             ctx, paste0("context=", ctx),
             NA_real_, score_salinity(ctx, 5),
             evidence = "PROJECT_DECISION",
             note = "v1.7 decision: natural/geogenic salinity is routed to Part 1 and is not scored in Part 2 (NA).")
  }

  # -------------------------------------------------------------------
  # 2.7 Compaction
  forest_comp_bounds <- c(1, 4, 7, 10, 13, 16, 19, 22)
  add_case("Compaction - forest surface", "S1.2.7.1",
           "no_signs", "0%", 0, score_compaction_forest_surface(0))
  add_case("Compaction - forest surface", "S1.2.7.1",
           "small_positive", "eps%", 1, score_compaction_forest_surface(eps))
  forest_boundary_scores <- c(1, 3, 4, 5, 6, 7, 8, 9)
  for (i in seq_along(forest_comp_bounds)) {
    b <- forest_comp_bounds[i]
    add_case("Compaction - forest surface", "S1.2.7.1",
             paste0("boundary_", b), paste0(b, "%"),
             forest_boundary_scores[i],
             score_compaction_forest_surface(b))
  }

  ag_comp_bounds <- c(5, 10, 15, 20, 30, 40, 50, 60)
  add_case("Compaction - agricultural surface", "S1.2.7.2",
           "no_signs", "0%", 0, score_compaction_ag_surface(0))
  add_case("Compaction - agricultural surface", "S1.2.7.2",
           "small_positive", "eps%", 1, score_compaction_ag_surface(eps))
  for (i in seq_along(ag_comp_bounds)) {
    b <- ag_comp_bounds[i]
    add_case("Compaction - agricultural surface", "S1.2.7.2",
             paste0("boundary_", b), paste0(b, "%"),
             i, score_compaction_ag_surface(b))
    add_case("Compaction - agricultural surface", "S1.2.7.2",
             paste0("just_above_", b), paste0(b, "+eps%"),
             min(i + 1, 9), score_compaction_ag_surface(b + eps))
  }

  for (s in 0:9) {
    add_case("Compaction - agricultural subsoil", "S1.2.7.3",
             paste0("qualitative_class_", s),
             paste0("pre-classified qualitative state=", s),
             s, score_compaction_subsoil(s),
             evidence = "SOURCE_ANCHOR",
             note = "The SI1 table is qualitative. The software currently asks the user to select the corresponding 0-9 class; semantic classification of the profile description remains a user/expert judgement.")
  }

  # Bulk-density proxy: source gives three anchor states per texture class,
  # but not a complete continuous interval rule.
  for (i in seq_len(nrow(cfg$bulk_density))) {
    rr <- cfg$bulk_density[i, , drop = FALSE]
    grp <- rr$source_group[1]
    suitable <- as.numeric(rr$suitable_lt[1])
    affects <- as.numeric(rr$affects_root_growth[1])
    restricts <- as.numeric(rr$restricts_gt[1])

    add_case("Compaction - bulk density proxy", "S1.2.7.4",
             paste0(grp, "_below_suitable"),
             paste0("BD=", suitable - eps),
             0, score_bulk_density_proxy(suitable - eps, grp, cfg$bulk_density))
    add_case("Compaction - bulk density proxy", "S1.2.7.4",
             paste0(grp, "_affects_anchor"),
             paste0("BD=", affects),
             3, score_bulk_density_proxy(affects, grp, cfg$bulk_density),
             evidence = "SOURCE_ANCHOR")
    add_case("Compaction - bulk density proxy", "S1.2.7.4",
             paste0(grp, "_above_restricts"),
             paste0("BD=", restricts + eps),
             9, score_bulk_density_proxy(restricts + eps, grp, cfg$bulk_density))
    add_case("Compaction - bulk density proxy", "S1.2.7.4",
             paste0(grp, "_continuous_interval_convention"),
             paste0("BD midpoint suitable/restrictive"),
             3,
             score_bulk_density_proxy(mean(c(suitable, restricts)), grp, cfg$bulk_density),
             evidence = "ORIGINAL_CODE",
             note = "v1.7 decision: retain the original-code 0 / 3 / 9 interval implementation.")
  }

  comprec <- list(compaction_method="field", compaction_surface_pct=16,
                  compaction_subsoil_class=8, bulk_density=NA_real_,
                  bulk_density_texture_group=NA_character_)
  add_case("Compaction aggregation", "S1.2.7.2 + S1.2.7.3",
           "surface_subsoil_mean",
           "surface severity=4; subsoil=8",
           6, calculate_compaction(comprec, "cropland", cfg$bulk_density)$severity,
           evidence = "PROJECT_DECISION",
           note = "v1.7 decision: agricultural compaction = mean(surface, subsoil).")
  comprec$compaction_subsoil_class <- NA_real_
  add_case("Compaction aggregation", "S1.2.7.2 + S1.2.7.3",
           "one_subscore_missing",
           "surface available; subsoil missing",
           NA_real_, calculate_compaction(comprec, "cropland", cfg$bulk_density)$severity,
           evidence = "PROJECT_DECISION",
           note = "v1.7 local-data rule: both surface and subsoil scores are required.")

  # -------------------------------------------------------------------
  # 2.8 SOC
  # S1.2.8.1: matrix of SOC-loss rows x current-SOC classes.
  soc_loss_matrix <- rbind(
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
  loss_reps <- c(0.1, -0.25, -1.0, -2.0, -3.5, -4.5, -5.5, -6.5, -7.5, -8.5)
  soc_reps <- c(5.0, 3.0, 1.5, 0.75)
  soc_names <- c(">=5", "2.5-<5", "1-<2.5", "0.5-<1")

  for (r in seq_along(loss_reps)) {
    for (cc in seq_along(soc_reps)) {
      add_case("SOC loss", "S1.2.8.1",
               paste0("loss_row_", r, "_soc_", soc_names[cc]),
               paste0("loss=", loss_reps[r], "; SOC=", soc_reps[cc]),
               soc_loss_matrix[r, cc],
               score_soc_loss(loss_reps[r], soc_reps[cc]))
    }
  }

  # Exact loss boundaries in a representative >=5% SOC column.
  loss_boundaries <- c(0, -0.5, -1.5, -3, -4, -5, -6, -7, -8)
  loss_boundary_expected <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  for (i in seq_along(loss_boundaries)) {
    add_case("SOC loss", "S1.2.8.1",
             paste0("exact_loss_boundary_", i),
             paste0("loss=", loss_boundaries[i], "; SOC=5"),
             loss_boundary_expected[i],
             score_soc_loss(loss_boundaries[i], 5))
  }
  add_case("SOC loss", "S1.2.8.1 + AI4SoilHealth h1 extension",
           "h1_no_loss", "loss=0; SOC=0.4",
           0, score_soc_loss(0, 0.4),
           evidence = "PROJECT_DECISION",
           note = "v1.7 h1 extension for current SOC <0.5%: no loss/positive change -> 0.")
  add_case("SOC loss", "S1.2.8.1 + AI4SoilHealth h1 extension",
           "h1_small_loss", "loss=-0.25; SOC=0.4",
           8, score_soc_loss(-0.25, 0.4),
           evidence = "PROJECT_DECISION",
           note = "v1.7 h1 extension: <0 to -0.5 -> 8.")
  add_case("SOC loss", "S1.2.8.1 + AI4SoilHealth h1 extension",
           "h1_larger_loss", "loss=-0.6; SOC=0.4",
           9, score_soc_loss(-0.6, 0.4),
           evidence = "PROJECT_DECISION",
           note = "v1.7 h1 extension: loss below -0.5 -> 9.")

  ratio_denominators <- c(8, 9, 10, 11, 12, 13)
  ratio_scores <- c(0, 1, 3, 5, 7, 8)
  for (i in seq_along(ratio_denominators)) {
    den <- ratio_denominators[i]
    add_case("SOC/clay ratio", "S1.2.8.2",
             paste0("ratio_boundary_1_", den),
             paste0("SOC/clay=1/", den),
             ratio_scores[i],
             score_soc_clay_ratio(1, den))
  }
  add_case("SOC/clay ratio", "S1.2.8.2",
           "below_1_13", "SOC/clay just below 1/13",
           9, score_soc_clay_ratio(1 - eps, 13))

  # SOC-content intervals were explicitly clarified in v1.7 to be complete,
  # monotonic and non-overlapping.
  soc_content_cases <- list(
    c(5.1,0), c(5,1), c(4.75,1), c(4.5,2), c(4.25,2), c(4,3),
    c(3.75,3), c(3.5,4), c(3.25,4), c(3,5), c(2.75,5), c(2.5,6),
    c(2.25,6), c(2,7), c(1.75,7), c(1.5,8), c(1.0,8), c(0.5,9), c(0.4,9)
  )
  for (sc in soc_content_cases) {
    x <- as.numeric(sc[1]); expected <- as.numeric(sc[2])
    add_case("SOC content", "S1.2.8.3 clarified",
             paste0("clarified_SOC_", gsub("\\.", "_", as.character(x))),
             paste0("SOC=", x), expected, score_soc_content(x),
             evidence = "PROJECT_DECISION",
             note = "v1.7 clarification: complete non-overlapping SOC-content intervals.")
  }


  # Overall SOC equations.
  socrec <- list(soc_loss_9y = -0.25, soc_pct = 3, clay_pct = 30)
  loss_s <- score_soc_loss(socrec$soc_loss_9y, socrec$soc_pct)
  ratio_s <- score_soc_clay_ratio(socrec$soc_pct, socrec$clay_pct)
  content_s <- score_soc_content(socrec$soc_pct)
  add_case("SOC overall - agriculture", "Eq. 1.2.8.1",
           "three_term_mean",
           "loss=-0.25; SOC=3%; clay=30%",
           mean(c(loss_s, ratio_s, content_s)),
           calculate_soc(socrec, "cropland")$severity)

  socrec$soc_loss_9y <- NA_real_
  add_case("SOC overall - agriculture", "AI4SoilHealth v1.7 local-data rule",
           "loss_missing_component_not_scored",
           "SOC=3%; clay=30%; loss missing",
           NA_real_,
           calculate_soc(socrec, "cropland")$severity,
           evidence = "PROJECT_DECISION",
           note = "v1.7 decision: do not apply SI1 Eq. 1.2.8.2; missing SOC loss makes the whole SOC component NA and triggers the missing-data note.")

  fsoc <- list(soc_loss_9y = -0.25, soc_pct = 3, clay_pct = 30)
  add_case("SOC overall - forest", "S1.2.8",
           "forest_loss_only",
           "loss=-0.25; SOC=3%",
           score_soc_loss(-0.25, 3),
           calculate_soc(fsoc, "forest")$severity)

  # -------------------------------------------------------------------
  # SI2 Table S2.1: published-European applicability/status matrix.
  components <- c(
    "erosion","landslide","metal_cu","metal_hg","metal_zn","metal_cd",
    "metal_ni","metal_pb","metal_sb","metal_as","metal_cr","metal_co",
    "nitrogen","p_excess","p_mining","pesticide","salinisation",
    "compaction","soc"
  )

  expected_status <- list(
    cropland = setNames(rep("x", length(components)), components),
    grassland = setNames(rep("x", length(components)), components),
    forest = setNames(rep("x", length(components)), components)
  )
  expected_status$grassland["p_mining"] <- "na"
  expected_status$grassland["pesticide"] <- "nr"

  expected_status$forest[c("p_excess","p_mining","pesticide","salinisation")] <- "nr"
  expected_status$forest[c("compaction","soc")] <- "na"

  for (lu in names(expected_status)) {
    for (comp in components) {
      rr <- cfg$applicability[
        cfg$applicability$effective_land_use == lu &
          cfg$applicability$component == comp, , drop = FALSE
      ]
      obs_status <- if (nrow(rr) == 1) rr$s2_1_status[1] else NA_character_
      exp_status <- unname(expected_status[[lu]][comp])
      add_case("Applicability", "S2.1",
               paste0(lu, "_", comp, "_status"),
               paste0(lu, "/", comp),
               exp_status, obs_status)

      exp_eu <- identical(exp_status, "x")
      obs_eu <- if (nrow(rr) == 1) isTRUE(rr$published_eu[1]) else NA
      add_case("Applicability", "S2.1",
               paste0(lu, "_", comp, "_published_eu"),
               paste0(lu, "/", comp, " published_eu"),
               exp_eu, obs_eu)
    }
  }

  # Full-scheme extension of SI2 'na' rows is intentional and flagged.
  for (key in list(
    c("grassland","p_mining"),
    c("forest","compaction"),
    c("forest","soc")
  )) {
    lu <- key[1]
    comp <- key[2]
    rr <- cfg$applicability[
      cfg$applicability$effective_land_use == lu &
        cfg$applicability$component == comp, , drop = FALSE
    ]
    add_case("Applicability", "S2.1",
             paste0(lu, "_", comp, "_full_scheme_extension"),
             paste0(lu, "/", comp),
             TRUE,
             if (nrow(rr) == 1) rr$full_scheme[1] else NA,
             evidence = "PROJECT_DECISION",
             note = "v1.7 decision: SI2 'na' may be assessed in Full scheme when valid local data are available; without data it is excluded from the record-level denominator. 'nr' is always excluded.")

    add_case("Applicability", "v1.7 record-level rule",
             paste0(lu, "_", comp, "_na_without_data_excluded"),
             paste0(lu, "/", comp, "; severity=NA"),
             FALSE,
             part2_record_applicable(lu, comp, "full_scheme", cfg$applicability,
                                     severity=NA_real_),
             evidence = "PROJECT_DECISION")
    add_case("Applicability", "v1.7 record-level rule",
             paste0(lu, "_", comp, "_na_with_data_included"),
             paste0(lu, "/", comp, "; severity=3"),
             TRUE,
             part2_record_applicable(lu, comp, "full_scheme", cfg$applicability,
                                     severity=3),
             evidence = "PROJECT_DECISION")
  }

  add_case("Applicability", "v1.7 record-level rule",
           "grassland_pesticide_nr_always_excluded",
           "grassland/pesticide; severity=3",
           FALSE,
           part2_record_applicable("grassland", "pesticide", "full_scheme",
                                   cfg$applicability, severity=3),
           evidence = "PROJECT_DECISION")

  add_case("Salinisation routing", "v1.7 record-level rule",
           "natural_salinity_routed_out_of_denominator",
           "cropland/salinisation; routed_out=TRUE",
           FALSE,
           part2_record_applicable("cropland", "salinisation", "full_scheme",
                                   cfg$applicability, severity=NA_real_, routed_out=TRUE),
           evidence = "PROJECT_DECISION")

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

summarise_part2_threshold_audit <- function(audit_df) {
  if (is.null(audit_df) || nrow(audit_df) == 0) {
    return(data.frame(
      total = 0, pass = 0, fail = 0, flag = 0,
      source_defined = 0, original_code = 0, project_decision = 0, stringsAsFactors = FALSE
    ))
  }

  data.frame(
    total = nrow(audit_df),
    pass = sum(audit_df$status == "PASS"),
    fail = sum(audit_df$status == "FAIL"),
    flag = sum(audit_df$status == "FLAG"),
    source_defined = sum(audit_df$evidence %in% c("SOURCE_DEFINED", "SOURCE_ANCHOR")),
    original_code = sum(audit_df$evidence == "ORIGINAL_CODE"),
    project_decision = sum(audit_df$evidence == "PROJECT_DECISION"),
    stringsAsFactors = FALSE
  )
}
