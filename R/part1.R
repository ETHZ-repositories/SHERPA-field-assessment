# SHERPA Part 1: intrinsic soil health -------------------------------------

score_cropland_fvc <- function(fvc) {
  if (is.na(fvc) || fvc < 0 || fvc > 100) return(NA_real_)
  if (fvc >= 80) return(10)
  if (fvc >= 75) return(9)
  if (fvc >= 70) return(8)
  if (fvc >= 65) return(7)
  if (fvc >= 60) return(6)
  if (fvc >= 55) return(5)
  if (fvc >= 50) return(4)
  if (fvc >= 45) return(3)
  if (fvc >= 40) return(2)
  1
}

score_grassland_fvc <- function(fvc) {
  if (is.na(fvc) || fvc < 0 || fvc > 100) return(NA_real_)
  # Boundaries follow the R implementation linked by the paper.
  # Its exact 60% value is unclassified; v1.1 closes that single gap as score 2.
  if (fvc == 100) return(10)
  if (fvc > 98) return(9)
  if (fvc > 95) return(8)
  if (fvc > 90) return(7)
  if (fvc > 80) return(6)
  if (fvc > 75) return(5)
  if (fvc > 70) return(4)
  if (fvc > 65) return(3)
  if (fvc >= 60) return(2)
  1
}

score_soil_structure <- function(x) {
  map <- c(
    granular = 10,
    subangular = 8,
    angular_cloddy_lt100 = 5,
    angular_cloddy_gt100_columnar_platy = 3,
    single_grain_massive = 1
  )
  if (is.null(x) || is.na(x) || !x %in% names(map)) return(NA_real_)
  unname(map[[x]])
}

score_organic_fertilizer <- function(pct) {
  if (is.na(pct) || pct < 0 || pct > 100) return(NA_real_)
  if (pct == 100) return(10)
  if (pct >= 95) return(9)
  if (pct >= 80) return(8)
  if (pct >= 65) return(7)
  if (pct >= 50) return(6)
  if (pct >= 35) return(5)
  if (pct >= 20) return(4)
  if (pct >= 10) return(3)
  if (pct > 0) return(2)
  1
}

forest_branch <- function(climate, altitude_m, sand_pct, bedrock_class) {
  climate <- char_or_na(climate)
  altitude_m <- num_or_na(altitude_m)
  sand_pct <- num_or_na(sand_pct)
  bedrock_class <- char_or_na(bedrock_class)
  # SI1 prose uses sand >=85%; Table S1.1 prints >85%. v1.1 adopts >=85%
  # because the preceding explanatory text explicitly separates soils with >=85%.
  if (!is.na(bedrock_class) && bedrock_class == "high_sandy") return("high_sandy")
  if (!is.na(climate) && climate == "ET") return("high_sandy")
  if (!is.na(altitude_m) && altitude_m > 1400) return("high_sandy")
  if (!is.na(sand_pct) && sand_pct >= 85) return("high_sandy")

  if (!is.na(climate) && climate %in% c("Csa", "Cfb", "Dfb", "Dfc", "SHERPA_NORMAL") &&
      !is.na(altitude_m) && altitude_m <= 1400 &&
      !is.na(sand_pct) && sand_pct < 85) {
    if (!is.na(bedrock_class) && bedrock_class %in% c("base_rich", "medium_base")) {
      return(bedrock_class)
    }
    return("needs_bedrock")
  }
  "incomplete"
}

score_forest_base_rich_disturbance <- function(pct) {
  if (is.na(pct) || pct < 0 || pct > 100) return(NA_real_)
  if (pct <= 10) return(8)
  if (pct <= 15) return(7)
  if (pct <= 20) return(6)
  if (pct <= 30) return(5)
  if (pct <= 40) return(4)
  if (pct <= 50) return(3)
  if (pct <= 60) return(2)
  1
}

forest_branch_label <- function(branch) {
  branch <- char_or_na(branch)
  if (is.na(branch)) return("Not resolved")
  labels <- c(
    base_rich = "Base-rich / calcareous branch (S1.1.1.1)",
    medium_base = "Medium-base branch (S1.1.1.2)",
    high_sandy = "ET / high-altitude / high-sand branch (S1.1.2)",
    needs_bedrock = "Bedrock class required to resolve forest branch",
    incomplete = "Forest branch incomplete"
  )
  if (branch %in% names(labels)) unname(labels[[branch]]) else branch
}

score_forest_condition <- function(branch, condition, disturbance_pct = NA_real_) {
  if (is.na(branch) || branch %in% c("incomplete", "needs_bedrock")) return(NA_real_)
  if (is.na(condition) || condition == "") return(NA_real_)

  if (branch == "base_rich" && condition == "disturbance") {
    return(score_forest_base_rich_disturbance(disturbance_pct))
  }

  if (grepl("^score_[0-9]+$", condition)) {
    out <- suppressWarnings(as.numeric(sub("^score_", "", condition)))
    if (!is.na(out) && out >= 1 && out <= 10) return(out)
  }
  NA_real_
}

calculate_part1 <- function(record) {
  notes <- character(0)
  route <- route_land_use(record)

  if (route == "not_considered") {
    return(list(score = NA_real_, route = route,
                sub_scores = data.frame(component="Part 1", score=NA_real_),
                notes = "SI1 Table S1.0: wetlands/organic soils (>20% OC) and any drained land are not considered in the published SHERPA key."))
  }

  if (route == "grassland_unresolved") {
    return(list(score = NA_real_, route = route,
                sub_scores = data.frame(component="Grassland permanence", score=NA_real_),
                notes = "Grassland permanence must be resolved before Part 1 can be scored."))
  }

  if (route == "cropland") {
    fvc <- score_cropland_fvc(record$fvc_pct)
    structure_raw <- score_soil_structure(record$soil_structure)
    structure_timing_valid <- isTRUE(record$structure_after_8_weeks)
    structure <- if (!is.na(structure_raw) && structure_timing_valid) structure_raw else NA_real_
    fertilizer <- score_organic_fertilizer(record$organic_fertilizer_pct)

    if (!structure_timing_valid && !is.na(structure_raw)) {
      notes <- c(notes,
        "Soil structure was entered but excluded because valid assessment timing was not confirmed (>=8 weeks after ploughing, or no recent ploughing).")
    }

    if (!is.na(fvc) && !is.na(fertilizer) && !is.na(structure)) {
      score <- mean(c(fvc, structure, fertilizer))
      equation <- "Eq. 1.3.1: mean(FVC, soil structure, fertiliser)"
    } else if (!is.na(fvc) && !is.na(fertilizer) && is.na(structure)) {
      score <- mean(c(fvc, fertilizer))
      equation <- "Eq. 1.3.2: mean(FVC, fertiliser); soil structure missing"
      notes <- c(notes, "SI1 explicitly permits Eq. 1.3.2 when soil structure is unavailable, with increased uncertainty.")
    } else {
      score <- NA_real_
      equation <- "Not calculable"
      notes <- c(notes, "Cropland Part 1 requires FVC and organic-fertiliser share; soil structure is preferred and may be omitted only under Eq. 1.3.2.")
    }

    subs <- data.frame(
      component = c("FVC", "Soil structure", "Organic fertiliser", "Part 1"),
      score = c(fvc, structure, fertilizer, score),
      stringsAsFactors = FALSE
    )
    return(list(score=score, route=route, sub_scores=subs,
                equation=equation, notes=unique(notes)))
  }

  if (route == "grassland") {
    fvc <- score_grassland_fvc(record$fvc_pct)
    if (is.na(fvc)) notes <- c(notes, "Permanent-grassland Part 1 requires FVC.")
    subs <- data.frame(component=c("Permanent-grassland FVC","Part 1"),
                       score=c(fvc,fvc), stringsAsFactors=FALSE)
    return(list(score=fvc, route=route, sub_scores=subs,
                equation="Table S1.2.1 FVC class", notes=notes))
  }

  if (route == "forest") {
    branch <- forest_branch(record$forest_climate, record$altitude_m,
                            record$sand_pct, record$bedrock_class)
    score <- score_forest_condition(branch, record$forest_condition,
                                    record$forest_disturbance_pct)
    if (branch == "incomplete") {
      notes <- c(notes, "Forest Part 1 routing requires a supported climate class plus altitude and sand content.")
    }
    if (branch == "needs_bedrock") {
      notes <- c(notes, "Forest Part 1 requires the bedrock/mineralogical class in the low-altitude, sand <85% branch.")
    }
    if (!is.na(branch) && branch == "high_sandy" &&
        identical(record$forest_condition, "score_6")) {
      notes <- c(notes, "The score-6 row in SI1 Table S1.1.2 reads '70% coverage; less than 3% disturbed'. This unusual published wording is retained.")
    }
    subs <- data.frame(
      component=c("Forest branch","Part 1"),
      score=c(NA_real_,score), stringsAsFactors=FALSE
    )
    return(list(score=score, route=route, branch=branch,
                branch_label=forest_branch_label(branch),
                sub_scores=subs,
                equation=paste("Forest decision branch:", branch), notes=unique(notes)))
  }

  list(score=NA_real_, route=route,
       sub_scores=data.frame(component="Part 1",score=NA_real_),
       notes="Unsupported land-use route.")
}
