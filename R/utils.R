# SHERPA v1.4 utilities -----------------------------------------------------

num_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  if (is.numeric(x)) return(ifelse(length(x) == 0, NA_real_, as.numeric(x[1])))
  x <- trimws(as.character(x[1]))
  if (is.na(x) || x == "") return(NA_real_)
  suppressWarnings(as.numeric(x))
}

char_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- trimws(as.character(x[1]))
  if (is.na(x) || x == "" || tolower(x) == "unknown") return(NA_character_)
  x
}

coalesce_chr <- function(x, default="") {
  if (is.null(x) || length(x) == 0) return(default)
  y <- as.character(x[1])
  if (is.na(y) || !nzchar(y)) return(default)
  y
}

is_na_scalar <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[1])
}

safe_eq <- function(x, value) {
  !is.null(x) && length(x) > 0 && !is.na(x[1]) && identical(as.character(x[1]), as.character(value))
}

safe_in <- function(x, values) {
  !is.null(x) && length(x) > 0 && !is.na(x[1]) && as.character(x[1]) %in% values
}

mean_available <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

clamp <- function(x, lo, hi) {
  if (is.na(x)) return(NA_real_)
  min(max(x, lo), hi)
}

# Convert a positive degradation severity (0..9) to the negative Part 2 score.
negative_score <- function(severity) {
  if (is.na(severity)) return(NA_real_)
  -as.numeric(severity)
}


# Aggregate applicable Part 2 negative scores using the published mean-of-
# available logic. This helper is used both by the application and by the
# legacy regression tests, so the tested aggregation path is the same path
# used in production calculations.
aggregate_part2_negative_scores <- function(negative_scores,
                                            aggregation_mode = "available_mean",
                                            n_expected = length(negative_scores)) {
  x <- as.numeric(negative_scores)
  available <- !is.na(x)
  n_available <- sum(available)

  mean_negative <- if (any(available)) mean(x[available]) else NA_real_

  if (safe_eq(aggregation_mode, "complete_case") && n_available < n_expected) {
    mean_negative <- NA_real_
  }

  list(
    mean_negative = mean_negative,
    severity_magnitude = if (is.na(mean_negative)) NA_real_ else -mean_negative,
    n_available = n_available,
    n_expected = n_expected,
    completeness = if (n_expected > 0) n_available / n_expected else NA_real_
  )
}

# Score a positive quantity by ordered upper boundaries.
# <= threshold[1] => 0; > threshold[9] => 9.
severity_from_upper_bounds <- function(x, thresholds) {
  if (is.na(x)) return(NA_real_)
  if (x < 0) return(NA_real_)
  sum(x > thresholds)
}

# Effective SHERPA land-use route.
# Wetland/organic/drained systems are explicitly "not considered" in SI1 Table S1.0.
route_land_use <- function(record) {
  if (isTRUE(record$drained_soil) || isTRUE(record$organic_soil_gt20) ||
      identical(record$land_use, "wetland")) {
    return("not_considered")
  }

  lu <- coalesce_chr(record$land_use, "unsupported")
  if (identical(lu, "forest")) return("forest")

  if (identical(lu, "grassland")) {
    gp <- char_or_na(record$grassland_permanence)
    if (!is.na(gp) && gp %in% c("permanent_all_year", "permanent_mediterranean")) {
      return("grassland")
    }
    if (!is.na(gp) && gp == "non_permanent") {
      return("cropland")
    }
    return("grassland_unresolved")
  }

  if (lu %in% c("cropland", "orchard", "vineyard")) return("cropland")

  "unsupported"
}

source_state <- function(value, user_supplied = TRUE) {
  if (is.character(value)) {
    if (length(value)==0 || is.na(value[1]) || value[1] == "") return("MISSING")
  } else if (is.numeric(value)) {
    if (length(value)==0 || is.na(value[1])) return("MISSING")
  }
  if (isTRUE(user_supplied)) "USER" else "OTHER"
}

safe_named <- function(x, name, default = NA_real_) {
  if (is.null(x[[name]])) return(default)
  x[[name]]
}
