# SHERPA field assessment — release v1.0
#
# USER VERSION
#
# Recommended:
#   1. Unzip the complete folder.
#   2. Open SHERPA_field_assessment_v1_0.Rproj in RStudio.
#   3. Open START_SHERPA.R and click Source.

find_sherpa_root <- function() {
  this_file <- tryCatch(sys.frame(1)$ofile, error=function(e) NULL)

  candidates <- c(
    if (!is.null(this_file) && length(this_file)==1 && nzchar(this_file))
      dirname(this_file) else character(0),
    getwd(),
    file.path(getwd(),"..")
  )

  for (x in candidates) {
    p <- normalizePath(x,winslash="/",mustWork=FALSE)
    if (file.exists(file.path(p,"app.R")) &&
        file.exists(file.path(p,"R","engine.R")) &&
        dir.exists(file.path(p,"config"))) {
      return(p)
    }
  }

  stop(
    "SHERPA folder could not be located. Keep all files in the unzipped folder and open SHERPA_field_assessment_v1_0.Rproj.",
    call.=FALSE
  )
}

project_root <- find_sherpa_root()

required_files <- c(
  "app.R",
  "R/utils.R","R/part1.R","R/part2.R","R/proxy.R","R/engine.R","R/validation.R",
  "config/heavy_metals.csv",
  "config/crop_n_yield.csv",
  "config/pesticide_pli_thresholds.csv",
  "config/bulk_density_texture.csv",
  "config/part2_applicability.csv",
  "config/forest_lithology_s2_4.csv",
  "config/proxy_manifest.csv",
  "config/lithology_crosswalk_PROVISIONAL_v0_1.csv",
  "config/usda_texture_crosswalk_v1_8_10.csv",
  "data/validation/regression_legacy_12_cases.csv",
  "www/styles.css"
)

missing_files <- required_files[!file.exists(file.path(project_root,required_files))]
if (length(missing_files)>0) {
  stop(
    paste0("The SHERPA user folder is incomplete. Missing:\n - ",
           paste(missing_files,collapse="\n - ")),
    call.=FALSE
  )
}

if (!requireNamespace("shiny",quietly=TRUE)) {
  stop(
    "The R package 'shiny' is not installed.\nInstall it once with:\ninstall.packages('shiny')",
    call.=FALSE
  )
}

message("Starting SHERPA field assessment — release v1.0")
if (!requireNamespace("terra",quietly=TRUE)) {
  message("European raster support requires package 'terra': install.packages('terra')")
}

shiny::runApp(appDir=project_root,launch.browser=TRUE)
