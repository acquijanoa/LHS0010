#!/usr/bin/env Rscript
# Full pipeline from project root: metadata → derive → SES → estimates → reports
# (+ optional spatial if LHS_RUN_SPATIAL=1 and data/shp/Shp_mgd.shp exists).
#
# Usage: Rscript scripts/run_all.R

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(script_dir)
  setwd(project_root)
}

run <- function(script, description) {
  message("Running: ", description, " [", script, "]")
  result <- system2("Rscript", script, stdout = TRUE, stderr = TRUE)
  if (length(attr(result, "status")) && attr(result, "status") != 0) {
    stop("Failed: ", script, "\n", paste(result, collapse = "\n"))
  }
  invisible(result)
}

run_maybe <- function(script, description) {
  if (!file.exists(script)) {
    message("Skip (missing script): ", script)
    return(invisible(NULL))
  }
  run(script, description)
}

# 0) Metadata (docs/*.json) — needs raw .sas7bdat in data/raw/
run_maybe("scripts/01_metadata/generate_variable_metadata.R", "Metadata HNIR*")
run_maybe("scripts/01_metadata/generate_coir61fl_metadata.R", "Metadata COIR61")
run_maybe("scripts/01_metadata/generate_coir72fl_metadata.R", "Metadata COIR72")

# 1) Derive datasets
run("scripts/02_derive/derive_hnir62.R", "HNIR62 derived")
run("scripts/02_derive/derive_hnir72fl.R", "HNIR72FL MICS derived")
run("scripts/02_derive/derive_coir61.R", "COIR61 derived")
run("scripts/02_derive/derive_coir72.R", "COIR72 derived")

# 2) Regional SES (for spatial cov model + documentation)
run("scripts/02_derive/derive_regional_socioeconomic.R", "Regional SES (adolescents)")

# 3) Design-based region × age × wave (HNIR) + logit VCOV (spatial + inference)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
run("scripts/03_analysis/honduras_region_age_prevalence_logit.R", "HNIR Adol/YA logit + vcov")

# 4) Reports
run("scripts/04_reporting/create_covariate_report_table.R", "Covariate report tables")
run("scripts/04_reporting/print_derived_dictionary.R", "Derived dictionary (console)")

# 5) Optional spatial (Honduras): shapefile + CmdStan
spatial <- Sys.getenv("LHS_RUN_SPATIAL", "") %in% c("1", "TRUE", "true")
shp_ok <- file.exists("data/shp/Shp_mgd.shp")
if (spatial && shp_ok) {
  run("scripts/03_analysis/honduras_spatial_smooth_stan.R", "Honduras Stan spatial smooth")
  run("scripts/03_analysis/honduras_spatial_plot_maps.R", "Honduras spatial maps")
  run("scripts/03_analysis/honduras_posterior_year_diff.R", "Honduras posterior year diff")
} else if (spatial && !shp_ok) {
  message("LHS_RUN_SPATIAL set but data/shp/Shp_mgd.shp missing — skip spatial (see docs/REPRODUCIBILITY.md)")
}

message("Pipeline complete.")
