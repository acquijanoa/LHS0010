#!/usr/bin/env Rscript
# Run full pipeline from project root: derive -> estimates -> reports.
# Usage: Rscript scripts/run_all.R  (run from project root)

# Set project root
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

# 1) Derive datasets
run("scripts/02_derive/derive_hnir62.R", "HNIR62 derived")
run("scripts/02_derive/derive_hnir72.R", "HNIR72 derived")
run("scripts/02_derive/derive_coir61.R", "COIR61 derived")
run("scripts/02_derive/derive_coir72.R", "COIR72 derived")

# 2) Age-by-region estimates
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
run(
  c("scripts/03_analysis/estimate_age_region.R", "output/tables/age_region_estimates.csv"),
  "Age-by-region direct estimates"
)

# 4) Reports
run("scripts/04_reporting/create_covariate_report_table.R", "Covariate report tables")
run("scripts/04_reporting/print_derived_dictionary.R", "Derived dictionary (console)")

message("Pipeline complete.")
