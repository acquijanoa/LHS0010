library(dplyr)
library(haven)

# Run from project root when invoked via Rscript
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

dat <- readRDS("data/derived/hnir62fl_derived.rds")


des_grp <- svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = dat, nest = TRUE)
