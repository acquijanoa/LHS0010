#!/usr/bin/env Rscript
# Install CRAN packages needed for LHS0010 (run once).
# CmdStan itself: cmdstanr::install_cmdstan() after this script.

pkgs <- c(
  "dplyr", "tidyr", "readr", "purrr", "tibble",
  "haven", "jsonlite", "survey", "reporter",
  "sf", "spdep", "ggplot2",
  "cmdstanr", "posterior", "tidybayes", "Matrix", "ggpattern"
)

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Installing ", p, " ...")
    install.packages(p, repos = "https://cloud.r-project.org")
  } else {
    message("OK: ", p)
  }
}

# cmdstanr from Stan r-universe (recommended)
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  message("Installing cmdstanr from https://stan-dev.r-universe.dev ...")
  install.packages(
    "cmdstanr",
    repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org")
  )
}

for (p in setdiff(pkgs, "cmdstanr")) install_if_missing(p)

message("\nDone. Next in R:  cmdstanr::check_cmdstan_toolchain()  then  cmdstanr::install_cmdstan()")
