#!/usr/bin/env Rscript
# Metadata JSON for data/raw/HNIR72SD/bh.sav (birth history + design vars).
# Output: docs/bh_metadata.json
#
#   Rscript scripts/01_metadata/generate_bh_metadata.R

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

suppressPackageStartupMessages({
  library(haven)
  library(jsonlite)
})

input_path <- Sys.getenv("BH_SAV_PATH", "data/raw/HNIR72SD/bh.sav")
output_dir <- "docs"
if (!file.exists(input_path)) {
  stop("Missing file: ", input_path, "\nSet BH_SAV_PATH if elsewhere.", call. = FALSE)
}

message("Reading: ", input_path)
data <- read_sav(input_path)
dataset_name <- tools::file_path_sans_ext(basename(input_path))
metadata <- list(dataset = dataset_name, variables = list())

for (var_name in names(data)) {
  var_metadata <- list(
    name = var_name,
    label = {
      lab <- attr(data[[var_name]], "label")
      if (is.null(lab)) "" else as.character(lab)
    },
    labels = list(),
    pct_missing = round(100 * sum(is.na(data[[var_name]])) / nrow(data), 2)
  )
  value_labels_attr <- attr(data[[var_name]], "labels")
  if (!is.null(value_labels_attr)) {
    codes <- as.character(unname(value_labels_attr))
    labels <- names(value_labels_attr)
    if (is.null(labels)) labels <- codes
    vl <- as.list(labels)
    names(vl) <- codes
    var_metadata$labels <- vl
  }
  metadata$variables[[var_name]] <- var_metadata
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- file.path(output_dir, paste0(dataset_name, "_metadata.json"))
write_json(metadata, output_file, pretty = TRUE, auto_unbox = TRUE)
message("Saved: ", output_file, " (", length(metadata$variables), " variables)")
