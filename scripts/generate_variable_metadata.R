# Generate variable metadata JSON for HNIR* datasets
# Author: Generated script
# Date: 2026-03-07

library(haven)
library(dplyr)
library(jsonlite)

# Find the SAS setup file for a dataset
find_sas_setup_file <- function(sas_file_path) {
  data_dir <- dirname(sas_file_path)
  base_name <- sub("\\.(sas7bdat|sav)$", "", basename(sas_file_path), ignore.case = TRUE)
  candidates <- c(
    file.path(data_dir, paste0(toupper(base_name), ".SAS")),
    file.path(data_dir, paste0(tolower(base_name), ".sas")),
    file.path(data_dir, paste0(base_name, ".SAS")),
    file.path(data_dir, paste0(base_name, ".sas"))
  )

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  return(NULL)
}

# Parse SAS format definitions into value-label mappings
parse_sas_format_definitions <- function(sas_setup_path) {
  lines <- readLines(sas_setup_path, warn = FALSE, encoding = "latin1")
  formats <- list()
  current_format <- NULL

  for (line in lines) {
    trimmed <- trimws(line)

    if (is.null(current_format)) {
      match <- regexec("^value\\s+([\\$A-Za-z0-9_]+)", trimmed, ignore.case = TRUE)
      groups <- regmatches(trimmed, match)[[1]]
      if (length(groups) > 1) {
        current_format <- groups[2]
        formats[[current_format]] <- list()
      }
      next
    }

    if (trimmed == ";") {
      current_format <- NULL
      next
    }

    if (!grepl("=", trimmed, fixed = TRUE)) {
      next
    }

    parts <- strsplit(trimmed, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      next
    }

    left <- trimws(parts[1])
    right <- trimws(paste(parts[-1], collapse = "="))
    right <- sub(";\\s*$", "", right)

    if (grepl("^\".*\"$", right)) {
      right <- sub("^\"(.*)\"$", "\\1", right)
    } else if (grepl("^'.*'$", right)) {
      right <- sub("^'(.*)'$", "\\1", right)
    }

    formats[[current_format]][[left]] <- right
  }

  return(formats)
}

# Parse variable-to-format mappings from SAS setup file
parse_variable_formats <- function(sas_setup_path) {
  lines <- readLines(sas_setup_path, warn = FALSE, encoding = "latin1")
  mappings <- list()

  for (line in lines) {
    match <- regexec("^\\s*attrib\\s+([A-Za-z0-9_]+)\\s+.*\\bformat\\s*=\\s*([\\$A-Za-z0-9_]+)\\.?", line, ignore.case = TRUE)
    groups <- regmatches(line, match)[[1]]
    if (length(groups) > 2) {
      var_name <- groups[2]
      fmt_name <- groups[3]
      fmt_name <- sub("\\.$", "", fmt_name)
      mappings[[var_name]] <- fmt_name
      next
    }

    match <- regexec("^\\s*format\\s+([A-Za-z0-9_]+)\\s+([\\$A-Za-z0-9_]+)\\.?", line, ignore.case = TRUE)
    groups <- regmatches(line, match)[[1]]
    if (length(groups) > 2) {
      var_name <- groups[2]
      fmt_name <- groups[3]
      fmt_name <- sub("\\.$", "", fmt_name)
      mappings[[var_name]] <- fmt_name
    }
  }

  return(mappings)
}

# Function to extract variable metadata and generate JSON
generate_metadata_json <- function(sas_file_path, output_dir = "docs", sas_setup_path = NULL) {
  
  # Read the dataset
  cat("Reading:", sas_file_path, "\n")
  ext <- tolower(tools::file_ext(sas_file_path))
  if (ext == "sas7bdat") {
    data <- read_sas(sas_file_path)
  } else if (ext == "sav") {
    data <- read_sav(sas_file_path)
  } else {
    stop("Unsupported file type: ", sas_file_path)
  }

  if (is.null(sas_setup_path)) {
    sas_setup_path <- find_sas_setup_file(sas_file_path)
  }

  format_definitions <- list()
  variable_formats <- list()

  if (!is.null(sas_setup_path)) {
    cat("Using SAS setup:", sas_setup_path, "\n")
    format_definitions <- parse_sas_format_definitions(sas_setup_path)
    variable_formats <- parse_variable_formats(sas_setup_path)
  }
  
  # Extract dataset name from file path
  dataset_name <- tools::file_path_sans_ext(basename(sas_file_path))
  
  # Initialize metadata list
  metadata <- list()
  metadata$dataset <- dataset_name
  metadata$variables <- list()
  
  # Extract metadata for each variable
  for (var_name in names(data)) {
    var_metadata <- list()
    
    # Variable name
    var_metadata$name <- var_name
    
    # Variable label
    var_label <- attr(data[[var_name]], "label")
    var_metadata$label <- if (is.null(var_label)) "" else var_label
    
    # Value labels
    format_name <- variable_formats[[var_name]]
    if (!is.null(format_name)) {
      format_values <- format_definitions[[format_name]]
      if (!is.null(format_values)) {
        var_metadata$labels <- format_values
      } else {
        var_metadata$labels <- list()
      }
    } else {
      value_labels_attr <- attr(data[[var_name]], "labels")
      if (!is.null(value_labels_attr)) {
        codes <- as.character(unname(value_labels_attr))
        labels <- names(value_labels_attr)
        if (is.null(labels)) {
          labels <- codes
        }
        value_list <- as.list(labels)
        names(value_list) <- codes
        var_metadata$labels <- value_list
      } else {
        var_metadata$labels <- list()
      }
    }
    
    # Calculate percentage missing
    n_missing <- sum(is.na(data[[var_name]]))
    n_total <- nrow(data)
    pct_missing <- round((n_missing / n_total) * 100, 2)
    var_metadata$pct_missing <- pct_missing
    
    # Add to metadata list
    metadata$variables[[var_name]] <- var_metadata
  }
  
  # Ensure output directory exists
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Create output filename
  output_file <- file.path(output_dir, paste0(dataset_name, "_metadata.json"))
  
  # Write JSON file
  write_json(metadata, output_file, pretty = TRUE, auto_unbox = TRUE)
  cat("Saved metadata to:", output_file, "\n\n")
  
  return(metadata)
}

# Find all HNIR* .sas7bdat files in data/ subdirectories
data_dir <- "data"
hnir_files <- list.files(
  path = data_dir,
  pattern = "^[Hh][Nn][Ii][Rr].+\\.(sas7bdat|sav)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(hnir_files) > 0) {
  base_names <- tolower(sub("\\.(sas7bdat|sav)$", "", basename(hnir_files)))
  selected_indices <- tapply(seq_along(hnir_files), base_names, function(idx) {
    files <- hnir_files[idx]
    sav_files <- files[grepl("\\.sav$", files, ignore.case = TRUE)]
    if (length(sav_files) > 0) {
      sav_files[1]
    } else {
      files[1]
    }
  })
  hnir_files <- unname(unlist(selected_indices))
}

cat("Found", length(hnir_files), "HNIR* dataset(s):\n")
print(hnir_files)
cat("\n")

# Generate metadata for each dataset
if (length(hnir_files) > 0) {
  for (sas_file in hnir_files) {
    generate_metadata_json(sas_file)
  }
  cat("All metadata files generated successfully!\n")
} else {
  cat("No HNIR* datasets found.\n")
}
