#!/usr/bin/env Rscript
# Direct estimates and design-based variances by age group (Adolescent, Young Adult) and region.
# One combined dataset per country (two surveys stacked); estimates derived independently per country.
# Parallel by country: set MC_CORES or SLURM_CPUS_PER_TASK (e.g. #SBATCH --cpus-per-task=2 on Longleaf).

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(parallel)
  library(readr)
  library(survey)
  library(tibble)
  library(tidyr)
})

# Run from project root when invoked via Rscript
args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

options(survey.lonely.psu = "adjust")

# Per country: list of (path, survey_label) to stack into one dataset
country_specs <- list(
  Colombia = list(
    paths = c("data/derived/coir61fl_derived.rds", "data/derived/coir72fl_derived.rds"),
    surveys = c("2010", "2015")
  ),
  Honduras = list(
    paths = c("data/derived/hnir62fl_derived.rds","data/derived/hnir72sd_derived.rds"),
    surveys = c("2011-12", "2019")
  )
)

# Restrict to adolescents (agegroup_c4 = 1) and young adults (2, 3); label for output
agegroup_ay_levels <- c("Adolescent", "Young Adult")
age_group2_levels <- c("Adolescent", "YoungAdult")

prepare_country_dataset <- function(paths, surveys) {
  stopifnot(length(paths) == length(surveys))
  dfs <- map2(paths, surveys, function(p, s) {
    if (!file.exists(p)) return(NULL)
    readRDS(p) %>%
      mutate(
        survey = s,
        psu_id = as.numeric(psu_id),
        strat = as.numeric(strat),
        weight = as.numeric(weight),
        region = as.character(region),
        unmet_need = as.numeric(unmet_need)
      )
  })
  valid <- !map_lgl(dfs, is.null)
  if (!all(valid)) return(NULL)
  bound <- bind_rows(dfs)
  # Restrict to adolescents and young adults using derived indicators (no re-derivation)
  bound %>%
    filter(Adolescent == 1L | Young_Adult == 1L) %>%
    mutate(
      agegroup_ay = if_else(Adolescent == 1L, "Adolescent", "Young Adult"),
      agegroup_ay = factor(agegroup_ay, levels = agegroup_ay_levels),
      age_group2 = if_else(Adolescent == 1L, "Adolescent", "YoungAdult"),
      age_group2 = factor(age_group2, levels = age_group2_levels)
    ) %>%
    filter(
      !is.na(unmet_need),
      !is.na(agegroup_ay),
      !is.na(region),
      region != "",
      !is.na(psu_id),
      !is.na(strat),
      !is.na(weight)
    ) %>%
    mutate(
      strat_combined = paste(survey, strat, sep = "_"),
      psu_combined = paste(survey, psu_id, sep = "_"),
      region = factor(region)
    )
}

# Build JKn replicate design for a country dataset (for variance / replicate count)
build_rep_design <- function(df) {
  des <- svydesign(
    ids = ~psu_combined,
    strata = ~strat_combined,
    weights = ~weight,
    data = df,
    nest = TRUE
  )
  as.svrepdesign(subset(des, !is.na(age_group2)), type = "JKn", mse = TRUE)
}

# Number of jackknife replicates (columns of replicate weights)
get_n_replicates <- function(df) {
  repdes <- build_rep_design(df)
  ncol(weights(repdes, type = "replication"))
}

# Replicate estimate for one jackknife weight: point estimates by domain (survey_year, agegroup, region)
get_replicate_estimate <- function(df, rep_index, country_name) {
  repdes <- build_rep_design(df)
  rw <- weights(repdes, type = "replication")
  if (rep_index < 1L || rep_index > ncol(rw)) stop("rep_index out of range")
  repdes_k <- update(repdes, .weights = rw[, rep_index])
  est <- svyby(
    ~unmet_need,
    ~agegroup_ay + region + survey,
    design = repdes_k,
    FUN = svymean,
    na.rm = TRUE,
    keep.names = FALSE
  ) %>%
    as_tibble() %>%
    transmute(
      country = country_name,
      survey_year = as.character(survey),
      agegroup = as.character(agegroup_ay),
      region = as.character(region),
      replicate_estimate = unmet_need
    )
  est
}

# Replicate design (JKn) and 2x2 covariance matrix per region (Adolescent vs YoungAdult)
cov_matrix_by_region <- function(df) {
  des <- svydesign(
    ids = ~psu_combined,
    strata = ~strat_combined,
    weights = ~weight,
    data = df,
    nest = TRUE
  )
  repdes <- as.svrepdesign(subset(des, !is.na(age_group2)), type = "JKn", mse = TRUE)
  est <- svyby(
    ~unmet_need,
    ~region + age_group2,
    design = repdes,
    FUN = svymean,
    covmat = TRUE,
    na.rm = TRUE
  )
  V <- vcov(est)
  domain_key <- est[, c("region", "age_group2")]
  # Extract 2x2 block per region (rows: Adolescent, YoungAdult)
  regions <- unique(as.character(domain_key$region))
  out <- list()
  for (r in regions) {
    idx <- which(domain_key$region == r)
    if (length(idx) != 2L) next
    out[[r]] <- V[idx, idx, drop = FALSE]
    rownames(out[[r]]) <- colnames(out[[r]]) <- as.character(domain_key$age_group2[idx])
  }
  out
}

estimate_direct <- function(df) {
  design <- svydesign(
    ids = ~psu_combined,
    strata = ~strat_combined,
    weights = ~weight,
    data = df,
    nest = TRUE
  )

  direct_ay_region <- svyby(
    ~unmet_need,
    ~agegroup_ay + region + survey,
    design,
    svymean,
    vartype = c("se", "var", "ci", "cv"),
    na.rm = TRUE,
    keep.names = FALSE,
    drop.empty.groups = TRUE
  ) %>%
    as_tibble() %>%
    rename(
      estimate = unmet_need,
      std_error = se,
      variance = var,
      ci_lower = ci_l,
      ci_upper = ci_u,
      cv = cv
    )

  n_unweighted <- df %>%
    count(agegroup_ay, region, survey, name = "n_unweighted")

  weighted_totals <- df %>%
    group_by(agegroup_ay, region, survey) %>%
    summarise(weighted_total = sum(weight, na.rm = TRUE), .groups = "drop")

  direct_ay_region %>%
    left_join(n_unweighted, by = c("agegroup_ay", "region", "survey")) %>%
    left_join(weighted_totals, by = c("agegroup_ay", "region", "survey")) %>%
    transmute(
      survey_year = as.character(survey),
      agegroup = as.character(agegroup_ay),
      region = as.character(region),
      estimate,
      std_error,
      variance,
      ci_lower,
      ci_upper,
      cv,
      n_unweighted,
      weighted_total
    )
}

process_country <- function(spec, country_name) {
  paths <- spec$paths
  surveys <- spec$surveys
  df <- prepare_country_dataset(paths, surveys)
  if (is.null(df) || !nrow(df)) {
    warning(sprintf("%s: no data after combining surveys; skipping", country_name))
    return(NULL)
  }
  estimates <- estimate_direct(df) %>%
    mutate(country = country_name, .before = 1)
  cov_by_region <- cov_matrix_by_region(df)
  list(estimates = estimates, cov_by_region = cov_by_region)
}

# Modes: --nrep (print/write replicate count), --combine (variance from replicates), integer (replicate task), or default (full run)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- "output/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
replicate_dir <- file.path(out_dir, "replicate_estimates")
dir.create(replicate_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Mode: --nrep (write replicate count for sbatch array) ----
if (length(args) >= 1 && args[1] == "--nrep") {
  counts <- integer(0)
  for (cn in names(country_specs)) {
    df <- prepare_country_dataset(country_specs[[cn]]$paths, country_specs[[cn]]$surveys)
    if (is.null(df) || !nrow(df)) next
    nrep <- get_n_replicates(df)
    counts[cn] <- nrep
  }
  total <- sum(counts)
  count_file <- file.path(out_dir, "replicate_counts.txt")
  writeLines(c(
    as.character(total),
    paste(names(counts), counts, sep = "\t")
  ), count_file)
  message("Total jackknife replicates: ", total)
  cat(total, "\n")
  quit(save = "no", status = 0)
}

# ---- Mode: integer = SLURM array task (one jackknife replicate) ----
if (length(args) >= 1 && !is.na(suppressWarnings(as.integer(args[1]))) &&
    as.character(as.integer(args[1])) == args[1]) {
  task_id <- as.integer(args[1])
  count_file <- file.path(out_dir, "replicate_counts.txt")
  if (!file.exists(count_file)) {
    stop("Run with --nrep first to create ", count_file)
  }
  lines <- readLines(count_file)
  total <- as.integer(lines[1])
  if (task_id < 0 || task_id >= total) {
    message("Task ", task_id, " >= total ", total, "; skipping.")
    quit(save = "no", status = 0)
  }
  # Parse "CountryName\tnrep" to get cumulative offsets
  country_nrep <- setNames(integer(0), character(0))
  for (i in seq(2, length(lines))) {
    parts <- strsplit(lines[i], "\t")[[1]]
    if (length(parts) >= 2) country_nrep[parts[1]] <- as.integer(parts[2])
  }
  offset <- 0
  country_name <- NULL
  rep_index <- NA_integer_
  for (cn in names(country_nrep)) {
    nrep <- country_nrep[cn]
    if (task_id < offset + nrep) {
      country_name <- cn
      rep_index <- task_id - offset + 1L
      break
    }
    offset <- offset + nrep
  }
  if (is.null(country_name)) {
    message("Task ", task_id, " could not be mapped to a country; skipping.")
    quit(save = "no", status = 0)
  }
  spec <- country_specs[[country_name]]
  df <- prepare_country_dataset(spec$paths, spec$surveys)
  if (is.null(df) || !nrow(df)) stop(country_name, ": no data")
  rep_est <- get_replicate_estimate(df, rep_index, country_name)
  out_rds <- file.path(replicate_dir, paste0("replicate_", task_id, ".rds"))
  saveRDS(rep_est, out_rds)
  message("Saved ", out_rds)
  quit(save = "no", status = 0)
}

# ---- Mode: --combine (compute SE from replicate estimates, update CSVs) ----
if (length(args) >= 1 && args[1] == "--combine") {
  full_path <- file.path(out_dir, "full_estimates.rds")
  if (!file.exists(full_path)) stop("Run full estimation first to create ", full_path)
  full_est <- readRDS(full_path)
  rds_files <- list.files(replicate_dir, pattern = "^replicate_[0-9]+\\.rds$", full.names = TRUE)
  if (!length(rds_files)) stop("No replicate files in ", replicate_dir)
  rep_est_all <- bind_rows(lapply(rds_files, readRDS))
  # JKn variance = (r-1)/r * sum((theta_k - theta)^2) per domain; theta = full estimate
  by_domain <- rep_est_all %>%
    left_join(
      full_est %>% select(country, survey_year, agegroup, region, estimate),
      by = c("country", "survey_year", "agegroup", "region")
    ) %>%
    group_by(country, survey_year, agegroup, region) %>%
    summarise(
      n_repl = n(),
      variance = sum((replicate_estimate - estimate)^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      scale = (n_repl - 1) / n_repl,
      variance = scale * variance,
      std_error = sqrt(variance)
    )
  full_est <- full_est %>%
    select(-std_error, -variance, -ci_lower, -ci_upper, -cv) %>%
    left_join(
      by_domain %>% select(country, survey_year, agegroup, region, std_error, variance),
      by = c("country", "survey_year", "agegroup", "region")
    ) %>%
    mutate(
      ci_lower = pmax(0, estimate - 1.96 * std_error),
      ci_upper = pmin(1, estimate + 1.96 * std_error),
      cv = ifelse(estimate > 0, 100 * std_error / estimate, NA_real_)
    )
  for (cn in unique(full_est$country)) {
    out_csv <- file.path(out_dir, paste0("age_region_estimates_", cn, ".csv"))
    full_est %>% filter(country == cn) %>% select(-country) %>% write_csv(out_csv)
    message("Saved ", out_csv, " (with JKn SE from replicates)")
  }
  quit(save = "no", status = 0)
}

# ---- Default: full run (all countries, point estimates + save for combine) ----
args_output <- if (length(args) >= 1) args[1] else character(0)
if (length(args_output)) out_dir <- dirname(args_output[[1]])

n_cores <- as.integer(Sys.getenv("MC_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", "2")))
n_cores <- max(1L, min(n_cores, length(country_specs)))
use_parallel <- n_cores > 1L && length(country_specs) > 1L && .Platform$OS.type == "unix"

if (use_parallel) {
  message("Running ", length(country_specs), " countries in parallel (", n_cores, " cores)")
  processed_list <- mclapply(seq_along(country_specs), function(i) {
    process_country(country_specs[[i]], names(country_specs)[i])
  }, mc.cores = n_cores)
  names(processed_list) <- names(country_specs)
  processed <- compact(processed_list)
} else {
  message("Running countries sequentially")
  processed <- imap(country_specs, process_country) %>% compact()
}

results <- bind_rows(map(processed, ~ .x$estimates))

if (!nrow(results)) {
  stop("No country datasets were processed; check that derived RDS files exist.")
}

for (cn in names(processed)) {
  out_csv <- file.path(out_dir, paste0("age_region_estimates_", cn, ".csv"))
  results %>%
    filter(country == cn) %>%
    select(-country) %>%
    write_csv(out_csv)
  message("Saved ", cn, " estimates to ", out_csv)

  out_rds <- file.path(out_dir, paste0("age_region_cov_2x2_", cn, ".rds"))
  saveRDS(processed[[cn]]$cov_by_region, out_rds)
  message("Saved ", cn, " 2x2 cov matrices by region to ", out_rds)
}

saveRDS(results, file.path(out_dir, "full_estimates.rds"))

if (length(args) == 0) {
  print(results, n = nrow(results))
}
