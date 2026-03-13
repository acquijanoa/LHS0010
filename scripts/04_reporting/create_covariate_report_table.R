#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(reporter)
  library(survey)
  library(tibble)
})

options(survey.lonely.psu = "adjust")

# Run from project root when invoked via Rscript
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ------------------------------
# User config (easy include/exclude)
# ------------------------------
row_spec <- list(
  categorical = c(
    "education_c4",
    "unmet_need",
    "married_union",
    "urban",
    "windex_c3",
    "pregnant",
    "sexually_active",
    "using_modern_contraceptive",
    "employed"
  ),
  continuous = c(
    "parity"
  )
)

# If TRUE, categorical percentages use full age-group denominator by adding
# an explicit "Missing" row.
# If FALSE (default), percentages match svymean(~factor(var), subset_design),
# i.e., they are among non-missing values of each variable.
include_missing_level <- FALSE

# Age groups to compare (derived agegroup_c4 coding)
age_codes <- c(adolescent = 1L, young_adult = 2L)
age_labels <- c(adolescent = "Adolescent", young_adult = "Young Adults")

# Survey file stem -> (country_code, country_name, year_label) for grouping and spanning headers
# File stem is basename without _derived.rds (e.g. coir61fl, coir72fl, hnir62fl, hnir72sd)
survey_config <- list(
  coir61fl = list(country_code = "COIR", country_name = "Colombia", year_label = "2010"),
  coir72fl = list(country_code = "COIR", country_name = "Colombia", year_label = "2015"),
  hnir62fl = list(country_code = "HNIR", country_name = "Honduras", year_label = "2011-12"),
  hnir72sd = list(country_code = "HNIR", country_name = "Honduras", year_label = "2019")
)

var_labels <- c(
  education_c4 = "Education",
  unmet_need = "Unmet Need",
  married_union = "Married/In Union",
  urban = "Urban Residence",
  windex_c3 = "Wealth Index (3-level)",
  pregnant = "Currently Pregnant",
  sexually_active = "Sexually Active",
  using_modern_contraceptive = "Using Modern Contraceptive",
  employed = "Currently Working",
  parity = "Parity"
)

value_labels <- list(
  education_c4 = c(`0` = "No education", `1` = "Primary", `2` = "Secondary", `3` = "Higher"),
  unmet_need = c(`0` = "No", `1` = "Yes"),
  married_union = c(`0` = "No", `1` = "Yes"),
  urban = c(`0` = "Rural", `1` = "Urban"),
  windex_c3 = c(`1` = "Low", `2` = "Middle", `3` = "High"),
  pregnant = c(`0` = "No", `1` = "Yes"),
  sexually_active = c(`0` = "No", `1` = "Yes"),
  using_modern_contraceptive = c(`0` = "No", `1` = "Yes"),
  unfecund = c(`0` = "No", `1` = "Yes"),
  employed = c(`0` = "No", `1` = "Yes")
)

format_pct <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f%%", x))
format_mean <- function(x) ifelse(is.na(x), "NA", sprintf("%.2f", x))

to_numeric_safe <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  vals <- suppressWarnings(as.character(haven::as_factor(x, levels = "values")))
  out <- suppressWarnings(as.numeric(vals))
  if (!all(is.na(out))) return(out)
  lbl <- suppressWarnings(as.character(haven::as_factor(x, levels = "labels")))
  suppressWarnings(as.numeric(lbl))
}

safe_svymean <- function(formula, design_obj) {
  out <- tryCatch(svymean(formula, design_obj, na.rm = TRUE), error = function(e) NULL)
  out
}

summarize_categorical <- function(df, var, design_vars = c("psu_id", "strat", "weight")) {
  if (!var %in% names(df)) return(NULL)

  tmp <- df %>%
    transmute(
      age_grp = age_grp,
      value = .data[[var]],
      psu_id = .data[[design_vars[1]]],
      strat = .data[[design_vars[2]]],
      weight = .data[[design_vars[3]]]
    )

  tmp <- tmp %>%
    mutate(
      value_chr = as.character(value),
      value_chr = ifelse(is.na(value_chr) | value_chr == "NA", NA_character_, value_chr)
    )

  tmp_target <- tmp %>% filter(age_grp %in% unname(age_codes))
  levels_all <- sort(unique(tmp_target$value_chr[!is.na(tmp_target$value_chr)]))
  if (!length(levels_all)) return(NULL)

  # Build design on the full analysis dataset, then domain-subset by age group.
  des_full <- svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = tmp, nest = TRUE)

  rows <- list()

  grp_stats <- list()
  for (grp in names(age_codes)) {
    grp_code <- age_codes[[grp]]
    dat_grp <- tmp_target %>% filter(age_grp == grp_code)
    dat_grp_nm <- dat_grp %>% filter(!is.na(value_chr))

    n_by_level <- setNames(rep(0L, length(levels_all)), as.character(levels_all))
    pct_by_level <- setNames(rep(NA_real_, length(levels_all)), as.character(levels_all))

    if (nrow(dat_grp) > 0) {
      dat_for_n <- if (include_missing_level) {
        dat_grp %>% mutate(value_chr = coalesce(value_chr, "Missing"))
      } else {
        dat_grp_nm
      }

      levels_for_grp <- if (include_missing_level) {
        sort(unique(c(levels_all, "Missing")))
      } else {
        levels_all
      }

      n_by_level <- setNames(rep(0L, length(levels_for_grp)), levels_for_grp)
      pct_by_level <- setNames(rep(NA_real_, length(levels_for_grp)), levels_for_grp)

      n_obs <- table(factor(dat_for_n$value_chr, levels = levels_for_grp))
      n_by_level[names(n_obs)] <- as.integer(n_obs)

      if (include_missing_level) {
        des_grp <- subset(des_full, age_grp == grp_code)
        p_raw <- sapply(levels_for_grp, function(lv) {
          est <- safe_svymean(~I(coalesce(value_chr, "Missing") == lv), des_grp)
          if (is.null(est)) NA_real_ else as.numeric(coef(est)[1]) * 100
        })
        names(p_raw) <- levels_for_grp

        denom <- sum(p_raw, na.rm = TRUE)
        if (is.finite(denom) && denom > 0 && abs(denom - 100) < 1e-6) {
          pct_by_level <- p_raw
        } else if (is.finite(denom) && denom > 0) {
          pct_by_level <- (p_raw / denom) * 100
        } else {
          pct_by_level <- p_raw
        }
      } else {
        # Match svymean(~factor(var), subset_design) among non-missing values.
        if (nrow(dat_grp_nm) > 0) {
          des_grp <- subset(des_full, age_grp == grp_code & !is.na(value_chr))
          est <- tryCatch(
            svymean(~factor(value_chr, levels = levels_for_grp), des_grp, na.rm = TRUE),
            error = function(e) NULL
          )
          if (!is.null(est)) {
            pct_by_level <- as.numeric(coef(est)) * 100
            names(pct_by_level) <- levels_for_grp
          }
        }
      }
    }

    grp_stats[[grp]] <- list(n = n_by_level, pct = pct_by_level)
  }

  levels_out <- if (include_missing_level) sort(unique(c(levels_all, "Missing"))) else levels_all

  for (lv in levels_out) {
    lv_key <- as.character(lv)
    lv_name <- if (lv_key == "Missing") "Missing" else (value_labels[[var]][[lv_key]] %||% lv_key)

    row <- tibble(
      characteristic_group = var_labels[[var]] %||% var,
      characteristic = lv_name,
      n_adolescent = as.character(grp_stats$adolescent$n[[lv_key]] %||% 0L),
      stat_adolescent = format_pct(grp_stats$adolescent$pct[[lv_key]]),
      n_young_adult = as.character(grp_stats$young_adult$n[[lv_key]] %||% 0L),
      stat_young_adult = format_pct(grp_stats$young_adult$pct[[lv_key]])
    )

    rows[[length(rows) + 1]] <- row
  }

  bind_rows(rows)
}

summarize_continuous <- function(df, var, design_vars = c("psu_id", "strat", "weight")) {
  if (!var %in% names(df)) return(NULL)

  tmp <- df %>%
    transmute(
      age_grp = age_grp,
      value = to_numeric_safe(.data[[var]]),
      psu_id = .data[[design_vars[1]]],
      strat = .data[[design_vars[2]]],
      weight = .data[[design_vars[3]]]
    )

  if (!any(tmp$age_grp %in% unname(age_codes) & !is.na(tmp$value))) return(NULL)

  des <- svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = tmp, nest = TRUE)
  des_target <- subset(des, age_grp %in% unname(age_codes) & !is.na(value))

  row <- tibble(
    characteristic_group = var_labels[[var]] %||% var,
    characteristic = "Mean",
    n_adolescent = "",
    stat_adolescent = "",
    n_young_adult = "",
    stat_young_adult = ""
  )

  for (grp in names(age_codes)) {
    grp_code <- age_codes[[grp]]
    grp_n <- sum(tmp$age_grp == grp_code & !is.na(tmp$value), na.rm = TRUE)
    mn <- NA_real_
    des_grp <- subset(des_target, age_grp == grp_code)
    if (nrow(des_grp) > 0) {
      est <- tryCatch(svymean(~value, des_grp, na.rm = TRUE), error = function(e) NULL)
      if (!is.null(est)) mn <- as.numeric(coef(est)[1])
    }
    if (grp == "adolescent") {
      row$n_adolescent <- as.character(grp_n)
      row$stat_adolescent <- format_mean(mn)
    } else {
      row$n_young_adult <- as.character(grp_n)
      row$stat_young_adult <- format_mean(mn)
    }
  }

  row
}

# Placeholder row(s) when a variable has no data in this survey (so the final merged table still has the row).
placeholder_categorical <- function(var) {
  lbls <- value_labels[[var]]
  if (is.null(lbls) || length(lbls) == 0) return(NULL)
  tibble(
    characteristic_group = var_labels[[var]] %||% var,
    characteristic = unname(lbls),
    n_adolescent = "",
    stat_adolescent = "NA",
    n_young_adult = "",
    stat_young_adult = "NA"
  )
}

placeholder_continuous <- function(var) {
  tibble(
    characteristic_group = var_labels[[var]] %||% var,
    characteristic = "Mean",
    n_adolescent = "",
    stat_adolescent = "NA",
    n_young_adult = "",
    stat_young_adult = "NA"
  )
}

build_summary_table <- function(df) {
  required <- c("Adolescent", "Young_Adult", "psu_id", "strat", "weight", "unmet_need")
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop("Dataset is missing required fields: ", paste(missing_required, collapse = ", "))
  }

  df_clean <- df %>%
    mutate(
      Adolescent = as.integer(Adolescent),
      Young_Adult = as.integer(Young_Adult),
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight),
      age_grp = if_else(Adolescent == 1L, 1L, if_else(Young_Adult == 1L, 2L, NA_integer_))
    ) %>%
    filter(
      !is.na(unmet_need),
      !is.na(age_grp),
      !is.na(psu_id),
      !is.na(strat),
      !is.na(weight)
    )

  parts <- list()

  for (v in row_spec$categorical) {
    part <- summarize_categorical(df_clean, v)
    if (!is.null(part)) {
      parts[[length(parts) + 1]] <- part
    } else {
      ph <- placeholder_categorical(v)
      if (!is.null(ph)) parts[[length(parts) + 1]] <- ph
    }
  }

  for (v in row_spec$continuous) {
    part <- summarize_continuous(df_clean, v)
    if (!is.null(part)) {
      parts[[length(parts) + 1]] <- part
    } else {
      parts[[length(parts) + 1]] <- placeholder_continuous(v)
    }
  }

  bind_rows(parts)
}

prepare_analysis_df <- function(df) {
  required <- c("Adolescent", "Young_Adult", "psu_id", "strat", "weight", "unmet_need")
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop("Dataset is missing required fields: ", paste(missing_required, collapse = ", "))
  }

  df %>%
    mutate(
      Adolescent = as.integer(Adolescent),
      Young_Adult = as.integer(Young_Adult),
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight)
    ) %>%
    filter(
      !is.na(unmet_need),
      (Adolescent == 1L | Young_Adult == 1L),
      !is.na(psu_id),
      !is.na(strat),
      !is.na(weight)
    )
}

# Build one merged table for a country: one summary table per survey, joined on (characteristic_group, characteristic).
# Returns list(merged_tbl, n_surveys, n_adol, n_ya) for successful surveys only.
build_country_table <- function(file_paths, stems) {
  stopifnot(length(file_paths) == length(stems))
  tables <- list()
  n_adol_vec <- integer(0)
  n_ya_vec <- integer(0)

  for (i in seq_along(file_paths)) {
    dat <- readRDS(file_paths[i])
    dat_analysis <- prepare_analysis_df(dat)
    tbl_i <- build_summary_table(dat_analysis)
    if (!nrow(tbl_i)) {
      warning("No table rows for ", file_paths[i], "; skipping survey.")
      next
    }
    s <- length(tables) + 1L
    n_adol_vec[s] <- sum(dat_analysis$Adolescent == 1L, na.rm = TRUE)
    n_ya_vec[s] <- sum(dat_analysis$Young_Adult == 1L, na.rm = TRUE)
    value_cols <- c("n_adolescent", "stat_adolescent", "n_young_adult", "stat_young_adult")
    tbl_i <- tbl_i %>%
      rename_with(~ paste0(.x, "_", s), .cols = all_of(value_cols))
    tables[[s]] <- tbl_i
  }

  if (length(tables) == 0) return(NULL)
  merged <- tables[[1]]
  for (i in seq_along(tables)[-1]) {
    merged <- merged %>%
      left_join(
        tables[[i]] %>% select(characteristic_group, characteristic, ends_with(paste0("_", i))),
        by = c("characteristic_group", "characteristic")
      )
  }

  # Reorder columns so AYA is top level: all Adolescent cols (each survey: n, %), then all Young Adults cols
  n_surveys <- length(tables)
  stub_cols <- c("characteristic_group", "characteristic")
  adol_cols <- as.vector(rbind(
    paste0("n_adolescent_", seq_len(n_surveys)),
    paste0("stat_adolescent_", seq_len(n_surveys))
  ))
  ya_cols <- as.vector(rbind(
    paste0("n_young_adult_", seq_len(n_surveys)),
    paste0("stat_young_adult_", seq_len(n_surveys))
  ))
  merged <- merged %>% select(all_of(c(stub_cols, adol_cols, ya_cols)))

  list(
    merged_tbl = merged,
    n_surveys = n_surveys,
    n_adol = n_adol_vec,
    n_ya = n_ya_vec
  )
}

# Render one RTF per country: AYA (Adolescent / Young Adults) as top level, survey years nested under each.
render_country_report <- function(country_result, year_labels, country_name, output_path) {
  tbl_df <- country_result$merged_tbl
  n_surveys <- country_result$n_surveys
  n_adol <- country_result$n_adol
  n_ya <- country_result$n_ya

  report_title <- paste0("Table 1. Baseline Characteristics by Age Group (", country_name, ")")
  report_subtitle <- "Unweighted counts and survey-weighted percentages/means; age group over survey year"

  tbl <- create_table(tbl_df, first_row_blank = TRUE) %>%
    titles(report_title, report_subtitle) %>%
    stub(c(characteristic_group, characteristic), label = "Characteristics") %>%
    define(characteristic_group, label_row = TRUE, blank_after = TRUE) %>%
    define(characteristic, indent = 0.2, align = "left")

  # Columns are ordered: Adolescent (survey1 n, %, survey2 n, %, ...), then Young Adults (survey1 n, %, ...)
  for (s in seq_len(n_surveys)) {
    n_a <- paste0("n_adolescent_", s)
    sa <- paste0("stat_adolescent_", s)
    n_y <- paste0("n_young_adult_", s)
    sy <- paste0("stat_young_adult_", s)
    tbl <- tbl %>%
      define(n_a, label = "Unweighted n", align = "center", standard_eval = TRUE) %>%
      define(sa, label = "Weighted %/Mean", align = "center", standard_eval = TRUE) %>%
      define(n_y, label = "Unweighted n", align = "center", standard_eval = TRUE) %>%
      define(sy, label = "Weighted %/Mean", align = "center", standard_eval = TRUE)
  }

  # Reporter: level 1 = closest to columns, level 2 = above it. So AYA on top = level 2, survey year below = level 1.
  n_a_1 <- paste0("n_adolescent_", 1)
  sa_last <- paste0("stat_adolescent_", n_surveys)
  n_y_1 <- paste0("n_young_adult_", 1)
  sy_last <- paste0("stat_young_adult_", n_surveys)
  tbl <- tbl %>%
    spanning_header(from = n_a_1, to = sa_last, label = age_labels[["adolescent"]], level = 2, standard_eval = TRUE) %>%
    spanning_header(from = n_y_1, to = sy_last, label = age_labels[["young_adult"]], level = 2, standard_eval = TRUE)

  # Level 1 (row below AYA): one span per survey year under each AYA (e.g. 2010 (N=...), 2015 (N=...) for Colombia)
  for (s in seq_len(n_surveys)) {
    n_a <- paste0("n_adolescent_", s)
    sa <- paste0("stat_adolescent_", s)
    n_y <- paste0("n_young_adult_", s)
    sy <- paste0("stat_young_adult_", s)
    year_lbl <- year_labels[s]
    tbl <- tbl %>%
      spanning_header(from = n_a, to = sa, label = paste0(year_lbl, " (N=", n_adol[s], ")"), level = 1, standard_eval = TRUE) %>%
      spanning_header(from = n_y, to = sy, label = paste0(year_lbl, " (N=", n_ya[s], ")"), level = 1, standard_eval = TRUE)
  }

  tbl <- tbl %>%
    footnotes("Weighted estimates use survey::svydesign with dataset-specific psu_id/strat/weight.")

  rpt <- create_report(output_path, output_type = "RTF", font = "Times", font_size = 10) %>%
    add_content(tbl)

  write_report(rpt)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  input_files <- if (length(args) > 0) {
    args
  } else {
    list.files(
      path = "data/derived",
      pattern = "(hnir|coir).+_derived\\.rds$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  }

  if (!length(input_files)) {
    stop("No derived datasets found. Provide RDS paths as args or place files in data/derived/.")
  }

  # Stem = basename without _derived.rds (e.g. coir61fl, hnir72sd)
  stems <- tolower(sub("_derived\\.rds$", "", basename(input_files), ignore.case = TRUE))
  # Group by country_code
  country_codes <- vapply(stems, function(s) {
    cfg <- survey_config[[s]]
    if (is.null(cfg)) NA_character_ else cfg$country_code
  }, character(1))
  valid <- !is.na(country_codes)
  if (!any(valid)) {
    stop("No derived files matched survey_config (coir61fl, coir72fl, hnir62fl, hnir72sd).")
  }
  input_files <- input_files[valid]
  stems <- stems[valid]
  country_codes <- country_codes[valid]

  dir.create("output/reports", showWarnings = FALSE, recursive = TRUE)

  for (country_code in unique(country_codes)) {
    idx <- which(country_codes == country_code)
    files_country <- input_files[idx]
    stems_country <- stems[idx]
    # Order by survey year (use order of survey_config for consistency)
    order_idx <- order(match(stems_country, names(survey_config)))
    files_country <- files_country[order_idx]
    stems_country <- stems_country[order_idx]

    cfg1 <- survey_config[[stems_country[1]]]
    country_name <- cfg1$country_name
    year_labels <- vapply(stems_country, function(s) {
      survey_config[[s]]$year_label
    }, character(1))

    country_result <- build_country_table(files_country, stems_country)
    if (is.null(country_result)) {
      warning("No table produced for ", country_code, "; skipping.")
      next
    }

    out_path <- file.path("output", "reports", paste0(country_code, "_characteristics.rtf"))
    render_country_report(country_result, year_labels, country_name, out_path)
    message("Saved: ", out_path)
  }
}

main()
