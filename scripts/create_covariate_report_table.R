#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(reporter)
  library(survey)
  library(tibble)
})

options(survey.lonely.psu = "adjust")

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
    "unfecund",
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

var_labels <- c(
  education_c4 = "Education",
  unmet_need = "Unmet Need",
  married_union = "Married/In Union",
  urban = "Urban Residence",
  windex_c3 = "Wealth Index (3-level)",
  pregnant = "Currently Pregnant",
  sexually_active = "Sexually Active",
  using_modern_contraceptive = "Using Modern Contraceptive",
  unfecund = "Unfecund",
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
      age_grp = agegroup_c4,
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
      age_grp = agegroup_c4,
      value = to_numeric_safe(.data[[var]]),
      psu_id = .data[[design_vars[1]]],
      strat = .data[[design_vars[2]]],
      weight = .data[[design_vars[3]]]
    )

  if (!any(tmp$age_grp %in% unname(age_codes) & !is.na(tmp$value))) return(NULL)

  # Domain estimation from full design.
  des <- svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = tmp, nest = TRUE)
  des_target <- subset(des, age_grp %in% unname(age_codes) & !is.na(value))

  mean_by_age <- tryCatch(
    svyby(
      ~value,
      ~age_grp,
      des_target,
      svymean,
      na.rm = TRUE,
      keep.var = FALSE
    ),
    error = function(e) NULL
  )

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
    if (!is.null(mean_by_age)) {
      mrow <- mean_by_age[mean_by_age$age_grp == grp_code, , drop = FALSE]
      if (nrow(mrow) == 1) mn <- as.numeric(mrow$value)
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

build_summary_table <- function(df) {
  required <- c("agegroup_c4", "psu_id", "strat", "weight", "unmet_need")
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop("Dataset is missing required fields: ", paste(missing_required, collapse = ", "))
  }

  df_clean <- df %>%
    mutate(
      agegroup_c4 = as.integer(agegroup_c4),
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight)
    ) %>%
    filter(
      !is.na(unmet_need),
      !is.na(agegroup_c4),
      !is.na(psu_id),
      !is.na(strat),
      !is.na(weight)
    )

  parts <- list()

  for (v in row_spec$categorical) {
    part <- summarize_categorical(df_clean, v)
    if (!is.null(part)) parts[[length(parts) + 1]] <- part
  }

  for (v in row_spec$continuous) {
    part <- summarize_continuous(df_clean, v)
    if (!is.null(part)) parts[[length(parts) + 1]] <- part
  }

  bind_rows(parts)
}

prepare_analysis_df <- function(df) {
  required <- c("agegroup_c4", "psu_id", "strat", "weight", "unmet_need")
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop("Dataset is missing required fields: ", paste(missing_required, collapse = ", "))
  }

  df %>%
    mutate(
      agegroup_c4 = as.integer(agegroup_c4),
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight)
    ) %>%
    filter(
      !is.na(unmet_need),
      !is.na(agegroup_c4),
      !is.na(psu_id),
      !is.na(strat),
      !is.na(weight)
    )
}

render_report <- function(tbl_df, df_analysis, output_path, dataset_label) {
  n_adol <- sum(as.integer(df_analysis$agegroup_c4) == age_codes[["adolescent"]], na.rm = TRUE)
  n_ya <- sum(as.integer(df_analysis$agegroup_c4) == age_codes[["young_adult"]], na.rm = TRUE)

  report_title <- paste0("Table 1. Baseline Characteristics by Age Group (", dataset_label, ")")
  report_subtitle <- "Unweighted counts and survey-weighted percentages/means"

  tbl <- create_table(tbl_df, first_row_blank = TRUE) %>%
    titles(report_title, report_subtitle) %>%
    stub(c(characteristic_group, characteristic), label = "Characteristics") %>%
    define(characteristic_group, label_row = TRUE, blank_after = TRUE) %>%
    define(characteristic, indent = 0.2, align = "left") %>%
    define(n_adolescent, label = "Unweighted n", align = "center") %>%
    define(stat_adolescent, label = "Weighted %/Mean", align = "center") %>%
    define(n_young_adult, label = "Unweighted n", align = "center") %>%
    define(stat_young_adult, label = "Weighted %/Mean", align = "center") %>%
    spanning_header(from = "n_adolescent", to = "stat_adolescent", label = paste0(age_labels[["adolescent"]], " (N=", n_adol, ")")) %>%
    spanning_header(from = "n_young_adult", to = "stat_young_adult", label = paste0(age_labels[["young_adult"]], " (N=", n_ya, ")")) %>%
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
      full.names = TRUE
      , ignore.case = TRUE
    )
  }

  if (!length(input_files)) {
    stop("No derived datasets found. Provide RDS paths as args or place files in data/derived/.")
  }

  dir.create("output", showWarnings = FALSE, recursive = TRUE)

  for (fp in input_files) {
    dat <- readRDS(fp)
    dat_analysis <- prepare_analysis_df(dat)
    out_name <- paste0(tools::file_path_sans_ext(basename(fp)), "_characteristics.rtf")
    out_path <- file.path("output", out_name)

    tbl_df <- build_summary_table(dat_analysis)
    if (!nrow(tbl_df)) {
      warning("No table rows produced for ", fp, "; skipping")
      next
    }

    dataset_label <- toupper(tools::file_path_sans_ext(basename(fp)))

    render_report(tbl_df, dat_analysis, out_path, dataset_label)

    message("Saved: ", out_path)
  }
}

main()
