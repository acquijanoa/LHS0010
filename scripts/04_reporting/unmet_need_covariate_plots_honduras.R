#!/usr/bin/env Rscript
# Honduras (DHS/MICS derived): one ggplot per covariate vs unmet need (weighted prevalence + 95% CI).
# Writes PNG figures and a Markdown report. Run from project root:
#   Rscript scripts/04_reporting/unmet_need_covariate_plots_honduras.R
# Optional: Rscript scripts/04_reporting/unmet_need_covariate_plots_honduras.R hnir62fl
#           Rscript scripts/04_reporting/unmet_need_covariate_plots_honduras.R hnir72sd

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(scales)
  library(stringr)
  library(survey)
  library(tidyr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

options(survey.lonely.psu = "adjust")

# --- paths / wd ---
args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

trail <- commandArgs(trailingOnly = TRUE)
survey_key <- if (length(trail) >= 1) tolower(trail[[1]]) else "hnir62fl"
if (!survey_key %in% c("hnir62fl", "hnir72sd", "both")) {
  survey_key <- "hnir62fl"
}

rds_map <- list(
  hnir62fl = list(path = "data/derived/hnir62fl_derived.rds", label = "Honduras DHS 2011–12"),
  hnir72sd = list(path = "data/derived/hnir72sd_derived.rds", label = "Honduras MICS 2019")
)

out_dir <- "output/reports/unmet_need_honduras"
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Human-readable covariate labels (visualization: clear axis titles)
var_labels <- c(
  education_c4 = "Education",
  married_union = "Married or in union",
  urban = "Place of residence",
  windex_c3 = "Wealth (tertile)",
  pregnant = "Currently pregnant",
  sexually_active = "Sexually active or in union (eligibility-related)",
  using_modern_contraceptive = "Using modern contraception",
  employed = "Currently working",
  Adolescent = "Age group (adolescent vs young adult)",
  region = "Region",
  parity = "Parity (number of children ever born)"
)

value_labels <- list(
  education_c4 = c(`0` = "None", `1` = "Primary", `2` = "Secondary", `3` = "Higher"),
  married_union = c(`0` = "No", `1` = "Yes"),
  urban = c(`0` = "Rural", `1` = "Urban"),
  windex_c3 = c(`1` = "Low", `2` = "Middle", `3` = "High"),
  pregnant = c(`0` = "No", `1` = "Yes"),
  sexually_active = c(`0` = "No", `1` = "Yes"),
  using_modern_contraceptive = c(`0` = "No", `1` = "Yes"),
  employed = c(`0` = "No", `1` = "Yes"),
  Adolescent = c(`0` = "Young adult (20–34)", `1` = "Adolescent (<20)")
)

# Covariates to plot (exclude outcome). Skip variables that are all NA in a given survey.
categorical_covars <- c(
  "education_c4", "married_union", "urban", "windex_c3",
  "pregnant", "sexually_active", "using_modern_contraceptive", "employed",
  "Adolescent", "region"
)

prepare_one_survey <- function(path, survey_label) {
  if (!file.exists(path)) {
    warning("Missing file: ", path)
    return(NULL)
  }
  readRDS(path) %>%
    mutate(
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight),
      unmet_need = as.numeric(unmet_need),
      region = as.character(region)
    ) %>%
    filter(
      Adolescent == 1L | Young_Adult == 1L,
      !is.na(unmet_need),
      !is.na(psu_id), !is.na(strat), !is.na(weight), weight > 0
    ) %>%
    mutate(survey_label = survey_label)
}

label_levels <- function(x, var, labs) {
  if (is.null(labs) || length(labs) == 0) return(as.character(x))
  key <- as.character(x)
  out <- unname(labs[key])
  out[is.na(out)] <- key[is.na(out)]
  factor(out, levels = unique(c(unname(labs), setdiff(unique(out), unname(labs)))))
}

# Weighted prevalence of unmet_need by covariate level + 95% CI
prevalence_by_factor <- function(des, var) {
  fml <- as.formula(paste0("~unmet_need"))
  byf <- as.formula(paste0("~", var))
  s <- tryCatch(
    svyby(fml, byf, des, svymean, na.rm = TRUE, keep.var = TRUE),
    error = function(e) NULL
  )
  if (is.null(s)) return(NULL)
  cn <- colnames(s)
  idx <- grep("^unmet_need", cn)[1]
  if (is.na(idx)) return(NULL)
  se_col <- paste0(cn[idx], "SE")
  if (!se_col %in% colnames(s)) {
    # svyby names SE columns differently in some versions
    se_col <- grep("SE$", colnames(s), value = TRUE)[1]
  }
  prop <- as.numeric(s[[cn[idx]]])
  se <- if (!is.na(se_col) && se_col %in% names(s)) as.numeric(s[[se_col]]) else NA_real_
  lvl <- if (var %in% names(s)) as.character(s[[var]]) else as.character(s[, 1])
  tibble(
    level = lvl,
    prop = prop,
    se = se,
    lo = pmax(0, prop - 1.96 * se),
    hi = pmin(1, prop + 1.96 * se)
  )
}

prevalence_parity_binned <- function(df) {
  if (!"parity" %in% names(df)) return(NULL)
  d <- df %>% mutate(
    parity_b = case_when(
      is.na(parity) ~ NA_character_,
      parity <= 0 ~ "0",
      parity == 1 ~ "1",
      parity == 2 ~ "2",
      TRUE ~ "3+"
    )
  )
  d <- d %>% filter(!is.na(parity_b))
  if (nrow(d) < 30) return(NULL)
  des2 <- run_survey(d)
  prevalence_by_factor(des2, "parity_b")
}

plot_prevalence <- function(dat, title, subtitle, ylab = "Weighted prevalence of unmet need") {
  if (is.null(dat) || nrow(dat) < 1) return(NULL)
  dat <- dat %>%
    mutate(
      level = factor(level, levels = unique(level)),
      pct = prop * 100,
      lo_pct = lo * 100,
      hi_pct = hi * 100
    )
  ggplot(dat, aes(x = reorder(level, pct), y = pct)) +
    geom_col(fill = "#2C5282", width = 0.72, alpha = 0.92) +
    geom_errorbar(aes(ymin = lo_pct, ymax = hi_pct), width = 0.22, linewidth = 0.35) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, NA),
      labels = label_percent(scale = 0.01),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = ylab
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

run_survey <- function(df) {
  svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = df, nest = TRUE)
}

md_lines <- character()

append_md <- function(...) {
  md_lines <<- c(md_lines, ...)
}

# --- load ---
if (survey_key == "both") {
  d62 <- prepare_one_survey(rds_map$hnir62fl$path, rds_map$hnir62fl$label)
  d72 <- prepare_one_survey(rds_map$hnir72sd$path, rds_map$hnir72sd$label)
  if (is.null(d62) && is.null(d72)) stop("No Honduras derived RDS found.")
  df_list <- compact(list(d62, d72))
  df <- bind_rows(df_list) %>%
    mutate(
      strat = as.numeric(factor(paste(survey_label, strat))),
      psu_id = as.numeric(factor(paste(survey_label, psu_id)))
    )
  report_title <- "Unmet need vs covariates — Honduras (combined waves)"
} else {
  spec <- rds_map[[survey_key]]
  df <- prepare_one_survey(spec$path, spec$label)
  if (is.null(df)) stop("Could not load: ", spec$path)
  report_title <- paste0("Unmet need vs covariates — ", spec$label)
}

des <- run_survey(df)

append_md(
  "# Unmet need and covariates (Honduras)",
  "",
  paste0("**Survey:** ", unique(df$survey_label) %>% paste(collapse = "; "), "."),
  "",
  "**Analytic sample:** Women in adolescent or young-adult age groups with non-missing unmet need, ",
  "using survey weights (cluster/strata design). *Unmet need* is binary (yes = spacing or limiting need).",
  "",
  "## Visualization choices",
  "",
  "- **Outcome on the vertical axis:** weighted **prevalence of unmet need** (proportion × 100), ",
  "so you can compare levels of need across covariate categories.",
  "- **Error bars:** approximate **95% confidence intervals** from the survey design (uncertainty matters).",
  "- **Horizontal bars:** easier to read long category labels (Cleveland / small-multiples friendly).",
  "- **Single hue:** reduces chart junk; emphasis stays on *magnitude and uncertainty*, not decoration.",
  "",
  "---",
  ""
)

safe_filename <- function(s) {
  s %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

plot_idx <- 0L
for (v in categorical_covars) {
  if (!v %in% names(df)) next
  if (all(is.na(df[[v]]))) next
  des_v <- des
  sub_df <- df %>% filter(!is.na(.data[[v]]))
  if (nrow(sub_df) < 50) next
  des_v <- run_survey(sub_df)
  labs_map <- value_labels[[v]]
  if (v == "region") {
    prev <- prevalence_by_factor(des_v, v)
  } else {
    sub_df <- sub_df %>% mutate(!!v := label_levels(.data[[v]], v, labs_map))
    des_v <- run_survey(sub_df)
    prev <- prevalence_by_factor(des_v, v)
  }
  if (is.null(prev) || nrow(prev) < 2) next
  plot_idx <- plot_idx + 1L
  fn <- file.path(fig_dir, paste0(sprintf("%02d", plot_idx), "_", safe_filename(v), ".png"))
  p <- plot_prevalence(
    prev,
    title = var_labels[[v]] %||% v,
    subtitle = "Among weighted analytic sample; bars = prevalence, lines = 95% CI"
  )
  if (!is.null(p)) {
    ggsave(fn, p, width = 8, height = max(3.5, 0.35 * nrow(prev) + 2), dpi = 150, bg = "white")
    rel <- file.path("figures", basename(fn))
    append_md(
      paste0("## ", plot_idx, ". ", var_labels[[v]] %||% v),
      "",
      paste0("![", v, "](", rel, ")"),
      ""
    )
  }
}

# Parity: binned ordinal prevalence
if ("parity" %in% names(df) && sum(!is.na(df$parity)) > 50) {
  prev_p <- prevalence_parity_binned(df)
  if (!is.null(prev_p) && nrow(prev_p) >= 2) {
    plot_idx <- plot_idx + 1L
    fn <- file.path(fig_dir, paste0(sprintf("%02d", plot_idx), "_parity.png"))
    p <- plot_prevalence(
      prev_p %>% mutate(level = factor(level, levels = c("0", "1", "2", "3+"))),
      title = "Parity",
      subtitle = "Weighted prevalence of unmet need by number of children ever born (binned)"
    )
    ggsave(fn, p, width = 8, height = 4, dpi = 150, bg = "white")
    append_md(
      paste0("## ", plot_idx, ". Parity"),
      "",
      paste0("![parity](figures/", basename(fn), ")"),
      ""
    )
  }
}

append_md(
  "---",
  "",
  "*Report generated by `scripts/04_reporting/unmet_need_covariate_plots_honduras.R`.*"
)

report_path <- file.path(out_dir, "unmet_need_covariates_report.md")
writeLines(md_lines, report_path, useBytes = FALSE)
message("Wrote ", report_path)
message("Figures in ", fig_dir)
