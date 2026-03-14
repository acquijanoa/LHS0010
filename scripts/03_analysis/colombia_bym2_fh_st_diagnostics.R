#!/usr/bin/env Rscript
# Diagnostics (Rhat, ESS, sampler) + PPC for Colombia BYM2 FH fit.
#
#   Rscript scripts/03_analysis/colombia_bym2_fh_st_diagnostics.R
#
# Requires: cmdstanr, readr, dplyr, bayesplot, ggplot2
# Inputs: output/spatial_colombia_st/colombia_bym2_fh_st_fit.rds
#         output/tables/colombia_region_adolescent_logit_prevalence.csv
# Outputs: output/spatial_colombia_st/diagnostics/

suppressPackageStartupMessages({
  library(cmdstanr)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(bayesplot)
  library(ggplot2)
  library(scales)
})

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

fit_rds <- "output/spatial_colombia_st/colombia_bym2_fh_st_fit.rds"
est_csv <- "output/tables/colombia_region_adolescent_logit_prevalence.csv"
out_dir <- "output/spatial_colombia_st/diagnostics"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(fit_rds), file.exists(est_csv))

regions_all <- c(
  "Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas", "Caqueta", "Cauca",
  "Cesar", "Cordoba", "Cundinamarca", "Choco", "Huila", "La Guajira", "Magdalena", "Meta",
  "Narimo", "Norte de Santander", "Quindio", "Risaralda", "Santander", "Sucre", "Tolima",
  "Valle", "Arauca", "Casanare", "Putumayo", "San Andres y Providencia", "Amazonas",
  "Guainia", "Guaviare", "Vaupes", "Vichada"
)
surveys <- c("2005", "2010", "2015")

fit <- readRDS(fit_rds)

summ <- fit$summary()
write_csv(summ, file.path(out_dir, "summary_all_parameters.csv"))

bad_rhat <- summ %>% filter(rhat > 1.01 | is.na(rhat))
if (nrow(bad_rhat)) write_csv(bad_rhat, file.path(out_dir, "warning_rhat_gt_1_01.csv"))

low_ess <- summ %>% filter(ess_bulk < 400 | ess_tail < 400 | is.na(ess_bulk))
if (nrow(low_ess)) write_csv(low_ess, file.path(out_dir, "warning_ess_below_400.csv"))

diag_txt <- capture.output(print(fit$diagnostic_summary()))
writeLines(diag_txt, file.path(out_dir, "sampler_diagnostic_summary.txt"))
message("Sampler diagnostics:\n", paste(diag_txt, collapse = "\n"))

tbl <- read_csv(est_csv, show_col_types = FALSE, col_types = readr::cols(survey = readr::col_character())) %>%
  mutate(region = as.character(region), survey = as.character(survey))

grid <- expand.grid(region = regions_all, survey = as.character(surveys), stringsAsFactors = FALSE) %>%
  mutate(region = as.character(region), survey = as.character(survey)) %>%
  left_join(tbl %>% select(region, survey, has_obs, logit_prevalence, se_logit), by = c("region", "survey")) %>%
  mutate(
    has_obs = as.integer(ifelse(is.na(has_obs), 0L, has_obs)),
    y = ifelse(has_obs == 1L & is.finite(logit_prevalence), logit_prevalence, 0),
    v = ifelse(has_obs == 1L & is.finite(se_logit) & se_logit > 0, se_logit^2, 1.0)
  )

idx <- which(grid$has_obs == 1L)
y_obs <- grid$y[idx]
v_obs <- pmax(grid$v[idx], 1e-10)
cell_lab <- paste(grid$region[idx], grid$survey[idx], sep = " | ")

theta_vars <- paste0("theta[", idx, "]")
draws_theta <- fit$draws(variables = theta_vars, format = "draws_matrix")
n_draws <- nrow(draws_theta)
S <- min(n_draws, 4000L)
set.seed(1)
rows_s <- sample.int(n_draws, S)
theta_s <- draws_theta[rows_s, , drop = FALSE]

y_rep <- matrix(NA_real_, nrow = S, ncol = length(idx))
for (j in seq_along(idx)) {
  y_rep[, j] <- theta_s[, j] + sqrt(v_obs[j]) * rnorm(S)
}

p1 <- ppc_dens_overlay(y_obs, y_rep) +
  labs(title = "Colombia PPC: logit direct estimates", subtitle = "y vs replicated y | theta, v") +
  theme_bw()
ggsave(file.path(out_dir, "ppc_dens_overlay_logit.png"), p1, width = 8, height = 5, dpi = 120)

p2 <- ppc_intervals(y_obs, y_rep, prob = 0.5, prob_outer = 0.9) +
  labs(title = "Colombia PPC: 50% & 90% intervals (by cell)") +
  theme_bw() +
  theme(axis.text.y = element_blank())
ggsave(file.path(out_dir, "ppc_intervals_by_cell.png"), p2, width = 8, height = 6, dpi = 120)

ggsave(file.path(out_dir, "ppc_stat_mean.png"), ppc_stat(y_obs, y_rep, stat = "mean") + theme_bw(), width = 6, height = 4, dpi = 120)
ggsave(file.path(out_dir, "ppc_stat_sd.png"), ppc_stat(y_obs, y_rep, stat = "sd") + theme_bw(), width = 6, height = 4, dpi = 120)

# ----- National total prevalence: p_total[t] = inv_logit(alpha + beta[t]) -----
p_total_draws <- tryCatch(
  fit$draws(variables = "p_total", format = "draws_matrix"),
  error = function(e) NULL
)
if (is.null(p_total_draws) || ncol(p_total_draws) != length(surveys)) {
  dm <- fit$draws(
    variables = c("alpha", paste0("beta[", seq_along(surveys), "]")),
    format = "draws_matrix"
  )
  a <- dm[, "alpha"]
  n <- length(a)
  p_total_draws <- matrix(NA_real_, n, length(surveys))
  colnames(p_total_draws) <- surveys
  p_total_draws[, 1] <- plogis(a)
  for (t in seq_len(length(surveys))[-1]) {
    p_total_draws[, t] <- plogis(a + dm[, paste0("beta[", t, "]")])
  }
} else {
  colnames(p_total_draws) <- surveys
}
readr::write_csv(
  tibble(
    survey = surveys,
    p_total_mean = as.numeric(colMeans(p_total_draws)),
    p_total_q025 = as.numeric(apply(p_total_draws, 2L, quantile, 0.025)),
    p_total_q975 = as.numeric(apply(p_total_draws, 2L, quantile, 0.975))
  ),
  file.path(dirname(out_dir), "colombia_bym2_fh_st_p_total_by_wave.csv")
)
pt_long <- as_tibble(p_total_draws) %>%
  pivot_longer(everything(), names_to = "survey_wave", values_to = "prevalence")
pt_long$survey_wave <- factor(pt_long$survey_wave, levels = surveys)
p_pt <- ggplot(pt_long, aes(x = survey_wave, y = prevalence)) +
  geom_violin(fill = "steelblue", alpha = 0.35, color = NA) +
  stat_summary(fun = median, geom = "point", size = 2) +
  stat_summary(
    fun.min = function(z) quantile(z, 0.025),
    fun.max = function(z) quantile(z, 0.975),
    geom = "linerange",
    linewidth = 0.8
  ) +
  labs(
    title = "Colombia: national total prevalence by wave (alpha + beta)",
    subtitle = "Posterior of inv_logit(alpha + beta[t]); no spatial random effect",
    x = "Survey wave",
    y = "Prevalence (proportion)"
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  theme_bw()
ggsave(file.path(out_dir, "p_total_national_by_wave.png"), p_pt, width = 8, height = 5, dpi = 120)

summ_pt <- tibble(
  survey = surveys,
  mean = colMeans(p_total_draws),
  q025 = apply(p_total_draws, 2L, quantile, 0.025),
  q975 = apply(p_total_draws, 2L, quantile, 0.975)
)
p_pt2 <- ggplot(summ_pt, aes(x = survey, y = mean, ymin = q025, ymax = q975, group = 1)) +
  geom_ribbon(alpha = 0.2, fill = "steelblue") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  labs(
    title = "Colombia: national total prevalence (mean & 95% CrI)",
    subtitle = "inv_logit(alpha + beta[t])",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  theme_bw()
ggsave(file.path(out_dir, "p_total_national_mean_interval.png"), p_pt2, width = 7, height = 4.5, dpi = 120)

theta_mean <- colMeans(draws_theta)
z <- (y_obs - theta_mean) / sqrt(v_obs)
write_csv(
  tibble(cell = cell_lab, y = y_obs, theta_mean = theta_mean, v = v_obs, z = z),
  file.path(out_dir, "standardized_residuals_observed_cells.csv")
)

message("Wrote Colombia diagnostics to ", normalizePath(out_dir, mustWork = FALSE))
