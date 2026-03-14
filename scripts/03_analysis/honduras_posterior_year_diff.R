#!/usr/bin/env Rscript
# Stack posteriors from Honduras 2011-12 and 2019 fits; posterior of (2019 - 2011-12) by region.
# Reads saved CmdStan fits + smoothed CSV (region order = Stan index).
#
#   Rscript scripts/03_analysis/honduras_posterior_year_diff.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
})

args_cmd <- commandArgs(trailingOnly = FALSE)
sp <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(sp) == 1) {
  setwd(dirname(dirname(dirname(normalizePath(sp, mustWork = FALSE)))))
}

out_dir <- "output/spatial_honduras"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

slug12 <- "2011_12"
slug19 <- "2019"
fit12_path <- file.path(out_dir, paste0("fit_", slug12, ".rds"))
fit19_path <- file.path(out_dir, paste0("fit_", slug19, ".rds"))
csv12 <- file.path(out_dir, "smoothed_prevalence_2011_12.csv")
csv19 <- file.path(out_dir, "smoothed_prevalence_2019.csv")

stopifnot(file.exists(fit12_path), file.exists(fit19_path), file.exists(csv12), file.exists(csv19))

fit12 <- readRDS(fit12_path)
fit19 <- readRDS(fit19_path)
lab12 <- read_csv(csv12, show_col_types = FALSE) %>% mutate(idx = row_number())
lab19 <- read_csv(csv19, show_col_types = FALSE) %>% mutate(idx = row_number())

draws12 <- fit12$draws(variables = "p_smoothed", format = "draws_matrix")
draws19 <- fit19$draws(variables = "p_smoothed", format = "draws_matrix")
n12 <- ncol(draws12)
n19 <- ncol(draws19)
n_draw <- min(nrow(draws12), nrow(draws19))
if (n_draw < 100L) stop("Too few posterior draws.")

# Long stack: one row per draw × region × year
stack_year <- function(draws, lab, year_label) {
  R <- nrow(lab)
  stopifnot(ncol(draws) == R)
  # draws columns are p_smoothed[1], ...
  dm <- draws[seq_len(n_draw), , drop = FALSE]
  long <- as_tibble(dm)
  names(long) <- paste0("r", seq_len(R))
  long$draw_id <- seq_len(n_draw)
  long %>%
    pivot_longer(-draw_id, names_to = "r", values_to = "p") %>%
    mutate(idx = as.integer(sub("^r", "", r))) %>%
    left_join(lab %>% select(idx, REGION_NAME), by = "idx") %>%
    transmute(
      draw_id,
      REGION_NAME,
      year = year_label,
      p
    )
}

s12 <- stack_year(draws12, lab12, "2011-12")
s19 <- stack_year(draws19, lab19, "2019")
stacked <- bind_rows(s12, s19)
write_csv(stacked, file.path(out_dir, "posterior_p_stacked_long.csv"))

# Regions present in BOTH years (same name — note 2019 SPS/DC split vs 2011 merged depts)
regions_both <- intersect(lab12$REGION_NAME, lab19$REGION_NAME)
if (length(regions_both) < 3L) stop("Too few common regions.")

w12 <- s12 %>% filter(REGION_NAME %in% regions_both) %>%
  select(draw_id, REGION_NAME, p_12 = p)
w19 <- s19 %>% filter(REGION_NAME %in% regions_both) %>%
  select(draw_id, REGION_NAME, p_19 = p)
# Pair draws by draw_id (same index in each posterior — independent sampling OK for diff distribution)
diff_long <- inner_join(w19, w12, by = c("draw_id", "REGION_NAME")) %>%
  mutate(diff = p_19 - p_12)

summ <- diff_long %>%
  group_by(REGION_NAME) %>%
  summarise(
    mean_diff = mean(diff),
    q025 = quantile(diff, 0.025),
    q975 = quantile(diff, 0.975),
    p_decrease = mean(diff < 0),
    p_increase = mean(diff > 0),
    .groups = "drop"
  ) %>%
  arrange(mean_diff)

write_csv(summ, file.path(out_dir, "posterior_diff_2019_minus_2011_12.csv"))
write_csv(diff_long, file.path(out_dir, "posterior_diff_draws_long.csv"))

# --- Plots ---
summ2 <- summ %>% mutate(REGION_NAME = reorder(REGION_NAME, mean_diff))

p1 <- ggplot(summ2, aes(x = mean_diff, y = REGION_NAME)) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.6) +
  geom_errorbarh(aes(xmin = q025, xmax = q975), height = 0.2, linewidth = 0.35) +
  geom_point(size = 2.2, color = "#2c7fb8") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  labs(
    title = "Honduras adolescents: posterior mean change in smoothed P(unmet need)",
    subtitle = "2019 − 2011-12 (paired posterior draws); common region names only",
    x = "Difference in probability",
    y = NULL,
    caption = "Bars = 95% posterior interval for regional difference."
  )
ggsave(file.path(plot_dir, "posterior_diff_regions_bars.png"), p1, width = 9, height = max(5, 0.28 * nrow(summ2)), dpi = 160, limitsize = FALSE)

# Stacked densities: 2011 vs 2019 for each region (faceted)
s_sub <- stacked %>% filter(REGION_NAME %in% regions_both)
p2 <- ggplot(s_sub, aes(x = p, fill = year)) +
  geom_density(alpha = 0.45) +
  facet_wrap(~REGION_NAME, scales = "free_y", ncol = 4) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("2011-12" = "#3182bd", "2019" = "#e6550d"), name = NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    title = "Stacked posteriors: smoothed P(unmet) by region and survey",
    x = "Probability",
    y = NULL
  )
ggsave(file.path(plot_dir, "posterior_p_stacked_densities.png"), p2, width = 11, height = ceiling(length(regions_both) / 4) * 2.2, dpi = 160, limitsize = FALSE)

# Difference densities by region (optional small multiples)
p3 <- ggplot(diff_long, aes(x = diff)) +
  geom_density(fill = "gray35", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~REGION_NAME, scales = "free_y", ncol = 4) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Posterior of regional change (2019 − 2011-12)", x = "Difference", y = NULL)
ggsave(file.path(plot_dir, "posterior_diff_densities_by_region.png"), p3, width = 11, height = ceiling(length(regions_both) / 4) * 2, dpi = 160, limitsize = FALSE)

message("Wrote:")
message("  ", file.path(out_dir, "posterior_p_stacked_long.csv"))
message("  ", file.path(out_dir, "posterior_diff_2019_minus_2011_12.csv"))
message("  ", file.path(plot_dir, "posterior_diff_regions_bars.png (main year-diff plot)"))
