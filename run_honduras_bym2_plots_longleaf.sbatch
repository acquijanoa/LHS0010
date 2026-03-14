#!/bin/bash
#SBATCH --job-name=bym2_plots
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=8g
#SBATCH -t 01:00:00
#SBATCH --output=logs/bym2_plots_%j.out
#SBATCH --error=logs/bym2_plots_%j.err
#SBATCH --chdir=/work/users/a/q/aquijano/LHS0010
#
# Choropleth PNGs under output/spatial_honduras/plots/
# Prereq: BYM2 fit outputs, e.g. shared_bym2_fh_st_prevalence_by_region_time.csv (ST)
#         or shared_bym2_fh_both_waves_region_summary.csv (non-ST)

set -euo pipefail

module purge
module add r/4.4.0

if [[ -n "${LHS_R_LIBS_USER:-}" ]]; then
  export R_LIBS_USER="${LHS_R_LIBS_USER}"
else
  export R_LIBS_USER="$(Rscript -e 'cat(file.path(Sys.getenv("HOME"), "R", paste0(R.version$platform, "-library"), paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed=TRUE)[[1]][1])))')"
fi
mkdir -p "${R_LIBS_USER}"

Rscript -e '
  user <- Sys.getenv("R_LIBS_USER")
  dir.create(user, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user, .libPaths()))
  need_rcpp <- TRUE
  if (requireNamespace("Rcpp", quietly = TRUE)) {
    need_rcpp <- packageVersion("Rcpp") < "1.1.0"
  }
  if (need_rcpp) {
    install.packages("Rcpp", lib = user, repos = "https://cloud.r-project.org")
  }
'

Rscript scripts/03_analysis/honduras_shared_bym2_plot_maps.R
