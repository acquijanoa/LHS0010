#!/bin/bash
#SBATCH --job-name=bym2_st
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 08:00:00
#SBATCH --output=logs/bym2_st_%j.out
#SBATCH --error=logs/bym2_st_%j.err
#SBATCH --chdir=/work/users/a/q/aquijano/LHS0010

set -euo pipefail

module purge
module add r/4.4.0

# Personal library only (Longleaf: no writes to system R). R prepends R_LIBS_USER to .libPaths().
# Override if needed: export LHS_R_LIBS_USER=/path/to/my/Rlibs before sbatch
if [[ -n "${LHS_R_LIBS_USER:-}" ]]; then
  export R_LIBS_USER="${LHS_R_LIBS_USER}"
else
  export R_LIBS_USER="$(Rscript -e 'cat(file.path(Sys.getenv("HOME"), "R", paste0(R.version$platform, "-library"), paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed=TRUE)[[1]][1])))')"
fi
mkdir -p "${R_LIBS_USER}"

# --- R packages for honduras_shared_bym2_fay_herriot_st.R (all into R_LIBS_USER) ---
# CRAN — spatial:     Rcpp (>=1.1), units, sf, spdep  [spdep also needs sp, spData, boot, deldir, LearnBayes]
# CRAN — tidyverse:  tibble, stringr, purrr, readr, dplyr, tidyr
# CRAN — Stan draws: Matrix, posterior
# CRAN — cmdstanr:   jsonlite, processx  (Imports; install before cmdstanr helps on slow networks)
# NOT CRAN:          cmdstanr  → https://mc-stan.org/r-packages/
# One-time outside R: CmdStan — R: cmdstanr::install_cmdstan()
# -----------------------------------------------------------------------------------
Rscript -e '
  user <- Sys.getenv("R_LIBS_USER")
  dir.create(user, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user, .libPaths()))
  cran <- "https://cloud.r-project.org"
  stan_repos <- c("https://mc-stan.org/r-packages/", cran)

  if (!requireNamespace("Rcpp", quietly = TRUE) || packageVersion("Rcpp") < "1.1.0") {
    install.packages("Rcpp", lib = user, repos = cran)
  }

  cran_pkgs <- c(
    "units", "sf",
    "sp", "spData", "boot", "deldir", "LearnBayes", "spdep",
    "tibble", "stringr", "purrr", "readr", "dplyr", "tidyr",
    "Matrix", "posterior",
    "jsonlite", "processx"
  )
  miss <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) install.packages(miss, lib = user, repos = cran, dependencies = TRUE)

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    install.packages("cmdstanr", lib = user, repos = stan_repos, dependencies = TRUE)
  }
'

# CmdStan C++ (required for sampling). Fixes: "CmdStan path has not been set yet"
# 1) Prefer explicit path:  export CMDSTAN=$HOME/.cmdstan/cmdstan-2.35.0
# 2) Else auto-use newest ~/.cmdstan/cmdstan-* if present
# 3) Else install_cmdstan() into ~/.cmdstan (first run only; needs network; ~15–40 min)
#    Skip auto-install: export LHS_SKIP_INSTALL_CMDSTAN=1  then install once on login node:
#    Rscript -e "library(cmdstanr); check_cmdstan_toolchain(fix=TRUE); install_cmdstan(cores=4)"
Rscript -e '
  suppressPackageStartupMessages(library(cmdstanr))
  skip <- nzchar(Sys.getenv("LHS_SKIP_INSTALL_CMDSTAN", ""))
  cp <- Sys.getenv("CMDSTAN", "")
  ok <- function(p) nzchar(p) && dir.exists(file.path(p, "stan"))
  if (ok(cp)) {
    set_cmdstan_path(cp)
  } else {
    home <- Sys.getenv("HOME")
    cand <- Sys.glob(file.path(home, ".cmdstan", "cmdstan-*"))
    cand <- cand[vapply(cand, ok, logical(1))]
    if (length(cand)) {
      set_cmdstan_path(sort(cand, decreasing = TRUE)[1])
    } else if (!skip) {
      message("Installing CmdStan under ", file.path(home, ".cmdstan"), " (one-time)...")
      check_cmdstan_toolchain(fix = TRUE)
      install_cmdstan(cores = min(4L, parallel::detectCores()), quiet = FALSE)
    } else {
      stop("No CmdStan. Unset LHS_SKIP_INSTALL_CMDSTAN or set CMDSTAN=... or run install_cmdstan() on login.")
    }
  }
  message("CMDSTAN -> ", cmdstan_path())
  d <- file.path(home, ".cmdstan")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(cmdstan_path(), con = file.path(d, "PATH_FOR_SBATCH.txt"))
'

[[ -f "${HOME}/.cmdstan/PATH_FOR_SBATCH.txt" ]] && export CMDSTAN="$(head -1 "${HOME}/.cmdstan/PATH_FOR_SBATCH.txt")"

export LHS_BYM2_CHAINS="${LHS_BYM2_CHAINS:-4}"
export LHS_BYM2_WARMUP="${LHS_BYM2_WARMUP:-1500}"
export LHS_BYM2_SAMPLE="${LHS_BYM2_SAMPLE:-1500}"

Rscript scripts/03_analysis/honduras_shared_bym2_fay_herriot_st.R
