#!/bin/bash
#SBATCH --job-name=bym2_st
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 08:00:00
#SBATCH --output=logs/bym2_st_%j.out
#SBATCH --error=logs/bym2_st_%j.err

set -euo pipefail

module purge
module add r/4.4.0

export LHS_BYM2_CHAINS="${LHS_BYM2_CHAINS:-4}"
export LHS_BYM2_WARMUP="${LHS_BYM2_WARMUP:-1500}"
export LHS_BYM2_SAMPLE="${LHS_BYM2_SAMPLE:-1500}"

Rscript scripts/03_analysis/honduras_shared_bym2_fay_herriot_st.R

