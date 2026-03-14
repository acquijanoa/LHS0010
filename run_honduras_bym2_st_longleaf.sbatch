#!/bin/bash
#
# Longleaf SLURM — run Honduras spatio-temporal BYM2 Stan script.
# Follows UNC examples: https://help.rc.unc.edu/longleaf-slurm-examples/
# (multi-core job + R module + Rscript).
#
# Submit from repo root:
#   mkdir -p logs
#   sbatch scripts/03_analysis/run_honduras_bym2_st_longleaf.sbatch
#
# Optional (shell, before sbatch):
#   export REPO=/users/o/n/onyen/LHS0010
#   export LHS_BYM2_CHAINS=4 LHS_BYM2_WARMUP=1500 LHS_BYM2_SAMPLE=1500

#SBATCH --job-name=bym2_st
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 08:00:00
#SBATCH --output=logs/bym2_st_%j.out
#SBATCH --error=logs/bym2_st_%j.err
## Optional email (edit onyen):
##SBATCH --mail-type=begin,end,fail
##SBATCH --mail-user=onyen@email.unc.edu

set -euo pipefail

REPO="${REPO:-$HOME/LHS0010}"
cd "${REPO}"
mkdir -p logs output/spatial_honduras

# R on Longleaf — same pattern as “Application commands → R” in:
# https://help.rc.unc.edu/longleaf-slurm-examples/
module purge
module add r/4.4.0

export LHS_BYM2_CHAINS="${LHS_BYM2_CHAINS:-4}"
export LHS_BYM2_WARMUP="${LHS_BYM2_WARMUP:-1500}"
export LHS_BYM2_SAMPLE="${LHS_BYM2_SAMPLE:-1500}"

echo "Host: $(hostname)  $(date)"
echo "REPO=$REPO"
echo "CPUs: ${SLURM_CPUS_PER_TASK:-?}"
module list
which Rscript
Rscript --version

# Equivalent to: R CMD BATCH --no-save scripts/.../wrapper.R
# Rscript keeps stdout/stderr in the SLURM .out/.err files above.
Rscript scripts/03_analysis/honduras_shared_bym2_fay_herriot_st.R

echo "Finished: $(date)"
