#!/bin/bash
#SBATCH --job-name=age_region_est
#SBATCH --array=0-499
#SBATCH --output=output/logs/estimate_age_region_%A_%a.out
#SBATCH --error=output/logs/estimate_age_region_%A_%a.err
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --partition=general

# One array task per jackknife replicate. Before submitting:
#   1. Run full estimation once: Rscript scripts/03_analysis/estimate_age_region.R
#   2. Create replicate count file: Rscript scripts/03_analysis/estimate_age_region.R --nrep
#   3. sbatch run_estimate_age_region.sbatch
# Tasks with id >= total replicates exit without error. After all tasks finish, run:
#   Rscript scripts/03_analysis/estimate_age_region.R --combine

cd "${SLURM_SUBMIT_DIR:-.}"

mkdir -p output/logs output/tables output/tables/replicate_estimates

module purge
module add r/4.4.0

Rscript scripts/03_analysis/estimate_age_region.R "${SLURM_ARRAY_TASK_ID}" \
  > output/logs/estimate_age_region_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.Rout 2>&1
