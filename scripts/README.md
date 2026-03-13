# Scripts

Run all steps from **project root** (see main README).

**Paths:** All scripts use project-relative paths (`data/`, `output/`, `docs/`). When you run a script with `Rscript scripts/.../file.R` from the repo root, the script sets the working directory to the project root automatically, so there are no path conflicts. If you run a script interactively (e.g. Source in RStudio), set the working directory to the project root first (e.g. **Session → Set Working Directory → To Project Directory**), or paths like `data/derived` will be resolved relative to the current directory and may fail.

- **01_metadata** – Generate variable metadata JSON from SAS files.
- **02_derive** – Build derived analysis datasets from raw DHS recode files.
- **03_analysis** – Direct estimates and spatial modeling.
- **04_reporting** – Covariate tables, derived-variable dictionary, and Honduras unmet-need plots (`unmet_need_covariate_plots_honduras.R` → `output/reports/unmet_need_honduras/unmet_need_covariates_report.md`).
- **scratch** – One-off or test scripts.
- **run_all.R** – Run full pipeline: metadata → derive → estimates → reports.
