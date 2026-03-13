# LHS0010

This repository contains scripts to derive and document variables from DHS Honduras (HNIR) and Colombia (COIR) datasets. Data files are not included in version control. Place the DHS data files in the expected folders and run the pipeline to reproduce derived datasets and outputs.

**Project (Latin America Health Studies, LHS0010):** Geographic inequities in adolescent unmet need for modern contraception in Honduras.

**Objective:** Quantify adolescent (<19) versus adult (24–35) differences in unmet need for modern contraception and assess subnational geographic inequities using DHS/ENDESA data and spatial smoothing.

---

## Project structure

```
LHS0010/
  data/                    # Not in git. Raw + derived data (see Data layout below).
  docs/                    # Prose and variable metadata JSON (see docs/README.md).
  output/
    tables/                # CSV outputs (e.g. age-by-region estimates).
    reports/               # RTF/PDF reports (e.g. baseline characteristics).
  scripts/
    01_metadata/           # Generate variable metadata JSON from SAS.
    02_derive/             # Derive analysis datasets from raw recode files.
    03_analysis/           # Modeling and direct estimates.
    04_reporting/          # Tables and report generation.
    scratch/               # One-off or test scripts.
    run_all.R              # Single entry point to run the full pipeline.
```

---

## Data layout

Create and populate `data/` as follows (all paths relative to project root). See **`data/README.md`** for the same layout.

| Path | Description |
|------|-------------|
| `data/raw/HNIR62SD/hnir62fl.sas7bdat` | Honduras DHS IR 62 recode |
| `data/raw/HNIR72SD/hnir72sd.sas7bdat` | Honduras DHS IR 72 recode |
| `data/raw/COIR61SD/COIR61FL.SAS7BDAT` | Colombia DHS IR 61 recode |
| `data/raw/COIR72SD/COIR72FL.SAS7BDAT` | Colombia DHS IR 72 recode |
| `data/derived/*.rds` | Derived datasets (produced by `02_derive` scripts) |

Optional: SAS setup files (e.g. `HNIR62FL.SAS`) in the same folder as the `.sas7bdat` for value labels in metadata. The `data/raw/` and `data/derived/` directories are kept in the tree via `.gitkeep`; contents are not versioned.

---

## Scripts

| Folder | Script | Purpose |
|--------|--------|---------|
| **01_metadata** | `generate_variable_metadata.R` | Build JSON variable metadata for HNIR* SAS files in `data/raw/`. |
| **01_metadata** | `generate_coir61fl_metadata.R` | Build JSON variable metadata for COIR61FL. |
| **01_metadata** | `generate_coir72fl_metadata.R` | Build JSON variable metadata for COIR72FL. |
| **02_derive** | `derive_hnir62.R` | Derive analysis variables for HNIR62 → `data/derived/hnir62fl_derived.rds`. |
| **02_derive** | `derive_hnir72.R` | Derive analysis variables for HNIR72 → `data/derived/hnir72sd_derived.rds`. |
| **02_derive** | `derive_coir61.R` | Derive analysis variables for COIR61 → `data/derived/coir61fl_derived.rds`. |
| **02_derive** | `derive_coir72.R` | Derive analysis variables for COIR72 → `data/derived/coir72fl_derived.rds`. |
| **03_analysis** | `estimate_age_region.R` | Design-based age-by-region unmet-need estimates (CSV). |
| **03_analysis** | `smooth_OR.R` | Spatial smoothing (ICAR, etc.). |
| **04_reporting** | `create_covariate_report_table.R` | Baseline characteristics tables (RTF) → `output/reports/`. |
| **04_reporting** | `print_derived_dictionary.R` | Print derived-variable dictionary to console. |

All scripts assume they are run from the **project root** (paths like `data/derived` are relative to root). When invoked via `Rscript scripts/.../file.R`, each script sets the working directory to the project root automatically.

---

## Reproduce results

**Option 1 – Full pipeline (from project root):**

```bash
Rscript scripts/run_all.R
```

This runs, in order: metadata generation → derive → age-by-region estimates → covariate report tables → dictionary print.

**Option 2 – Step by step (from project root):**

1. Generate variable metadata:
   - `Rscript scripts/01_metadata/generate_variable_metadata.R`
   - `Rscript scripts/01_metadata/generate_coir61fl_metadata.R`
   - `Rscript scripts/01_metadata/generate_coir72fl_metadata.R`
2. Generate derived datasets:
   - `Rscript scripts/02_derive/derive_hnir62.R`
   - `Rscript scripts/02_derive/derive_hnir72.R`
   - `Rscript scripts/02_derive/derive_coir61.R`
   - `Rscript scripts/02_derive/derive_coir72.R`
3. (Optional) Age-by-region direct estimates:
   - `Rscript scripts/03_analysis/estimate_age_region.R output/tables/age_region_estimates.csv`
4. Covariate report tables:
   - `Rscript scripts/04_reporting/create_covariate_report_table.R`
5. Print derived dictionary:
   - `Rscript scripts/04_reporting/print_derived_dictionary.R`

---

## Outputs

| Output | Produced by |
|--------|-------------|
| `docs/*_metadata.json` | `01_metadata` scripts |
| `data/derived/*.rds` | `02_derive` scripts |
| `output/tables/age_region_estimates.csv` | `estimate_age_region.R` |
| `output/reports/*_characteristics.rtf` | `create_covariate_report_table.R` |

See `output/README.md` and `docs/README.md` for more detail.
