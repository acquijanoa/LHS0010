# LHS0010

This repository contains scripts to derive and document variables from DHS Honduras datasets (HNIR52, HNIR62). Data files are not included in version control. If you place the DHS data files in the expected folders, you can reproduce the derived datasets and documentation outputs using the scripts below.

## Scripts
- `scripts/generate_variable_metadata.R`: Generates JSON variable metadata from HNIR SAS files.
- `scripts/derive_hnir52.R`: Derives analysis variables for HNIR52.
- `scripts/derive_hnir62.R`: Derives analysis variables for HNIR62.
- `scripts/print_derived_dictionary.R`: Prints a data dictionary for derived variables.
- `scripts/generate_unmet_need_flowchart.js`: Generates `docs/unmet_need_flowchart.svg`.

## Reproduce results
1) Obtain DHS Honduras recode files and place them at:
   - `data/HNIR52SD/hnir52fl.sas7bdat`
   - `data/HNIR62SD/hnir62fl.sas7bdat`
   (Optional, for metadata value labels: `data/HNIR52SD/HNIR52FL.SAS` and `data/HNIR62SD/HNIR62FL.SAS`.)
2) Generate variable metadata:
   - `Rscript scripts/generate_variable_metadata.R`
3) Generate derived datasets:
   - `Rscript scripts/derive_hnir52.R`
   - `Rscript scripts/derive_hnir62.R`
4) Print the derived-variable dictionary:
   - `Rscript scripts/print_derived_dictionary.R`
5) Generate the unmet-need flowchart:
   - `node scripts/generate_unmet_need_flowchart.js`

## Outputs
- `docs/hnir52fl_metadata.json`, `docs/hnir62fl_metadata.json`: Variable metadata.
- `docs/unmet_need_derivation.tex`: Unmet need derivation text and figure.
- `docs/unmet_need_flowchart.svg`: Flowchart of unmet need derivation.
- `data/derived/*.rds`: Derived datasets (not tracked in git).
