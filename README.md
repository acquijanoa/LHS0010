# LHS0010

This repository contains scripts to derive and document variables from DHS Honduras datasets (HNIR52, HNIR62). Data files are not included in version control.

## Scripts
- `scripts/generate_variable_metadata.R`: Generates JSON variable metadata from HNIR SAS files.
- `scripts/derive_hnir52.R`: Derives analysis variables for HNIR52.
- `scripts/derive_hnir62.R`: Derives analysis variables for HNIR62.
- `scripts/print_derived_dictionary.R`: Prints a data dictionary for derived variables.
- `scripts/generate_unmet_need_flowchart.js`: Generates `docs/unmet_need_flowchart.svg`.

## Outputs
- `docs/hnir52fl_metadata.json`, `docs/hnir62fl_metadata.json`: Variable metadata.
- `docs/unmet_need_derivation.tex`: Unmet need derivation text and figure.
- `docs/unmet_need_flowchart.svg`: Flowchart of unmet need derivation.
- `data/derived/*.rds`: Derived datasets (not tracked in git).
