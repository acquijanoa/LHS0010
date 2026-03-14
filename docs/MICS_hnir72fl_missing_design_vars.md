# MICS Honduras 2019 (`hnir72fl`) — missing design variables

## Error (remember this)

```text
Error: Missing required variables: wmweight, stratum, PSU, windex5
Execution halted
```

`derive_hnir72fl.R` **requires** these on the **women** dataset:

| Variable   | Role |
|-----------|------|
| `wmweight` | Woman’s sampling weight |
| `stratum`  | Sample stratum |
| `PSU`      | Primary sampling unit (cluster) |
| `windex5`  | Wealth index quintile |

## Why it happens

- Official **UNICEF SAS** women files include them.
- Some **SPSS** (`hnir72fl.sav` / `wm.sav`) **national** exports **drop** them.
- **`hh.sav`** / **`hl.sav`** in the same project were checked: they **do not** contain these four names either—merging hh/hl does **not** fix the error.

## What to do

1. Prefer **SAS women** from UNICEF → `hnir72fl.sas7bdat` → `HNIR72FL_DATA_PATH` or place under `data/raw/HNIR72SD/`.
2. Or merge from any MICS file that **still has** the four vars (e.g. full **`bh.sav`**) onto women by **`HH1`, `HH2`, `LN`**; full sample coverage still needs the complete women file with weights.

See also: `data/raw/HNIR72SD/README.md`.
