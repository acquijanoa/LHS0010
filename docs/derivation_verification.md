# Derivation Verification Report

Verification of `derive_*.R` scripts against `*_metadata.json` (raw variable catalogs) and Coordinating Center–style derived variable definitions.

## Summary

| Dataset   | Metadata file              | Derive script        | Status |
|----------|----------------------------|----------------------|--------|
| COIR72FL | `docs/coir72fl_metadata.json` | `scripts/derive_coir72.R` | OK; derivations match metadata and DHS conventions |
| HNIR62FL | `docs/hnir62fl_metadata.json` | `scripts/derive_hnir62.R` | OK; aligned with COIR72 (interview filter and required_vars added) |
| HNIR72SD | `docs/hnir72sd_metadata.json` | `scripts/derive_hnir72.R` | OK; MICS-style variables match metadata |

---

## 1. COIR72 (Colombia) — `derive_coir72.R` ↔ `coir72fl_metadata.json`

- **Source vars**: All required variables (`V005`, `V015`, `V021`, `V022`, `V025`, `V012`, `V106`, `V190`, `V201`, `V213`, `V215`, `V225`, `V313`, `V376`, `V3A08D`, `V501`, `V525`, `V527`, `V528`, `V602`, `V603`, `V714`, `SDEPTO`) are present in metadata.
- **Region**: Correctly uses **SDEPTO** (Department) for Colombia, not V024; `region_map` and `region_code = SDEPTO` match metadata labels.
- **Interview**: `interviewed` from V015 (1 = Completed, 2–7 = not); output filtered to `interviewed == 1L`. Matches metadata (Result of individual interview).
- **Pregnant**: V213 (1 = Yes, 0 = No). Matches.
- **Sexually active**: V528 95 = “Within last 4 weeks” → 1; V528 31 = “31+ days” → 0; V527 ranges (100–199, 200–299, 995 = active; 300–399, 400–499 = not). Matches metadata (Time since last sex).
- **Unfecund**: V215 993 (Hysterectomy), 994 (In menopause), 996 (Never menstruated); V376 23 (Hysterectomy); V3A08D 1 (menopausal); V602 6 (Never had sex). All codes present in metadata; derivation is correct.
- **Unmet need**: Uses V225 (wanted pregnancy), V602/V603 (fertility preference / waiting time), V313 (modern method). Logic matches standard DHS unmet need (0 = none, 1 = spacing, 2 = limiting).
- **Wealth (windex_c3)**: V190 1–2 → 1, 3 → 2, 4–5 → 3. Matches metadata (Wealth index).
- **Education (education_c4)**: V106 0–3 → 0–3. Matches.
- **Age group (agegroup_c4)**: V012 &lt;19, 19–24, 25–34, 35+. Matches.

**Verdict**: Derivations are appropriate and consistent with metadata and DHS conventions.

---

## 2. HNIR62 (Honduras) — `derive_hnir62.R` ↔ `hnir62fl_metadata.json`

- **Source vars**: Required variables (including V015, V024) are checked at run time; all exist in metadata.
- **Region**: Uses **V024** (Region) with department names; no `region_code` (only `region`), which is consistent for this file.
- **Interview**: **Fixed.** Script now defines `interviewed` from V015 and filters to `interviewed == 1L`, aligning with COIR72 and HNIR72.
- **Unfecund**: Uses V215 **994** (In menopause/had hysterectomy) and **996** (Never menstruated) only. Metadata does not have 993 for Honduras (994 is combined menopause/hysterectomy); derivation is correct.
- **V376**: 23 = “Menopausal, hysterectomy” in metadata; script uses V376 == 23. Correct.
- **Sexually active**: V528 95 or 0–30 days → 1; V528 31 → 0; V527 ranges same as COIR72. Matches metadata.
- **Unmet need / modern contraceptive / parity / windex_c3 / education_c4 / agegroup_c4**: Same logic as COIR72; source variables and codes align with metadata.

**Verdict**: Derivations are appropriate. Interview filter and required-vars check were added for consistency.

---

## 3. HNIR72 (Honduras MICS-style) — `derive_hnir72.R` ↔ `hnir72sd_metadata.json`

- **Source vars**: All required variables (e.g. `wmweight`, `stratum`, `PSU`, `WM17`, `HH6`, `HH7`, `WB4`, `WB5`, `WB6A`, `WB6B`, `CP1`, `CP2`, `CP4*`, `UN2`, `UN4`, `UN7`, `UN8U`, `UN8N`, `UN12*`, `UN14*`, `SB2*`, `windex5`) are present in metadata.
- **Interview**: WM17 1 = “COMPLETADA” → interviewed; filter to `interviewed == 1L`. Matches metadata.
- **Urban**: HH6 1 = URBANO, 2 = RURAL. Matches.
- **Region**: HH7 with `region_map` including 19 (SAN PEDRO SULA), 20 (DISTRITO CENTRAL). Matches metadata (Región/Departamento).
- **Education (education_c4)**: WB5 (attended school), WB6A (level), WB6B (grade); mapping 0/1/2/3 matches metadata (WB6A labels).
- **Pregnant**: CP1 = 1. Matches (“Está embarazada ahora”).
- **Sexually active**: Married/union or (SB2U/SB2N: last sex ≤30 days or ≤4 weeks). SB2U/SB2N labels in metadata support this.
- **Unfecund**: UN12B/C/D/E/H/X (Menopausia, Nunca menstruó, Histerectomía, etc.). Matches metadata labels.
- **Contraceptive / modern method**: CP2, CP4A–J (modern), CP4L/M/X (traditional/other). Matches.
- **Unmet need**: UN2, UN4 (wanted pregnancy); UN7, UN8U/UN8N (want more / timing); `kids_soon` and `unmet_need_c3` logic align with metadata (UN7, UN8* labels).

**Verdict**: Derivations are appropriate and consistent with metadata.

---

## Cross-cutting notes

- **Metadata role**: The `*_metadata.json` files describe **raw** variables (name, label, value labels, pct_missing). They do not define derived variables; the derive scripts are the source of truth for derived definitions.
- **Dictionary**: `scripts/print_derived_dictionary.R` describes the common derived schema (e.g. region from “V024”); for Colombia, the actual region variable is **SDEPTO** in the script, not V024. The dictionary is generic across IR datasets.
- **Practice alignment**: All three scripts now restrict to completed interviews, use the same unmet need coding (0/1/2 and binary `unmet_need`), and use design variables (strat, psu_id, weight) in a consistent way for survey-weighted analysis.

---

*Generated as part of derivation verification against Coordinating Center practices.*
