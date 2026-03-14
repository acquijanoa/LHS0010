# Derived analysis datasets

| File | Description |
|------|-------------|
| `*_derived.rds` | Individual-level recodes. **hnir72fl** (MICS 2019) collapses HH7 19/20 → CORTES / FRANCISCO MORAZAN; **region_code** 1–18 (`derive_hnir72fl.R`). |
| `regional_socioeconomic_adol.csv` | **Six** design-based proportions per region × survey (adolescents), each with an SE. See `scripts/02_derive/regional_socioeconomic_VARIABLES.md`. |

### The six socioeconomic variables (all included)

1. **p_poorest_wealth_quintile** — poorest wealth quintile  
2. **p_richest_wealth_quintile** — top quintile  
3. **p_no_education** — no education  
4. **p_secondary_or_higher** — secondary or higher  
5. **p_married_union_adolescent** — married / in union  
6. **p_parity_two_or_more** — two or more births  

Build: `Rscript scripts/02_derive/derive_regional_socioeconomic.R`
