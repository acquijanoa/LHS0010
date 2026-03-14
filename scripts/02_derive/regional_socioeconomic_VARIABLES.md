# Regional socioeconomic variables (adolescents, design-based)

Each row = one **region × survey**. Proportions are **survey-weighted** (`svyby`); `se_*` = linearization SE.

| # | Column (proportion) | Column (SE) | Definition |
|---|---------------------|-------------|------------|
| 1 | `p_poorest_wealth_quintile` | `se_poorest_wealth_quintile` | Poorest wealth quintile (DHS V190 / MICS windex5 = 1) |
| 2 | `p_richest_wealth_quintile` | `se_richest_wealth_quintile` | Richest quintile (= 5) |
| 3 | `p_no_education` | `se_no_education` | No education (`education_c4 == 0`) |
| 4 | `p_secondary_or_higher` | `se_secondary_or_higher` | Secondary or higher (`education_c4 >= 2`) |
| 5 | `p_married_union_adolescent` | `se_married_union_adolescent` | Married / in union (`married_union == 1`) |
| 6 | `p_parity_two_or_more` | `se_parity_two_or_more` | Two or more live births (`parity >= 2`) |

Script: `derive_regional_socioeconomic.R` → `regional_socioeconomic_adol.csv`

Bayes model uses the six **p_*** columns (standardized per survey) as **X**; `beta[1]`…`beta[6]` match the table order above.
