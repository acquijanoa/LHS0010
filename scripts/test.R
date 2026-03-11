library(dplyr)
library(haven)


dat <- readRDS("../data/derived/hnir62fl_derived.rds")


des_grp <- svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = dat, nest = TRUE)
