#analysis with model predictions
library(tidyverse)
library(scales)

dtrn <- d_wk$train
dtrn$pred <- success_model$predictions

dtst <- d_wk$test
dtst$pred <- test_preds$predictions

d_oe <- bind_rows(dtrn, dtst) %>% 
  inner_join(route_combos, by = c("GameID", "EventID")) %>% 
  inner_join(select(plays, GameID, EventID, CoverageScheme), by = c("GameID", "EventID")) %>% 
  mutate(over_exp = off_success - pred) 

min_samples <- 10

summarise_d_oe <- function(grp_vars) {
  stopifnot(all(grp_vars %in% names(d_oe)))
  d_oe_summarised <- d_oe %>% 
    #select(GameID, EventID, route_combo, CoverageScheme, off_success, pred, over_exp) %>% 
    group_by_at(vars(one_of(grp_vars))) %>% 
    summarise(
      samples = n(),
      n_success = sum(off_success),
      n_exp_success = sum(pred),
      ttl_over_exp = sum(over_exp),
      mean_over_exp = mean(over_exp)
    ) %>% 
    ungroup() %>% 
    mutate(
      success = n_success/samples,
      exp_success = n_exp_success/samples,
      soe = success - exp_success
    )
  list(
    d_oe_summarised = d_oe_summarised,
    grp_vars = grp_vars
  )
}

create_soe_table <- function(d_oe_list) {
  d_oe_list %>% 
    chuck("d_oe_summarised") %>% 
    filter(samples >= min_samples) %>% 
    arrange(desc(soe)) %>% 
    mutate(soe_pct = scales::percent_format(accuracy = 0.1)(soe)) %>% 
    select(d_oe_list$grp_vars, Samples = samples, `Succes Over Expected` = soe_pct)
}

rc_soe <- summarise_d_oe(c("route_combo")) %>% 
  create_soe_table() %>% 
  rename(`Route Combo` = route_combo)

rc_cov_soe <- summarise_d_oe(c("route_combo", "CoverageScheme")) %>% 
  create_soe_table() %>% 
  rename(`Route Combo` = route_combo, Coverage = CoverageScheme)

# arrange_soe <- function(df) {
#   df %>% 
#     mutate(soe = as.numeric(str_remove(`Succes Over Expected`, "%"))/100) %>% 
#     arrange(desc(soe)) %>% 
#     head(10) %>% 
#     select(-soe)
# }
# 
# rc_cov_soe %>% 
#   split(.$Coverage) %>% 
#   map(arrange_soe)