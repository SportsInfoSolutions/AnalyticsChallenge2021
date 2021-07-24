find_top_routes <- function(filtered_route_data) {
  filtered_route_data %>% 
  group_by(no_routes, rt_combo) %>%
    summarize(
      ct=n()
    ) %>%
    group_by(no_routes) %>% 
    mutate(rt_pct = 100*ct/sum(ct)) %>% 
    #we don't care about single route combos
    #4 route combos don't happen enough for me to care either
    filter(no_routes > 1, no_routes < 4) %>% 
    #we also don't care about blocking
    filter(!(str_detect(rt_combo,"Blocking"))) %>% 
    slice_max(order_by = rt_pct, n = 5)
}