require(tidyverse)

#function to determine the CLEANED route combos on any side of the ball (L,R, NULL)
determine_routes <- function(skill_data) {
  skill_data %>%
    select(GameID, EventID, Order_OutsideToInside, SideOfCenter, rt_fixed) %>%
    filter(!(rt_fixed == "NULL")) %>%
    group_by(GameID, EventID, SideOfCenter) %>% 
    arrange(Order_OutsideToInside) %>%
    mutate(rt_combo = paste(rt_fixed, sep = ";", collapse = ";")) %>%
    slice_head(n = 1) %>%
    mutate(no_routes = str_count(rt_combo, ";") + 1) %>% 
    select(GameID, EventID, SideOfCenter, no_routes, rt_combo)
}  
