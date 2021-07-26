#function to clean routes (remove unnecessary tags)
clean_routes <- function(skill_data) {
  skill_data %>%
    mutate(rt_fixed = str_remove(Route, "Chip -"),
           rt_fixed = str_remove(Route, " - Right"),
           rt_fixed = str_remove(Route, " - Left")) %>%
    select(GameID, EventID, PlayerId, rt_fixed)
  
}

