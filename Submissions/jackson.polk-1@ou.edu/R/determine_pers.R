require(tidyverse)


#function to determine the personnel on a given play. 
determine_pers <- function(skill_data) {
  skill_data %>%
    select(GameID, EventID, RosterPosition) %>%
    mutate(TE = ifelse(RosterPosition == "TE", 1, 0),
           RB = ifelse(RosterPosition == "RB", 1, 0)) %>%
    group_by(GameID, EventID) %>% 
    summarize(
      nTE = sum(TE, na.rm = T),
      nRB = sum(RB, na.rm = T)
    ) %>%
    mutate(pers = paste0(nTE, nRB)) %>%
    select(GameID, EventID, pers)
}
