#function to determine wr formation on play
require(tidyverse)

#function to determine 4 new variables:
# number of receivers to either side of center, the formation (nxn) and the passing strength
determine_wr_form <- function(skill_data) {
  skill_data %>%
    group_by(GameID, EventID, SideOfCenter) %>%
    mutate(Position_OutsideToInside = paste0(RosterPosition, collapse = ";")) %>%
    mutate(no_receivers = n()) %>%
    filter(!(SideOfCenter == "NULL")) %>%
    select(GameID, EventID, Position_OutsideToInside, Order_OutsideToInside, SideOfCenter, no_receivers) %>%
    slice_head(n = 1) %>% 
    group_by(GameID, EventID) %>%
    arrange(Order_OutsideToInside) %>% 
    mutate(form = paste0(no_receivers, collapse = "x")) %>%
    select(GameID, EventID, SideOfCenter, Position_OutsideToInside, no_receivers)
}
  