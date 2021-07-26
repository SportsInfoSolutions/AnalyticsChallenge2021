require(tidyverse)

#function to get wr position order from outside to inside
#data must have gameid, eventid, side of center, order_outside to inside, rosterposition
determine_pos_oti <- function(data) {
  data %>%
    group_by(GameID, EventID, SideOfCenter) %>%
    arrange(Order_OutsideToInside) %>%
    mutate(Position_OutsideToInside = paste0(RosterPosition, collapse = "/")) %>%
    ungroup() %>% 
    select(GameID, EventID, SideOfCenter, Position_OutsideToInside)
}

