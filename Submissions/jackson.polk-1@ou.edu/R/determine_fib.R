require(tidyverse)

#function to determine if a play was ran in an FIB
#uses form_data from determine_wr_form.R
determine_fib <- function(pbp_data, form_data) {
  
  fib_data <- form_data %>%
    group_by(GameID, EventID) %>%
    spread(key = SideOfCenter, value = no_receivers) %>%
    summarize(
      L = max(L, na.rm = T),
      R = max(R, na.rm = T)
    ) %>%
    mutate(strength = ifelse(L == R, "C",
                             ifelse(L > R, "L", "R")))
  
  pbp_data %>%
    select(GameID, EventID, Hash) %>% 
    left_join(fib_data) %>%
    mutate(FIB = ifelse(Hash == 2, 0, 
                        ifelse(Hash == 1 & strength == "L", 1, 
                               ifelse(Hash == 3 & strength == "R", 1, 0)))) %>%
    select(GameID, EventID, FIB, strength)
}  
