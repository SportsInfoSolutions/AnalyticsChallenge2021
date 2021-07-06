library(tidyverse)

#load in the files

pbp <- read_csv(paste0(getwd(), "/Data/PlayByPlay.csv"))
ginfo <- read_csv(paste0(getwd(), "/Data/GameInfo.csv"))
tpoints <- read_csv(paste0(getwd(), "/Data/PlayerTotalPoints.csv"))
splayers <- read_csv(paste0(getwd(), "/Data/SkillPositionPlayers.csv"))

names(splayers)

#Create New Variables
#FIB
#RedZone

unique(pbp$Hash)
unique(splayers$SideOfCenter)
unique(splayers$Order_OutsideToInside)
unique(splayers$Route)

#identify FIB
fib_data <- pbp %>%
  #select relevant variables
  select(GameID, EventID, Hash) %>%
  #join with skill players
  left_join(splayers %>% select(GameID, EventID, PlayerId, 
                                OnFieldPosition, Order_OutsideToInside, 
                                SideOfCenter)) %>%
  #identify FIB L/R
  mutate(iFIB = ifelse(SideOfCenter == "L" & Hash == 1 & Order_OutsideToInside == 3 & OnFieldPosition %in% c("SWR", "TE"), 1, 
                      ifelse(SideOfCenter == "R" & Hash == 3 & Order_OutsideToInside == 3 & OnFieldPosition %in% c("SWR", "TE"), 1, 0))) %>%
  group_by(EventID, GameID) %>%
  mutate(FIB = max(iFIB)) %>%
  ungroup() %>% select(GameID, EventID, FIB)

#identify unique route concepts

getRoutes <- function(x) {
  paste0(ifelse(Order_OutsideToInside == 1, Route, NA))
}

pbp %>%
  left_join(splayers) %>%
  select(GameID, EventID, OnFieldPosition, SideOfCenter, Order_OutsideToInside, Route) %>%
  group_by(GameID, EventID) %>%
  mutate(leftSide = ifelse(SideOfCenter == "L",
                           paste0(
                             ifelse(Order_OutsideToInside == 1, Route, NA),
                             " :-: ",
                             ifelse(Order_OutsideToInside == 2, Route, NA),
                             " :-: ",
                             ifelse(Order_OutsideToInside == 3, Route, NA),
                             " :-: ",
                             ifelse(Order_OutsideToInside == 4, Route, NA)
                           ), NA)) %>% 
  select(leftSide)
  
