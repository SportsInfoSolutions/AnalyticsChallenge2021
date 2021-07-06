library(tidyverse)
library(RCurl)

#load in the files

pbp <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/PlayByPlay.csv")
pbp <- read.csv(text = pbp)

ginfo <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/GameInfo.csv")
ginfo <- read.csv(text = ginfo)

tpoints <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/PlayerTotalPoints.csv")
tpoints <- read.csv(text = tpoints)

splayers <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/SkillPositionPlayers.csv")
splayers <- read.csv(text = splayers)

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
  
