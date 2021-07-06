library(tidyverse)
library(RCurl)
library(fastDummies)

#load in the files

pbp_raw <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/PlayByPlay.csv")
pbp_raw <- read.csv(text = pbp_raw)

ginfo_raw <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/GameInfo.csv")
ginfo_raw <- read.csv(text = ginfo_raw)

tpoints_raw <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/PlayerTotalPoints.csv")
tpoints_raw <- read.csv(text = tpoints_raw)

splayers_raw <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Data/SkillPositionPlayers.csv")
splayers_raw <- read.csv(text = splayers_raw)

pbp <- pbp_raw
ginfo <- ginfo_raw
tpoints <- tpoints_raw
splayers <- splayers_raw

names(splayers)

#Create New Variables
#FIB
#RedZone

unique(pbp$Hash)
unique(splayers$SideOfCenter)
unique(splayers$Order_OutsideToInside)
unique(splayers$Route)

#clean data
splayers <- splayers %>% dummy_cols(select_columns = "SideOfCenter") %>%
  select(-SideOfCenter_NULL)

alignments <- splayers %>% group_by(EventID, GameID) %>% summarise(left = sum(SideOfCenter_L),
                                                           right = sum(SideOfCenter_R))

pbp <- pbp %>% left_join(alignments, by = c("EventID", "GameID"))

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
  

#RBS DONT MATTER
