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

#remove unused DF
rm(pbp_raw, ginfo_raw, tpoints_raw, splayers_raw)

#Create New Variables
#FIB
#clean data
splayers <- splayers %>% dummy_cols(select_columns = "SideOfCenter") %>%
  select(-SideOfCenter_NULL)

alignments <- splayers %>% group_by(EventID, GameID) %>% summarise(left = sum(SideOfCenter_L),
                                                           right = sum(SideOfCenter_R))

#create the FIB variable
pbp <- pbp %>% left_join(alignments, by = c("EventID", "GameID")) %>% 
  mutate(FIB_R = if_else(right > left & Hash == 3, 1, 0),
         FIB_L = if_else(left > right & Hash == 1, 1, 0),
         FIB = if_else(FIB_L == 1 | FIB_R == 1, 1, 0))

#Create Personnel, Formation, and Strength variables
PFS_data <- pbp %>%
  #Subquery 1 to get the number of running backs on a play
  left_join(pbp %>% 
              left_join(splayers %>% select(GameID, EventID, OnFieldPosition)) %>% 
              filter(OnFieldPosition == "B") %>%
              group_by(GameID, EventID) %>%
              summarize(
                RBct = n()
              ) %>%
              select(GameID, EventID, RBct)) %>%
  #Subquery 2 to get the number of TE on a play
  left_join(pbp %>% 
              left_join(splayers %>% select(GameID, EventID, OnFieldPosition)) %>% 
              filter(OnFieldPosition == "TE") %>%
              group_by(GameID, EventID) %>%
              summarize(
                TEct = n()
              ) %>%
              select(GameID, EventID, TEct)) %>%
  #create variables
  #Pers = #RB + #TE, Formation = left x right, Strength = "passing strength"
  mutate(PERS = paste0(ifelse(is.na(RBct), 0, RBct),
                       ifelse(is.na(TEct), 0, TEct)),
         FORM = paste0(left, "x", right), 
         STRENGTH = ifelse(left > right, "L", 
                           ifelse(right > left, "R","C"))) %>%
  select(GameID, EventID, PERS, FORM, STRENGTH)


