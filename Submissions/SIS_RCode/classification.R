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

join_raw <- getURL("https://raw.githubusercontent.com/jackp01k/AnalyticsChallenge2021/main/Submissions/SIS_RCode/Local_Data/team_join.csv")
join_raw <- read.csv(text = join_raw)

pbp <- pbp_raw
ginfo <- ginfo_raw
tpoints <- tpoints_raw
splayers <- splayers_raw
joinfastR <- join_raw

rm(pbp_raw, ginfo_raw, tpoints_raw, splayers_raw, join_raw)

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

#create personnel detail data
master_pers_frame <- splayers %>%
  mutate(sideNum = paste0(SideOfCenter, Order_OutsideToInside)) %>%
  spread(sideNum, RosterPosition)

DETAIL_data <- master_pers_frame %>% 
  select(GameID, EventID) %>% 
  #subqueries for L1-4 (ordered outside to inside)
  left_join(
    master_pers_frame %>% 
      filter(!is.na(L1)) %>%
      select(GameID, EventID, L1)
  ) %>%
  left_join(
    master_pers_frame %>% 
      filter(!is.na(L2)) %>%
      select(GameID, EventID, L2)
  ) %>%
  left_join(
    master_pers_frame %>% 
      filter(!is.na(L3)) %>%
      select(GameID, EventID, L3)
  ) %>%
  left_join(
    master_pers_frame %>% 
      filter(!is.na(L4)) %>%
      select(GameID, EventID, L4)
  ) %>%
  #subqueries for R1-4 (ordered outside to inside)
  left_join(
    master_pers_frame %>% 
      filter(!is.na(R1)) %>%
      select(GameID, EventID, R1)
  ) %>%
  left_join(
    master_pers_frame %>% 
      filter(!is.na(R2)) %>%
      select(GameID, EventID, R2)
  ) %>%
  left_join(
    master_pers_frame %>% 
      filter(!is.na(R3)) %>%
      select(GameID, EventID, R3)
  ) %>%
  left_join(
    master_pers_frame %>% 
      filter(!is.na(R4)) %>%
      select(GameID, EventID, R4)
  ) %>%
  mutate(L_DETAIL = paste0(L1, "::", L2,  "::", L3, "::", L4),
         R_DETAIL = paste0(R1, "::", R2,  "::", R3, "::", R4),
         #A_DETAIL is from left to right across the entire field, 8 possible positions
         A_DETAIL = paste0(L1, "::", L2,  "::", L3, "::", L4, "::", R4, "::", R3,  "::", R2, "::", R1)) %>%
  select(GameID, EventID, L_DETAIL, R_DETAIL, A_DETAIL)

rm(master_pers_frame)

master_pers_frame <- splayers %>%
  filter(!(str_detect(Route, "Screen"))) %>% 
  filter(!(Route == "NULL"), !(Route == "Jet Sweep Pass")) %>% 
  filter(!(Route == "Blocking")) %>% 
  mutate(
    #Remove right/left tags
    Route = str_remove(Route, " - Left"),
    Route = str_remove(Route, " - Right"),
    #Remove chip tags on routes
    Route = str_remove(Route, "Chip - ")
  ) %>%
  mutate(sideNum = paste0(SideOfCenter, Order_OutsideToInside)) %>%
  spread(sideNum, Route)

ROUTE_data <- master_route_data %>% 
  select(GameID, EventID) %>% 
  #subqueries for L1-4 (ordered outside to inside)
  left_join(
    master_route_data %>% 
      filter(!is.na(L1)) %>%
      select(GameID, EventID, L1)
  ) %>%
  left_join(
    master_route_data %>% 
      filter(!is.na(L2)) %>%
      select(GameID, EventID, L2)
  ) %>%
  left_join(
    master_route_data %>% 
      filter(!is.na(L3)) %>%
      select(GameID, EventID, L3)
  ) %>%
  left_join(
    master_route_data %>% 
      filter(!is.na(L4)) %>%
      select(GameID, EventID, L4)
  ) %>%
  #subqueries for R1-4 (ordered outside to inside)
  left_join(
    master_route_data %>% 
      filter(!is.na(R1)) %>%
      select(GameID, EventID, R1)
  ) %>%
  left_join(
    master_route_data %>% 
      filter(!is.na(R2)) %>%
      select(GameID, EventID, R2)
  ) %>%
  left_join(
    master_route_data %>% 
      filter(!is.na(R3)) %>%
      select(GameID, EventID, R3)
  ) %>%
  left_join(
    master_route_data %>% 
      filter(!is.na(R4)) %>%
      select(GameID, EventID, R4)
  ) %>%
  mutate(Lr_DETAIL = paste0(L1, "::", L2,  "::", L3, "::", L4),
         Rr_DETAIL = paste0(R1, "::", R2,  "::", R3, "::", R4),
         #A_DETAIL is from left to right across the entire field, 8 possible positions
         Ar_DETAIL = paste0(L1, "::", L2,  "::", L3, "::", L4, "::", R4, "::", R3,  "::", R2, "::", R1)) %>%
  select(GameID, EventID, Lr_DETAIL, Rr_DETAIL, Ar_DETAIL) %>% 
  group_by(GameID, EventID) %>% summarise(Left_Exact = first(Lr_DETAIL),
                                          Right_Exact = first(Rr_DETAIL),
                                          All_Exact = first(Ar_DETAIL))



