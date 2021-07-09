library(tidyverse)
library(RCurl)
library(fastDummies)
library(xgboost)
library(ggplot2)

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

#clean data
splayers <- splayers %>% dummy_cols(select_columns = "SideOfCenter") %>%
  select(-SideOfCenter_NULL)

alignments <- splayers %>% group_by(EventID, GameID) %>% summarise(left = sum(SideOfCenter_L),
                                                                   right = sum(SideOfCenter_R))

#create the FIB variable
model_data <- pbp %>% left_join(alignments, by = c("EventID", "GameID")) %>% 
  mutate(FIB_R = if_else(right > left & Hash == 3, 1, 0),
         FIB_L = if_else(left > right & Hash == 1, 1, 0),
         FIB = if_else(FIB_L == 1 | FIB_R == 1, 1, 0)) %>%
  select(GameID, EventID, Quarter, TimeLeft, Down, ToGo, SideOfField, StartYard,
         EventType, Shotgun, DropType, RPO, CoverageScheme, EPA, left, right, FIB)

model_data <- model_data %>% 
  mutate(YardsToGoal = if_else(SideOfField == "Own", abs(50-StartYard)+50,
                       if_else(SideOfField == "Oppo", abs(50-StartYard), 0))) %>%
  select(-SideOfField, -StartYard)

model_data <- model_data %>% 
  mutate(TimeRemaining = ifelse(Quarter == 1, 2700+TimeLeft,
                         ifelse(Quarter == 2, 1800+TimeLeft, 
                         ifelse(Quarter == 3, 900+TimeLeft,    
                         ifelse(Quarter == 4, TimeLeft, TimeLeft))))) %>%
  select(-TimeLeft, -Quarter) %>% rename("TimeLeft" = TimeRemaining)

model_data <- model_data %>% dummy_cols(select_columns = "DropType") %>% 
  select(-DropType)

model_data <- model_data %>% dummy_cols(select_columns = "CoverageScheme") %>% 
  select(-CoverageScheme)

model_data$EPA <- as.numeric(model_data$EPA) 

model_data <- model_data %>% filter(EventType == "pass" |
                                    EventType == "challenge pass") %>%
  filter(!is.na(EPA)) %>%
  select(-EventType)

pos_data <- splayers %>% select(GameID, EventID, OnFieldPosition)

pbp_pos_data <- left_join(model_data, pos_data, by=c("GameID", "EventID")) %>%
  dummy_cols(select_columns = "OnFieldPosition") 

names <- data.frame(n = colnames(pbp_pos_data)) %>% dplyr::slice(45:59)

pbp_pos_data <- pbp_pos_data %>% group_by(GameID, EventID) %>%
  summarise(B = sum(OnFieldPosition_B),
            SWR = sum(OnFieldPosition_SWR),
            TE = sum(OnFieldPosition_TE),
            WR = sum(OnFieldPosition_WR))

model_data <- model_data %>% left_join(pbp_pos_data, by=c("GameID", "EventID"))
  
route_data <- splayers %>% select(GameID, EventID, Route)

pbp_route_data <- left_join(model_data, route_data, by=c("GameID", "EventID")) %>%
  dummy_cols(select_columns = "Route") 

pbp_route_data <- pbp_route_data %>% group_by(GameID, EventID) %>%
  summarise(across(Route_Angle:Route_Whip, sum))

model_Data <- model_data %>% left_join(pbp_route_data, by=c("GameID", "EventID")) %>%
  select(-GameID, -EventID, -Route_NULL)

model_label <- as.matrix(model_Data %>% select(EPA))
model_data <- as.matrix(model_Data %>% select(-EPA))

model_matrix <- xgb.DMatrix(data = model_data, label = model_label)

model <- xgboost(data = model_matrix, nrounds = 100)

importance_matrix <- xgb.importance(model = model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#Test Train

ind <- sample(c(TRUE, FALSE), nrow(model_Data), replace=TRUE, prob=c(0.5, 0.5))
test <- model_Data[ind, ]
train <- model_Data[!ind, ]

tr_data <- as.matrix(train %>% select(-EPA))
tr_label <- as.matrix(train %>% select(EPA))

te_data <- as.matrix(test %>% select(-EPA))
te_label <- as.matrix(test %>% select(EPA))

train_data <- xgb.DMatrix(data = tr_data, label = tr_label)

train_model <- xgboost(data = train_data, nrounds = 100)

pred <- predict(train_model, te_data)
pd <- test
pd$Pred <- pred
err <- mean(abs(test$EPA - pred))

importance_matrix <- xgb.importance(model = train_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#Plot Error

pd <- pd %>% select(TimeLeft, EPA, Pred)

pda <- pd %>% select(TimeLeft, EPA) %>% mutate(Type = "A")
pdb <- pd %>% select(TimeLeft, EPA = Pred) %>% mutate(Type = "B")

pd <- bind_rows(pda, pdb)

ggplot(data = pd, aes(x = TimeLeft, y = EPA, color = Type)) +
  geom_point()

