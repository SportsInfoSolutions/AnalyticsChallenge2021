library(tidyverse)

pbp <- read_csv("Data/PlayByPlay.csv")

unique(pbp$CoverageScheme)

skills <- read_csv("Data/SkillPositionPlayers.csv")

unique(skills$SideOfCenter)

skills[skills$Order_OutsideToInside=="NULL","Order_OutsideToInside"] <- NA
skills$Order_OutsideToInside <- as.integer(skills$Order_OutsideToInside)

skills %>% 
  filter(OnFieldPosition=="B" & grepl("Right|Left", Route))

unique(skills$OnFieldPosition)

skills %>% 
  filter(OnFieldPosition == 'B') %>% 
  pull(Route) %>% 
  unique()

skills %>% 
  group_by(GameID, EventID) %>% 
  summarise(RightPlayers = sum(SideOfCenter=='R'),
            LeftPlayers = sum(SideOfCenter=='L'),
            BackfieldPlayers = sum(OnFieldPosition=='B')) %>% 
ungroup() %>% 
  group_by(RightPlayers, LeftPlayers, BackfieldPlayers) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

skills %>% filter(FastMotion==1 & OnFieldPosition =='SWR')

unique(skills$Route)

formation_info <- skills %>% 
  group_by(GameID, EventID) %>% 
  summarise(RightPlayers = sum(SideOfCenter=='R'),
            LeftPlayers = sum(SideOfCenter=='L'),
            BackfieldPlayers = sum(OnFieldPosition=='B')) %>% 
  ungroup() %>% 
  filter(RightPlayers + LeftPlayers + BackfieldPlayers == 5) %>% 
  mutate(Formation = case_when(RightPlayers==LeftPlayers ~ "2x2",
                               (RightPlayers==3 & LeftPlayers==1) | (RightPlayers==1 & LeftPlayers==3) ~ "3x1",
                               BackfieldPlayers==2 ~ "2-Backs",
                               BackfieldPlayers==3 ~ "3-Backs",
                               (RightPlayers==3 & LeftPlayers==2) | (RightPlayers==2 & LeftPlayers==3) ~ "Empty 3x2",
                               RightPlayers > 3 | LeftPlayers>3 ~ "Empty 4x1"),
         Strength = case_when(RightPlayers > LeftPlayers ~ "R",
                              RightPlayers < LeftPlayers ~ "L",
                              RightPlayers==LeftPlayers ~ "Balanced"))

unique(pbp$EventType)
unique(pbp$DropType)

ggplot(data = pbp %>% filter(grepl("pass", EventType) & !DropType %in% c("Spike", "Flea Flicker", "RB/WR Pass", "WR Reverse Pass", "Double Reverse Pass")),
       aes(x = EPA,
           y = CoverageScheme)) + geom_jitter()

passIDs <- pbp %>% filter(grepl("pass", EventType) & !DropType %in% c("Spike", "Flea Flicker", "RB/WR Pass", "WR Reverse Pass", "Double Reverse Pass")) %>% select(GameID, EventID)

skills <- skills %>% 
  mutate(RouteMod = ifelse(IsBlocking==1, "Block", Route))

skills %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!='QB') %>% 
  select(GameID, EventID, OnFieldPosition, SideOfCenter, Order_OutsideToInside, RouteMod)
  

routeCombinations <- skills %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenter==Strength ~ "Strong",
                                 SideOfCenter!=Strength & Strength!="Balanced" & OnFieldPosition!="B"~ "WeakA",
                                 OnFieldPosition=="B" ~ "WeakBackfield",
                                 Strength=="Balanced" & SideOfCenter=="R" ~ "Strong",
                                 Strength=="Balanced" & SideOfCenter=="L" ~ "WeakA")) %>% 
  group_by(GameID, EventID, OnFieldPosition) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         AlignOrder = paste0(StrengthInd, OrderID)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, Route, IsBlocking, AlignOrder) %>% 
  filter(Route!='NULL' & Route!="Blocking") %>% 
  group_by(GameID, EventID, Formation, Strength) %>% 
  arrange(GameID, EventID, StrengthInd, OrderID) %>% 
  summarise(Routes = paste(Route, collapse=" // "),
            RouteIDs = paste(AlignOrder, collapse = " // "))

routeCombinations

unique(routeCombinations$Routes)

view(skills %>% 
  filter(Route!="NULL" & Route!="Blocking") %>% 
  group_by(Route) %>% 
  summarise(count = n()) %>% 
  arrange(-count))

skills %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenter==Strength ~ "Strong",
                                 SideOfCenter!=Strength & Strength!="Balanced" & OnFieldPosition!="B"~ "WeakA",
                                 OnFieldPosition=="B" ~ "WeakBackfield",
                                 Strength=="Balanced" & SideOfCenter=="R" ~ "Strong",
                                 Strength=="Balanced" & SideOfCenter=="L" ~ "WeakA")) %>% 
  group_by(GameID, EventID, OnFieldPosition) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         AlignOrder = paste0(StrengthInd, OrderID)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, Route, IsBlocking, AlignOrder) %>% 
  filter(Route=="Curl") %>% 
  group_by(AlignOrder) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

curlPlays <- skills %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenter==Strength ~ "Strong",
                                 SideOfCenter!=Strength & Strength!="Balanced" & OnFieldPosition!="B"~ "WeakA",
                                 OnFieldPosition=="B" ~ "WeakBackfield",
                                 Strength=="Balanced" & SideOfCenter=="R" ~ "Strong",
                                 Strength=="Balanced" & SideOfCenter=="L" ~ "WeakA")) %>% 
  group_by(GameID, EventID, OnFieldPosition) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         AlignOrder = paste0(StrengthInd, OrderID)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, Route, IsBlocking, AlignOrder )%>% 
  filter(Route=="Curl") %>% 
  ungroup() %>% 
  select(GameID, EventID) %>% 
  distinct(GameID, EventID)


skills %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenter==Strength ~ "Strong",
                                 SideOfCenter!=Strength & Strength!="Balanced" & OnFieldPosition!="B"~ "WeakA",
                                 OnFieldPosition=="B" ~ "WeakBackfield",
                                 Strength=="Balanced" & SideOfCenter=="R" ~ "Strong",
                                 Strength=="Balanced" & SideOfCenter=="L" ~ "WeakA")) %>% 
  group_by(GameID, EventID, OnFieldPosition) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         AlignOrder = paste0(StrengthInd, OrderID)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, Route, IsBlocking, AlignOrder) %>% 
  inner_join(curlPlays) %>% 
  filter(!Route %in% c("Curl", "Blocking", "NULL")) %>% 
  group_by(Route) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

  
