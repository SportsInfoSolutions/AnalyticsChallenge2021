library(tidyverse)
library(tidytext)

pbp <- read_csv("Data/PlayByPlay.csv")

unique(pbp$CoverageScheme)
unique(pbp$Hash)
pbp %>% 
  filter(grepl("pass", EventType)) %>% 
  inner_join(formation_info) %>% 
  group_by(Formation,Hash) %>% 
  summarise(count = n()) %>% 
  group_by(Formation) %>% 
  mutate(perc = count/sum(count))

skills <- read_csv("Data/SkillPositionPlayers.csv")

skills %>% 
  filter(SideOfCenter=="R" & Route == "Flat - Left")

unique(skills$SideOfCenter)

skills[skills$Order_OutsideToInside=="NULL","Order_OutsideToInside"] <- NA
skills$Order_OutsideToInside <- as.integer(skills$Order_OutsideToInside)

skills %>% 
  filter(OnFieldPosition=="B" & grepl("Right|Left", Route))

skills2 <- skills %>% 
  group_by(GameID, EventID) %>% 
  mutate(SideOfCenterMod = case_when(grepl("Right", Route) & !grepl("Swing", Route) ~ "R",
                                   grepl("Left", Route) & !grepl("Swing", Route) ~ "L",
                                   TRUE ~ SideOfCenter),
         RouteMod = str_remove_all(Route, " - Left| - Right|Chip - | |&|-"),
         RouteMod = ifelse(RouteMod=="Go/Fly", "Go", RouteMod)) %>% 
  inner_join(pbp %>% select(GameID, EventID, EventType, Hash)) %>% 
  filter(grepl("pass", EventType)) %>% 
  mutate(HashMod = case_when(Hash==1 ~ "L",
                             Hash==2 ~ "M",
                             Hash==3 ~ "R"))

unique(skills2$RouteMod)

skills %>% 
  filter(OnFieldPosition == 'B') %>% 
  pull(Route) %>% 
  unique()

skills2 %>% 
  group_by(GameID, EventID) %>% 
  filter(OnFieldPosition!="QB") %>% 
  summarise(RightPlayers = sum(SideOfCenterMod=='R'),
            LeftPlayers = sum(SideOfCenterMod=='L'),
            BackfieldPlayers = sum(SideOfCenterMod=="NULL")) %>% 
ungroup() %>% 
  group_by(RightPlayers, LeftPlayers, BackfieldPlayers) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

skills %>% filter(FastMotion==1 & OnFieldPosition =='SWR')

unique(skills$Route)

formation_info <- skills2 %>% 
  group_by(GameID, EventID) %>% 
  filter(OnFieldPosition!="QB") %>% 
  summarise(RightPlayers = sum(SideOfCenterMod=='R'),
            LeftPlayers = sum(SideOfCenterMod=='L'),
            BackfieldPlayers = sum(SideOfCenterMod=="NULL"),
            RightWR = sum(grepl("WR", OnFieldPosition) & SideOfCenterMod=="R"),
            LeftWR = sum(grepl("WR", OnFieldPosition) & SideOfCenterMod=="L")) %>% 
  ungroup() %>% 
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
  

routeCombinations <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & SideOfCenterMod!="NULL"~ "Field",
                                 OnFieldPosition=="B" & SideOfCenterMod=="NULL" ~ "RB",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="R") | (LeftPlayers > RightPlayers & SideOfCenterMod=="L"))  ~ "Field",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="L") | (LeftPlayers > RightPlayers & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="R") | (LeftWR > RightWR & SideOfCenterMod=="L")) ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="L") | (LeftWR > RightWR & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="R" ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="L" ~ "Boundary")) %>% 
  group_by(GameID, EventID, StrengthInd) %>%
  arrange(GameID, EventID, StrengthInd, Order_OutsideToInside) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         AlignOrder = paste0(StrengthInd, OrderID)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, RouteMod, IsBlocking, AlignOrder, Hash) %>% 
  filter(RouteMod!='NULL' & RouteMod!="Blocking") %>% 
  group_by(GameID, EventID, Formation, Hash) %>% 
  arrange(GameID, EventID, RouteMod) %>% 
  summarise(Routes = paste(RouteMod, collapse=", "),
            RouteIDs = paste(AlignOrder, collapse = ", "))

routeCombinations 

routeCombinations %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes, token = "skip_ngrams", n=2, k=3) %>% 
  arrange(GameID, EventID) %>% 
  filter(grepl(" ", bigram)) %>% 
  count(bigram, sort=T)




unique(routeCombinations$Routes)

skills %>% 
  filter(Route!="NULL" & Route!="Blocking") %>% 
  group_by(Route) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

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
  summarise(count = n(),
            perc = count) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(perc = count/sum(count))

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



outsideCurlPlays <- skills %>% 
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
  filter(Route=="Curl" & AlignOrder=="Strong1") %>% 
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
  inner_join(outsideCurlPlays) %>% 
  filter(!Route %in% c("Curl", "Blocking", "NULL")) %>% 
  group_by(AlignOrder, Route) %>% 
  summarise(count = n()) %>% 
  arrange(-count)



skills %>% 
  filter(Route!="NULL" & Route!="Blocking") %>% 
  group_by(GameID, EventID, Route) %>% 
  summarise(RouteCount = n()) %>% 
  group_by(Route) %>% 
  summarise(TotalRoutes = sum(RouteCount),
            UniquePlays = sum(ifelse(RouteCount > 0, 1, 0)),
            PlaysWithMoreThan1 = sum(ifelse(RouteCount > 1, 1, 0))/UniquePlays) %>% 
  arrange(-UniquePlays)

unique(skills2$RouteMod)
