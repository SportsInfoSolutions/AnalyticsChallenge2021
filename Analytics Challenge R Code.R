library(tidyverse)
library(tidytext)
library(rstanarm)

pbp <- read_csv("Data/PlayByPlay.csv", na='NULL')

unique(pbp$CoverageScheme)
unique(pbp$Hash)
pbp %>% 
  filter(grepl("pass", EventType)) %>% 
  inner_join(formation_info) %>% 
  group_by(Formation,Hash) %>% 
  summarise(count = n()) %>% 
  group_by(Formation) %>% 
  mutate(perc = count/sum(count))

skills <- read_csv("Data/SkillPositionPlayers.csv", na='NULL')

skills %>% 
  filter(SideOfCenter=="R" & Route == "Flat - Left")

unique(skills$SideOfCenter)

skills[skills$Order_OutsideToInside=="NULL","Order_OutsideToInside"] <- NA
skills$Order_OutsideToInside <- as.integer(skills$Order_OutsideToInside)

skills %>% 
  filter(OnFieldPosition=="B" & grepl("Right|Left", Route))

skills2 <- skills %>% 
  group_by(GameID, EventID) %>% 
  mutate(SideOfCenterMod = case_when(grepl("Right", Route) & !grepl("Swing", Route)  ~ "R",
                                   grepl("Left", Route) & !grepl("Swing", Route)  ~ "L",
                                   TRUE ~ SideOfCenter),
         RouteMod = str_remove_all(Route, " - Left| - Right|Chip - | |&|-"),
         RouteMod = ifelse(RouteMod=="Go/Fly", "Go", RouteMod)) %>% 
  inner_join(pbp %>% select(GameID, EventID, EventType, Hash)) %>% 
  filter(grepl("pass", EventType)) %>% 
  mutate(HashMod = case_when(Hash==1 ~ "L",
                             Hash==2 ~ "M",
                             Hash==3 ~ "R"))

unique(skills2 %>% filter(FastMotion==1) %>% pull(RouteMod))

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

unique(skills2$SideOfCenterMod)

formation_info <- skills2 %>% 
  group_by(GameID, EventID) %>% 
  filter(OnFieldPosition!="QB") %>% 
  summarise(RightPlayers = sum(SideOfCenterMod=='R', na.rm=T),
            LeftPlayers = sum(SideOfCenterMod=='L', na.rm=T),
            BackfieldPlayers = sum(is.na(SideOfCenterMod)),
            RightWR = sum(grepl("WR", OnFieldPosition) & SideOfCenterMod=="R"),
            LeftWR = sum(grepl("WR", OnFieldPosition) & SideOfCenterMod=="L")) %>% 
  ungroup() %>% 
  mutate(Formation = case_when(RightPlayers==LeftPlayers ~ "2x2",
                               (RightPlayers==3 & LeftPlayers==1) | (RightPlayers==1 & LeftPlayers==3) ~ "3x1",
                               BackfieldPlayers==2 ~ "2-Backs",
                               BackfieldPlayers==3 ~ "3-Backs",
                               (RightPlayers==3 & LeftPlayers==2) | (RightPlayers==2 & LeftPlayers==3) ~ "3x2",
                               RightPlayers > 3 | LeftPlayers>3 ~ "4x1"),
         Strength = case_when(RightPlayers > LeftPlayers ~ "R",
                              RightPlayers < LeftPlayers ~ "L",
                              RightPlayers==LeftPlayers ~ "Balanced"))

unique(pbp$EventType)
unique(pbp$DropType)

ggplot(data = pbp %>% filter(grepl("pass", EventType) & !DropType %in% c("Spike", "Flea Flicker", "RB/WR Pass", "WR Reverse Pass", "Double Reverse Pass")),
       aes(x = EPA,
           y = CoverageScheme)) + geom_jitter()

passIDs <- pbp %>% filter(grepl("pass", EventType) & !DropType %in% c("Spike", "Flea Flicker", "RB/WR Pass", "WR Reverse Pass", "Double Reverse Pass")) %>% select(GameID, EventID)



routeCombinations <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="R") | (LeftPlayers > RightPlayers & SideOfCenterMod=="L"))  ~ "Field",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="L") | (LeftPlayers > RightPlayers & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="R") | (LeftWR > RightWR & SideOfCenterMod=="L")) ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="L") | (LeftWR > RightWR & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="R" ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="L" ~ "Boundary")) %>% 
  group_by(GameID, EventID, StrengthInd) %>%
  arrange(GameID, EventID, StrengthInd, Order_OutsideToInside) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         Align = paste0(StrengthInd, OrderID),
         AlignOrder = case_when(Align=="Field1" ~ 1,
                                Align=="Field2" ~ 2,
                                Align=="Field3" ~ 3,
                                Align=="Field4" ~ 4,
                                Align=="RB1" ~ 5,
                                Align=="RB2" ~ 6,
                                Align=="RB3" ~ 7,
                                Align=="Boundary4" ~ 8,
                                Align=="Boundary3" ~ 9,
                                Align=="Boundary2" ~ 10,
                                Align=="Boundary1" ~ 11)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, RouteMod, IsBlocking, AlignOrder, Hash, Align) %>% 
  filter(!is.na(RouteMod) & !RouteMod %in%  c("Blocking")) %>% 
  group_by(GameID, EventID, Formation, Hash, StrengthInd) %>% 
  arrange(GameID, EventID, Align) %>% 
  summarise(Routes = paste(RouteMod, collapse=", "),
            RouteIDs = paste(Align, collapse = ", "))

routeCombinations
View(routeCombinations %>% 
       inner_join(pbp %>% select(GameID, EventID, CoverageScheme)) %>% 
             filter(StrengthInd!="RB") %>% 
             group_by(Routes, CoverageScheme) %>% 
       count() %>% 
       arrange(Routes, CoverageScheme))

View(routeCombinations %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes, token = "skip_ngrams", n=3, k=3) %>% 
  arrange(GameID, EventID) %>% 
  filter(lengths(str_split(bigram, " "))>=1) %>% 
  count(bigram, sort=T) )

pbp %>% inner_join(passIDs)

View(skills2 %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition=="B") %>% 
  group_by(Route) %>% 
  count() %>% 
  arrange(-n))

unique(skills2$RouteMod)

View (
skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="R") | (LeftPlayers > RightPlayers & SideOfCenterMod=="L"))  ~ "Field",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="L") | (LeftPlayers > RightPlayers & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="R") | (LeftWR > RightWR & SideOfCenterMod=="L")) ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="L") | (LeftWR > RightWR & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="R" ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="L" ~ "Boundary")) %>% 
  filter(StrengthInd != "RB") %>% 
  group_by(GameID, EventID, StrengthInd) %>%
  arrange(GameID, EventID, StrengthInd, Order_OutsideToInside) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         Align = paste0(StrengthInd, OrderID),
         AlignOrder = case_when(Align=="Field1" ~ 1,
                                Align=="Field2" ~ 2,
                                Align=="Field3" ~ 3,
                                Align=="Field4" ~ 4,
                                Align=="RB1" ~ 5,
                                Align=="RB2" ~ 6,
                                Align=="RB3" ~ 7,
                                Align=="Boundary4" ~ 8,
                                Align=="Boundary3" ~ 9,
                                Align=="Boundary2" ~ 10,
                                Align=="Boundary1" ~ 11),
         OppoSide = ifelse(StrengthInd=="Field", "Boundary", "Field")) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, RouteMod, IsBlocking, AlignOrder, Hash, Align, OppoSide) %>% 
  filter(!is.na(RouteMod) & !RouteMod %in% c("Blocking", "RunFake") & RouteMod %in% c("Drag", "Dig", "DeepCross", "Post")) %>% 
  inner_join(routeCombinations %>% 
               ungroup() %>% 
               select(GameID, EventID, StrengthInd, Routes, RouteIDs), by=c("GameID", "EventID", "OppoSide" = "StrengthInd")) %>% 
  mutate(Routes2 = paste(RouteMod, Routes, sep=", ")) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes2, token = "skip_ngrams", n=2, k=3) %>% 
  mutate(NumOppRoutes = str_split(Routes, ", ") %>% lengths()) %>% 
  group_by(GameID, EventID, StrengthInd, RouteMod) %>% 
  slice(2:(unique(NumOppRoutes)+1)) %>% 
  ungroup() %>% 
  mutate(bigram2 = sapply(lapply(str_split(bigram, " "), sort), paste, collapse=" ")) %>% 
  count(bigram2, sort=T)
)

sapply(lapply(str_split(routeCombinations$Routes, ", "), sort), paste, collapse=", ")

routeCombinations
unique(routeCombinations2$Routes)

View(routeCombinations2 %>% 
       inner_join(pbp %>% select(GameID, EventID, CoverageScheme)) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes, token = "skip_ngrams", n=3, k=3) %>% 
  arrange(GameID, EventID) %>% 
  filter(lengths(str_split(bigram, " "))>=2) %>% 
    group_by(CoverageScheme) %>% 
  count(bigram, sort=T) )


passPlay3 <- routeCombinations %>% 
  select(-RouteIDs) %>% 
  pivot_wider(names_from = StrengthInd, values_from = Routes) %>% 
  inner_join(pbp %>% select(GameID, EventID, CoverageScheme, Down, ToGo, DropType, FumbleByPasser, FumbleByReceiver, PressureOnPlay, SackOnPlay, RPO, OffensiveYardage, EPA)) %>% 
  filter(CoverageScheme != "Screen") %>% 
  mutate(Boundary = ifelse(is.na(Boundary), "No Route", Boundary),
         Field = ifelse(is.na(Field), "No Route", Field),
         RB = ifelse(is.na(RB), "No Route", RB),
         FumbleByReceiver = ifelse(is.na(FumbleByReceiver), 0, FumbleByReceiver),
         DownDist = case_when(Down==1 ~ "1st",
                              Down==2 & ToGo >= 7 ~ "2nd & Long",
                              Down==2 & ToGo < 7 & ToGo >= 4 ~ "2nd & Med",
                              Down==2 & ToGo < 4 ~ "2nd & Short",
                              Down==3 & ToGo >= 7 ~ "3rd & Long",
                              Down==3 & ToGo < 7 & ToGo >= 4 ~ "3rd & Med",
                              Down==3 & ToGo < 4 ~ "3rd & Short",
                              Down==4 & ToGo >= 7 ~ "4th & Long",
                              Down==4 & ToGo < 7 & ToGo >= 4 ~ "4th & Med",
                              Down==4 & ToGo < 4 ~ "4th & Short")) %>% 
  left_join(skills2 %>% filter(Target==1) %>% select(GameID, EventID, TargetRoute = RouteMod, Reception)) %>% 
  mutate(TargetRoute = ifelse(is.na(TargetRoute), "None", TargetRoute),
         Reception = ifelse(is.na(Reception), 0, Reception))


new_route_model <- stan_lmer(EPA ~ DownDist +
                                   RPO +
                                   (1 | Field:CoverageScheme) +
                                   (1 | Boundary:CoverageScheme) +
                                   (1 | RB:CoverageScheme),
                             data = passPlay3,
                             chains = 4,
                             cores = 4,
                             iter = 4500,
                             warmup = 2000,
                             prior = normal(location = 0, scale = 1, autoscale=F),
                             prior_intercept = normal(location = 0, scale = 1, autoscale=F),
                             QR=T)

summary(new_route_model)
print(new_route_model)

routeDf <- as.data.frame(new_route_model)

routeDf %>% 
  select_at(vars(contains("(Intercept)") & contains("CoverageScheme") & !contains("Sigma"))) %>% 
  pivot_longer(names_to = "variable", values_to = "samples") %>% 
  group_by(variable) %>% 
  summarise(med = median(samples))

new_route_model$coefficients

routeCombinations2 <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="R") | (LeftPlayers > RightPlayers & SideOfCenterMod=="L"))  ~ "Field",
                                 HashMod=="M" & ((RightPlayers > LeftPlayers & SideOfCenterMod=="L") | (LeftPlayers > RightPlayers & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="R") | (LeftWR > RightWR & SideOfCenterMod=="L")) ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & ((RightWR > LeftWR & SideOfCenterMod=="L") | (LeftWR > RightWR & SideOfCenterMod=="R")) ~ "Boundary",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="R" ~ "Field",
                                 HashMod=="M" & RightPlayers==LeftPlayers & RightWR==LeftWR & SideOfCenterMod=="L" ~ "Boundary")) %>% 
  group_by(GameID, EventID, StrengthInd) %>%
  arrange(GameID, EventID, StrengthInd, Order_OutsideToInside) %>% 
  mutate(OrderID = ifelse(OnFieldPosition=="B", row_number(), Order_OutsideToInside),
         Align = paste0(StrengthInd, OrderID),
         AlignOrder = case_when(Align=="Field1" ~ 1,
                                Align=="Field2" ~ 2,
                                Align=="Field3" ~ 3,
                                Align=="Field4" ~ 4,
                                Align=="RB1" ~ 5,
                                Align=="RB2" ~ 6,
                                Align=="RB3" ~ 7,
                                Align=="Boundary4" ~ 8,
                                Align=="Boundary3" ~ 9,
                                Align=="Boundary2" ~ 10,
                                Align=="Boundary1" ~ 11)) %>% 
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, RouteMod, IsBlocking, AlignOrder, Hash, Align) %>% 
  filter(!is.na(RouteMod) & !RouteMod %in%  c("Blocking", "RunFake") & StrengthInd != "RB") %>% 
  group_by(GameID, EventID, Formation, Hash) %>% 
  arrange(GameID, EventID, AlignOrder) %>% 
  summarise(Routes = paste(RouteMod, collapse=", "),
            RouteIDs = paste(Align, collapse = ", "))

View(routeCombinations2 %>% 
       inner_join(pbp %>% select(GameID, EventID, CoverageScheme)) %>% 
       filter(CoverageScheme!="Screen") %>% 
  group_by(Routes, CoverageScheme) %>% 
  count(., sort=T))


curl_id_loc <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
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
  group_by(GameID, EventID, StrengthInd) %>% 
  summarise(curl_count = sum(grepl("Curl", RouteMod))) %>% 
  ungroup() %>% 
  filter(curl_count > 0) %>% 
  select(-curl_count)



curl_bigrams <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
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
  select(GameID, EventID, OnFieldPosition, StrengthInd, OrderID, Formation, Strength, RouteMod, IsBlocking, AlignOrder, Hash) %>% 
  group_by(GameID, EventID, Formation, StrengthInd) %>% 
  arrange(GameID, EventID, RouteMod) %>% 
  summarise(Routes = paste(RouteMod, collapse=", "),
            RouteIDs = paste(AlignOrder, collapse = ", ")) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes, token = "skip_ngrams", n=3, k=3) %>% 
  filter(lengths(str_split(bigram, " "))==2) %>% 
  count(bigram, sort=T) %>% 
  ungroup() %>% 
  mutate(perc = n/sum(n))
  
curl_bigrams

sum(curl_bigrams$n)





unique(routeCombinations$Routes)

skills2 %>% 
  filter(!is.na(RouteMod) & RouteMod!="Blocking") %>% 
  group_by(RouteMod) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(perc = count/sum(count)) %>% 
  filter(row_number()<=10)

top_10_routes <- 
  skills2 %>% 
  filter(!is.na(RouteMod) & RouteMod!="Blocking") %>% 
  group_by(RouteMod) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  mutate(perc = count/sum(count))

top_10_routes
skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
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
  filter(RouteMod %in% top_10_routes$RouteMod) %>% 
  group_by(RouteMod, Formation, AlignOrder) %>% 
  summarise(count = n()) %>% 
  group_by(RouteMod) %>% 
  mutate(perc = count/sum(count)) %>% 
  arrange(RouteMod, -perc)





top_10_bigrams <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
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
  group_by(GameID, EventID, StrengthInd) %>% 
  filter(sum(grepl(paste(top_10_routes$RouteMod, collapse='|'), RouteMod))>0) %>% 
  filter(!is.na(RouteMod) & RouteMod!="Blocking") %>%  
  group_by(GameID, EventID, Formation, StrengthInd) %>% 
  arrange(GameID, EventID, RouteMod) %>% 
  summarise(Routes = paste(RouteMod, collapse=", "),
            RouteIDs = paste(AlignOrder, collapse = ", ")) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes, token = "skip_ngrams", n=3, k=3) %>% 
  filter(lengths(str_split(bigram, " "))>=2) %>% 
  count(bigram, sort=T) %>% 
  ungroup() %>% 
  mutate(perc = n/sum(n))

View(top_10_bigrams)

top_10_bigrams %>% filter(bigram == "drag drag")

map_df(top_10_routes$RouteMod, function(x){
  df <- tibble(Route = x)
  filter_df <- top_10_bigrams %>% 
                filter(grepl(tolower(paste(x)), bigram)) %>% 
    arrange(-n) %>% 
    filter(row_number()<=5) %>% 
    ungroup() %>% 
    pull(bigram) %>% 
    str_remove(., tolower(paste(x))) %>% 
    str_trim()
  df$combo <- list(filter_df)
  df <- df %>% unnest(cols=c(combo))
  
  return(df)
})


map_df(top_10_routes$RouteMod, function(x){
  df <- tibble(Route = x)
  df$NumPlays <- skills2 %>% 
                  filter(RouteMod==paste(x)) %>% 
                  distinct(GameID, EventID) %>% 
                  nrow()
  
  df$NumRoutes <- skills2 %>% 
                  filter(RouteMod==paste(x)) %>% 
                  nrow()
                  
  df$Tgt <- skills2 %>% 
            ungroup() %>% 
            summarise(targets = sum(Target==1 & RouteMod==paste(x), na.rm=T)) %>% 
            pull(targets)
  
  df$Completion <- skills2 %>% 
    ungroup() %>% 
    summarise(catches = sum(Reception==1 & RouteMod==paste(x), na.rm=T)) %>% 
    pull(catches)
  
  df$EPA <- skills2 %>% 
            filter(RouteMod==paste(x) & Target==1) %>% 
            distinct(GameID, EventID) %>% 
            inner_join(pbp, by=c("GameID", "EventID")) %>% 
            pull(EPA) %>% 
            sum(., na.rm=T)
  
  df$EPA_sd <- skills2 %>% 
    filter(RouteMod==paste(x) & Target==1) %>% 
    distinct(GameID, EventID) %>% 
    inner_join(pbp, by=c("GameID", "EventID")) %>% 
    pull(EPA) %>% 
    na.omit() %>% 
    sd()
  
  df$Yards <- skills2 %>% 
    filter(RouteMod==paste(x) & Target==1) %>% 
    distinct(GameID, EventID) %>% 
    inner_join(pbp, by=c("GameID", "EventID")) %>% 
    pull(OffensiveYardage) %>% 
    sum(., na.rm=T)
  
  
  df <- df %>% 
        mutate(TgtPerc = Tgt/NumRoutes,
               CompPerc = Completion/Tgt,
               YRR = Yards/NumRoutes,
               EPA_per_target = EPA/Tgt)
  return(df)
})






View(
skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
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
  group_by(GameID, EventID, StrengthInd) %>% 
  filter(!is.na(RouteMod) & RouteMod!="Blocking") %>%  
  group_by(GameID, EventID, Formation) %>% 
  arrange(GameID, EventID, RouteMod) %>% 
  summarise(Routes = paste(RouteMod, collapse=", "),
            RouteIDs = paste(AlignOrder, collapse = ", ")) %>% 
  ungroup() %>% 
  unnest_tokens(bigram, Routes, token = "skip_ngrams", n=3, k=3) %>% 
  filter(lengths(str_split(bigram, " "))>=2) %>% 
  count(bigram, sort=T) %>% 
  ungroup() %>% 
  mutate(perc = n/sum(n))
)

routeCombinations %>% 
  group_by(Routes) %>% 
  summarise(count=n()) %>% 
  arrange(-count)

pass_plays <- pbp %>% 
              inner_join(passIDs) %>% 
              inner_join(skills2 %>% filter(Target==1) %>% select(GameID, EventID, RouteMod)) %>% 
              mutate(DownDist = case_when(Down==1 ~ "1st",
                                          Down==2 & ToGo >= 7 ~ "2nd & Long",
                                          Down==2 & ToGo < 7 & ToGo >= 4 ~ "2nd & Med",
                                          Down==2 & ToGo < 4 ~ "2nd & Short",
                                          Down==3 & ToGo >= 7 ~ "3rd & Long",
                                          Down==3 & ToGo < 7 & ToGo >= 4 ~ "3rd & Med",
                                          Down==3 & ToGo < 4 ~ "3rd & Short",
                                          Down==4 & ToGo >= 7 ~ "4th & Long",
                                          Down==4 & ToGo < 7 & ToGo >= 4 ~ "4th & Med",
                                          Down==4 & ToGo < 4 ~ "4th & Short"),
                     HalfRemain = case_when(Quarter==1 ~ 900 + TimeLeft,
                                            Quarter==2 ~ TimeLeft,
                                            Quarter==3 ~ 900 + TimeLeft,
                                            Quarter==4 ~ TimeLeft),
                     ToGoal = ifelse(SideOfField=="Own", StartYard+50, StartYard),
                     ScoreDiff = OffTeamScoreBefore - DefTeamScoreBefore)


basic_route_model <- stan_lmer(EPA ~ DownDist +
                                     HalfRemain +
                                     ToGoal +
                                     ScoreDiff +
                                     (1 | DropType) +
                                     (1 + RouteMod | CoverageScheme),
                           data = pass_plays,
                           chains = 4,
                           cores = 4,
                           warmup = 1000,
                           iter = 3500,
                           QR=T)

summary(basic_route_model)
print(basic_route_model)

sort(unique(skills2$RouteMod))

nonTargetRoutes <- skills2 %>% 
  inner_join(formation_info, by=c("GameID", "EventID")) %>% 
  inner_join(passIDs) %>% 
  filter(OnFieldPosition!="QB") %>% 
  ungroup() %>% 
  mutate(StrengthInd = case_when(SideOfCenterMod==HashMod ~ "Boundary",
                                 SideOfCenterMod!=HashMod & HashMod!="M" & !is.na(SideOfCenterMod)~ "Field",
                                 OnFieldPosition=="B" & is.na(SideOfCenterMod) ~ "RB",
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
  select(OnFieldPosition, StrengthInd, OrderID, Formation, Strength, RouteMod, IsBlocking, AlignOrder, Hash, Target) %>% 
  group_by(GameID, EventID, StrengthInd) %>% 
  filter(sum(Target)> 0) %>% 
  filter(!is.na(RouteMod) & RouteMod!="Blocking" & Target==0) %>% 
  ungroup() %>% 
  select(GameID, EventID, RouteMod) %>% 
  ungroup() %>% 
  mutate(Count = 1) %>% 
  pivot_wider(names_from = RouteMod, values_from = Count, values_fn = list(Count = sum), values_fill=0)

pass_plays2 <- pass_plays %>% 
                left_join(nonTargetRoutes) %>% 
                mutate_at(names(nonTargetRoutes)[-c(1,2)], .funs = function(x){ifelse(is.na(x), 0, x)}) %>% 
                mutate_at(names(nonTargetRoutes)[-c(1,2)], .funs = function(x){ifelse(x >= 1, 1, 0)})

View(pass_plays2 %>% filter(is.na(Curl)))

names(nonTargetRoutes)[-c(1,2)]


paste("DownDist", "HalfRemain", "ToGoal", "ScoreDiff", "(1|DropType) + ")

complex_route_model <- stan_lmer(EPA ~ DownDist +
                                        HalfRemain +
                                        ToGoal +
                                        ScoreDiff +
                                        (1 | DropType) +
                                        (1 +
                                           Curl +
                                           Post +
                                           Flat +
                                           Dig +
                                           Seam +
                                           Fade +
                                           DeepCross +
                                           Slant +
                                           Go +
                                           Whip +
                                           Corner +
                                           Out +
                                           Drag +
                                           Comeback +
                                           ScreenQuick +
                                           Pick +
                                           Beneath +
                                           OverBall +
                                           StickNod +
                                           Swing +
                                           PostCorner +
                                           OutUp +
                                           CornerPost +
                                           Wheel +
                                           ScreenBubble +
                                           CheckRelease +
                                           RunFake +
                                           Angle +
                                           Jerk +
                                           Quick +
                                           HitchGo +
                                           Leak +
                                           Sluggo +
                                           ScreenBeneath +
                                           ScreenTE +
                                           Chip +
                                           ScreenRB +
                                           ScreenTunnel | RouteMod:CoverageScheme),
                                 data = pass_plays2,
                                 chains=4, 
                                 cores=4,
                                 warmup=2000,
                                 iter = 4500,
                                 QR=T)

summary(complex_route_model)
print(complex_route_model)


sort(unique(skills2$RouteMod))

skills2 %>% filter(RouteMod=="OverBall")
sort(unique(passPlay3$DropType))

                                         
passPlay4 %>% filter(GameID == 2793 & EventID == 15)                               
(str_count("Slant, Slant", "Slant") + str_count("Slant, Slant", "Flat") + str_count("Slant, Slant", "ScreenBubble"))==length(str_split("Slant, Slant", ", ")[[1]])                                   
                                     
idRoutes <- function(x){
  y <- case_when((grepl("Go|Fade|Seam|PostCorner", x) & !grepl("Up", x) & 
                    (str_count(x, "Corner") + str_count(x, "Out") + str_count(x, "Flat") + str_count(x, "Whip")) >=2 & 
                    (str_count(x, "Flat")+str_count(x, "Whip"))<=1) ~ "Sail",
                 ((str_count(x, "Go") + str_count(x, "Seam") + str_count(x, "Fade") + str_count(x, "Post") + 
                     str_count(x, "DeepCross") + str_count(x, "Comeback")) >= 3 & str_count(x, "Comeback")<=1) ~ "999",
                 (!grepl("Go|Fade|Seam|PostCorner|Curl", x) & 
                      (str_count(x, "Corner") + str_count(x, "Out") + str_count(x, "Flat") + str_count(x, "Comeback") + str_count(x, "Whip")) >= 3 &
                      str_count(x, "Comeback") <= 1)~ "Flood",
    (grepl("Corner|Out", x) & grepl("Curl|OverBall|Drag|Slant|DeepCross|Dig", x) & grepl("Flat|Whip|Out", x) & 
       (str_count(x, "Corner") + str_count(x, "Out") + str_count(x, "Flat") + str_count(x, "Whip")) >= 2) & !grepl("OutUp", x) ~ "Spot",
    (grepl("Post, Corner|Post, PostCorner|Post, Flat, Corner|Corner, Post", x)) ~ "Scissors",
    (grepl("Dig, Curl, Seam|Dig, Curl, Go|Dig, Out, Seam|Dig, Curl, Post", x)) ~ "Sucker",
    grepl("Dig", x) & str_count(x, "Dig") >= 2 & !grepl(", Corner", x) ~ "Levels",
    grepl("Slant", x) & (str_count(x, "Slant") + str_count(x, "Flat") + str_count(x, "ScreenBubble"))==length(str_split(x, ", ")[[1]]) ~ "Slant-Flat",
    (grepl("Dig, Seam|Dig, Go|Dig, Fade|Seam, Dig|Dig, Sluggo|Dig, Flat, Seam", x)) ~ "Dagger",
    (grepl("Drag, Dig|Drag, Seam|Drag, DeepCross|DeepCross, Dig", x) & !grepl("Post", x)) ~ "Drive", 
    grepl("Post, Dig|Post, Curl|Post, Dig, Whip|Post, OverBall", x) ~ "Mill",
    grepl("Post, Dig, Curl|Post, Curl, Dig|Dig, Post, Curl", x) ~ "Bow-Mill",
    (grepl("Dig, Post|Dig, Out, Post|Dig, Slant, Post", x)) ~ "Crease",
                 grepl("Post, Dig, Whip", x) ~ "Ole Miss Mill",
                 (str_count(x, "Curl") + str_count(x, "OverBall") + str_count(x, "Flat") + str_count(x, "Quick") + str_count(x, "Swing") + 
                    str_count(x, "CheckRelease") + str_count(x, "Whip") + str_count(x, "Out")) == length(str_split(x, ", ")[[1]]) & 
      grepl("Curl|OverBall", x) & grepl("Flat|Out", x) ~ "Curl-Flat",
                 (str_count(x, "Curl") + str_count(x, "OverBall")) == length(str_split(x, ", ")[[1]]) ~ "Curls",
                 x %in% c("Corner, Flat", "Flat, Corner", "Out, Corner", "Corner, Out", "Whip, Corner", "Corner, Whip", "Whip, Corner, Dig", "Swing, Corner, Flat", "Corner, CheckRelease") ~ "Flat-7",
                 grepl(", Corner", x) & (str_count(x, "Corner") + str_count(x, "Curl") + str_count(x, "OverBall") + str_count(x, "Drag") +
                                           str_count(x, "Dig") + str_count(x, "Slant") + str_count(x, "DeepCross")) == length(str_split(x, ", ")[[1]])|
                   x %in% c("Corner, Curl") ~ "Smash-7",
                 grepl("Curl, Fade|Slant, Fade", x) ~ "Smash-Fade",
                 grepl("Dig, Curl|Dig, Stick|Dig, OverBall|Dig, Flat, Curl", x) ~ "Bow",
                 grepl("Post|Seam|Fade|Go", x) & (str_count(x, "Go") + str_count(x, "Fade") + str_count(x, "Seam") + str_count(x, "Post")) >=1 &
                   grepl("Out|Flat|Whip|Swing", x) & !grepl("Dig|Slant|Post|Drag|Curl", x) ~ "Vert-Out",
                 (str_count(x, "Dig") + str_count(x, "Slant")) >= 2 & grepl("Dig", x) ~ "Levels-Mod",
                 grepl("Post, Post", x)|x %in% c("Post, Slant", "Slant, Post") ~ "Double-Post",
                 x %in% c("Drag", "DeepCross") ~ "Iso Cross",
                 x=="Beneath" ~ "Under",
                 x %in% c("Go", "Fade", "FadeBackShoulder") ~ "Iso Deep",
                 grepl("Go|Fade|Seam|Post", x) & grepl("Curl|OverBall", x) & (str_count(x, "Go") + str_count(x, "Fade") + str_count(x, "Seam") +
                                                                                str_count(x, "Post") + str_count(x, "Curl") + str_count(x, "OverBall"))==length(str_split(x, ", ")[[1]]) ~ "Vert-Curl",
                 x %in% c("Curl, Dig", "Curl, Curl, Dig") ~ "Invert-Bow",
                 x %in% c("Flat, Flat", "Flat, Out", "Out, Out", "Out, Flat", "Out, Out, Flat", "Out", "Out, Whip", "Whip, Out",
                          "Comeback, Out", "Comeback, Flat", "Out, Comeback", "Flat, Comeback", "Comeback, Whip", "Swing, Out, Out") ~ "All-Outs",
                 grepl("Sluggo|HitchGo|OutUp", x) ~ "Double Move",
                 x %in% c("Go, Slant", "Go, Slant, Slant", "Go, Drag", "Go, Slant, Drag", "Fade, Slant", "Fade, Slant, Slant") ~ "Vert-Under",
                 x %in% c("Go, Post", "Fade, Post", "Post, Go", "Post, Seam", "Seam, Post", "FadeBackShoulder, Post") ~ "Go-Post",
                 x %in% c("Dig, DeepCross", "Post, DeepCross") ~ "Y-Cross",
                 grepl("Post|Dig|DeepCross", x) & grepl("Wheel", x) ~ "Scissor-Wheel",
                 (str_count(x, "Go") + str_count(x, "Seam") + str_count(x, "Fade")) == length(str_split(x, ", ")[[1]])~ "Verts",
                 x %in% c("Dig, Drag") ~ "Invert Drive",
                 x=="No Route" ~ "",
                 str_split(x, ", ")[[1]][1] %in% c("Comeback", "Out") & grepl("Curl|Seam|Go|Fade|Post", x) & 
                   !grepl("Dig|Slant|Drag", x) ~ "OutBreak-Stem",
                 x=="ScreenBubble" ~ "Bubble",
                 lengths(str_split(x, " "))==1 ~ paste("Iso ", x),
                 grepl("Sluggo", "HitchGo", "StickNod", "OutUp", x) ~ "Double-Move",
                 grepl("Curl", x) & grepl("Seam|Go|Post|Fade", x) & grepl("Out|Flat|Whip|CheckRelease|ScreenQuick", x) ~ "Sail-Curl-Flat",
                 grepl("Post, Wheel|Go, Wheel|Seam, Wheel|Wheel, Post", x) ~ "Post-Wheel",
                 grepl("Go|Fade|Seam", x) & grepl("Dig", x) & (str_count(x, "Go") + str_count(x, "Fade") + str_count(x, "Seam") +
                                                                 str_count(x, "Dig"))==length(str_split(x, ", ")[[1]]) ~ "Go-Dig",
                 grepl("Curl", x) & grepl(", Wheel", x) ~ "Curl-Wheel",
                 grepl("Slant", x) & grepl("Curl|OverBall", x) ~ "Slant-Curl-Flat",
                 grepl("Dig", x) & grepl("Out|Flat|Swing", x) & !grepl("Go|Seam|Fade", x) ~ "Dig-Out",
                 grepl("Post", x) & grepl("Out|Flat|Swing|Whip", x) ~ "Post-Out",
                 grepl("Go|Fade", x) & grepl("Dig", x) & grepl("Flat|Out", x) ~ "Go-Dig-Out",
                 grepl("Comeback, Dig", x) ~ "Comeback-Dig",
                 str_count(x, "Quick")==length(str_split(x, ", ")[[1]]) ~ "Quick")
  
  return(y)
}
                                     
passPlay4 %>% filter(GameID==2793 & EventID==15) %>% select(Field)

passPlay4 <- passPlay3 %>% 
  select(-FumbleByPasser, -FumbleByReceiver, -PressureOnPlay, -SackOnPlay) %>% 
  mutate(RouteConcept = case_when((grepl("Fade|Go|FadeBackShoulder", Field) & 
                                     grepl("Flat|Out|Quick", Field) & 
                                     grepl("Curl|Stick|Quick", Field) & 
                                     grepl("Slant|Post", Boundary)) | 
                                    (grepl("Fade|Go|FadeBackShoulder", Boundary) & 
                                       grepl("Flat|Out|Quick", Boundary) & 
                                       grepl("Curl|Stick|Quick", Boundary) & 
                                       grepl("Slant|Post", Field)) ~ "Tare",
                                  (grepl("Go, Out|Fade, Out|Go, Corner|Fade, Corner|Corner, Out", Field) & grepl("Flat|Out|Drag", RB))|
                                    (grepl("Go, Out|Fade, Out|Go, Corner|Fade, Corner|Corner, Out", Boundary) & grepl("Flat|Out|Drag", RB)) ~ "Sail",
                                  (((grepl("Post", Field) & !grepl("Corner|Wheel", Field)) & grepl("Dig", Boundary)) & 
                                     (grepl("Drag", Field) | grepl("Drag", Boundary) | grepl("Drag|Swing|Dig", RB))) | 
                                    (((grepl("Post", Boundary) & !grepl("Corner|Wheel", Boundary)) & grepl("Dig", Field)) & 
                                       (grepl("Drag", Boundary) | grepl("Drag", Field) | grepl("Drag|Swing|Dig", RB))) ~ "NCAA",
                                  Field=="Corner, Curl" & RB=="Flat" & Boundary=="No Route" ~ "Spot",
                                    (grepl("Dig|DeepCross|OverBall", Field) & !grepl("Drag|Seam|Dig, Dig", Field) & grepl("Drag", Boundary) & !grepl("Dig", Boundary))|
                                    (grepl("Dig|DeepCross|OverBall", Boundary) & !grepl("Drag|Seam|Dig, Dig", Boundary) & grepl("Drag", Field) & !grepl("Dig", Field))|
                                    (grepl("Dig|DeepCross", Field) & !grepl("Drag|Seam|Dig, Dig", Field) & grepl("Drag|OverBall", Boundary) & !grepl("Dig", Boundary))|
                                    (grepl("Dig|DeepCross", Boundary) & !grepl("Drag|Seam|Dig, Dig", Boundary) & grepl("Drag|OverBall", Field) & !grepl("Dig", Field)) ~ "Shallow Cross",
                                  (grepl("Drag, Dig|Drag, DeepCross", Field)  & grepl("Drag", Boundary) & !grepl("Wheel", RB)) |
                                    (grepl("Drag, Dig|Drag, DeepCross", Boundary)  & grepl("Drag", Field) & !grepl("Wheel", RB))|
                                    (!grepl("Wheel", RB) & grepl("Dig|DeepCross", Field) & grepl("Drag", Field) & grepl("Drag", Boundary))|
                                    (!grepl("Wheel", RB) & grepl("Dig|DeepCross", Boundary) & grepl("Drag", Boundary) & grepl("Drag", Field)) ~ "Hi-Lo Crossers",
                                  ((grepl("Angle", Field)|grepl("Angle", RB)) & !grepl("Curl|Flat|Out|Comeback|Corner|Go|Fade", Field) & length(str_split(Field, ", ")[[1]])>2) |
                                    ((grepl("Angle", Boundary)|grepl("Angle", RB)) & grepl("OverBall|Dig|Drag|DeepCross", Boundary) & length(str_split(Boundary, ", ")[[1]]) > 2) ~ "Hi-Lo Triple-In Flood",
                                  (grepl("Drag", Field) & grepl("Drag", Boundary) & grepl("Wheel|Swing", RB)) |
                                    (grepl("Drag", Boundary) & grepl("Drag", Field) & grepl("Wheel|Swing", RB)) ~ "Hi-Lo Mesh",
                                  (grepl("DeepCross|Drag", Boundary) & grepl("Designed Rollout", DropType) & !grepl("Beneath", Boundary) & grepl("RunFake", RB))|
                                    (grepl("DeepCross|Drag", Field) & grepl("Designed Rollout", DropType) & !grepl("Beneath", Field) & grepl("RunFake", RB)) ~ " PA Boot",
                                  (grepl("DeepCross|Drag", Boundary) & grepl("Designed Rollout", DropType) & (grepl("Beneath", Boundary)|grepl("Beneath", RB)) & grepl("RunFake", RB))|
                                    (grepl("DeepCross|Drag", Field) & grepl("Designed Rollout", DropType) & (grepl("Beneath", Field)|grepl("Beneath", RB)) & grepl("RunFake", RB)) ~ "PA Swap Boot",
                                  (grepl("Go, Go|Go, Seam|Fade, Go|Fade, Seam|Comeback, Go|Comeback, Seam|Curl, Seam|Go, Post|Curl, Seam|Comeback, Curl|Curl, Fade|Comeback, Fade|FadeBackShoulder, Seam|Seam, FadeBackShoulder|Go, Fade", Field) & Formation=="2x2") &
                                    (grepl("Go, Go|Go, Seam|Fade, Go|Fade, Seam|Comeback, Go|Comeback, Seam|Curl, Seam|Go, Post|Curl, Seam|Comeback, Curl|Curl, Fade|Comeback, Fade|FadeBackShoulder, Seam|Seam, FadeBackShoulder|Go, Fade", Boundary) & Formation=="2x2") ~ "Four Verts",
                                  (grepl("Sluggo", Field) & grepl(", Seam", Boundary))|
                                    (grepl("Sluggo", Boundary) & grepl(", Seam", Field)) ~ "Pump-Seam",
                                  ((str_count(Field, "Corner") + str_count(Field, "Out") + str_count(Field, "Whip") + str_count(Field, "Flat") + str_count(Boundary, "DeepCross")) >= 3 & !grepl("Go|Fade|Post", Field)) |
                                    ((str_count(Boundary, "Corner") + str_count(Boundary, "Out") + str_count(Boundary, "Whip") + str_count(Boundary, "Flat") + str_count(Field, "DeepCross")) >=3 & !grepl("Go|Fade|Post", Boundary)) ~ "Flood",
                                  (grepl("Dig, Corner, Seam|Dig, Corner, DeepCross|Curl, Corner, Seam|Curl, Corner, DeepCross|Slant, Seam, Corner|Curl, Corner, Post|Drag, Corner, Seam", Field))|
                                    (grepl("Dig, Corner, Seam|Dig, Corner, DeepCross|Curl, Corner, Seam|Curl, Corner, DeepCross|Slant, Seam, Corner|Curl, Corner, Post|Drag, Corner, Seam", Boundary))|
                                    (Field %in% c("Curl, Corner", "Dig, Corner", "DeepCross, Corner, Flat") & grepl("Seam", Boundary))|
                                    (Boundary %in% c("Curl, Corner", "Dig, Corner") & grepl("Seam", Field)) ~ "Smash-Divide",
                                  ((Field=="DeepCross" & Boundary=="DeepCross")|
                                     (Field=="DeepCross" & Boundary=="Post")|
                                     (Field=="Post" & Boundary=="DeepCross")|
                                     (Field=="Post, Flat" & Boundary=="DeepCross")|
                                     (Field=="DeepCross" & Boundary=="Post, Flat")|
                                     (Field=="Go, DeepCross" & Boundary=="DeepCross")|
                                     (Field=="DeepCross" & Boundary=="Go")|
                                     (Field=="Go, Flat" & Boundary=="DeepCross")|
                                     (Field=="DeepCross" & Boundary=="Go, Flat")) ~ "Yankee",
                                  (grepl(" DeepCross", Field) & grepl("Go|Fade|Post", Boundary) & !grepl("Designed Rollout", DropType))|
                                    (grepl(" DeepCross", Boundary) & grepl("Go|Fade|Post", Field) & !grepl("Designed Rollout", DropType)) ~ "Slot-Cross",
                                  (grepl("Pick", Field)|grepl("Pick", Boundary)) ~ "Pick Play",
                                  (grepl("Drag", Field) & grepl("Corner", Field) & grepl("Drag", Boundary))|
                                    (grepl("Drag", Boundary) & grepl("Corner", Boundary) & grepl("Drag", Field)) ~ "Mesh - Corner",
                                  (str_count(Field, "Go")+str_count(Boundary, "Go")+str_count(Field, "Seam")+str_count(Boundary, "Seam")+
                                     str_count(Field, "Fade")+str_count(Boundary, "Fade") + str_count(Field, "Wheel") + str_count(Boundary, "Wheel") +
                                     str_count(Field, "Post") + str_count(Boundary, "Post"))>=3 & 
                                    (str_count(Field, "Go") + str_count(Field, "Seam") + str_count(Field, "Fade") + str_count(Field, "Post")) < 3 &
                                    (str_count(Boundary, "Go") + str_count(Boundary, "Seam") + str_count(Boundary, "Fade") + str_count(Boundary, "Post")) < 3 &
                                    (grepl("Drag|Slant|Whip|Out|Quick|Flat|Dig", Field)|
                                       grepl("Drag|Slant|Whip|Out|Quick|Flat|Dig", Boundary)|
                                       grepl("Drag|Slant|Angle|Whip|Out|Curl|Flat|Dig", RB)) ~ "Clear Out",
                                  (grepl("Drag", Field) & grepl("Drag", Boundary)) ~ "General Mesh",
                                  (grepl("Drag|Beneath|DeepCross", Boundary) & !grepl("Out|Corner|Swing", Boundary) & !grepl("Post|Dig|Drag|Slant|DeepCross", Field))|
                                    (grepl("Drag|Beneath|DeepCross", Field) & !grepl("Out|Corner|Swing", Field) & !grepl("Post|Dig|Drag|Slant|DeepCross", Boundary)) ~ "Over-OneSide",
                                  (grepl("Curl", Field) & grepl("Drag", Field) & grepl("Curl", Boundary) & grepl("Go|Seam|Post", Boundary))|
                                    (grepl("Curl", Boundary) & grepl("Drag", Boundary) & grepl("Curl", Field) & grepl("Go|Seam|Post", Field)) ~ "Triangle-Curl",
                                  (grepl("DeepCross", Boundary) & 
                                     (grepl("DeepCross", Field) | (str_count(Field, "Go") + str_count(Field, "Seam") + str_count(Field, "Post")) >=2))|
                                    (grepl("DeepCross", Field) & 
                                       (grepl("DeepCross", Boundary) | (str_count(Boundary, "Go") + str_count(Boundary, "Seam") + str_count(Boundary, "Post")) >=2)) ~ "Cross-Deep",
                                  TRUE ~ paste(idRoutes(Field), idRoutes(Boundary), sep = '//'))) 



View(passPlay4 %>% select(Field, Boundary, RB, RouteConcept))

View(
passPlay4 %>% 
  filter(RouteConcept=="Other" & (Field=="Whip, Flat, Corner")) %>% mutate(str_count("Whip", Field) + str_count("Flat", Field) + str_count("Corner", Field))
)
passPlay4 %>% filter(GameID==2793 & EventID==15) %>% select(Field)

length(str_split("Go, Curl", ", ")[[1]])

View(
  passPlay4 %>% 
    filter(RouteConcept=="Other") %>% 
    ungroup() %>% 
    unnest_tokens(bigram, Field, token="skip_ngrams", n=3, k=3) %>% 
    group_by(bigram) %>% 
    filter(str_split(bigram, " ") %>% lengths() > 2) %>% 
    count(sort=T) %>% 
    mutate(FieldConcept = case_when(bigram=="post out flat" ~ "Scissors-Out",
                                    grepl("curl curl curl|curl curl overball|curl overball curl|overball curl curl", bigram) ~ "All Curls",
                                    grepl("fade curl curl|go curl curl|seam curl curl|curl seam curl|curl curl seam", bigram) ~ "2-Curl 1-Deep",
                                    grepl("go go flat|go post flat|go seam flat|fade go flat|fade seam flat|fade post flat", bigram) ~ "2-Deep Clear Out"))
)

View(
passPlay4 %>% 
  filter(RouteConcept=="Other" & !grepl("Drag|Dig|DeepCross|Post", Field) & !grepl("Drag|Dig|DeepCross|Post", Boundary)) 
)

skills2 %>% filter(GameID==2863 & EventID==845)

passPlayModelDf <- passPlay4 %>% 
  filter(RouteConcept!="Other") %>% 
  mutate(PA = ifelse(grepl("RunFake", RB) & RPO==0, 1, 0),
         DownDist = case_when(Down==1 ~ "1st",
                              Down==2 & ToGo >= 7 ~ "2nd & Long",
                              Down==2 & ToGo < 7 & ToGo >= 4 ~ "2nd & Med",
                              Down==2 & ToGo < 4 ~ "2nd & Short",
                              Down==3 & ToGo >= 7 ~ "3rd & Long",
                              Down==3 & ToGo < 7 & ToGo >= 4 ~ "3rd & Med",
                              Down==3 & ToGo < 4 ~ "3rd & Short",
                              Down==4 & ToGo >= 7 ~ "4th & Long",
                              Down==4 & ToGo < 7 & ToGo >= 4 ~ "4th & Med",
                              Down==4 & ToGo < 4 ~ "4th & Short"),
         PassingDown = ifelse((Down==3 & ToGo > 3) | (Down==4 & ToGo > 3), 1, 0))

passPlayModel <- stan_lmer(EPA ~ DownDist + PA + RPO + (1 | RouteConcept:CoverageScheme),
                           data = passPlayModelDf,
                           warmup=2000,
                           iter=4500,
                           chains=4,
                           cores=4,
                           prior = normal(location=0, scale=1),
                           prior_intercept = normal(location=0, scale=1))

summary(passPlayModel)

modelDf <- as.data.frame(passPlayModel)

modelDf %>% 
  pivot_longer(names(modelDf), names_to = "Parameter", values_to = "Samples") %>% 
  filter(grepl("RouteConcept:CoverageScheme", Parameter) & !grepl("Sigma", Parameter)) %>% 
  group_by(Parameter) %>% 
  summarise(fifth = quantile(Samples, probs=c(.05)),
            med = median(Samples),
            ninetyfifth = quantile(Samples, probs=c(.95))) %>% 
  ungroup() %>% 
  arrange(-med) %>% 
  filter(row_number()<=20)

