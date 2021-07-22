options(scipen = 999)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(extrafont)
library(extrafontdb)
library(rlang)
library(stringr)
library(ggcorrplot)
library(ColorBre)
library(utils)
library(magrittr)
library(purrr)
library(reshape2)
library(viridis)
library(fuzzyjoin)
library(tidymodels)


###READING IN PLAY-BY-PLAY DATA
urlfile<-'https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/PlayByPlay.csv'
pbp<-read.csv(urlfile)

###READING IN SKILL PLAYER INFORMATION
urlfile<-'https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/SkillPositionPlayers.csv'
skill.players<-read.csv(urlfile)

##INNER-JOINING PLAY-BY-PLAY DATA AND SKILL PLAYER INFORMATION
complete.data <- pbp %>%
  inner_join(skill.players, by = c("GameID","EventID"))

complete.data$EPA <- as.numeric(as.character(complete.data$EPA))  ##BECAUSE EPA IN GIVEN DATA WAS IN CHARACTER FORMAT

##DROPPING ALL RUNNING PLAYS, DROPPING 'NULL' ROUTES AND 'BLOCKING' ROUTES 
complete.data <- complete.data %>%
  filter(str_detect(EventType, 'pass')) %>%
  filter(Route != "NULL" & Route != "Blocking" & Route != "Spike" & DropType != "Spike")

##GROUPING ROUTES INTO STRINGS
route.data <- complete.data %>%
  filter(Route != 'NULL') %>%
  group_by(GameID, EventID) %>%
  summarise(Route = toString(Route)) %>%
  ungroup() %>%
  dplyr::arrange(GameID, EventID, Route)

##DROPPING ANY DUPLICATED ROUTE.DATA ROWS
route.data <- route.data %>%
  distinct(Route, .keep_all = TRUE)


###########
##FILTERING BY SIS PROVIDED GROUPINGS
##THIS IS JUST FOR EXPLORATION PURPOSES TO GET A HANDLE ON THE DATA
###########

##USING `TIDYR` TO GROUP TOGETHER PASSING SCHEMES
short.route <- route.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(all(Route %in% c('Curl', 'Comeback', 'Jerk', 'Out', 'Over Ball', 'Whip'))) %>%
  summarise(Route = toString(Route))

##USING `TIDYR` TO GROUP TOGETHER PASSING SCHEMES
vertical.route <- route.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(all(Route %in% c('Go/Fly', 'Fade', 'Fade - Back Shoulder', 'Seam', 'Post', 'Corner'))) %>%
  summarise(Route = toString(Route))

##USING `TIDYR` TO GROUP TOGETHER PASSING SCHEMES
crossing.route <- route.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(all(Route %in% c('Drag', 'Dig', 'Deep Cross', 'Slant'))) %>%
  summarise(Route = toString(Route))

###########
##NOTE: Apparently no team ran a wholly interior route in all of 2020##
##########

##USING `TIDYR` TO GROUP TOGETHER PASSING SCHEMES
interior.route <- route.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(all(Route %in% c('Angle', 'Beneath', 'Check & Release', 'Chips', 'Flat Left', 'Flat Right', 'Leak',
                          'Swing Left', 'Swing Right', 'Wheel'))) %>%
  summarise(Route = toString(Route))

##USING `TIDYR` TO GROUP TOGETHER PASSING SCHEMES
screen.route <- route.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(all(Route %in% c('Beneath', 'Bubble', 'Drag', 'Quick', 'Shovel', 'TE', 'Tunnel', 'RB', 'Jet Sweep Pass'))) %>%
  summarise(Route = toString(Route))

##USING `TIDYR` TO GROUP TOGETHER PASSING SCHEMES
double.moves.route <- route.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(all(Route %in% c('Corner Post', 'Post Corner', 'Hitch & Go', 'Out & Up', 'Sluggo', 'Stick-Nod'))) %>%
  summarise(Route = toString(Route))

###########
##GATHERING ALL ROUTE COMBINATIONS FROM 2020 SEASON
###########

most.common <- complete.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  summarize(route.combos = toString(Route)) %>%
  ungroup()

###########
##FINDING MOST FREQUENT COMBINATIONS
###########

most.common.combos <- most.common %>%
  mutate(rn = row_number()) %>% 
  separate_rows(route.combos, sep=",\\s+") %>% 
  arrange(rn, route.combos) %>% 
  group_by(rn, GameID, EventID) %>% 
  summarise(route.combos = toString(route.combos)) %>% 
  ungroup %>% 
  count(route.combos)

###########
##PUTTING TOGETHER ALL PLAYS THAT INCLUDE AT LEAST THE COMMON COMBOS
###########

routes.combo1 <- most.common %>%
  filter(str_detect(route.combos, "\\bCurl\\b"),
         str_detect(route.combos, "\\bFlat - Left\\b"),
         str_detect(route.combos, "\\bFlat - Right\\b")) %>%
  mutate(combo = "combo1")

routes.combo2 <- most.common %>%
  filter(str_detect(route.combos, "\\bChip - Flat\\b"),
         str_detect(route.combos, "\\bCurl\\b")) %>%
  mutate(combo = "combo2")

routes.combo3 <- most.common %>%
  filter(str_detect(route.combos, "\\bBeneath\\b"),
         str_detect(route.combos, "\\bCorner\\b"),
         str_detect(route.combos, "\\bDeep Cross\\b"),
         str_detect(route.combos, "\\bRun Fake\\b")) %>%
  mutate(combo = "combo3")

routes.combo4 <- most.common %>%
  filter(str_detect(route.combos, "\\bFlat - Left\\b"),
         str_detect(route.combos, "\\bFlat - Right\\b"),
         str_detect(route.combos, "\\bOver Ball\\b"),
         str_detect(route.combos, "\\bSlant\\b"))  %>%
  mutate(combo = "combo4")

routes.combo5 <- most.common %>%
  filter(str_detect(route.combos, "\\bCurl\\b"),
         str_detect(route.combos, "\\bFlat - Left\\b"),
         str_detect(route.combos, "\\bFlat - Right\\b"),
         str_detect(route.combos, "\\bOver Ball\\b")) %>%
  mutate(combo = "combo5")

routes.combo6 <- most.common %>%
  filter(str_detect(route.combos, "\\bOut\\b"),
         str_detect(route.combos, "\\bRun Fake\\b"),
         str_detect(route.combos, "\\bScreen - Bubble\\b"))  %>%
  mutate(combo = "combo6")

###########
##BINDING ROUTE COMBOS DFS TOGETHER
###########

route.combos.combined <- rbind(routes.combo1, routes.combo2, routes.combo3, routes.combo4, routes.combo5, routes.combo6)


###########
##MERGING ROUTE.COMBOS.COMBINED WITH COMPLETE.DATA TO GET MICRO-DATA
###########

complete.data.routes <- complete.data %>%
  group_by(EventID, GameID) %>%
  slice(1)  ##SLICING TO JUST GET ONE ROW OF EVERY EVENTID SINCE THEY ARE REPEATED PER WR

complete.data.routes <- complete.data.routes %>%
  left_join(route.combos.combined, by = c("GameID" = "GameID", "EventID" = "EventID"))

complete.data.routes <- complete.data.routes[!is.na(complete.data.routes$combo), ]

###########
##SUMMARIZING TO GET EPA/ROUTE PER DEFENSIVE SCHEME
###########

options(digits = 5)

epa.per.combo <- complete.data.routes %>%
  group_by(combo, CoverageScheme) %>%
  summarize(avg.epa = mean(EPA))

###########
##PLOTTING CORRELATION MATRIX
###########

cleaned.labels <- c("Combo 1", "Combo 2", "Combo 3", "Combo 4", "Combo 5", "Combo 6")

ggplot(data = epa.per.combo, aes(x = CoverageScheme, y = combo, fill = avg.epa)) +
  geom_tile()+
  scale_fill_viridis(option="C", name = "EPA per\nCombo") +
  scale_y_discrete(labels= cleaned.labels) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(CoverageScheme, combo, label = sprintf("%0.2f", round(avg.epa, digits = 2))), color = "black", size = 5, fontface = "bold") +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(.75, 0.83),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  theme(panel.background=element_rect(fill="#FFFFFF")) +
  theme(plot.background=element_rect(fill="#FFFFFF")) +
  theme(panel.border=element_rect(colour="#FFFFFF")) +
  theme(panel.grid.major=element_line(colour="#FFFFFF",size=.75)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, size=11,colour="#000000",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#000000",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#000000",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#000000",face="bold",vjust=-.5)) +
  theme(plot.title=element_text(face="bold", colour="#000000",size=15)) +
  theme(aspect.ratio = 9 / 16) +
  ylab("") +
  xlab("")

ggsave("correlation-matrix.png", dpi = 400)


###########
##CREATING EPA ADJUSTMENT
###########

epa.per.combo.adjustment <- complete.data.routes %>%
  group_by(combo, CoverageScheme) %>%
  summarize(total = n(),
            total.epa = sum(EPA))

epa.per.combo.adjustment <- epa.per.combo.adjustment %>%
  group_by(combo) %>%
  mutate(adjusted.epa = sum(total.epa) * total / sum(total))


###########
##PLOTTING ADJUSTED CORRELATION MATRIX
###########

cleaned.labels <- c("Combo 1", "Combo 2", "Combo 3", "Combo 4", "Combo 5", "Combo 6")

ggplot(data = epa.per.combo.adjustment, aes(x = CoverageScheme, y = combo, fill = adjusted.epa)) +
  geom_tile()+
  scale_fill_viridis(option="C", name = "EPA per\nCombo") +
  scale_y_discrete(labels= cleaned.labels) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(CoverageScheme, combo, label = sprintf("%0.2f", round(adjusted.epa, digits = 2))), color = "black", size = 5, fontface = "bold") +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(.75, 0.83),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  theme(panel.background=element_rect(fill="#FFFFFF")) +
  theme(plot.background=element_rect(fill="#FFFFFF")) +
  theme(panel.border=element_rect(colour="#FFFFFF")) +
  theme(panel.grid.major=element_line(colour="#FFFFFF",size=.75)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text.x=element_text(angle = 50, size=11,colour="#000000",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#000000",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#000000",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#000000",face="bold",vjust=-.5)) +
  theme(plot.title=element_text(face="bold", colour="#000000",size=15)) +
  theme(aspect.ratio = 9 / 16) +
  ylab("") +
  xlab("")

ggsave("correlation-matrix-2.png", dpi = 400)

###########
##GETTING DIFFERENCES IN EPA/ADJUSTED EPA
###########

adjusted.differences <- epa.per.combo.adjustment %>%
  group_by(combo, CoverageScheme) %>%
  summarize(difference = adjusted.epa - total.epa)


###########
##GMOVING ON TO WIDE RECEIVER DATA WORK NOW
##WORKING WITH WR POSITIONING ON FIELD
##THIS IS JUST COMBO 3 VS. COVER 3 FOR PROOF OF THEORY
###########

complete.data.routes <- complete.data.routes %>%
  left_join(route.combos.combined, by = c("GameID" = "GameID", "EventID" = "EventID"))

combo3.cover3 <- complete.data.routes %>%
  filter(combo == "combo3" & CoverageScheme == "Cover 3")

wr.positioning <- combo3.cover3 %>%
  group_by(SideOfCenter, Order_OutsideToInside) %>%
  summarize(total = n(),
            total.epa = mean(EPA))

wr.position.adjustment <- wr.positioning %>%
  group_by(SideOfCenter) %>%
  mutate(adjusted.epa = sum(total.epa) * total / sum(total))


###########
##GATHERING INFORMATION FOR ALL ROUTE COMBOS/SCHEMES/SOCS/ORDERS
###########

all.wr.positioning <- complete.data.routes %>%
  group_by(combo, SideOfCenter, Order_OutsideToInside, CoverageScheme) %>%
  summarize(total = n(),
            total.epa = mean(EPA, na.rm = T))  

##DELETING ROWS THAT WERE EITHER DUPLICATED OR `NULL` - NOT SURE WHY. ALL DATA IS GOOD AND ACCOUNTED FOR AFTER DELETING.
all.wr.positioning <- all.wr.positioning[-c(28:38, 100:110, 162:168, 201:204, 236:243, 278:283, 297:398), ]

all.wr.positioning <- all.wr.positioning %>%
  group_by(combo) %>%
  mutate(adjusted.epa = sum(total.epa) * total / sum(total))


###########
##PLOTTING WR SOCS/ORDERS PER ROUTES COMBOS & SCHEMES
##THANKS TO THOSE ON THE `NFLVERSE` DISCOURSE FOR HELPFUL INSIGHTS ON HOW TO BEST DISPLAY ALL THIS INFORMATION
###########

PLOTTING GOES HERE


###########
##TIME TO JOIN IN `NFLFASTR` DATA SO WE CAN GET PLAYER PASS DEFENDED INFORMATION
##NOTE TO SELF: GET A BEER BECAUSE THEY IS REALLY, REALLY GOING TO SUCK - GAMEID AND EVENTID DIFFER BETWEEN DATASETS
##OKAY ... THAT WAS NOT TOO HARD AFTER ALL ... BUT ANY REASON TO HAVE A BEER IS COOL.
###########

nflfastrpbp <- nflfastR::load_pbp(2020) ##READINGIN THE NFLFASTR 2020 PBP DATA

just.receptions <- complete.data.routes



###CREATING THE NFLFASTR `GAME_ID` WITHIN THE SIS DATASET

just.receptions <- mutate(just.receptions,
                          id.for.merging = paste0(
                            "2020_", 
                            sprintf("%02d", Week), 
                            "_", 
                            case_when(
                              OffensiveTeam == "Cardinals" ~ "ARI",
                              OffensiveTeam == "Falcons" ~ "ATL",
                              OffensiveTeam == "Ravens" ~ "BAL",
                              OffensiveTeam == "Bills" ~ "BUF",
                              OffensiveTeam == "Panthers" ~ "CAR",
                              OffensiveTeam == "Bears" ~ "CHI",
                              OffensiveTeam == "Bengals" ~ "CIN",
                              OffensiveTeam == "Browns" ~ "CLE",
                              OffensiveTeam == "Cowboys" ~ "DAL",
                              OffensiveTeam == "Broncos" ~ "DEN",
                              OffensiveTeam == "Lions" ~ "DET",
                              OffensiveTeam == "Packers" ~ "GB",
                              OffensiveTeam == "Texans" ~ "HOU",
                              OffensiveTeam == "Colts" ~ "IND",
                              OffensiveTeam == "Jaguars" ~ "JAX",
                              OffensiveTeam == "Chiefs" ~ "KC",
                              OffensiveTeam == "Rams" ~ "LA",
                              OffensiveTeam == "Chargers" ~ "LAC",
                              OffensiveTeam == "Raiders" ~ "LV",
                              OffensiveTeam == "Dolphins" ~ "MIA",
                              OffensiveTeam == "Vikings" ~ "MIN",
                              OffensiveTeam == "Patriots" ~ "NE",
                              OffensiveTeam == "Saints" ~ "NO",
                              OffensiveTeam == "Giants" ~ "NYG",
                              OffensiveTeam == "Jets" ~ "NYJ",
                              OffensiveTeam == "Eagles" ~ "PHI",
                              OffensiveTeam == "Steelers" ~ "PIT",
                              OffensiveTeam == "Seahawks" ~ "SEA",
                              OffensiveTeam == "49ers" ~ "SF",
                              OffensiveTeam == "Buccaneers" ~ "TB",
                              OffensiveTeam == "Titans" ~ "TEN",
                              OffensiveTeam == "Football Team" ~ "WAS"
                            ),
                            "_",
                            case_when(
                              DefensiveTeam == "Cardinals" ~ "ARI",
                              DefensiveTeam == "Falcons" ~ "ATL",
                              DefensiveTeam == "Ravens" ~ "BAL",
                              DefensiveTeam == "Bills" ~ "BUF",
                              DefensiveTeam == "Panthers" ~ "CAR",
                              DefensiveTeam == "Bears" ~ "CHI",
                              DefensiveTeam == "Bengals" ~ "CIN",
                              DefensiveTeam == "Browns" ~ "CLE",
                              DefensiveTeam == "Cowboys" ~ "DAL",
                              DefensiveTeam == "Broncos" ~ "DEN",
                              DefensiveTeam == "Lions" ~ "DET",
                              DefensiveTeam == "Packers" ~ "GB",
                              DefensiveTeam == "Texans" ~ "HOU",
                              DefensiveTeam == "Colts" ~ "IND",
                              DefensiveTeam == "Jaguars" ~ "JAX",
                              DefensiveTeam == "Chiefs" ~ "KC",
                              DefensiveTeam == "Rams" ~ "LA",
                              DefensiveTeam == "Chargers" ~ "LAC",
                              DefensiveTeam == "Raiders" ~ "LV",
                              DefensiveTeam == "Dolphins" ~ "MIA",
                              DefensiveTeam == "Vikings" ~ "MIN",
                              DefensiveTeam == "Patriots" ~ "NE",
                              DefensiveTeam == "Saints" ~ "NO",
                              DefensiveTeam == "Giants" ~ "NYG",
                              DefensiveTeam == "Jets" ~ "NYJ",
                              DefensiveTeam == "Eagles" ~ "PHI",
                              DefensiveTeam == "Steelers" ~ "PIT",
                              DefensiveTeam == "Seahawks" ~ "SEA",
                              DefensiveTeam == "49ers" ~ "SF",
                              DefensiveTeam == "Buccaneers" ~ "TB",
                              DefensiveTeam == "Titans" ~ "TEN",
                              DefensiveTeam == "Football Team" ~ "WAS"
                            )))

###  !!!PROBLEM!! THIS FLIPS AWAY_TEAM & HOME_TEAM ... SO A MUTATION MUST BE DONE ON `NFLFASTR`
###               DATA AS WELL USING THE `POSTEAM` AND `DEFTEAM` VARIABLES
###               AND THEN WE MERGE ON THOSE NEW VARIABLES (THEORETICALLY SPEAKING, OF COURSE)

nflfastr.receptions <- nflfastrpbp %>%
  filter(play_type == "pass")


nflfastr.receptions <- mutate(nflfastr.receptions,
                              id.for.merging = paste0(
                                "2020_", 
                                sprintf("%02d", week), 
                                "_", 
                                case_when(
                                  posteam == "ARI" ~ "ARI",
                                  posteam == "ATL" ~ "ATL",
                                  posteam == "BAL" ~ "BAL",
                                  posteam == "BUF" ~ "BUF",
                                  posteam == "CAR" ~ "CAR",
                                  posteam == "CHI" ~ "CHI",
                                  posteam == "CIN" ~ "CIN",
                                  posteam == "CLE" ~ "CLE",
                                  posteam == "DAL" ~ "DAL",
                                  posteam == "DEN" ~ "DEN",
                                  posteam == "DET" ~ "DET",
                                  posteam == "GB" ~ "GB",
                                  posteam == "HOU" ~ "HOU",
                                  posteam == "IND" ~ "IND",
                                  posteam == "JAX" ~ "JAX",
                                  posteam == "KC" ~ "KC",
                                  posteam == "LA" ~ "LA",
                                  posteam == "LAC" ~ "LAC",
                                  posteam == "LV" ~ "LV",
                                  posteam == "MIA" ~ "MIA",
                                  posteam == "MIN" ~ "MIN",
                                  posteam == "NE" ~ "NE",
                                  posteam == "NO" ~ "NO",
                                  posteam == "NYG" ~ "NYG",
                                  posteam == "NYJ" ~ "NYJ",
                                  posteam == "PHI" ~ "PHI",
                                  posteam == "PIT" ~ "PIT",
                                  posteam == "SEA" ~ "SEA",
                                  posteam == "SF" ~ "SF",
                                  posteam == "TB" ~ "TB",
                                  posteam == "TEN" ~ "TEN",
                                  posteam == "WAS" ~ "WAS"
                                ),
                                "_",
                                case_when(
                                  defteam == "ARI" ~ "ARI",
                                  defteam == "ATL" ~ "ATL",
                                  defteam == "BAL" ~ "BAL",
                                  defteam == "BUF" ~ "BUF",
                                  defteam == "CAR" ~ "CAR",
                                  defteam == "CHI" ~ "CHI",
                                  defteam == "CIN" ~ "CIN",
                                  defteam == "CLE" ~ "CLE",
                                  defteam == "DAL" ~ "DAL",
                                  defteam == "DEN" ~ "DEN",
                                  defteam == "DET" ~ "DET",
                                  defteam == "GB" ~ "GB",
                                  defteam == "HOU" ~ "HOU",
                                  defteam == "IND" ~ "IND",
                                  defteam == "JAX" ~ "JAX",
                                  defteam == "KC" ~ "KC",
                                  defteam == "LA" ~ "LA",
                                  defteam == "LAC" ~ "LAC",
                                  defteam == "LV" ~ "LV",
                                  defteam == "MIA" ~ "MIA",
                                  defteam == "MIN" ~ "MIN",
                                  defteam == "NE" ~ "NE",
                                  defteam == "NO" ~ "NO",
                                  defteam == "NYG" ~ "NYG",
                                  defteam == "NYJ" ~ "NYJ",
                                  defteam == "PHI" ~ "PHI",
                                  defteam == "PIT" ~ "PIT",
                                  defteam == "SEA" ~ "SEA",
                                  defteam == "SF" ~ "SF",
                                  defteam == "TB" ~ "TB",
                                  defteam == "TEN" ~ "TEN",
                                  defteam == "WAS" ~ "WAS"
                                )))

nflfastr.receptions <- nflfastr.receptions %>%
  select(desc, pass_defense_1_player_name, pass_defense_1_player_id,
         solo_tackle_1_player_name, solo_tackle_1_player_id, air_epa)


###########
##ATTEMPTING TO DO A `FUZZYJOIN` OF THE DATA USING PLAY DESCRIPTION
## !!!DO THIS - IT IS WAYYYYYYYY EASIER AND IT WORKS JUST AS THE MERGING GODS INTENDED
###########

nflfastr.receptions <- nflfastr.receptions %>%
  rename("PlayDesc" = "desc")

merged.data <- stringdist_full_join(just.receptions, nflfastr.receptions, by = "PlayDesc")

cleaned.merged <- merged.data[!is.na(merged.data$route.combos), ]


###########
##DETERMINING WORST DEFENDERS PER ROUTE COMBO/COVERAGE SCHEME
###########

worst.defenders <- cleaned.merged %>%
  group_by(combo, CoverageScheme, solo_tackle_1_player_name) %>%
  filter(Target == 1 & Reception == 1) %>%
  summarize(total.air.epa = sum(air_epa, na.rm = T))

###########
##STEELERS VS. EAGLES WEEK #1 STUFF
###########

eagles.scheme <- complete.data.routes %>%
  group_by(CoverageScheme) %>%
  filter(DefensiveTeam == "Eagles") %>%
  summarize(total = n())

steelers.option <- complete.data.routes %>%
  group_by(combo) %>%
  filter(OffensiveTeam == "Steelers" & CoverageScheme == "Cover 1") %>%
  summarize(total = n())

steelers.route1 <- complete.data.routes %>%
  group_by(SideOfCenter, Order_OutsideToInside) %>%
  filter(OffensiveTeam == "Steelers" & combo == "combo1") %>%
  summarize(total.epa = mean(EPA))

worst.defenders.eagles <- merged.data %>%
  group_by(pass_defense_1_player_name) %>%
  filter(CoverageScheme == "Cover 1" & DefensiveTeam == "Eagles") %>%
  summarize(total.air.epa = mean(air_epa, na.rm = T))

steelers.wr <- merged.data %>%
  group_by(Name) %>%
  filter(SideOfCenter == "R" & Order_OutsideToInside == "3") %>%
  filter(OffensiveTeam == "Steelers") %>%
  summarize(total.epa = mean(EPA))

###########
##TEAM SPECIFIC STUFF
###########

most.common <- complete.data %>%
  separate_rows(Route, sep = ',\\s*') %>%
  group_by(GameID, EventID) %>%
  filter(OffensiveTeam == "Steelers") %>%
  summarize(route.combos = toString(Route)) %>%
  ungroup()

###########
##FINDING MOST FREQUENT COMBINATIONS
###########

most.common.combos <- most.common %>%
  mutate(rn = row_number()) %>% 
  separate_rows(route.combos, sep=",\\s+") %>% 
  arrange(rn, route.combos) %>% 
  group_by(rn, GameID, EventID) %>% 
  dplyr::summarise(route.combos = toString(route.combos)) %>% 
  ungroup %>% 
  count(route.combos)

###########
##PUTTING TOGETHER ALL PLAYS THAT INCLUDE AT LEAST THE COMMON COMBOS
###########

routes.combo1 <- most.common %>%
  filter(str_detect(route.combos, "\\bOut\\b"),
         str_detect(route.combos, "\\bRun Fake\\b"),
         str_detect(route.combos, "\\bSlant\\b")) %>%
  mutate(combo = "combo1")

routes.combo2 <- most.common %>%
  filter(str_detect(route.combos, "\\bFlat - Left\\b"),
         str_detect(route.combos, "\\bFlat - Right\\b"),
         str_detect(route.combos, "\\bGo/Fly\\b"),
         str_detect(route.combos, "\\bOut\\b"),
         str_detect(route.combos, "\\bSlant\\b")) %>%
  mutate(combo = "combo2")

routes.combo3 <- most.common %>%
  filter(str_detect(route.combos, "\\bFlat - Right\\b"),
         str_detect(route.combos, "\\bScreen - Quick\\b"),
         str_detect(route.combos, "\\bSlant\\b")) %>%
  mutate(combo = "combo3")

###########
##BINDING ROUTE COMBOS DFS TOGETHER
###########

route.combos.combined <- rbind(routes.combo1, routes.combo2, routes.combo3)

###########
##MERGING ROUTE.COMBOS.COMBINED WITH COMPLETE.DATA TO GET MICRO-DATA
###########

complete.data.routes <- complete.data %>%
  group_by(EventID, GameID) %>%
  slice(1)  ##SLICING TO JUST GET ONE ROW OF EVERY EVENTID SINCE THEY ARE REPEATED PER WR

complete.data.routes <- complete.data.routes %>%
  left_join(route.combos.combined, by = c("GameID" = "GameID", "EventID" = "EventID"))

complete.data.routes <- complete.data.routes[!is.na(complete.data.routes$combo), ]

###########
##SUMMARIZING TO GET EPA/ROUTE PER DEFENSIVE SCHEME
###########

options(digits = 5)

epa.per.combo <- complete.data.routes %>%
  group_by(combo, CoverageScheme) %>%
  summarize(avg.epa = mean(EPA))



#####
##END
#####

