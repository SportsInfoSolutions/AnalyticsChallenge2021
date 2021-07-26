# Which route combinations were most popular in the NFL in 2020? 
# Of these route combinations, which perform best against each coverage type?


library(tidyverse)
library(skimr)

data_path <- "Data/"

games_import <- read_csv(paste0(data_path, "GameInfo.csv"))
plays_import <- read_csv(paste0(data_path, "PlayByPlay.csv"))
players_import <- read_csv(paste0(data_path, "SkillPositionPlayers.csv"))
player_points_import <- read_csv(paste0(data_path, "PlayerTotalPoints.csv"))

# games -------------------------------------------------------------------

skimr::skim(games_import)

colSums(games_import=="NULL") %>% sum(na.rm=TRUE)

games <- games_import %>% 
  select(
    GameID = GameId, Week,
    HomeTeam, AwayTeam,
    CoveredStadium, TotalLine, SpreadLine,
    PrecipIntensity, PrecipProbability,
    Temperature, WindSpeed
  ) %>% 
  mutate(
    CoveredStadium = as.logical(CoveredStadium)
  ) #%>% 
  #skimr::skim()

# games %>% 
#   select(HomeTeam, AwayTeam, Week, SpreadLine, TotalLine) %>% 
#   filter(HomeTeam == "Chiefs" | AwayTeam == "Chiefs") %>% 
#   arrange(HomeTeam)
# 
# # positive spread line = home team favored by that many points
# # negative spread line = away team favored by that many points


# plays -------------------------------------------------------------------
skimr::skim(plays_import)
colSums(plays_import=="NULL")

plays <- plays_import

plays[plays=="NULL"] <- NA_character_
colSums(is.na(plays))
rowSums(is.na(plays))

sample_min <- 200

plays <- plays %>% 
  mutate(
    across(
      .cols = c(
        Attempt, Completion, ThrowAway, 
        contains("OnPlay"), contains("FumbleBy"),
        FirstDown, Touchdown, Safety, Turnover, 
        Shotgun, RPO
      ),
      .fns = ~as.logical(as.numeric(.x))
    )
  ) %>% #count(Hash)
  mutate(
    ThrowDepth = as.numeric(ThrowDepth),
    Hash = case_when(
      Hash == 1 ~ "L",
      Hash == 3 ~ "R",
      TRUE ~ "M"
    )
  ) %>% 
  filter(EventType == "pass") %>%  #scrambles that end up as rush could be from OL, etc. so excluding since goal is evaluating route combos
  filter(CoverageScheme != "Other") %>% 
  group_by(DropType) %>% #remove special/trick plays
  filter(n() >= sample_min) %>% 
  ungroup() %>% #count(DropType)
  group_by(CoverageScheme) %>% 
  filter(n() >= sample_min) %>% 
  ungroup() %>% #count(CoverageScheme)
  select(-c(Season, EventType, Spike, FumbleByRusher)) #%>% 
  #skimr::skim()


plays_spreadlines <- plays %>% 
  left_join(
    select(games, GameID, HomeTeam, SpreadLine), 
    by = c("GameID" = "GameID", "OffensiveTeam" = "HomeTeam")
  ) %>% 
  left_join(
    select(games, GameID, HomeTeam, SpreadLine), 
    by = c("GameID" = "GameID", "DefensiveTeam" = "HomeTeam"), 
    suffix = c(".off", ".def")
  ) %>% 
  mutate(off_spread_line = ifelse(is.na(SpreadLine.off), -SpreadLine.def, SpreadLine.off)) %>% 
  #select(contains("ID", ignore.case = FALSE), ends_with("Team"), contains("Line")) %>% 
  select(GameID, EventID, off_spread_line)

plays_success <- plays %>%
  mutate(
    yards_of_togo = OffensiveYardage/ToGo,
    off_success = case_when(
      Down == 1 & yards_of_togo >= 0.5 ~ TRUE,
      Down == 2 & yards_of_togo >= 0.7 ~ TRUE,
      (Down == 3 | Down == 4) & yards_of_togo ~ TRUE,
      TRUE ~ FALSE
    ),
    off_success_epa = EPA > 0
  ) %>%
  select(GameID, EventID, off_success, off_success_epa)

# players -----------------------------------------------------------------

skimr::skim(players_import)

players <- players_import

sum(players=="NULL")
players[players=="NULL"] <- NA_character_ #easier to identify, especially in skimr
sum(is.na(players))


# nonQB on field, no route, not blocking, not in motion
players %>%
  filter(is.na(Route) & OnFieldPosition != "QB") %>%
  # count(OnFieldPosition, IsBlocking, FastMotion) %>%
  # arrange(IsBlocking, FastMotion) %>% 
  filter(IsBlocking==0, FastMotion==0) %>% count(GameID, EventID)

nrow(count(players, GameID, EventID))-23
# minus23 plays



players <- players %>% 
  inner_join(select(plays, GameID, EventID, Hash), by = c("GameID", "EventID")) %>% 
  mutate(what = ifelse(OnFieldPosition != "QB" & is.na(Route) & IsBlocking==0 & FastMotion==0, "what", "okay")) %>% 
  #filter(what == "what") %>% count(GameID, EventID) %>% 
  group_by(GameID, EventID) %>% 
  filter(!any(what == "what")) %>% 
  ungroup() %>% 
  #filter(what == "what") %>% 
  #count(GameID, EventID) %>% #nrow()
  select(-what) %>% 
  # group_by(GameID, EventID) %>% 
  # filter(any(!is.na(Route))) %>% #remove plays no routes listed
  # ungroup() %>% 
  mutate(
    across(
      .cols = c(IsBlocking, FastMotion, Target, Reception),
      .fns = ~as.logical(as.numeric(.x))
    )
  ) %>% 
  mutate(
    SideOfCenter = ifelse(is.na(SideOfCenter), "B", SideOfCenter),
    Order_OutsideToInside = ifelse(is.na(Order_OutsideToInside), "B", Order_OutsideToInside)
  ) %>% #skimr::skim()
  mutate(Route = ifelse(is.na(Route), "NULL", Route)) %>% #skimr::skim()#for ease of filtering, etc
  left_join(select(plays, GameID, EventID, Hash)) %>% 
  mutate(
    wide_side = case_when(
      SideOfCenter == "B" ~ NA,
      Hash == "M" ~ NA,
      SideOfCenter != Hash ~ TRUE,
      TRUE ~ FALSE
    )
  )

#routes
players <- players %>% 
  mutate(
    route_cat = case_when(
      Route %in% c("Curl", "Comeback", "Jerk", "Out", "Over Ball", "Whip") ~ "Short",
      Route %in% c("Go/Fly", "Fade", "Fade - Back Shoulder", "Seam", "Post", "Corner") ~ "Vertical",
      Route %in% c("Drag", "Dig", "Deep Cross", "Slant") ~ "Crossing",
      Route != "Chip" & str_detect(Route, "Chip") ~ "Interior",
      Route %in% c("Angle", "Beneath", "Check & Release", "Flat - Right", "Flat - Left", "Leak", "Swing - Right", "Swing - Left", "Wheel") ~ "Interior",
      str_detect(Route, "Screen") ~ "Screen",
      Route %in% c("Jet Sweep Pass", "Quick") ~ "Screen",
      Route %in% c("Corner Post", "Post Corner", "Hitch & Go", "Out & Up", "Sluggo", "Stick - Nod") ~ "DoubleMoves",
      Route %in% c("Blocking", "Run Fake", "Pick", "Chip") ~ "Blocking",
      TRUE ~ "NULL"
    )
  ) %>% 
  mutate(
    Route = ifelse(Route == "Stick - Nod", "Stick-Nod", Route),
    route_split = str_split(Route, " - "),
    route_a = map_chr(route_split, 1),
    route_b = map_chr(route_split, 2, .null = NA_character_)
  ) %>% 
  mutate(
    route_new = case_when(
      is.na(route_b) ~ route_a,
      route_a == "Chip" ~ route_b,
      TRUE ~ route_a
    ),
    route_sub = case_when(
      is.na(route_b) ~ NA_character_,
      route_a == "Chip" ~ route_a,
      TRUE ~ route_b
    )
  ) %>% #count(Route, route_cat, route_new, route_sub) %>% print(n=Inf)
  mutate(
    route_tree = case_when(
      route_new %in% c("Flat", "Swing", "Check & Release") ~ "Flat",
      route_new %in% c("Slant") ~ "Slant",
      route_new %in% c("Comeback") ~ "Comeback",
      route_new %in% c("Curl") ~ "Curl",
      route_new %in% c("Out") ~ "Out",
      route_new %in% c("Jerk", "Drag", "Dig", "Beneath", "Over Ball", "Angle", "Leak") ~ "In",
      route_new %in% c("Corner", "Post Corner") ~ "Corner",
      route_new %in% c("Post", "Deep Cross", "Corner Post") ~ "Post",
      route_new %in% c("Go/Fly", "Fade", "Fade - Back Shoulder", "Seam", "Hitch & Go", "Out & Up", "Sluggo", "Stick-Nod", "Wheel") ~ "Streak",
      str_detect(route_new, "Screen") | route_new %in% c("Jet Sweep Pass", "Quick") ~ "Screen",
      route_new %in% c("Blocking", "Pick", "Chip") ~ "Blocking",
      TRUE ~ route_new #whip, runfake
    )
  ) %>% #count(Route, route_new, route_sub, route_tree, sort = TRUE)
  select(-c(route_a, route_b, route_split))


skimr::skim(players)

agg_route_combos <- function(route_var){
  stopifnot(route_var %in% names(players))
  
  players %>% 
    filter(Route != "NULL" & Route != "Run Fake") %>% 
    count(GameID, EventID, !!as.name(route_var)) %>% 
    arrange(!!as.name(route_var)) %>% 
    mutate(route_count = paste(n, !!as.name(route_var))) %>% 
    group_by(GameID, EventID) %>% 
    summarise(routes = list(sort(!!as.name(route_var)))) %>% 
    ungroup() %>% 
    mutate(route_combo = map_chr(routes, ~paste(.x, collapse = ", "))) %>% 
    mutate(
      route_combo_simplified = ifelse(
        str_detect(route_combo, "Screen") & str_detect(route_combo, "Blocking"),
        "Screen with Blockers",
        route_combo
      )
    )
}
  
agg_route_combos("Route") %>% count(route_combo_simplified, sort = TRUE)
agg_route_combos("route_new") %>% count(route_combo_simplified, sort = TRUE)
agg_route_combos("route_cat") %>% count(route_combo_simplified, sort = TRUE)
agg_route_combos("route_tree") %>% count(route_combo_simplified, sort = TRUE)

route_combos <- agg_route_combos("route_tree") %>% 
  select(GameID, EventID, route_combo = route_combo_simplified)


other_positions <- c("G", "C", "T", "DT", "S")

players_agg <- players %>% 
  group_by(GameID, EventID) %>% 
  summarise(
    nQB = sum(OnFieldPosition == "QB"),
    nB = sum(OnFieldPosition == "B"),
    nTE = sum(OnFieldPosition == "TE"),
    nSWR = sum(OnFieldPosition == "SWR"),
    nWR = sum(OnFieldPosition == "WR"),
    Screen = any(route_cat == "Screen"),
    backs_out = sum(RosterPosition %in% c("RB", "FB") & OnFieldPosition != "B"),
    qb_not_qb = sum(RosterPosition == "QB" & OnFieldPosition != "QB"),
    wr_in_backfield = sum(RosterPosition == "WR" & OnFieldPosition == "B"),
    out_of_pos_n = sum(RosterPosition %in% other_positions),
    pass_blockers = sum(IsBlocking),
    blocking_n = sum(Route == "Blocking"),
    runfake_n = sum(Route == "Run Fake"),
    motion = sum(FastMotion),
    route_leftwide = route_tree[Order_OutsideToInside==1 & SideOfCenter == "L"],
    route_rightwide = route_tree[Order_OutsideToInside==1 & SideOfCenter == "R"]#,
    # across(.cols = contains("route"), .fns = ~.x[Order_OutsideToInside==1 & SideOfCenter == "L"], .names = "{.col}.left_wide_route"),
    # across(.cols = contains("route"), .fns = ~.x[Order_OutsideToInside==1 & SideOfCenter == "R"], .names = "{.col}.right_wide_route")
  ) %>% 
  ungroup() %>% 
  #filter(nQB != 1) %>% nrow()
  mutate(personnel = paste0(nB, nTE))

route_combos_literal_noB <- players %>% 
  filter(route_tree != "NULL" & route_tree != "Run Fake" & !OnFieldPosition %in% c("QB", "B") & !is.na(wide_side)) %>% 
  # group_by(GameID, EventID) %>%
  # #filter(all(route_tree != "Screen")) %>%
  # filter(max(Order_OutsideToInside) <= 3) %>% 
  # ungroup() %>%
  #select(GameID, EventID, wide_side, Order_OutsideToInside, route_tree) %>% #count(Order_OutsideToInside)
  mutate(
    side = ifelse(wide_side, "W", "S"),
    alignment = paste0(side, Order_OutsideToInside)
  ) %>% 
  select(GameID, EventID, alignment, route_tree) %>% #count(GameID, EventID, alignment) %>% filter(n!=1) %>% nrow()
  pivot_wider(id_cols = c("GameID", "EventID"), names_from = alignment, values_from = route_tree) %>% 
  select(GameID, EventID, contains("S"), contains("W")) %>% 
  #filter(is.na(S1) | is.na(W1)) %>% 
  count(
    S1, S2, S3, S4,
    W1, W2, W3, W4,
    sort = TRUE
  )
  
route_combos_literal <- players %>% 
  filter(OnFieldPosition != "QB" & route_tree != "NULL" & route_tree != "Run Fake" & Hash != "M") %>% 
  group_by(GameID, EventID, OnFieldPosition) %>% 
  mutate(ind = row_number()) %>% 
  ungroup() %>% 
  mutate(
    side = case_when(
      is.na(wide_side) ~ "B",
      wide_side ~ "W",
      !wide_side ~ "S"
    ),
    alignment = ifelse(side == "B", paste0(side, ind), paste0(side, Order_OutsideToInside))
  ) %>% 
  pivot_wider(id_cols = c("GameID", "EventID"), names_from = alignment, values_from = route_tree) %>% 
  select(GameID, EventID, contains("S"), contains("W"), contains("B")) %>% 
  nest(B_routes = c(B1,B2,B3)) %>%
  mutate( #no distinction between B players so doing this to detect same combos
    B_routes = map(B_routes, unlist),
    B_routes = map(B_routes, ~.x[!is.na(.x)]),
    B_routes = map(B_routes, ~sort(.x)),
    B1 = map_chr(B_routes, 1, .null = NA_character_), 
    B2 = map_chr(B_routes, 2, .null = NA_character_),
    B3 = map_chr(B_routes, 3, .null = NA_character_)
  ) %>%
  count(
    S1, S2, S3, S4,
    W1, W2, W3, W4,
    B1, B2, B3,
    #B_routes,
    sort = TRUE
  )

route_combos_literal %>% group_by(W1, S1) %>% summarise(n = sum(n)) %>% ungroup() %>% arrange(desc(n))

rec_pos <- route_combos_literal %>% select(-contains("B"), -n) %>% names()
rec_routes <- rec_pos %>% map(~count(route_combos_literal, !!as.name(.x), sort = TRUE))
names(rec_routes) <- rec_pos

b_routes <- route_combos_literal %>% select(contains("B")) %>% unlist() %>% as_tibble() %>% count(value, sort = TRUE)
names(b_routes)[1] <- "B"

rec_routes[["B"]] <- b_routes

routes_by_alignment <- rec_routes %>% 
  imap(~mutate(.x, alignment = .y)) %>% 
  imap(~rename(.x, route = !!as.name(.y))) %>% 
  bind_rows() %>% 
  mutate(route = ifelse(is.na(route), "None", route))



# 1|1
# 1|2
# 1|3
# 1|4
# 2|1
# 2|2
# 2|3
# 3|1
# 3|2
# 4|1

plays_wr_per_side <- players %>% 
  filter(!OnFieldPosition %in% c("QB", "B") & Hash != "M") %>% 
  group_by(GameID, EventID) %>% 
  summarise(
    wide_n = sum(wide_side),
    short_n = sum(!wide_side)
  ) %>% 
  ungroup()

plays_data <- plays %>% 
  filter(Attempt & !FumbleByPasser & !FumbleByReceiver) %>% 
  inner_join(select(games, -c(Week, contains("Team"), SpreadLine)), by = "GameID") %>% 
  inner_join(plays_spreadlines, by = c("GameID", "EventID")) %>% 
  inner_join(plays_success, by = c("GameID", "EventID")) %>% 
  inner_join(players_agg, by = c("GameID", "EventID")) %>% 
  filter(qb_not_qb == 0 & wr_in_backfield == 0 & out_of_pos_n == 0) %>% 
  # consider pass_blockers and blocking_n and runfake_n and motion
  mutate(
    off_lead = OffTeamScoreBefore - DefTeamScoreBefore
  ) %>% 
  select(
    -c(
      OffensiveTeam, DefensiveTeam,
      PlayDesc,
      route_leftwide, route_rightwide, qb_not_qb, wr_in_backfield, out_of_pos_n,
      pass_blockers, blocking_n, runfake_n, motion,
      #play results
      FirstDown, Touchdown, Safety, Turnover, Attempt, Completion,
      ThrowAway, ThrowDepth, OffensiveYardage, EPA,
      contains("OnPlay"), contains("Fumble"),
      #info within other variables
      contains("TeamScore"), nQB, personnel, 
      DropType, CoverageScheme, RPO, Screen,
    )
  ) %>% 
  mutate(
    across(
      .cols = c(Shotgun, CoveredStadium, off_success, off_success_epa),
      .fns = ~ifelse(.x, 1, 0)
    ),
    Hash.L = ifelse(Hash == "L", 1, 0),
    Hash.R = ifelse(Hash == "R", 1, 0),
    SideOfField.Own = ifelse(SideOfField == "Own", 1, 0)
  ) %>% 
  select(-c(Hash, SideOfField))

