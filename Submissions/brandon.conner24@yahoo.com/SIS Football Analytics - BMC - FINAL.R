library(formattable)
library(tidyverse)
library(paletteer)
library(prismatic)
library(nflfastR)
library(gt)

# Add 'not in' function
`%!in%` = Negate(`%in%`)

# Load data files
gameinfo <- read.csv("Data/GameInfo.csv")
pbp <- read.csv("Data/PlayByPlay.csv")
playertotalpts <- read.csv("Data/PlayerTotalPoints.csv")
skillpositionplyrs <- read.csv("Data/SkillPositionPlayers.csv")

# Change names for ease of manipulation
names(skillpositionplyrs)[names(skillpositionplyrs) == "OnFieldPosition"] <- "OFP"
names(skillpositionplyrs)[names(skillpositionplyrs) == "RosterPosition"] <- "RosPos"

# Get team abbreviations from nflfastR
team_names <- nflfastR::teams_colors_logos %>% select(team_abbr, team_name, team_nick) %>% 
  filter(team_abbr %!in% c("SD", "LAR", "OAK", "STL")) # Get rid of old team names

# Create custom theme using Owen Phillips' template
theme_owen <- function () { 
  theme_minimal(base_size=11, base_family="Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'white', color = "white")
    )
}

# Create gameid master table for joining pbp to nflfastR pbp
gameids <- gameinfo %>% 
  select(GameId, Season, Week, HomeTeam, AwayTeam) %>% 
  left_join(team_names, by = c("HomeTeam" = "team_nick")) %>% # Offense abbr
  select(-team_name) %>% relocate(HomeTm = team_abbr, .after = HomeTeam) %>% 
  left_join(team_names, by = c("AwayTeam" = "team_nick")) %>% # Defense abbr
  select(-team_name) %>% relocate(AwayTm = team_abbr, .after = AwayTeam) %>% 
  mutate(nflfastRid = if_else(Week < 10,
                              paste(Season,"_", 0, Week, "_", AwayTm, "_", HomeTm, sep = ""),
                              paste(Season,"_", Week, "_", AwayTm, "_", HomeTm, sep = "")))

# Get pass plays----------------------------------------------------------------------------------
# Keep scrambles for now; use those as 0 EPA/CPOE
dropbacks <- nflfastR::load_pbp(2020) %>% 
  dplyr::filter(qb_dropback == 1, !str_detect(desc, "TWO-POINT CONVERSION"), # Filter out two-point conversions
                qb_spike == 0) # Select all dropbacks using nflfastr

roster_2020 <- nflfastR::fast_scraper_roster(2020) %>% 
  dplyr::select(position, gsis_id) # Pull roster data

dropbacks <- dropbacks %>%  # Filter out passes made by non-QBs as those are mostly trick plays
  dplyr::left_join(roster_2020, by = c("passer_id" = "gsis_id")) %>% 
  dplyr::filter(position == "QB") %>%
  unique() %>%
  dplyr::select(game_id, qtr, quarter_seconds_remaining, qb_dropback)

pass_plays <- gameids %>% # Use dropbacks to get pass plays in pbp set
  dplyr::select(GameID = GameId, nflfastRid) %>% 
  dplyr::left_join(pbp, by = c("GameID")) %>% 
  dplyr::left_join(dropbacks, by = c("nflfastRid" = "game_id", "Quarter" = "qtr",
                                     "TimeLeft" = "quarter_seconds_remaining"), suffix = c("_SIS", "_nflfastR")) %>% 
  dplyr::filter(qb_dropback == 1, CoverageScheme %!in% c("NULL", "Other", "Screen", "Spike")) %>% 
  # Get rid of screens/spikes/plays without coverages/unknown coverages
  dplyr::mutate(game_event = paste(GameID, EventID, sep = "_"))

# Filter skill position players table to get passing plays---------------------------------
pass_routes <- skillpositionplyrs %>% 
  mutate(game_event = paste(GameID, EventID, sep = "_")) %>% 
  mutate(Route = str_replace_all(Route, "Chip - ", "")) %>% # Rename chips
  mutate(Route = str_replace_all(Route, " - Left", "")) %>% # Rename directional routes
  mutate(Route = str_replace_all(Route, " - Right", "")) %>% 
  filter(game_event %in% pass_plays$game_event)

# Get pbp from nflfastR and select relevant columns------------------------------------------------
pbp_nflfastR <- load_pbp(2020) %>% 
  filter(qb_dropback == 1, !str_detect(desc, "TWO-POINT CONVERSION"),
         qb_spike == 0) %>%  # Use same filters as before 
  select(game_id, qtr, quarter_seconds_remaining,
         # Items to join by
         air_yards, pass_length, yards_after_catch,
         score_differential, goal_to_go, down,
         interception, qb_hit, sack, touchdown, pass_touchdown,
         penalty, penalty_team, penalty_yards, aborted_play, no_huddle,
         passer_player_name, # For determining QB quality
         # Play details
         surface, temp, wind,
         # Game environment
         epa, qb_epa, wpa, vegas_wpa, air_epa, air_wpa, yac_wpa,
         cp, cpoe, success, first_down,
         xyac_epa, xyac_fd, xyac_success)

pbp_nflfastR <- pbp_nflfastR %>% # Fix Gardner Minshew issue with name
  mutate(passer_player_name = if_else(str_detect(passer_player_name, "Minshew"),
                                      "G.Minshew", passer_player_name))

# Get QB stats for CPOE ability
future::plan("multisession")

# Prior 3 seasons for larger sample
cpoe_table <- load_pbp(seasons = c(2018, 2019, 2020))

cpoe_table <- cpoe_table %>% # Fix Minshew/RG3 issues with name
  mutate(passer_player_name = if_else(str_detect(passer_player_name, "Minshew"),
                                      "G.Minshew", passer_player_name)) %>% 
  mutate(passer_player_name = if_else(str_detect(passer_player_name, "R.Griffin"),
                                      "R.Griffin", passer_player_name))

# Create table by passer
cpoe_table <- cpoe_table %>%
  dplyr::filter(!is.na(cpoe)) %>%
  dplyr::group_by(passer_player_name) %>%
  dplyr::summarize(cpoe = mean(cpoe), Atts = n()) %>% 
  dplyr::rename(passer_cpoe = cpoe)

# Get route combination for all wide receivers ------------------------------------
all_routes <- pass_routes %>% 
  filter(OFP != "QB", OFP != "B", # Excluding quarterbacks
         Route %!in% c("NULL", "Blocking", "Run Fake", "Chip")) %>%  
  # Filtering out non-routes to remove them from analysis
  mutate(SideOrder = paste(SideOfCenter, Order_OutsideToInside, sep = "")) %>% 
  # Pivoting table to get all routes for each play onto a single row
  pivot_wider(id_cols = c("GameID", "EventID", "game_event"),
              names_from = c(SideOrder),
              values_from = c(Route, PlayerId, OFP, RosPos, Target)) %>% # Select columns
  mutate(Target = replace_na(Target_L1, 0) + replace_na(Target_L2, 0) +
           replace_na(Target_L3, 0) + replace_na(Target_L4, 0) +
           replace_na(Target_R1, 0) + replace_na(Target_R2, 0) +
           replace_na(Target_R3, 0) + replace_na(Target_R4, 0)) %>% 
  select(GameID, EventID, game_event,
         Rt_L1 = Route_L1, Rt_L2 = Route_L2, Rt_L3 = Route_L3, Rt_L4 = Route_L4,
         Rt_R1 = Route_R1, Rt_R2 = Route_R2, Rt_R3 = Route_R3, Rt_R4 = Route_R4,
         Target) %>%
  mutate(total_rts = 8 - str_count(paste(Rt_L1, Rt_L2, Rt_L3, Rt_L4,
                                         Rt_R1, Rt_R2, Rt_R3,Rt_R4, sep = "-"), "NA"))

# Get top ten route combinations
top_all <- all_routes %>% 
  mutate(comblist = apply(.[4:11], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(game_event, comblist) %>%
  unnest(comblist) %>%
  mutate(route_n = str_count(comblist, ",")) %>% 
  # Count number of routes
  group_by(game_event) %>% 
  mutate(max_routes = max(route_n)) %>% 
  filter(route_n == max_routes, route_n > 0) %>% 
  # Get only route pairings; include only full route combos
  ungroup() %>% 
  group_by(comblist) %>% 
  summarise(x = n()) %>%
  arrange(desc(x)) %>%
  slice(1:10)

# Bar graph showing top ten route combinations for all WR/TE
top_all %>% 
  mutate(comblist = fct_reorder(comblist, x, sum)) %>% 
  ggplot(aes(x = comblist, y = x, fill = x)) + geom_col(position = "dodge") +
  geom_text(aes(label = x), hjust = -0.25) +
  coord_flip() + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 135)) + labs(x = "", y = "") +
  theme_bw() + labs(title = "Route Pairs and Trios", subtitle = "2020 Season") +
  theme(legend.position = "none", panel.grid = element_blank())

# Get evaluation metrics from nflfastR (EPA and CPOE) 
# Filter pass plays for routes in top 10 all
pass_alls <- all_routes  %>% 
  mutate(comblist = apply(.[4:11], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(GameID, EventID, game_event,
         Target, comblist) %>%
  unnest(comblist) %>%
  mutate(route_n = str_count(comblist, ",")) %>% 
  group_by(game_event) %>% 
  mutate(max_routes = max(route_n)) %>% 
  filter(route_n == max_routes, route_n > 0,
         comblist %in% top_all$comblist) %>% 
  select(-route_n, -max_routes) %>% 
  # Add pbp detail
  left_join(pass_plays, by = c("GameID", "EventID", "game_event")) 

# Join to nflfastR play-by-play
pass_alls <- pass_alls %>% 
  left_join(pbp_nflfastR, by = c("nflfastRid" = "game_id", "Quarter" = "qtr",
                                 "TimeLeft" = "quarter_seconds_remaining"),
            suffix = c("_SIS", "_nflfastR"))


eval_all <- pass_alls %>% 
  mutate(cp = replace_na(cp, .01)) %>% # Low probability passes
  mutate(total_epa = if_else(Target == 1,
                             replace_na(air_epa, 0) + replace_na(xyac_epa,0), 0),
         # Use the expected yac to strip out receiver differences
         cpoe = if_else(Target == 1, cpoe, 0),
         success = if_else(Target == 1, success, 0)
  ) %>%  
  mutate(xepa = cp * total_epa, # Use expected completion percentage
         cpoe = replace_na(cpoe, 0)) %>% 
  group_by(CoverageScheme, comblist) %>%
  summarize(plays = n(), 
            target_rt = mean(Target),
            xepa = mean(xepa, na.rm = TRUE),
            success = mean(success, na.rm = TRUE),
            cpoe = mean(cpoe)) %>% 
  mutate(comblistDuplicate = comblist,
         sum_plays = sum(plays)) %>% 
  mutate(play_rt = 100 * plays / sum_plays,
         max_epa = if_else(xepa == max(xepa), "max", "not")) %>%
  # Create max_epa by coverage for graph purposes
  ungroup()

# Filter of all "max epa" plays for graph
top_xepa_all <- eval_all %>% 
  filter(max_epa == "max")

# Get mean xepa of all pass plays for comparison
mean_xepa_all <- pass_plays %>% 
  left_join(pbp_nflfastR, by = c("nflfastRid" = "game_id", "Quarter" = "qtr",
                                 "TimeLeft" = "quarter_seconds_remaining"),
            suffix = c("_SIS", "_nflfastR")) %>% 
  mutate(total_epa = if_else(Attempt == 1,
                             replace_na(air_epa, 0) + replace_na(xyac_epa,0), 0)) %>% 
  mutate(xepa = cp * total_epa) %>% 
  summarise(avg = mean(xepa, na.rm = TRUE))

# Order Coverage Scheme factor by xepa for play for visualization purposes
eval_all %>% 
  ungroup() %>% 
  group_by(CoverageScheme) %>% 
  summarise(total = max(xepa)) %>% 
  arrange(desc(total))

eval_all$CoverageScheme <- factor(eval_all$CoverageScheme, # Ordered by xepa
                                  levels = c("Cover 6", "Man Cover 2","Cover 0",
                                             "Tampa 2", "Combination", "Cover 2",
                                             "Cover 1", "Cover 3",
                                             "Cover 4", "Prevent"))

# Graph xepa by coverage for all route combinations
eval_all %>%
  filter(max_epa == "not") %>% 
  ggplot(aes(x = xepa, y = CoverageScheme)) +
  # jitter background points
  geom_jitter(data = mutate(eval_all, comblist = NULL), 
              aes(group = comblistDuplicate), 
              height = 0.05, 
              size = 4.5, 
              color = 'gray80', 
              alpha = .4) +
  # add vertical line at 0
  geom_vline(xintercept = mean_xepa_all$avg, 
             linetype = 'dashed', 
             size = .5, 
             color = 'gray50') +
  # add point for each route
  geom_jitter(data = top_xepa_all,
              aes(group = comblistDuplicate, 
                  fill = comblistDuplicate, 
                  color = after_scale(clr_darken(fill, 0.3))), 
              height = 0.05, 
              size = 6.5, 
              shape = 21, 
              alpha = 1) +
  # add color palette
  scale_fill_manual(values = c("#00B8AAFF", 
                               "#FD625EFF", 
                               "#F2C811FF", 
                               "#8AD4EBFF", 
                               "#FE9666FF", 
                               "#A66999FF", 
                               "#0072B2FF", 
                               "#374649FF")) +
  scale_color_manual(values = c("#00B8AAFF", 
                                "#FD625EFF", 
                                "#F2C811FF", 
                                "#8AD4EBFF", 
                                "#FE9666FF", 
                                "#A66999FF", 
                                "#0072B2FF", 
                                "#374649FF"))  +
  # tweak x-axis
  scale_x_continuous(breaks = seq(-0.5, 2.5, 1)) + 
  # turn off coord clipping
  coord_cartesian(clip = 'off') +  
  theme_owen() + # I probably owe Owen Phillips a lunch after this 
  # make theme tweaks
  theme(plot.title.position = 'plot',
        plot.title = element_text(face ='bold', size = 13),
        plot.subtitle = element_text(size = 8),
        strip.text.x = element_text(size = 7, face = "bold"),
        panel.spacing.x = unit(1, "lines"), 
        plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 6), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10),
        legend.position = 'bottom',
        legend.text = element_text(size = 9.5), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-3,-10),
        plot.caption = element_text(hjust = 0, face = "italic")) +
  # tweak legend
  guides(fill = guide_legend(keyheight = .75)) +
  # add title, subitle, and axis labels
  labs(fill = "", 
       color = "", 
       y = "", 
       x = "Estimated EPA per Play",
       title = "Points Added by Most Popular Route Combinations", 
       subtitle = "Best by Coverage | Data based on the 2020 NFL season",
       caption = "Dotted line represents mean EPA of all pass plays.")

# Create CPOE table for all route combinations/coverage schemes
adj_cpoe_all <- pass_alls %>% 
  left_join(cpoe_table, by = "passer_player_name") %>%
  # Add passer CPOE to adjust for QB quality
  mutate(cpoe = if_else(Target == 1, cpoe, 0),
         passer_cpoe = if_else(Target == 1, passer_cpoe, 0)) %>%  
  group_by(CoverageScheme, comblist) %>%
  summarize(plays = n(), 
            target_rt = mean(Target),
            cpoe = mean(cpoe, na.rm = TRUE)/100,
            passer_cpoe = mean(passer_cpoe, na.rm = TRUE)/100) %>%
  mutate(cpoe_difference = cpoe - passer_cpoe) %>%
  arrange(desc(cpoe_difference)) %>%
  ungroup() %>% 
  select(comblist, CoverageScheme, plays,
         tgt_rt = target_rt, adj_cpoe = cpoe_difference) %>% 
  mutate(CoverageScheme = str_replace_all(CoverageScheme, " ", "_")) %>% 
  pivot_wider(id_cols = c("comblist"),
              names_from = c(CoverageScheme),
              values_from = c(tgt_rt, adj_cpoe)) %>% 
  select(comblist, # Selected target rates here, but do not need
         tgt_rt_Cover_0, adj_cpoe_Cover_0,
         tgt_rt_Cover_1, adj_cpoe_Cover_1,
         tgt_rt_Cover_2, adj_cpoe_Cover_2,
         tgt_rt_Man_Cover_2, adj_cpoe_Man_Cover_2,
         tgt_rt_Tampa_2, adj_cpoe_Tampa_2,
         tgt_rt_Cover_3, adj_cpoe_Cover_3,
         tgt_rt_Cover_4, adj_cpoe_Cover_4,
         tgt_rt_Cover_6, adj_cpoe_Cover_6,
         tgt_rt_Combination, adj_cpoe_Combination,
         tgt_rt_Prevent, adj_cpoe_Prevent)

# Create adjusted CPOE table
adj_cpoe_all %>% 
  select(comblist, adj_cpoe_Cover_0, adj_cpoe_Cover_1,
         adj_cpoe_Cover_2, adj_cpoe_Man_Cover_2,
         adj_cpoe_Tampa_2, adj_cpoe_Cover_3,
         adj_cpoe_Cover_4, adj_cpoe_Cover_6,
         adj_cpoe_Combination, adj_cpoe_Prevent) %>% 
  gt() %>% 
  tab_header(
    title = md("**QB-Adjusted CPOE**"), 
    subtitle = md("By route combination and coverage")
  )%>% 
  cols_label(comblist = "Route Combo", 
             adj_cpoe_Cover_0 = "Cover 0",
             adj_cpoe_Cover_1 = "Cover 1",
             adj_cpoe_Cover_2 = "Cover 2",
             adj_cpoe_Man_Cover_2 = "Man Cover 2",
             adj_cpoe_Tampa_2 = "Tampa 2",
             adj_cpoe_Cover_3 = "Cover 3",
             adj_cpoe_Cover_4 = "Cover 4",
             adj_cpoe_Cover_6 = "Cover 6",
             adj_cpoe_Combination = "Combo.",
             adj_cpoe_Prevent = "Prevent")%>%
  fmt_percent(
    columns = vars(2:11),
    decimals = 1
  ) %>% 
  fmt_missing(columns = 2:11,
              missing_text = "-") %>% 
  data_color(
    columns = 2:11,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlBu",
        n = 11,
        direction = 1
      ) %>% as.character(),
      domain = c(-.7, .45), 
      na.color = "white"
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = 2:11
  ) %>% 
  opt_row_striping() %>% 
  tab_options(
    table.font.names = "Consolas", 
    table.background.color = "white",
    table.font.size = 15,
    heading.title.font.size = 20,
    heading.subtitle.font.size = 15,
    column_labels.font.size = 13,
    column_labels.font.weight = 'bold',
    data_row.padding = px(10)
  ) %>% 
  gtsave("CPOE_all.png")

# Rerun models for pass combinations by side of formation -----------------------------------
# Create data frame of all pass routes for each play--------------------------------------------
routes_by_formation <- pass_routes %>% 
  filter(SideOfCenter != "NULL", # Excluding backs for now
         Route %!in% c("NULL", "Blocking", "Run Fake", "Chip")) %>% 
  # Filtering out non-routes to remove them from analysis
  mutate(SideOrder = paste(SideOfCenter, Order_OutsideToInside, sep = "")) %>% 
  # Pivoting table to get all routes for each play onto a single row
  pivot_wider(id_cols = c("GameID", "EventID", "game_event", "SideOfCenter"),
              names_from = c(Order_OutsideToInside), # Get group for each side
              values_from = c(Route, PlayerId, OFP, RosPos, Target)) %>% 
  mutate(Formation = str_remove_all(paste(OFP_1, OFP_2, OFP_3, OFP_4, sep = "-"), "-NA")) %>%
  mutate(Formation = str_remove_all(Formation, "NA-"), # Get rid of NA values
         Target = replace_na(Target_1, 0) + replace_na(Target_2, 0) +
           replace_na(Target_3, 0) + replace_na(Target_4, 0)) %>% 
  select(GameID, EventID, game_event, Formation,
         Route_1, Route_2, Route_3, Route_4, SideOfCenter, Target) %>%
  # Select columns, then add formation type
  mutate(Form_type = case_when( # This is an inexact science to prevent too many formation types
    str_count(Formation, "R") == 4 ~ "Quads", 
    str_count(Formation, "R") == 3 ~ "Trips",
    str_count(Formation, "R") == 2 & str_count(Formation, "TE") <= 1 ~ "Twins",
    str_count(Formation, "TE") == 3 ~ "3 TE",
    str_count(Formation, "WR") == 1 ~ "Single-wide",
    str_count(Formation, "SR") == 1 ~ "Single-slot",
    str_count(Formation, "TE") == 2 ~ "2 TE",
    str_count(Formation, "TE") ==1 ~ "Single TE"
  ))

# Get most frequent route pairs split into two columns
route_list <- routes_by_formation %>% 
  mutate(comblist = apply(.[5:8], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(game_event, SideOfCenter, comblist) %>%
  unnest(comblist) %>%
  unique() %>%
  # Filter duplicates so that no double counting
  filter(str_detect(comblist, ",")) %>% 
  filter(str_count(comblist, ",") == 1) %>%
  # Get only combination pairs
  select(-game_event, -SideOfCenter) %>% 
  separate(comblist, sep = ", ", c("Rt_1", "Rt_2"))

# Use route list to get most frequent pairings
rt_1 <- route_list %>% 
  group_by(Rt_1) %>% 
  summarise(count = n()) %>% 
  rename(Rt = Rt_1)

rt_2 <- route_list %>% 
  group_by(Rt_2) %>% 
  summarise(count = n())%>% 
  rename(Rt = Rt_2)

top_routes <- rt_1 %>% 
  left_join(rt_2, by = "Rt", suffix = c("_1", "_2")) %>%
  group_by(Rt) %>% 
  mutate(count = replace_na(count_1, 0) + replace_na(count_2,0)) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>%
  slice(1:10)

# Create table showing route pairing frequency
routes_by_formation %>% 
  mutate(comblist = apply(.[5:8], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(game_event, SideOfCenter, comblist) %>%
  unnest(comblist) %>% unique() %>%
  # Get unique so there's no double counting
  filter(str_detect(comblist, ",")) %>% 
  filter(str_count(comblist, ",") == 1) %>% select(-game_event, -SideOfCenter) %>% 
  separate(comblist, sep = ", ", c("Rt_1", "Rt_2")) %>% table() %>% 
  as_tibble() %>% filter(Rt_1 %in% top_routes$Rt & Rt_2 %in% top_routes$Rt) %>% 
  filter(n > 0) %>% 
  ggplot(aes(x = Rt_1, y = Rt_2)) + geom_tile(aes(fill = n), color = "black") + 
  geom_text(aes(label = sprintf("%1.0f", n)), vjust = 1, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + theme_minimal() +
  coord_flip() + theme(legend.position = "none", panel.grid.major.x = element_blank()) + 
  labs(x = "", y = "", title = "Top Route Pairings: 2020")

# Get route combinations by pairs/trios-----------------------------------------------------------------
# Top pairings
top_pairs <- routes_by_formation %>% 
  mutate(comblist = apply(.[5:8], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(game_event, Formation, Form_type, comblist) %>%
  unnest(comblist) %>%
  mutate(route_n = str_count(comblist, ",")) %>% 
  # Count number of routes
  group_by(game_event) %>% 
  mutate(max_routes = max(route_n)) %>% 
  filter(route_n == max_routes, route_n == 1) %>% 
  # Get only route pairings; include only full route combos
  ungroup() %>% 
  group_by(comblist) %>% 
  summarise(x = n()) %>%
  arrange(desc(x)) %>% 
  slice(1:10)

# Top combination by trips formation
top_trios <- routes_by_formation %>% 
  mutate(comblist = apply(.[5:8], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(game_event, Formation, Form_type, comblist) %>%
  unnest(comblist) %>%
  mutate(route_n = str_count(comblist, ",")) %>% 
  # Count number of routes
  group_by(game_event) %>% 
  mutate(max_routes = max(route_n)) %>% 
  filter(route_n == max_routes, route_n == 2) %>% 
  # Get only route trios; include only full route combos
  ungroup() %>% 
  group_by(comblist) %>% 
  summarise(x = n()) %>%
  arrange(desc(x)) %>% 
  slice(1:10)

# Bar graph showing top pairs/trios 
top_pairs %>% 
  rbind(top_trios) %>% 
  mutate(Routes = if_else(str_count(comblist, ",") == 1, "Pairs", "Trios")) %>% 
  mutate(comblist = fct_reorder(comblist, x, sum)) %>% 
  ggplot(aes(x = comblist, y = x, fill = x)) + geom_col(position = "dodge") +
  geom_text(aes(label = x), hjust = -0.25) +
  coord_flip() + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 550)) + labs(x = "", y = "") +
  theme_bw() + labs(title = "Route Pairs and Trios", subtitle = "2020 Season") +
  theme(legend.position = "none", panel.grid = element_blank())

# Get plays/route details for top pairs and trios---------------------------------------------------
pass_pairs <- routes_by_formation %>% 
  mutate(comblist = apply(.[5:8], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(GameID, EventID, game_event,
         Formation, Form_type,
         Target, comblist) %>%
  unnest(comblist) %>%
  mutate(route_n = str_count(comblist, ",")) %>% 
  group_by(game_event) %>% 
  mutate(max_routes = max(route_n)) %>% 
  filter(route_n == max_routes, route_n == 1,
         comblist %in% top_pairs$comblist )%>% 
  select(-route_n, -max_routes) %>% 
  # Add pbp detail
  left_join(pass_plays, by = c("GameID", "EventID", "game_event")) 

pass_trios <- routes_by_formation %>% 
  mutate(comblist = apply(.[5:8], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>% 
  select(GameID, EventID, game_event,
         Formation, Form_type,
         Target, comblist) %>%
  unnest(comblist) %>%
  mutate(route_n = str_count(comblist, ",")) %>% 
  group_by(game_event) %>% 
  mutate(max_routes = max(route_n)) %>% 
  filter(route_n == max_routes, route_n == 2,
         comblist %in% top_trios$comblist) %>% 
  select(-route_n, -max_routes) %>% 
  # Add pbp detail
  left_join(pass_plays, by = c("GameID", "EventID", "game_event")) 

# Join pass pairs and trios to the nflfastR pbp data
pass_pairs <- pass_pairs %>% 
  left_join(pbp_nflfastR, by = c("nflfastRid" = "game_id", "Quarter" = "qtr",
                                 "TimeLeft" = "quarter_seconds_remaining"),
            suffix = c("_SIS", "_nflfastR"))

pass_trios <- pass_trios %>% 
  left_join(pbp_nflfastR, by = c("nflfastRid" = "game_id", "Quarter" = "qtr",
                                 "TimeLeft" = "quarter_seconds_remaining"),
            suffix = c("_SIS", "_nflfastR"))

#Evaluate pairs and trios by EPA, CPOE -----------------------------------------------------------
eval_metrics <- pass_pairs %>% 
  rbind(pass_trios) %>% 
  mutate(cp = replace_na(cp, .01)) %>% # Low probability passes
  mutate(total_epa = if_else(Target == 1,
                             replace_na(air_epa, 0) + replace_na(xyac_epa,0), 0),
         # Use the expected yac to strip out receiver differences
         cpoe = if_else(Target == 1, cpoe, 0),
         success = if_else(Target == 1, success, 0)
  ) %>%  
  mutate(xepa = cp * total_epa,
         cpoe = replace_na(cpoe, 0)) %>% 
  group_by(CoverageScheme, comblist) %>%
  summarize(plays = n(), 
            target_rt = mean(Target),
            xepa = mean(xepa, na.rm = TRUE),
            success = mean(success, na.rm = TRUE),
            cpoe = mean(cpoe)) %>% 
  mutate(comblistDuplicate = comblist,
         sum_plays = sum(plays)) %>%
  mutate(play_rt = 100 * plays / sum_plays,
         max_epa = if_else(xepa == max(xepa), "max", "not")) %>%
  ungroup()

# Filter out top epa plays for graph
top_xepa <- eval_metrics %>% 
  filter(max_epa == "max")

# Get mean epa play for pass routes
mean_xepa <- eval_all %>% 
  ungroup() %>% 
  summarise(avg = stats::weighted.mean(xepa, plays))

# Reorder coverage scheme based on epa
eval_metrics %>% 
  ungroup() %>% 
  group_by(CoverageScheme) %>% 
  summarise(total = max(xepa)) %>% 
  arrange(desc(total))

eval_metrics$CoverageScheme <- factor(eval_metrics$CoverageScheme, # Ordered by xepa
                                      levels = c("Tampa 2", "Cover 6", "Prevent",
                                                 "Cover 0", "Combination", "Cover 2",
                                                 "Man Cover 2", "Cover 4", 
                                                 "Cover 1", "Cover 3"))

# Graph epa by coverage scheme
eval_metrics %>%
  filter(max_epa == "not") %>% 
  ggplot(aes(x = xepa, y = CoverageScheme)) +
  # jitter background points
  geom_jitter(data = mutate(eval_metrics, comblist = NULL), 
              aes(group = comblistDuplicate), 
              height = 0.05, 
              size = 4.5, 
              color = 'gray80', 
              alpha = .4) +
  # add vertical line at 0
  geom_vline(xintercept = mean_xepa$avg, 
             linetype = 'dashed', 
             size = .5, 
             color = 'gray50') +
  # add point for each route
  geom_jitter(data = top_xepa,
              aes(group = comblistDuplicate, 
                  fill = comblistDuplicate, 
                  color = after_scale(clr_darken(fill, 0.3))), 
              height = 0.05, 
              size = 6.5, 
              shape = 21, 
              alpha = 1) +
  # add color palette
  scale_fill_manual(values = c("#00B8AAFF", 
                               "#FD625EFF", 
                               "#F2C811FF", 
                               "#8AD4EBFF", 
                               "#FE9666FF", 
                               "#A66999FF", 
                               "#0072B2FF", 
                               "#374649FF")) +
  scale_color_manual(values = c("#00B8AAFF", 
                                "#FD625EFF", 
                                "#F2C811FF", 
                                "#8AD4EBFF", 
                                "#FE9666FF", 
                                "#A66999FF", 
                                "#0072B2FF", 
                                "#374649FF"))  +
  # tweak x-axis
  scale_x_continuous(breaks = seq(-0.5, 2.5, 1)) + 
  # turn off coord clipping
  coord_cartesian(clip = 'off') +  
  theme_owen() + # I probably owe Owen Phillips a lunch after this 
  # make theme tweaks
  theme(plot.title.position = 'plot',
        plot.title = element_text(face ='bold', size = 13),
        plot.subtitle = element_text(size = 8),
        strip.text.x = element_text(size = 7, face = "bold"),
        panel.spacing.x = unit(1, "lines"), 
        plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 6), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        legend.position = 'bottom',
        legend.text = element_text(size = 9.5), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10)) +
  # tweak legend
  guides(fill = guide_legend(keyheight = .75)) +
  # add title, subitle, and axis labels
  labs(fill = "", 
       color = "", 
       y = "", 
       x = "Estimated EPA per Play",
       title = "Points Added by Most Popular Pairs/Trios", 
       subtitle = "Best by Coverage | Data based on the 2020 NFL season")

# Repeat EPA chart by pairs since most best combos are trios
eval_metrics2 <- pass_pairs %>%
  mutate(cp = replace_na(cp, .01)) %>% # Low probability passes
  mutate(total_epa = if_else(Target == 1,
                             replace_na(air_epa, 0) + replace_na(xyac_epa,0), 0),
         # Use the expected yac to strip out receiver differences
         cpoe = if_else(Target == 1, cpoe, 0),
         success = if_else(Target == 1, success, 0)
  ) %>%  
  mutate(xepa = cp * total_epa,
         cpoe = replace_na(cpoe, 0)) %>% 
  group_by(CoverageScheme, comblist) %>%
  summarize(plays = n(), 
            target_rt = mean(Target),
            xepa = mean(xepa, na.rm = TRUE),
            success = mean(success, na.rm = TRUE),
            cpoe = mean(cpoe)) %>% 
  mutate(comblistDuplicate = comblist,
         sum_plays = sum(plays)) %>%
  mutate(play_rt = 100 * plays / sum_plays,
         max_epa = if_else(xepa == max(xepa), "max", "not")) %>%
  ungroup()

# Get top epa pairs for graph
top_xepa2 <- eval_metrics2 %>% 
  filter(max_epa == "max")

# Reorder factor again based on epa
eval_metrics2 %>% 
  ungroup() %>% 
  group_by(CoverageScheme) %>% 
  summarise(total = max(xepa)) %>% 
  arrange(desc(total))

eval_metrics2$CoverageScheme <- factor(eval_metrics2$CoverageScheme, # Ordered by xepa
                                       levels = c("Combination", "Cover 0", "Cover 6",
                                                  "Prevent", "Man Cover 2", "Tampa 2", 
                                                  "Cover 1", "Cover 4",
                                                  "Cover 3", "Cover 2"))

# Create graph again
eval_metrics2 %>%
  filter(max_epa == "not") %>% 
  ggplot(aes(x = xepa, y = CoverageScheme)) +
  # jitter background points
  geom_jitter(data = mutate(eval_metrics2, comblist = NULL), 
              aes(group = comblistDuplicate), 
              height = 0.05, 
              size = 4.5, 
              color = 'gray80', 
              alpha = .4) +
  # add vertical line at 0
  geom_vline(xintercept = mean_xepa$avg, 
             linetype = 'dashed', 
             size = .5, 
             color = 'gray50') +
  # add point for each route
  geom_jitter(data = top_xepa2,
              aes(group = comblistDuplicate, 
                  fill = comblistDuplicate, 
                  color = after_scale(clr_darken(fill, 0.3))), 
              height = 0.05, 
              size = 6.5, 
              shape = 21, 
              alpha = 1) +
  # add color palette
  scale_fill_manual(values = c("#00B8AAFF", 
                               "#FD625EFF", 
                               "#F2C811FF", 
                               "#8AD4EBFF", 
                               "#FE9666FF", 
                               "#A66999FF", 
                               "#0072B2FF", 
                               "#374649FF")) +
  scale_color_manual(values = c("#00B8AAFF", 
                                "#FD625EFF", 
                                "#F2C811FF", 
                                "#8AD4EBFF", 
                                "#FE9666FF", 
                                "#A66999FF", 
                                "#0072B2FF", 
                                "#374649FF"))  +
  # tweak x-axis
  scale_x_continuous(breaks = seq(-0.5, 2.5, 1)) + 
  # turn off coord clipping
  coord_cartesian(clip = 'off') +  
  theme_owen() + # I probably owe Owen Phillips a lunch after this 
  # make theme tweaks
  theme(plot.title.position = 'plot',
        plot.title = element_text(face ='bold', size = 13),
        plot.subtitle = element_text(size = 8),
        strip.text.x = element_text(size = 7, face = "bold"),
        panel.spacing.x = unit(1, "lines"), 
        plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 6), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        legend.position = 'bottom',
        legend.text = element_text(size = 9.5), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10)) +
  # tweak legend
  guides(fill = guide_legend(keyheight = .75)) +
  # add title, subitle, and axis labels
  labs(fill = "", 
       color = "", 
       y = "", 
       x = "Estimated EPA per Play",
       title = "Points Added by Most Popular Pairs", 
       subtitle = "Best by Coverage | Data based on the 2020 NFL season")

# Create CPOE table for top twenty route combinations ------------------------------------
adj_cpoe <- pass_pairs %>% 
  rbind(pass_trios) %>% 
  left_join(cpoe_table, by = "passer_player_name") %>%
  mutate(success = if_else(Target == 1, success, 0),
         cpoe = if_else(Target == 1, cpoe, 0),
         passer_cpoe = if_else(Target == 1, passer_cpoe, 0)) %>%  
  group_by(CoverageScheme, comblist) %>%
  summarize(plays = n(), 
            target_rt = mean(Target),
            success = mean(success, na.rm = TRUE),
            cpoe = mean(cpoe, na.rm = TRUE)/100,
            passer_cpoe = mean(passer_cpoe, na.rm = TRUE)/100) %>%
  mutate(cpoe_difference = cpoe - passer_cpoe) %>%
  arrange(desc(cpoe_difference)) %>%
  ungroup() %>% 
  select(comblist, CoverageScheme, plays,
         tgt_rt = target_rt, adj_cpoe = cpoe_difference) %>% 
  mutate(CoverageScheme = str_replace_all(CoverageScheme, " ", "_")) %>% 
  pivot_wider(id_cols = c("comblist"),
              names_from = c(CoverageScheme),
              values_from = c(tgt_rt, adj_cpoe)) %>% 
  select(comblist,
         tgt_rt_Cover_0, adj_cpoe_Cover_0,
         tgt_rt_Cover_1, adj_cpoe_Cover_1,
         tgt_rt_Cover_2, adj_cpoe_Cover_2,
         tgt_rt_Man_Cover_2, adj_cpoe_Man_Cover_2,
         tgt_rt_Tampa_2, adj_cpoe_Tampa_2,
         tgt_rt_Cover_3, adj_cpoe_Cover_3,
         tgt_rt_Cover_4, adj_cpoe_Cover_4,
         tgt_rt_Cover_6, adj_cpoe_Cover_6,
         tgt_rt_Combination, adj_cpoe_Combination,
         tgt_rt_Prevent, adj_cpoe_Prevent)

# Create table for pairs and trios
adj_cpoe %>% 
  select(comblist, adj_cpoe_Cover_0, adj_cpoe_Cover_1,
         adj_cpoe_Cover_2, adj_cpoe_Man_Cover_2,
         adj_cpoe_Tampa_2, adj_cpoe_Cover_3,
         adj_cpoe_Cover_4, adj_cpoe_Cover_6,
         adj_cpoe_Combination, adj_cpoe_Prevent) %>% 
  gt() %>% 
  tab_header(
    title = md("**QB-Adjusted CPOE**"), 
    subtitle = md("By route combination and coverage")
  )%>% 
  cols_label(comblist = "Route Combo", 
             adj_cpoe_Cover_0 = "Cover 0",
             adj_cpoe_Cover_1 = "Cover 1",
             adj_cpoe_Cover_2 = "Cover 2",
             adj_cpoe_Man_Cover_2 = "Man Cover 2",
             adj_cpoe_Tampa_2 = "Tampa 2",
             adj_cpoe_Cover_3 = "Cover 3",
             adj_cpoe_Cover_4 = "Cover 4",
             adj_cpoe_Cover_6 = "Cover 6",
             adj_cpoe_Combination = "Combo.",
             adj_cpoe_Prevent = "Prevent")%>%
  fmt_percent(
    columns = vars(2:11),
    decimals = 1
  ) %>% 
  fmt_missing(columns = 2:11,
              missing_text = "-") %>% 
  data_color(
    columns = 2:11,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlBu",
        n = 11,
        direction = 1
      ) %>% as.character(),
      domain = c(-.7, .5), 
      na.color = "white"
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = 2:11
  ) %>% 
  opt_row_striping() %>% 
  tab_options(
    table.font.names = "Consolas", 
    table.background.color = "white",
    table.font.size = 15,
    heading.title.font.size = 20,
    heading.subtitle.font.size = 15,
    column_labels.font.size = 13,
    column_labels.font.weight = 'bold',
    data_row.padding = px(10)
  ) %>% 
  gtsave("CPOE_pairs_trios.png")

# Calculations for notes ------------------------------------------------------------------------
skillpositionplyrs %>% 
  filter(OFP == "B", Route != "NULL") %>% 
  group_by(Route) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))

pass_alls %>% 
  left_join(cpoe_table, by = "passer_player_name") %>%
  # Add passer CPOE to adjust for QB quality
  mutate(cpoe = if_else(Target == 1, cpoe, 0),
         passer_cpoe = if_else(Target == 1, passer_cpoe, 0)) %>%  
  group_by(CoverageScheme, comblist) %>%
  summarize(plays = n(), 
            target_rt = mean(Target),
            cpoe = mean(cpoe, na.rm = TRUE)/100,
            passer_cpoe = mean(passer_cpoe, na.rm = TRUE)/100) %>%
  mutate(cpoe_difference = cpoe - passer_cpoe) %>% 
  ungroup() %>% 
  select(-plays, -target_rt, -cpoe, -passer_cpoe) %>% 
  left_join(eval_all, by = c("CoverageScheme", "comblist")) %>% 
  select(CoverageScheme, comblist, cpoe_difference,
         xepa, play_rt) %>% 
  group_by(CoverageScheme) %>% 
  mutate(cpoe_rk = rank(-cpoe_difference),
         xepa_rk = rank(-xepa),
         play_rk = rank(-play_rt)) %>% 
  mutate(total_rk = rank(cpoe_rk + xepa_rk)) %>% 
  mutate(eff_score = play_rk - total_rk) %>% 
  print(n = 75)

pass_plays %>% 
  mutate(is_redzone = if_else(SideOfField == "Oppo" & StartYard <= 20, TRUE, FALSE),
         is_inside10 = if_else(SideOfField == "Oppo" & StartYard <= 10, TRUE, FALSE)) %>% 
  group_by(CoverageScheme) %>% 
  summarise(Other = sum(is_redzone == 0),
            Red_zone = sum(is_redzone == 1),
            inside_10 = sum(is_inside10 == 1)) %>% 
  ungroup() %>% 
  mutate(Other = Other/sum(Other),
         Red_Zone = Red_zone/sum(Red_zone),
         Inside_10 = inside_10/sum(inside_10)) %>% 
  rename(Coverage = CoverageScheme) %>%  
  mutate(rz_diff = Red_Zone - Other,
         ten_diff = Inside_10 - Other) %>% 
  select(Coverage, Other, Red_Zone, rz_diff, Inside_10, ten_diff) %>% 
  gt()  %>% 
  tab_header(
    title = md("**Coverage by Area of Field**"), 
    subtitle = md("2020 Season")
  )%>% 
  cols_label(Coverage = "Coverage", 
             Other = "Non-Red Zone",
             Red_Zone = "Rate",
             rz_diff = html("&Delta;"), 
             Inside_10 = "Rate", 
             ten_diff =  html("&Delta;")) %>% 
  tab_spanner(
    label =  md("Red Zone"),
    columns = vars(Red_Zone, 
                   rz_diff)
  ) %>% 
  tab_spanner(
    label =  md("Inside 10"),
    columns = vars(Inside_10,
                   ten_diff)
  ) %>%
  fmt_percent(
    columns = 2:6,
    decimals = 1
  ) %>% 
  data_color(
    columns = vars(rz_diff, ten_diff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlBu",
        n = 11,
        direction = 1
      ) %>% as.character(),
      domain = c(-.25, .25), 
      na.color = "white"
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = Other
  ) %>% 
  opt_row_striping() %>% 
  tab_options(
    table.font.names = "Consolas", 
    table.background.color = "white",
    table.font.size = 15,
    heading.title.font.size = 20,
    heading.subtitle.font.size = 15,
    column_labels.font.size = 13,
    column_labels.font.weight = 'bold',
    data_row.padding = px(10)
  ) %>% 
  gtsave("coverages.png")
