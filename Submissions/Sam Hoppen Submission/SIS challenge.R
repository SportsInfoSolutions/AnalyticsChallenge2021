# load libraries
library(tidyverse)
library(dplyr)
library(caret)
library(corrplot)
library(fastDummies)
library(psych)
library(MASS)
library(glmnet)
library(ggplot2)
library(ggrepel)
library(gt)
library(RColorBrewer)
library(ca)

# load plot image themes
source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')

# load pbp data and player positions data
pbp <- read_csv(url("https://raw.githubusercontent.com/samhoppen/AnalyticsChallenge2021/main/Data/PlayByPlay.csv"))
player_positions <- read_csv(url("https://raw.githubusercontent.com/samhoppen/AnalyticsChallenge2021/main/Data/SkillPositionPlayers.csv"))

# load colors for player routes chart
teams_colors_logos <- nflfastR::teams_colors_logos %>% 
  filter(team_abbr != "OAK",
         team_abbr != "STL",
         team_abbr != "SD",
         team_abbr != "LA")


#### CREATE NEW ROUTES ####
# convert routes into new route tree definition of route
new_routes <- player_positions %>% 
  mutate(route_tree = case_when(Route == "Curl" ~ "Curl",
                                Route == "Out" ~ "Out",
                                Route == "Slant" ~ "Slant",
                                Route == "Dig" ~ "Dig",
                                Route == "Drag" ~ "Drag",
                                Route == "Deep Cross" ~ "Post",
                                Route == "Flat - Right" ~ "Flat",
                                Route == "Flat - Left" ~ "Flat",
                                Route == "Screen - RB" ~ "Screen",
                                Route == "Go/Fly" ~ "Go",
                                Route == "Post" ~ "Post",
                                Route == "Corner" ~ "Corner",
                                Route == "Fade" ~ "Go",
                                Route == "Screen - Tunnel" ~ "Screen",
                                Route == "Chip - Flat" ~ "Flat",
                                Route == "Screen - Bubble" ~ "Screen",
                                Route == "Seam" ~ "Go",
                                Route == "Beneath" ~ "Drag",
                                Route == "Whip" ~ "Dig",
                                Route == "Swing - Left" ~ "Swing",
                                Route == "Over Ball" ~ "Other",
                                Route == "Check & Release" ~ "Swing",
                                Route == "Swing - Right" ~ "Swing",
                                Route == "Comeback" ~ "Comeback",
                                Route == "Wheel" ~ "Swing",
                                Route == "Fade - Back Shoulder" ~ "Go",
                                Route == "Angle" ~ "Swing",
                                Route == "Screen - Quick" ~ "Screen",
                                Route == "Chip - Curl" ~ "Curl",
                                Route == "Quick" ~ "Screen",
                                Route == "Screen - TE" ~ "Screen",
                                Route == "Jet Sweep Pass" ~ "Drag",
                                Route == "Hitch & Go" ~ "Go",
                                Route == "Out & Up" ~ "Go",
                                Route == "Chip - Drag" ~ "Drag",
                                Route == "Post Corner" ~ "Corner",
                                Route == "Corner Post" ~ "Post",
                                Route == "Sluggo" ~ "Go",
                                Route == "Jerk" ~ "Dig",
                                Route == "Chip - Seam" ~ "Go",
                                Route == "Screen - Drag" ~ "Screen",
                                Route == "Screen - Beneath" ~ "Screen",
                                Route == "Screen - Shovel" ~ "Screen",
                                Route == "Stick - Nod" ~ "Curl",
                                Route == "Leak" ~ "Go",
                                Route == "Chip" ~ "Other",
                                Route == "Pick" ~ "Other",
                                Route == "Blocking" ~ "No Intended Route",
                                Route == "Run Fake" ~ "No Intended Route",
                                Route == "NULL" ~ "No Intended Route",
                                TRUE ~ "Other"))

# create df with new route combination field and converting variables to factors
routes_data <- new_routes  %>% 
  filter(Route != "NULL") %>%                                                           # remove NULL routes
  group_by(GameID, EventID, route_tree) %>%
  summarize(count = n()) %>%                                                            # count the number of various routes run on a play
  pivot_wider(names_from = route_tree, values_from = count, values_fill = 0) %>%        # create individual columns for each route type
  mutate(route_combo = paste0(Comeback, Corner, Curl,                                   # create new route combination identifier
                              Dig, Drag, Flat,
                              Go, Other,
                              Out, Post, Screen,
                              Slant, Swing)) %>% 
  left_join(pbp[,-c(3:10, 39)]) %>%                                                     # join pbp data, removing Season, Week, Offensive/Defensive Team, Score, Quarter, TimeLeft, and PlayDesc  
  filter((EventType != "rush" | EventType != "challenged rush"), Spike != 1) %>%        # filter only on plays that are a pass
  mutate(StartYard100 = if_else(SideOfField == "Own",                                   # create new field that calculates distance from end zone
                                100-StartYard,
                                StartYard),
         success = as.factor(if_else(EPA > 0, 1, 0)),                                   # create success variable; 1 for positive-EPA plays and 0 for other plays
         # convert binary variables into factor for model creation
         Down = as.factor(Down),
         Hash = as.factor(Hash),
         FirstDown = as.factor(FirstDown),
         Touchdown = as.factor(Touchdown),
         Safety = as.factor(Safety),
         Turnover = as.factor(Turnover),
         Attempt = as.factor(if_else(Attempt == "NULL", "0", Attempt)),
         Completion = as.factor(if_else(Completion == "NULL", "0", Completion)),
         Spike = as.factor(Spike),
         ThrowAway = as.factor(if_else(ThrowAway == "NULL", "0", ThrowAway)),
         ThrowDepth = as.numeric(ThrowDepth),
         Shotgun = as.factor(Shotgun),
         RPO = as.factor(RPO),
         PressureOnPlay = as.factor(if_else(PressureOnPlay == "NULL", "0", PressureOnPlay)),
         SackOnPlay = as.factor(if_else(SackOnPlay == "NULL", "0", SackOnPlay)),
         PassBreakupOnPlay = as.factor(if_else(PassBreakupOnPlay == "NULL", "0", PassBreakupOnPlay)),
         InterceptionOnPlay = as.factor(if_else(InterceptionOnPlay == "NULL", "0", InterceptionOnPlay))) %>% 
  # remove any observations with NAs in them
  na.omit()

# saving all routes as a separate df and removing individual route columns from primary df
routes_info <- routes_data[, c(3:16)]
routes_data <- routes_data[, -c(3:16)]

#### CORRESPONDENCE ANALYSIS AND ROUTE COUNTS ####

# run correspondence analysis
routes_ca <- ca(routes_info)

# save routes CA data as dataframe
routes_ca_data <- as.data.frame(routes_ca$colcoord[,c(1:2)]) %>% 
  rownames_to_column(var = "route_type")

# create plot of CA to identify which routes are run most often with other routes
ca_chart <- ggplot(data = routes_ca_data) +
  geom_segment(aes(x = 0, xend = Dim1,
                   y = 0, yend = Dim2), arrow = arrow(length=unit(0.5,"cm")), alpha = 0.6, color = "dodgerblue4", size = 1) +
  geom_text_repel(aes(x = Dim1, y = Dim2, label = route_type), color = "red3", size = 5) +
  theme_FE +
  labs(title = "Using correspondence analysis to find commonalities in routes used together",
       subtitle = "Similarity of routes is determined by angle between arrow of two route types",
       caption = "Figure: @SamHoppen | Data: Sports Info Solutions",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme(legend.position = "none")


brand_nfl_plot(ca_chart,
               asp = 16/9,
               logo = F,
               save_name = paste0("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts/Routes CA chart.png"))

# calculate how many times different routes were run
route_counts <- new_routes %>% 
  filter(Route != "NULL") %>% 
  group_by(route_tree) %>% 
  summarize(count = n()) %>% 
  filter(route_tree != "No Intended Route")

# create plot of route counts
route_count_chart <- ggplot(data = route_counts) +
  geom_col(aes(x = reorder(route_tree, -count), y = count), color = "red3", fill = "dodgerblue4") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=6),
                     limits = c(0, 14000),
                     labels = scales::comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_FE +
  labs(title = "Total counts of each type of route run in 2020",
       subtitle = "Pass plays only",
       caption = "Figure: @SamHoppen | Data: Sports Info Solutions",
       x = NULL,
       y = "Total Routes Run") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank())


brand_nfl_plot(route_count_chart,
               asp = 16/9,
               logo = F,
               save_name = paste0("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts/Routes counts chart.png"))

# create list of route combination names
route_combo_names <- new_routes  %>% 
  filter(RosterPosition != "QB") %>%                                                           # remove NULL routes
  arrange(GameID, EventID, route_tree) %>% 
  group_by(GameID, EventID) %>% 
  mutate(route_num = paste0("route_",row_number())) %>% 
  dplyr::select(GameID, EventID, route_num, route_tree) %>% 
  pivot_wider(names_from = route_num, values_from = route_tree, values_fill = "No Intended Route") %>% 
  ungroup() %>% 
  mutate(route_combo_name = paste0(route_1,", ", route_2,", ", route_3,", ", route_4,", ", route_5)) %>% 
  dplyr::select(GameID, EventID, route_combo_name) %>% 
  distinct() %>% 
  arrange(GameID, EventID)

# count how many times a route combination is used
route_combo_counts <- route_combo_names  %>% 
  group_by(route_combo_name) %>% 
  summarize(count = n()) %>% 
  arrange(-count) %>% 
  mutate(count_rank = row_number(),
         route_label = gsub(" ", "\n", route_combo_name)) %>% 
  filter(count_rank <= 21)
  
# create plot of route combo counts
route_count_chart <- ggplot(data = route_combo_counts[c(2:21),]) +
  geom_col(aes(x = reorder(route_label, -count), y = count), color = "red3", fill = "dodgerblue4") +
  #geom_text(aes(x = reorder(route_combo_name, -count), y = count*1.2, label = route_label), size = 3, color = "black")+
  scale_y_continuous(breaks = scales::pretty_breaks(n=6),
                     #limits = c(0, 425),
                     labels = scales::comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_FE +
  labs(title = "Total counts of top 20 route combinations in 2020",
       subtitle = "Pass plays only",
       caption = "Figure: @SamHoppen | Data: Sports Info Solutions",
       x = NULL,
       y = "Total Routes Run") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank())


brand_nfl_plot(route_count_chart,
               asp = 16/9,
               logo = F,
               save_name = paste0("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts/Routes combo counts chart.png"))


#### DATA CLEAN UP ####

# create new dummy variables for the different drop types
routes_fin <- routes_data %>% 
  dummy_cols(select_columns = "DropType") %>% 
  dplyr::select(-c(SideOfField, EventType, FumbleByPasser, FumbleByRusher,
                   FumbleByReceiver, EPA, DropType, StartYard, Spike)) %>% 
  # move the success column to the beginning of the df
  relocate(success, .before = GameID)

# save the route combos, coverages, GameID, and EventID as individual columns
# will be removed in next step prior to model-building
route_combos <- as.data.frame(routes_fin[,4])
#route_successes <- routes_fin[,1]
route_coverages <- routes_fin[,18]
game_ids <- routes_fin[,2]
event_ids <- routes_fin[,3]

# remove the aforementioned columns
routes_fin <- routes_fin[,-c(2:4, 18)]

# create new df to use for calculating 
corr_stats <- routes_fin %>%
  # rename all the drop columns so that they fit in corrplot better
  dplyr::rename(`0/1 Step` = `DropType_0/1 Step`,
                `3 Step` = `DropType_3 Step`,
                `5 Step` = `DropType_5 Step`,
                `7 Step` = `DropType_7 Step`,
                `Basic Screen` = `DropType_Basic Screen`,
                `Designed Rollout Left` = `DropType_Designed Rollout Left`,
                `Designed Rollout Right` = `DropType_Designed Rollout Right`,
                `Double Reverse Pass` = `DropType_Double Reverse Pass`,
                `Flea Flicker` = `DropType_Flea Flicker`,
                `Other Drop` = DropType_Other,
                `RB/WR Pass` = `DropType_RB/WR Pass`,
                `RPO Drop` = DropType_RPO,
                `RPO Move` = `DropType_RPO Move`,
                `WR Reverse Pass` = `DropType_WR Reverse Pass`)

# convert factors to numeric for correlation 
corr_stats[,c(1:34)] <- sapply(corr_stats[,c(1:34)], as.numeric)
# calculate correlation coefficients
corr_vals <- cor(corr_stats)

# create a corrplot of the variables
corrplot(corr_vals)

# drop type variables yield no correlation to anything, so they will be removed from dataset moving forward
routes_fin2 <- routes_fin[,c(1:20)]


#### MODEL BUILDING ####
# set initial seed
set.seed(123)
# create initial logistic regression model to see its results
# predicting success in order to determine "expected success"
success_model <- glm(success ~ ., data = routes_fin2, family = "binomial")
# evaluate results
summary(success_model)
# several insignificant variables exist based on p-value
# need to try some variable selection methods to find more parsimonious model


# create empty and full versions of model
success_model_empty <- glm(success ~ 1, data = routes_fin2, family = "binomial")
success_model_full <- glm(success ~ ., data = routes_fin2, family = "binomial")

# evaluate model with backward selection
step_backward <- stepAIC(success_model_full, direction="backward")
# backward selection model results: success ~ Down + ToGo + FirstDown + Touchdown + Turnover + Attempt + Completion + OffensiveYardage + StartYard100

# evaluate model with forward selection
step_forward <- stepAIC(success_model_empty,direction="forward", scope=list(upper=success_model_full,lower=success_model_empty))
# forward selection results: success ~ OffensiveYardage + FirstDown + Down + Turnover + ToGo + Touchdown + Completion + Attempt + StartYard100
# both forward and backward selection result in the same model

# build new model with stepwise selection results
success_model_stepwise <- glm(success ~ OffensiveYardage + FirstDown + Down + Turnover + ToGo + 
                                Touchdown + Completion + Attempt + StartYard100, 
                              data = routes_fin2, family = "binomial")

# evaluate the results of the model
summary(success_model_stepwise)
# both Turnover and Touchdown have non-significant p-values with wildly different beta values
# will use lasso regularized regression technique to shrink betas

# converting the x and y variables 
x_val <- data.matrix(routes_fin2[, -1])
y_val <- data.matrix(routes_fin2[, 1])

# use cross validation to find lambda value for lasso
success_model_lasso <- cv.glmnet(x_val, y_val, alpha = 1, family = "binomial")

success_model_lasso$lambda.min
# lambda min = 0.0003841255
success_model_lasso$lambda.1se
# lambda 1se = 0.001287436
log(success_model_lasso$lambda.min)

# final expected success model
success_model_lasso_fin <- glmnet(x_val, y_val, alpha = 1, family = "binomial", lambda = 0.001287436) ## NEED TO CHANGE THIS
# new beta values of model
success_model_lasso_fin$beta
# slightly different model than stepwise selection
# doesn't include attempt and also includes throwdepth, RPO, and shotgun

# calcualte expected success
predict <- predict(success_model_lasso_fin, x_val, s="lambda.1se", type = "response")

# add expected success, route combos, and coverages back to dataset
routes_fin2$xp_success <- round(predict, 3)
# combine all saved dataframse into one
routes_fin2 <- cbind(routes_fin2,
                     route_combos,
                     route_coverages,
                     game_ids,
                     event_ids)

#### EVALUATION ####

routes_fin2 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/AnalyticsChallenge2021/main/Data/SIS%20Routes%20Data.csv"))
new_routes <- read_csv(url("https://raw.githubusercontent.com/samhoppen/AnalyticsChallenge2021/main/Data/SIS%20Player%20Routes.csv"))

# create dataframe for targeted route and join to routes dataframe
targeted_routes <- new_routes %>% 
  filter(Target == 1) %>% 
  dplyr::select(GameID, EventID, Name, route_tree) %>% 
  rename(targeted_route = route_tree,
         targeted_player = Name)

# pull only team names
teams <- pbp %>% 
  dplyr::select(GameID, EventID, OffensiveTeam) %>% 
  distinct()

# create list of route combination names
route_combo_names <- new_routes  %>% 
  filter(RosterPosition != "QB") %>%                                                           # remove NULL routes
  arrange(GameID, EventID, route_tree) %>% 
  group_by(GameID, EventID) %>% 
  mutate(route_num = paste0("route_",row_number())) %>% 
  dplyr::select(GameID, EventID, route_num, route_tree) %>% 
  pivot_wider(names_from = route_num, values_from = route_tree, values_fill = "No Intended Route") %>% 
  ungroup() %>% 
  mutate(route_combo_name = paste0(route_1,", ", route_2,", ", route_3,", ", route_4,", ", route_5)) %>% 
  dplyr::select(GameID, EventID, route_combo_name) %>% 
  distinct() %>% 
  arrange(GameID, EventID)

# join newly-created dataframes with existing data frame
routes_fin2 <- routes_fin2 %>% 
  left_join(targeted_routes) %>% 
  left_join(teams) %>% 
  left_join(route_combo_names)

# calculate route combos with highest success rate over expected
route_combo_success <- routes_fin2 %>% 
  group_by(route_combo_name) %>% 
  summarize(plays = n(),
            success_rate = mean(success),
            xp_sr = mean(xp_success)) %>% 
  ungroup() %>% 
  mutate(sroe = success_rate - xp_sr) %>% 
  filter(plays >= 25) %>% 
  arrange(-sroe) %>% 
  mutate(sroe_rank = row_number())

# create chart for route combination success rates
route_chart1 <- route_combo_success %>% 
  filter(sroe_rank <= 25) %>% 
  gt() %>% 
  tab_header(title = html("Top 25 targeted route combinations in<br>Success Rate Over Expected (SROE)"),
             subtitle = "Minimum 25 plays used") %>% 
  cols_label(route_combo_name = "Route Combo",
             plays = "Plays",
             success_rate = "Success Rate",
             sroe = "SROE") %>% 
  cols_width(vars(route_combo_name) ~ px(225),
             vars(success_rate) ~ px(125),
             vars(sroe) ~ px(125)) %>% 
  cols_align(align = "center",
             columns = vars(route_combo_name, plays, success_rate, sroe))%>% 
  data_color(
    columns = vars(success_rate, sroe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::RdBu"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  cols_hide(columns = vars(xp_sr, sroe_rank)) %>% 
  fmt_percent(columns = vars(success_rate, sroe),
              decimals = 1) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Sports Info Solutions"
  )

gt::gtsave(route_chart1, file = file.path("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts", "Route combo success rates.png"))


# calculate route combos with highest success rate over expected by different coverages
route_combo_success_cov <- routes_fin2 %>% 
  group_by(route_combo_name, CoverageScheme) %>% 
  summarize(plays = n(),
            success_rate = mean(success),
            xp_sr = mean(xp_success)) %>% 
  ungroup() %>% 
  mutate(sroe = success_rate - xp_sr) %>% 
  filter(plays >= 10) %>% 
  arrange(-sroe) %>% 
  mutate(sroe_rank = row_number())

# create chart for route combination success rates with coverage schemes included
route_chart2 <- route_combo_success_cov %>% 
  filter(sroe_rank <= 25) %>%
  gt() %>% 
  tab_header(title = html("Top 25 route combos in Success Rate Over<br>Expected (SROE) versus various coverage schemes"),
             subtitle = "Minimum 10 plays used") %>% 
  cols_label(route_combo_name = "Route Combo",
             CoverageScheme = "Coverage Scheme",
             plays = "Plays",
             success_rate = "Success Rate",
             sroe = "SROE") %>% 
  cols_width(vars(route_combo_name) ~ px(225),
             vars(CoverageScheme) ~ px(150),
             vars(success_rate) ~ px(125),
             vars(sroe) ~ px(125)) %>% 
  cols_align(align = "center",
             columns = vars(CoverageScheme, route_combo_name, plays, success_rate, sroe))%>% 
  data_color(
    columns = vars(success_rate, sroe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::RdBu"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  cols_hide(columns = vars(xp_sr, sroe_rank)) %>% 
  fmt_percent(columns = vars(success_rate, sroe),
              decimals = 1) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Sports Info Solutions"
  )

gt::gtsave(route_chart2, file = file.path("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts", "Route combo success rates vs cov.png"))

# evaluate which route combos are best against each coverage type
route_combo_success_cov2 <- routes_fin2 %>% 
  group_by(route_combo_name, CoverageScheme) %>% 
  summarize(plays = n(),
            success_rate = mean(success),
            xp_sr = mean(xp_success)) %>% 
  ungroup() %>% 
  mutate(sroe = success_rate - xp_sr) %>% 
  filter(plays >= 5) %>% 
  arrange(-sroe) %>% 
  mutate(sroe_rank = row_number())

best_route_combos <- route_combo_success_cov2 %>% 
  group_by(CoverageScheme) %>% 
  filter(sroe == max(sroe))

route_chart3 <- best_route_combos %>% 
  ungroup() %>%
  gt() %>% 
  tab_header(title = html("Top route combos against each coverage type"),
             subtitle = "Minimum 5 plays used") %>% 
  cols_label(route_combo_name = "Route Combo",
             CoverageScheme = "Coverage Scheme",
             plays = "Plays",
             success_rate = "Success Rate",
             sroe = "SROE") %>% 
  cols_width(vars(route_combo_name) ~ px(225),
             vars(CoverageScheme) ~ px(150),
             vars(success_rate) ~ px(125),
             vars(sroe) ~ px(125)) %>% 
  cols_align(align = "center",
             columns = vars(CoverageScheme, route_combo_name, plays, success_rate, sroe))%>% 
  data_color(
    columns = vars(success_rate, sroe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::RdBu"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  cols_hide(columns = vars(xp_sr, sroe_rank)) %>% 
  fmt_percent(columns = vars(success_rate, sroe),
              decimals = 1) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Sports Info Solutions"
  )

gt::gtsave(route_chart3, file = file.path("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts", "Best route combos vs cov.png"))


# evaluate which specific targeted routes have the highest SROE
route_success <- routes_fin2 %>% 
  group_by(targeted_route) %>% 
  summarize(plays = n(),
            success_rate = mean(success),
            xp_sr = mean(xp_success)) %>% 
  ungroup() %>% 
  mutate(sroe = success_rate - xp_sr) %>% 
  filter(plays >= 500,
         !is.na(targeted_route)) %>% 
  arrange(-sroe)

# create chart for individual route success rates when targeted
route_chart4 <- route_success %>% 
  gt() %>% 
  tab_header(title = html("Top targeted routes in<br>Success Rate Over Expected (SROE)")) %>% 
  cols_label(targeted_route = "Route",
             plays = "Plays Targeted",
             success_rate = "Success Rate",
             sroe = "SROE") %>% 
  cols_align(align = "center",
             columns = vars(targeted_route, plays, success_rate, sroe))%>% 
  data_color(
    columns = vars(success_rate, sroe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::RdBu"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  cols_hide(columns = vars(xp_sr)) %>% 
  fmt_percent(columns = vars(success_rate, sroe),
              decimals = 1) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Sports Info Solutions"
  )

gt::gtsave(route_chart4, file = file.path("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts", "Individual route success rates.png"))

# calculate route combos with highest success rate over expected by different coverages
route_success_cov <- routes_fin2 %>% 
  group_by(targeted_route, CoverageScheme) %>% 
  summarize(plays = n(),
            #success_rate = mean(as.numeric(success)-1),
            success_rate = mean(success),
            xp_sr = mean(xp_success)) %>% 
  ungroup() %>% 
  mutate(sroe = success_rate - xp_sr) %>% 
  filter(plays >= 50,
         !is.na(targeted_route), 
         targeted_route != "Other") %>% 
  arrange(-sroe) %>% 
  mutate(sroe_rank = row_number())

# create chart for individual route success rates when targeted, including coverage schemes
route_chart5 <- route_success_cov %>% 
  filter(sroe_rank <= 25) %>%
  gt() %>% 
  tab_header(title = html("Top 25 targeted routes in Success Rate Over<br>Expected (SROE) versus various coverage schemes"),
             subtitle = "Minimum 50 targeted plays") %>% 
  cols_label(targeted_route = "Route",
             CoverageScheme = "Coverage Scheme",
             plays = "Plays Targeted",
             success_rate = "Success Rate",
             sroe = "SROE") %>% 
  cols_align(align = "center",
             columns = vars(CoverageScheme, targeted_route, plays, success_rate, sroe))%>% 
  data_color(
    columns = vars(success_rate, sroe),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::RdBu"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  cols_hide(columns = vars(xp_sr, sroe_rank)) %>% 
  fmt_percent(columns = vars(success_rate, sroe),
              decimals = 1) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Sports Info Solutions"
  )

gt::gtsave(route_chart5, file = file.path("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts", "Individual route success rates vs cov.png"))


# evaluate which player routes have the highest SR
player_route_success <- routes_fin2 %>% 
  group_by(targeted_player,
           targeted_route,
           OffensiveTeam) %>% 
  summarize(targets = n(),
            success_rate = mean(success),
            xp_sr = mean(xp_success)) %>% 
  ungroup() %>% 
  mutate(sroe = success_rate - xp_sr,
         player_label = paste0(targeted_player, " (",targeted_route,")")) %>% 
  filter(targets >= 20,
         success_rate >= 0.5,
         !is.na(targeted_player)) %>% 
  arrange(-sroe) %>% 
  left_join(teams_colors_logos,
            by = c("OffensiveTeam" = "team_nick"))

# create plot of players' routes success rates
wr_chart <- ggplot(data = player_route_success) +
  geom_point(aes(x = success_rate, y = xp_sr, size = targets, color = I(team_color)), alpha = 0.6) + 
  geom_vline(xintercept = median(player_route_success$success_rate), linetype = "dashed") +
  geom_hline(yintercept = median(player_route_success$xp_sr), linetype = "dashed") +
  geom_text_repel(aes(x = success_rate, y = xp_sr, label = player_label), size = 4.5) + 
  scale_size_continuous(range = c(0, 10)) +#, breaks =  c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels = c("0%","10%", "20%", "30%", "40%", "50%")) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0.45, 0.85)) +
  scale_x_continuous(labels = scales::percent_format(accuracy=1),
                     limits = c(0.45, 0.85)) +
  theme_FE +
  labs(title = "Comparing wide receivers' success rate to expected success rate based on route type",
       subtitle = "Minimum 20 targets and 50% success rate for a route in 2020 | Size of bubble represents number of targets",
       caption = "Figure: @SamHoppen | Data: Sports Info Solutions",
       x = "Success Rate",
       y = "Expected Success Rate")  +
  theme(legend.position = "none")

brand_nfl_plot(wr_chart,
               asp = 16/9,
               logo = F,
               save_name = paste0("C:/Users/Hoppy/OneDrive/NFL Analysis/Charts/WR route success.png"))

