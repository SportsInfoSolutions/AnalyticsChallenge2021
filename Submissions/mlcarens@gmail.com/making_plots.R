### read in pbp csv
sis_pbp <- read.csv("PlayByPlay.csv")

library(nflfastR)
library(tidyverse)

pbp <- nflfastR::load_pbp(2020)

### filter for regular season and non-special teams
pbp <- pbp %>% filter(season_type=="REG") %>% filter(play_type == "pass" | play_type == "run")

#test_sis_pbp <- sis_pbp %>% filter(OffensiveTeam=="Texans" & DefensiveTeam=="Chiefs")
#test_pbp <- pbp %>% filter(posteam == "HOU" & defteam == "KC")

### change offensive and defensive teams to abbreviations in SIS data
abbrev <- read.csv("team_abbrev_lookup.csv")

sis_pbp <- left_join(sis_pbp, abbrev, by = c("OffensiveTeam" = "Team.Name"))
names(sis_pbp)[names(sis_pbp) == "Abbreviation"] <- "posteam"

#sis_pbp <- sis_pbp %>% rename("posteam" = Abbreviation)

sis_pbp <- left_join(sis_pbp, abbrev, by = c("DefensiveTeam" = "Team.Name"))
names(sis_pbp)[names(sis_pbp) == "Abbreviation"] <- "defteam"

#sis_pbp <- sis_pbp %>% rename("defteam" = Abbreviation)

### merge columns to make new id to join on
pbp$id_new <- paste(pbp$week,pbp$posteam, pbp$defteam, pbp$qtr, pbp$quarter_seconds_remaining, sep = "_")
sis_pbp$id_new <- paste(sis_pbp$Week, sis_pbp$posteam, sis_pbp$defteam, sis_pbp$Quarter, sis_pbp$TimeLeft, sep = "_")

### select what we we want from nflfastR
pbp_need <- pbp %>% select(id_new, wpa, yards_after_catch, desc)

### make final df with unqiue id
final_df <- left_join(sis_pbp, pbp_need, by = "id_new")
final_df$sis_id <- paste(final_df$GameID, final_df$EventID, sep = "_")

### add Venit's dataset and make unique id
venit <- read.csv("Updated Routes.csv")
venit$sis_id <- paste(venit$GameID, venit$EventID, sep = "_")
venit <- venit[,c(3:43, 46)]

### join final_df with venit
merged <- left_join(final_df, venit, by ="sis_id")

### filter for pass plays
merged <- merged %>% filter(EventType %in% c("pass", "challenge pass"))

merged2 <- merged %>% mutate(formation_fld_bdry = ifelse(Formation == "2 x 2" & Hash == 1, "Double Twins Field Right", 
                                                        ifelse(Formation == "2 x 2" & Hash == 3, "Double Twins Field Left",
                                                               ifelse(Formation == "2 x 2" & Hash == 2, "Double Twins MOF", 
                                                                      ifelse(Formation == "2 x 1" & Hash == 1 | Formation == "1 x 2" & Hash == 3, "Slot to Boundary",
                                                                             ifelse(Formation == "2 x 1" & Hash == 3 | Formation == "1 x 2" & Hash == 1, "Slot to Field",
                                                                                    ifelse(Formation == "2 x 1" & Hash == 2 | Formation == "1 x 2" & Hash == 2, "Slot MOF",
                                                                                           ifelse(Formation == "1 x 3" & Hash == 1 | Formation == "3 x 1" & Hash == 3, "Trips to Field",
                                                                                                  ifelse(Formation == "1 x 3" & Hash == 3 | Formation == "3 x 1" & Hash == 1, "Trips to Boundary",
                                                                                                         ifelse(Formation == "1 x 3" & Hash == 2 | Formation == "3 x 1" & Hash == 2, "Trips MOF",
                                                                                                                ifelse(Formation == "2 x 3" & Hash == 1 | Formation == "3 x 2" & Hash == 3, "Empty to Field",
                                                                                                                       ifelse(Formation == "2 x 3" & Hash == 3 | Formation == "3 x 2" & Hash == 1, "Empty to Boundary",
                                                                                                                              ifelse(Formation == "2 x 3" & Hash == 2 | Formation == "3 x 2" & Hash == 2, "Empty MOF",
                                                                                                                                     ifelse(Formation == "4 x 1" & Hash == 1 | Formation == "1 x 4" & Hash == 3, "Quads to Boundary",
                                                                                                                                            ifelse(Formation == "4 x 1" & Hash == 3 | Formation == "1 x 4" & Hash == 1, "Quads to Field",
                                                                                                                                                   ifelse(Formation == "4 x 1" & Hash == 2 | Formation == "1 x 4" & Hash == 2, "Quads MOF",
                                                                                                                                                          ifelse(Formation == "1 x 1" & Hash == 1, "Pro Field Right",
                                                                                                                                                                 ifelse(Formation == "1 x 1" & Hash == 3, "Pro Field Left",
                                                                                                                                                                        ifelse(Formation == "1 x 1" & Hash == 2, "Pro MOF",
                                                                                                                                                                               ifelse(Formation == "2 x 0" & Hash==1 | Formation=="0 x 2" & Hash == 3, "Twins to Boundary",
                                                                                                                                                                                      ifelse(Formation == "2 x 0" & Hash==3 | Formation=="0 x 2" & Hash == 1, "Twins to Boundary",
                                                                                                                                                                                             ifelse(Formation == "2 x 0" & Hash==2 | Formation=="0 x 2" & Hash == 2, "Twins MOF",
                                                                                                                                                                                                    ifelse(Formation == "3 x 0" & Hash==1 | Formation=="0 x 3" & Hash == 3, "Trio to Boundary",
                                                                                                                                                                                                           ifelse(Formation == "3 x 0" & Hash==3 | Formation=="0 x 3" & Hash == 1, "Trio to Field",
                                                                                                                                                                                                                  ifelse(Formation == "3 x 0" & Hash==2 | Formation=="0 x 3" & Hash == 2, "Trio MOF",
                                                                                                                                                                                                    0)))))))))))))))))))))))),
                             field_bdry_side = ifelse(Hash==1, "Field Right",
                                                      ifelse(Hash==3, "Field Left",
                                                             "MOF")),
                             target_side = ifelse(R1_Target == 1 | R2_Target == 1 | R3_Target == 1 | R4_Target == 1, "Right",
                                                        ifelse(L1_Target == 1 | L2_Target == 1 | L3_Target == 1 | L4_Target == 1, "Left",
                                                               "-")),
                             passing_strength = ifelse(field_bdry_side=="MOF" & (Formation == "1 x 3" | Formation == "2 x 2" | Formation =="2 x 3" | Formation == "1 x 2"), "Right",
                                                       ifelse(field_bdry_side == "MOF" & (Formation == "3 x 1" | Formation == "3 x 2" | Formation == "2 x 1"), "Left",
                                                              ifelse(field_bdry_side == "Field Right", "Right",
                                                                     ifelse(field_bdry_side == "Field Left", "Left",
                                                                            "-")))),
                             field_strong_1 = ifelse(passing_strength=="Right", R1_Route,
                                                     ifelse(passing_strength=="Left", L1_Route,
                                                            "")),
                             field_strong_2 = ifelse(passing_strength=="Right", R2_Route,
                                                     ifelse(passing_strength=="Left", L2_Route,
                                                            "")),
                             field_strong_3 = ifelse(passing_strength=="Right", R3_Route,
                                                     ifelse(passing_strength=="Left", L3_Route,
                                                            "")),
                             field_strong_4 = ifelse(passing_strength=="Right", R4_Route,
                                                     ifelse(passing_strength=="Left", L4_Route,
                                                            "")),
                             boundary_weak1 = ifelse(passing_strength=="Right", L1_Route,
                                                     ifelse(passing_strength == "Left", R1_Route,
                                                            "")),
                             boundary_weak2 = ifelse(passing_strength=="Right", L2_Route,
                                                     ifelse(passing_strength == "Left", R2_Route,
                                                            "")),
                             boundary_weak3 = ifelse(passing_strength=="Right", L3_Route,
                                                     ifelse(passing_strength == "Left", R3_Route,
                                                            "")),
                             boundary_weak4 = ifelse(passing_strength=="Right", L4_Route,
                                                     ifelse(passing_strength == "Left", R4_Route,
                                                            ""))
                             
                             )

### remove right and left from routes
merged2$field_strong_1 <- gsub("-Right", "", as.character(merged2$field_strong_1))
merged2$field_strong_2 <- gsub("-Right","", as.character(merged2$field_strong_2))
merged2$field_strong_3 <- gsub("-Right","", as.character(merged2$field_strong_3))
merged2$field_strong_4 <- gsub("-Right","", as.character(merged2$field_strong_4))
merged2$boundary_weak1 <- gsub("-Right", "", as.character(merged2$boundary_weak1))
merged2$boundary_weak2 <- gsub("-Right", "", as.character(merged2$boundary_weak2))
merged2$boundary_weak3 <- gsub("-Right", "", as.character(merged2$boundary_weak3))
merged2$boundary_weak4 <- gsub("-Right", "", as.character(merged2$boundary_weak4))

merged2$field_strong_1 <- gsub("-Left", "", as.character(merged2$field_strong_1))
merged2$field_strong_2 <- gsub("-Left","", as.character(merged2$field_strong_2))
merged2$field_strong_3 <- gsub("-Left","", as.character(merged2$field_strong_3))
merged2$field_strong_4 <- gsub("-Left","", as.character(merged2$field_strong_4))
merged2$boundary_weak1 <- gsub("-Left", "", as.character(merged2$boundary_weak1))
merged2$boundary_weak2 <- gsub("-Left", "", as.character(merged2$boundary_weak2))
merged2$boundary_weak3 <- gsub("-Left", "", as.character(merged2$boundary_weak3))
merged2$boundary_weak4 <- gsub("-Left", "", as.character(merged2$boundary_weak4))

### make route combo columns
merged2$all_combos <- paste(merged2$field_strong_1, merged2$field_strong_2, merged2$field_strong_3, merged2$field_strong_4, "|",
                    merged2$boundary_weak1, merged2$boundary_weak2, merged2$boundary_weak3, merged2$boundary_weak4, sep = "")

################## TESTING ##########################################################
test <- merged2 %>% select(formation_fld_bdry) %>% 
  group_by(formation_fld_bdry) %>%
  summarize(play_count = length(formation_fld_bdry))

test2 <- merged2 %>% select(all_combos, wpa) %>%
  group_by(all_combos) %>%
  summarize(play_count = length(all_combos),
    avg_wpa = mean(wpa)*100) %>%
  filter(play_count>=5)

######### find whether field or boundary was targeted
merged3 <- merged2 %>% filter(!is.na(wpa)) %>%
  mutate(target_field.or.boundary = ifelse(passing_strength == "Right" & target_side == "Right", "Field/Strong Rt",
                                            ifelse(passing_strength == "Left" & target_side == "Left", "Field/Strong Lt",
                                                  ifelse(passing_strength == "Right" & target_side == "Left", "Boundary/Weak Lt",
                                                         ifelse(passing_strength == "Left" & target_side=="Right", "Boundary/Weak Rt", ""))))
  
)

### find most common route combos on side of field
right_route_combos <- merged3 %>% filter(R2_Route != "" & R2_Route != "NULL" & !is.na(R2_Route)) %>%
  mutate(right_side_combo = paste(R1_Route, R2_Route, R3_Route, sep = " | ")) %>%
  group_by(right_side_combo) %>%
  summarise(count_right = length(right_side_combo))

left_route_combos <- merged3 %>% filter(L2_Route != "" & L2_Route != "NULL" & !is.na(L2_Route)) %>%
  mutate(left_side_combo = paste(L1_Route, L2_Route, L3_Route, sep = " | ")) %>%
  group_by(left_side_combo) %>%
  summarize(count_left = length(left_side_combo))

joined <- left_join(left_route_combos, right_route_combos, by= c("left_side_combo" = "right_side_combo"))

joined$sum <- joined$count_left + joined$count_right
############### MAKE FIELD/STRONG 1-2 DATAFRAME ###################
target_1.2_field.strong <- merged3 %>% filter(field_strong_2 != "") %>%
  filter((target_field.or.boundary == "Field/Strong Rt" & R1_Target == 1) |
                                                (target_field.or.boundary == "Field/Strong Rt" & R2_Target == 1) |
                                                (target_field.or.boundary == "Field/Strong Lt" & L2_Target ==1) |
                                  (target_field.or.boundary == "Field/Strong Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2 = paste(field_strong_1, field_strong_2, sep = " | ")) %>%
  select(combo_1.2, wpa)

target_1.2_field.strong <- target_1.2_field.strong[target_1.2_field.strong$combo_1.2 %in% names(sort(table(target_1.2_field.strong$combo_1.2), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2_field.strong, "combo_1.2", summarise, grp.median=median(wpa))

# Plot
g_1.2_strong <- ggplot(target_1.2_field.strong, aes(x=wpa, fill=combo_1.2)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1 and #2 aligned receivers to the 
       Field/Strong Side when targeted", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2_strong

############### MAKE BOUNDARY/WEAK 1-2 DATAFRAME ###################
target_1.2_boundary.weak <- merged3 %>% filter(boundary_weak2 != "" & boundary_weak1 != "") %>%
  filter((target_field.or.boundary == "Boundary/Weak Rt" & R1_Target == 1) |
           (target_field.or.boundary == "Boundary/Weak Rt" & R2_Target == 1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L2_Target ==1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2 = paste(boundary_weak1, boundary_weak2, sep = " | ")) %>%
  select(combo_1.2, wpa)

target_1.2_boundary.weak <- target_1.2_boundary.weak[target_1.2_boundary.weak$combo_1.2 %in% names(sort(table(target_1.2_boundary.weak$combo_1.2), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2_boundary.weak, "combo_1.2", summarise, grp.median=median(wpa))

# Plot
g_1.2_weak <- ggplot(target_1.2_boundary.weak, aes(x=wpa, fill=combo_1.2)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1 and #2 aligned receivers 
       to the Boundary/Weak Side when targeted", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2_weak

############### MAKE FIELD/STRONG 1-2-3 DATAFRAME ###################
target_1.2.3_field.strong <- merged3 %>% filter(field_strong_3 != "" & field_strong_2 != "") %>%
  filter((target_field.or.boundary == "Field/Strong Rt" & R1_Target == 1) |
           (target_field.or.boundary == "Field/Strong Rt" & R2_Target == 1) |
           (target_field.or.boundary == "Field_Strong Rt" & R3_Target == 1) |
           (target_field.or.boundary == "Field_Strong Lt" & L3_Target == 1) |
           (target_field.or.boundary == "Field/Strong Lt" & L2_Target ==1) |
           (target_field.or.boundary == "Field/Strong Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2.3 = paste(field_strong_1, field_strong_2, field_strong_3, sep = " | ")) %>%
  select(combo_1.2.3, wpa)

target_1.2.3_field.strong <- target_1.2.3_field.strong[target_1.2.3_field.strong$combo_1.2.3 %in% names(sort(table(target_1.2.3_field.strong$combo_1.2.3), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2.3_field.strong, "combo_1.2.3", summarise, grp.median=median(wpa))

# Plot
g_1.2.3_strong <- ggplot(target_1.2.3_field.strong, aes(x=wpa, fill=combo_1.2.3)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2.3),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1, #2, #3 aligned receivers to the 
       Field/Strong Side when targeted", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2.3_strong

############### MAKE BOUNDARY/WEAK 1-2-3 DATAFRAME ###################
target_1.2.3_boundary.weak <- merged3 %>% filter(boundary_weak3 != "" & boundary_weak3 !="NULL") %>%
  filter((target_field.or.boundary == "Boundary/Weak Rt" & R1_Target == 1) |
           (target_field.or.boundary == "Boundary/Weak Rt" & R2_Target == 1) |
           (target_field.or.boundary == "Boundary/Weak Rt" & R3_Target ==1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L3_Target==1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L2_Target ==1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2.3 = paste(boundary_weak1, boundary_weak2, boundary_weak3, sep = " | ")) %>%
  select(combo_1.2.3, wpa)

target_1.2.3_boundary.weak <- target_1.2.3_boundary.weak[target_1.2.3_boundary.weak$combo_1.2 %in% names(sort(table(target_1.2.3_boundary.weak$combo_1.2), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2.3_boundary.weak, "combo_1.2.3", summarise, grp.median=median(wpa))

# Plot
g_1.2.3_weak <- ggplot(target_1.2.3_boundary.weak, aes(x=wpa, fill=combo_1.2.3)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2.3),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1, #2, and #3 aligned receivers 
       to the Boundary/Weak Side when targeted", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2.3_weak

######### FILTERING FOR COVERAGES ###########################################
############### MAKE FIELD/STRONG 1-2 DATAFRAME ###################
target_1.2_field.strong <- merged3 %>% filter(field_strong_2 != "") %>%
  filter((target_field.or.boundary == "Field/Strong Rt" & R1_Target == 1) |
           (target_field.or.boundary == "Field/Strong Rt" & R2_Target == 1) |
           (target_field.or.boundary == "Field/Strong Lt" & L2_Target ==1) |
           (target_field.or.boundary == "Field/Strong Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2 = paste(field_strong_1, field_strong_2, sep = " | ")) %>%
  filter(CoverageScheme == "Man Cover 2") %>%
  select(combo_1.2, wpa) 

target_1.2_field.strong <- target_1.2_field.strong[target_1.2_field.strong$combo_1.2 %in% names(sort(table(target_1.2_field.strong$combo_1.2), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2_field.strong, "combo_1.2", summarise, grp.median=median(wpa))

# Plot
g_1.2_strong <- ggplot(target_1.2_field.strong, aes(x=wpa, fill=combo_1.2)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1 and #2 aligned receivers to the 
       Field/Strong Side when targeted vs. Man Cover 2", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2_strong

############### MAKE BOUNDARY/WEAK 1-2 DATAFRAME ###################
target_1.2_boundary.weak <- merged3 %>% filter(boundary_weak2 != "") %>%
  filter((target_field.or.boundary == "Boundary/Weak Rt" & R1_Target == 1) |
           (target_field.or.boundary == "Boundary/Weak Rt" & R2_Target == 1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L2_Target ==1) |
           (target_field.or.boundary == "Boundary/Weak Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2 = paste(boundary_weak1, boundary_weak2, sep = " | "))  %>%
  filter(CoverageScheme == "Cover 2") %>%
  select(combo_1.2, wpa)

target_1.2_boundary.weak <- target_1.2_boundary.weak[target_1.2_boundary.weak$combo_1.2 %in% names(sort(table(target_1.2_boundary.weak$combo_1.2), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2_boundary.weak, "combo_1.2", summarise, grp.median=median(wpa))

# Plot
g_1.2_weak <- ggplot(target_1.2_boundary.weak, aes(x=wpa, fill=combo_1.2)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1 and #2 aligned receivers 
       to the Boundary/Weak Side when targeted vs. Cover 2", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2_weak

############### MAKE FIELD/STRONG 1-2-3 DATAFRAME ###################
target_1.2.3_field.strong <- merged3 %>% filter(field_strong_3 != "" & field_strong_2 != "" & field_strong_3 != "NULL") %>%
  filter((target_field.or.boundary == "Field/Strong Rt" & R1_Target == 1) |
           (target_field.or.boundary == "Field/Strong Rt" & R2_Target == 1) |
           (target_field.or.boundary == "Field_Strong Rt" & R3_Target == 1) |
           (target_field.or.boundary == "Field_Strong Lt" & L3_Target == 1) |
           (target_field.or.boundary == "Field/Strong Lt" & L2_Target ==1) |
           (target_field.or.boundary == "Field/Strong Lt" & L1_Target == 1)) %>%
  mutate(combo_1.2.3 = paste(field_strong_1, field_strong_2, field_strong_3, sep = " | ")) %>%
  filter(CoverageScheme == "Cover 3") %>%
  select(combo_1.2.3, wpa)

target_1.2.3_field.strong <- target_1.2.3_field.strong[target_1.2.3_field.strong$combo_1.2.3 %in% names(sort(table(target_1.2.3_field.strong$combo_1.2.3), decreasing = TRUE)[1:5]), ]  

# get group medians
library(plyr)
mu <- ddply(target_1.2.3_field.strong, "combo_1.2.3", summarise, grp.median=median(wpa))

# Plot
g_1.2.3_strong <- ggplot(target_1.2.3_field.strong, aes(x=wpa, fill=combo_1.2.3)) +
  geom_density(alpha = .4) + guides(linetype=FALSE) + labs(fill = "Route Combos") + theme_bw() + xlim(-.1, .1) +
  geom_vline(data=mu, aes(xintercept=grp.median, color=combo_1.2.3),
             linetype="dashed", size = 2) + labs(color = "Median WPA by Route Combo") +
  labs(title="5 Most Common Route Combinations by #1, #2, #3 aligned receivers to the 
       Field/Strong Side when targeted vs. Cover 3", 
       x="WPA (Win Probablity Added)", y = "Density") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size=12))
g_1.2.3_strong

detach(package:plyr)
test_coverage <-  target_1.2.3_field.strong %>% select(CoverageScheme) %>%
  group_by(CoverageScheme) %>%
  summarize(length(CoverageScheme))

