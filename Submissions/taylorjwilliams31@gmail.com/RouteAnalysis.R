library(tidyverse)

setwd("~/Fun/SIS Challenge 2021")

# read in SIS provided data
games <- read.csv("GameInfo.csv")
pbp <- read.csv("PlayByPlay.csv")
tp <- read.csv("PlayerTotalPoints.csv")
skill_players <- read.csv("SkillPositionPlayers.csv")

# read in route target area mapping
rta <- read.csv("RouteTargetAreas.csv")

# join the route target areas to the skill players
skill_players <- skill_players %>% left_join(rta, by=c("SideOfCenter", "Route"))

# filter the QBs from the SPs and remove non-skill players
qbs <- skill_players %>% filter(OnFieldPosition == "QB")
skill_players <- skill_players %>% filter(OnFieldPosition != "QB")
skill_positions <- c("RB", "TE", "WR")
skill_players <- skill_players %>% filter(RosterPosition %in% skill_positions)

# one hot encode the route target area
skill_players <- skill_players %>% mutate(value = 1)  %>% spread(TargetArea, value,  fill = 0)

# clean pbp data
pbp <- pbp %>% 
  mutate(
    Kneel = ifelse(str_detect(PlayDesc, ' kneels '), 1, 0),
    Scramble = ifelse(str_detect(PlayDesc, ' scrambles '), 1, 0),
    EventTypeMod = ifelse((EventType == "pass" | EventType == "challenge pass" | Scramble == 1), "pass", "rush"),
    PassMod = ifelse(EventTypeMod == "pass", 1, 0),
    OnHash = ifelse(Hash %% 2 == 1, 1, 0),
    Success = ifelse(EPA > 0, 1, 0)
  )

drop_types <- tibble(DropType = c("0/1 Step", "3 Step", "5 Step", "7 Step", "Basic Screen", 
                                  "Designed Rollout Left", "Designed Rollout Right", "RPO", "RPO Move", "Spike", 
                                  "NULL"), 
                     DropTypeMod = c("0/1 Step", "3 Step", "5 Step", "7 Step", "Basic Screen", "Designed Rollout", 
                                     "Designed Rollout", "RPO", "RPO", "Spike", "NULL"))
pbp <- pbp %>% 
  left_join(drop_types, by = "DropType") %>% 
  mutate(DropTypeMod = replace_na(DropTypeMod, "Other"))

pbp %>% filter(DropTypeMod == "Other") %>% nrow()

pbp_clean <- pbp %>% 
  filter(Kneel == 0 & Spike != 1)

pbp_clean_slim <- subset(pbp_clean, select = c(GameID, EventID))
pbp_clean_slim$Play <- 1

# filter out junk plays from skill players table
skill_players <- skill_players %>% 
  left_join(pbp_clean_slim) %>% 
  filter(!is.na(Play)) %>% 
  select(-Play)

# aggregate route areas per play
routes_per_play <- skill_players %>% 
  group_by(GameID, EventID) %>% 
  summarise(
    Backfield = sum(Backfield),
    Crossing = sum(Crossing),
    DeepMiddle = sum(`Deep Middle`),
    DeepOutside = sum(`Deep Outside`),
    ShortMiddle = sum(`Short Middle`),
    ShortOutside = sum(`Short Outside`),
    NonRoute = sum(NonRoute)
  )

# cluster plays based on number of route target areas
df_cluster <- subset(routes_per_play, select = -c(GameID, EventID))
df_cluster <- df_cluster %>% mutate(id = row_number())
train <- df_cluster %>% sample_frac(.75)
test  <- anti_join(df_cluster, train, by = 'id')
df_cluster <- subset(df_cluster, select = -c(id))
train <- subset(train, select = -c(id))
test <- subset(test, select = -c(id))
rsq <- rep(0,6)
train_size <- nrow(train)
test_size <- nrow(test)

set.seed(31)
cluster_2 <- kmeans(train, centers = 2, nstart = 75)
rsq[2] <- cluster_2$betweenss / cluster_2$totss
cluster_3 <- kmeans(train, centers = 3, nstart = 75)
rsq[3] <- cluster_3$betweenss / cluster_3$totss
cluster_4 <- kmeans(train, centers = 4, nstart = 75)
rsq[4] <- cluster_4$betweenss / cluster_4$totss
cluster_5 <- kmeans(train, centers = 5, nstart = 75)
rsq[5] <- cluster_5$betweenss / cluster_5$totss
cluster_6 <- kmeans(train, centers = 6, nstart = 75)
rsq[6] <- cluster_6$betweenss / cluster_6$totss

plot(rsq)
lines(rsq)

set.seed(123)
cluster_3_test <- kmeans(test, centers = cluster_3$centers, nstart = 1)
dist_diff_3 <- mean(apply((cluster_3_test$centers - cluster_3$centers)^2,1,sum))
size_diff_3 <- mean((cluster_3_test$size / test_size - cluster_3$size / train_size)^2)
cluster_4_test <- kmeans(test, centers = cluster_4$centers, nstart = 1)
dist_diff_4 <- mean(apply((cluster_4_test$centers - cluster_4$centers)^2,1,sum))
size_diff_4 <- mean((cluster_4_test$size / test_size - cluster_4$size / train_size)^2)
cluster_5_test <- kmeans(test, centers = cluster_5$centers, nstart = 1)
dist_diff_5 <- mean(apply((cluster_5_test$centers - cluster_5$centers)^2,1,sum))
size_diff_5 <- mean((cluster_5_test$size / test_size - cluster_5$size / train_size)^2)
cluster_6_test <- kmeans(test, centers = cluster_6$centers, nstart = 1)
dist_diff_6 <- mean(apply((cluster_6_test$centers - cluster_6$centers)^2,1,sum))
size_diff_6 <- mean((cluster_6_test$size / test_size - cluster_6$size / train_size)^2)
# Compare distance and size differences among cluster solutions
differences <- as.data.frame(cbind(rbind(Three_Clusters = dist_diff_3, Four_Clusters = dist_diff_4, 
                                         Five_Clusters = dist_diff_5, Six_clusters = dist_diff_6),
                                   rbind(size_diff_3, size_diff_4, size_diff_5, size_diff_6)))
colnames(differences) <- c("Distance", "Size")
differences

centers_3 <- t(cluster_3$centers)
centers_3
centers_4 <- t(cluster_4$centers)
centers_4
centers_5 <- t(cluster_5$centers)
centers_5

# apply 3 cluster solution to full play set
cluster_4_all <- kmeans(df_cluster, centers = cluster_4$centers, nstart = 1)
routes_per_play$cluster <- cluster_4_all$cluster

# join the cluster data to the pbp data
pbp_clean <- pbp_clean %>% 
  left_join(routes_per_play, by=c("GameID", "EventID"))

pbp_clean <- pbp_clean %>% 
  mutate(EPA = as.numeric(EPA),
         EPA = replace_na(EPA, 0))
  
# analyze situations for particular clusters
cluster_summary <- pbp_clean %>% 
  group_by(cluster) %>% 
  summarise(
    AvgDown = mean(Down),
    AvgToGo = mean(ToGo),
    RPORate = mean(RPO),
    AvgYards = mean(OffensiveYardage),
    AvgEPA = mean(EPA),
    PassRate = mean(PassMod),
    OnHashRate = mean(OnHash),
    SuccessRate = mean(Success)
  )

# analyze pass sitchs by coverage
pass_coverage_summary <- pbp_clean %>% 
  filter(EventTypeMod == "pass") %>% 
  group_by(cluster, CoverageScheme) %>% 
  summarise(
    count = n(),
    AvgDown = mean(Down),
    AvgToGo = mean(ToGo),
    RPORate = mean(RPO),
    OnHashRate = mean(OnHash),
    AvgYards = mean(OffensiveYardage),
    AvgEPA = mean(EPA),
    SuccessRate = mean(Success)
  ) %>% 
  filter(count > 100)

# plot cluster results
ggplot(data=cluster_summary, aes(x=cluster, y=AvgDown, fill=as.factor(cluster))) +
  geom_bar(stat="identity") +
  scale_color_brewer(palette="Set1") +
  ggtitle("Average Down per Cluster") +
  labs(x="Cluster", fill="Cluster") +
  ylim(0, 4) +
  theme_minimal()

ggplot(data=cluster_summary, aes(x=cluster, y=AvgToGo, fill=as.factor(cluster))) +
  geom_bar(stat="identity") +
  scale_color_brewer(palette="Set1") +
  ggtitle("Average Yards to Go per Cluster") +
  labs(x="Cluster", fill="Cluster") +
  ylim(0, 10) +
  theme_minimal()

ggplot(data=cluster_summary, aes(x=cluster, y=AvgEPA, fill=as.factor(cluster))) +
  geom_bar(stat="identity") +
  scale_color_brewer(palette="Set1") +
  ggtitle("Average EPA per Cluster") +
  labs(x="Cluster", fill="Cluster") +
  theme_minimal()

ggplot(data=pass_coverage_summary, aes(x=CoverageScheme, y=AvgEPA, fill=as.factor(cluster))) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_color_brewer(palette="Set1") +
  ggtitle("Average EPA per Cluster per Coverage") +
  labs(x="Coverage Scheme", fill="Cluster") +
  theme_minimal()

ggplot(data=pass_coverage_summary, aes(x=CoverageScheme, y=SuccessRate, fill=as.factor(cluster))) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_color_brewer(palette="Set1") +
  ggtitle("Success Rate per Cluster per Coverage") +
  labs(x="Coverage Scheme", fill="Cluster") +
  ylim(0, 1)
  theme_minimal()
