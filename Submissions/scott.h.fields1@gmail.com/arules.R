library(dplyr)
library(tidyr)
library(stringr)
library(arules)

#specify routes and play IDs
plays <- SkillPositionPlayers[!SkillPositionPlayers$Route %in% c('NULL', 'Blocking', 'Run Fake', 'Pick'), ]%>% drop_na(Route)
plays$play <- paste(plays$GameID, '-', plays$EventID)
plays$Route <- paste(plays$OnFieldPosition, '- ', plays$SideOfCenter, '- ', plays$Route)
PlayByPlay$play <- paste(PlayByPlay$GameID, '-', PlayByPlay$EventID)

plays <- merge(plays, PlayByPlay)
plays <- plays %>% 
  filter(!grepl('NULL', EPA)) %>% 
  filter(!grepl('rush', EventType))

#limit routes dataframe to just play IDs and routes
routes <- plays %>%  select(play, Route)

# pair up all routes with each play
paired_routes <- routes %>% 
  group_by(play) %>% 
  summarise(
    routes = paste(Route, collapse = '/ ')
  )

sep_routes <- separate(paired_routes, routes, c('1','2','3','4','5'),'/ ')
sep_routes <- sep_routes %>% mutate_if(is.character, as.factor)
sep_routes <- sep_routes %>% select(-c(play))
write.csv(sep_routes, 'sep_routes.csv')

sep_routes_csv <- read.transactions("sep_routes.csv", sep=',')
rules <- sep_routes_csv %>% 
  apriori(parameter = list(supp = .005, conf = .25, target = 'rules'))
write.csv(rules %>% head(n = 40, by = "lift") %>% inspect, 'rules.csv')





#########
### Routes Grouped - Didn't use
#########


short_routes <- c('Chip - Curl', 'Curl', 'Comeback', 'Jerk', 'Out', 'Over Ball', 'Whip')
vertical_routes <- c('Go/Fly', 'Fade', 'Fade - Back Shoulder', 'Seam', 'Post', 'Corner', 'Chip - Seam')
crossing_routes <- c('Drag', 'Dig', 'Deep Cross', 'Slant', 'Chip - Drag')
interior_routes <- c('Angle', 'Beneath', 'Check & Release', 'Chip', 'Chip - Flat', 'Flat - Left', 'Flat - Right', 'Leak', 'Swing - Left', 'Swing - Right', 'Wheel')
screen_routes <- c('Beneath', 'Bubble', 'Screen - Drag', 'Quick', 'Screen - Shovel', 'Screen - TE', ' Tunnel', 'Screen - RB', 'Jet Sweep Pass', 'Screen - Bubble', 'Screen - Quick', 'Screen - Tunnel', 'Screen - Beneath')
double_moves <- c('Corner Post', 'Post Corner', 'Hitch & Go', 'Out & Up', 'Sluggo', 'Stick - Nod')
misc_routes <- c('Blocking', 'Run Fake', 'Pick')
missing_list <- c()

SkillPositionPlayers <- SkillPositionPlayers %>% 
  mutate(route_group = case_when(
    Route %in% short_routes ~ 'short',
    Route %in% vertical_routes ~ 'vertical',
    Route %in% crossing_routes ~ 'crossing',
    Route %in% interior_routes ~ 'interior',
    Route %in% screen_routes ~ 'screen',
    Route %in% double_moves ~ 'double',
    Route %in% misc_routes ~ 'misc',
  ))


#specify routes and play IDs
plays_rg <- SkillPositionPlayers[!SkillPositionPlayers$route_group %in% c('NULL', 'misc'), ] %>% drop_na(route_group)
plays_rg$play <- paste(plays_rg$GameID, '-', plays_rg$EventID)
plays_rg$Route <- paste(plays_rg$OnFieldPosition, '- ', plays_rg$SideOfCenter, '- ', plays_rg$route_group)

#limit routes dataframe to just play IDs and routes
routes_rg <- plays_rg %>%  select(play, Route)

# pair up all routes with each play
paired_routes_rg <- routes_rg %>% 
  group_by(play) %>% 
  summarise(
    routes_rg = paste(Route, collapse = '/ ')
  )

sep_routes_rg <- separate(paired_routes_rg, routes_rg, c('1','2','3','4','5'),'/ ')
sep_routes_rg <- sep_routes_rg %>% mutate_if(is.character, as.factor)
sep_routes_rg <- sep_routes_rg %>% select(-c(play))
write.csv(sep_routes_rg, 'sep_routes_rg.csv')


sep_routes_rg_csv <- read.transactions("sep_routes_rg.csv", sep=',')
rules <- sep_routes_rg_csv %>% 
  apriori(parameter = list(supp = .01, conf = .1, target = 'rules'))
rules %>% head(n = 20, by = "lift") %>% inspect

#################



### Combine with Play by Play




new_sep_routes <- separate(paired_routes, routes, c('1','2','3','4','5'),'/ ')
PlayByPlay$play <- paste(PlayByPlay$GameID, '-', PlayByPlay$EventID)
pbp <- merge(new_sep_routes, PlayByPlay)
pbp <- pbp %>% 
  filter(!grepl('NULL', EPA)) %>% 
  filter(!grepl('rush', EventType))


pbp$EPA <- as.numeric(pbp$EPA)
pbp %>% 
  select(CoverageScheme, EPA) %>% 
  group_by(CoverageScheme) %>% 
  summarise(mean = mean(EPA), count = n()) %>% 
  arrange(desc(count))

library(ggplot2)
library(hrbrthemes)
library(data.table)
setnames(pbp, old = c('1', '2', '3', '4', '5'), new = c('route1', 'route2', 'route3', 'route4', 'route5'))
pbp %>% 
  filter(grepl('WR -  L -  Curl', route1)) %>% 
  filter(CoverageScheme == 'Cover 0') %>% 
  ggplot(aes(x = EPA)) +
  geom_density(fill = 'cyan', color = 'cyan') +
  theme_ipsum()

pbp %>% 
  filter(grepl('\\<SWR -  R -  Slant\\>', paste(route1, route2, route3, route4, route5))) %>% 
  filter(grepl('\\<WR -  R -  Slant\\>', paste(route1, route2, route3, route4, route5))) %>% 
  filter(CoverageScheme == 'Cover 2') 




coverage_list <- unique(pbp$CoverageScheme)

route_coverage_epa <- data.frame(route_combo = character())


fill_RCE <- function(index, combo_name, name1, name2, name3, name4){
  route_coverage_epa <- route_coverage_epa %>% add_row(route_combo = combo_name)
  for(coverage in coverage_list){
    temp_pbp <- pbp %>% 
      filter(grepl(name1, paste(route1, route2, route3, route4, route5))) %>% 
      filter(grepl(name2, paste(route1, route2, route3, route4, route5))) %>% 
      filter(CoverageScheme == coverage)
    temp_pbp2 <- pbp %>% 
      filter(grepl(name3, paste(route1, route2, route3, route4, route5))) %>% 
      filter(grepl(name4, paste(route1, route2, route3, route4, route5))) %>% 
      filter(CoverageScheme == coverage)
    temp_pbp <- rbind(temp_pbp, temp_pbp2)
    mean_epa <- mean(as.numeric(temp_pbp$EPA))
    route_coverage_epa[index, coverage] <- mean_epa
    new_name = paste(coverage, 'Count', sep = ' ')
    route_coverage_epa[index, new_name] <- length(temp_pbp$play)
  }
  return(route_coverage_epa)
}
route_coverage_epa <- fill_RCE(1, 'SWR/WR Slant SS', '\\<SWR -  R -  Slant\\>', '\\<WR -  R -  Slant\\>', '\\<SWR -  L -  Slant\\>', '\\<WR -  L -  Slant\\>')
route_coverage_epa <- fill_RCE(2, 'WR Slant DS', '\\<WR -  R -  Slant\\>', '\\<WR -  L -  Slant\\>', '\\<ZZZZZZ\\>', '\\<ZZZZZZZZ\\>')
route_coverage_epa <- fill_RCE(3, 'WR Curl DS', '\\<WR -  R -  Curl\\>', '\\<WR -  L -  Curl\\>', '\\<ZZZZZZ\\>', '\\<ZZZZZZZZ\\>')
route_coverage_epa <- fill_RCE(4, 'WR Fly DS', '\\<WR -  R -  Go/Fly\\>', '\\<WR -  L -  Go/Fly\\>', '\\<ZZZZZZ\\>', '\\<ZZZZZZZZ\\>')
route_coverage_epa <- fill_RCE(5, 'SWR Fade/WR Curl SS', '\\<WR -  R -  Curl\\>', '\\<SWR -  R -  Fade\\>', '\\<WR -  L -  Curl\\>', '\\<SWR -  L -  Fade\\>')
route_coverage_epa <- fill_RCE(6, 'TE Flat/SWR Curl SS', '\\<TE -  R -  Flat - Right\\>', '\\<SWR -  R -  Curl\\>', '\\<TE -  L -  Flat - Left\\>', '\\<SWR -  L -  Curl\\>')
route_coverage_epa <- fill_RCE(7, 'SWR Over/WR Curl SS', '\\<SWR -  R -  Over Ball\\>', '\\<WR -  R -  Curl\\>', '\\<SWR -  L -  Over Ball\\>', '\\<WR -  L -  Curl\\>')
route_coverage_epa <- fill_RCE(8, 'SWR Flat/Slot Curl SS', '\\<SWR -  R -  Flat - Right\\>', '\\<SWR -  R -  Curl\\>', '\\<SWR -  L -  Flat - Left\\>', '\\<SWR -  L -  Curl\\>')
route_coverage_epa <- fill_RCE(9, 'SWR Curl DS', '\\<SWR -  R -  Curl\\>', '\\<SWR -  L -  Curl\\>', '\\<ZZZZZZ\\>', '\\<ZZZZZZZZ\\>')
route_coverage_epa <- fill_RCE(10, 'SWR Seam/WR Curl SS', '\\<WR -  R -  Curl\\>', '\\<SWR -  R -  Seam\\>', '\\<WR -  L -  Curl\\>', '\\<SWR -  L -  Seam\\>')

fill_RCE_3 <- function(index, combo_name, name1, name2, name3, name4, name5, name6){
  route_coverage_epa <- route_coverage_epa %>% add_row(route_combo = combo_name)
  for(coverage in coverage_list){
    temp_pbp <- pbp %>% 
      filter(grepl(name1, paste(route1, route2, route3, route4, route5))) %>% 
      filter(grepl(name2, paste(route1, route2, route3, route4, route5))) %>%
      filter(grepl(name3, paste(route1, route2, route3, route4, route5))) %>%
      filter(CoverageScheme == coverage)
    temp_pbp2 <- pbp %>% 
      filter(grepl(name4, paste(route1, route2, route3, route4, route5))) %>% 
      filter(grepl(name5, paste(route1, route2, route3, route4, route5))) %>% 
      filter(grepl(name6, paste(route1, route2, route3, route4, route5))) %>%
      filter(CoverageScheme == coverage)
    temp_pbp <- rbind(temp_pbp, temp_pbp2)
    mean_epa <- mean(as.numeric(temp_pbp$EPA))
    route_coverage_epa[index, coverage] <- mean_epa
    new_name = paste(coverage, 'Count', sep = ' ')
    route_coverage_epa[index, new_name] <- length(temp_pbp$play)
  }
  return(route_coverage_epa)
}
route_coverage_epa <- fill_RCE_3(11, 'WR Slants DS/SWR Slant', '\\<SWR -  R -  Slant\\>', '\\<WR -  R -  Slant\\>', '\\<WR -  L -  Slant\\>', '\\<WR -  L -  Slant\\>', '\\<WR -  R -  Slant\\>', '\\<SWR -  L -  Slant\\>')
route_coverage_epa <- fill_RCE_3(12, 'WR Curls DS/SWR Seam', '\\<SWR -  R -  Seam\\>', '\\<WR -  R -  Curl\\>', '\\<WR -  L -  Curl\\>', '\\<WR -  L -  Curl\\>', '\\<WR -  R -  Curl\\>', '\\<SWR -  L -  Seam\\>')
route_coverage_epa <- fill_RCE_3(13, 'WR Curls DS/SWR Curl', '\\<SWR -  R -  Curl\\>', '\\<WR -  R -  Curl\\>', '\\<WR -  L -  Curl\\>', '\\<WR -  L -  Curl\\>', '\\<WR -  R -  Curl\\>', '\\<SWR -  L -  Curl\\>')
route_coverage_epa <- fill_RCE_3(14, 'SWR Curls DS/WR Curl', '\\<SWR -  R -  Curl\\>', '\\<WR -  R -  Curl\\>', '\\<SWR -  L -  Curl\\>', '\\<WR -  L -  Curl\\>', '\\<SWR -  R -  Curl\\>', '\\<SWR -  L -  Curl\\>')

# forgot this one earlier - whoops
route_coverage_epa <- fill_RCE(15, 'TE Corner/RB Flat SS', '\\<TE -  R -  Corner\\>', '\\<B -  NULL -  Flat - Right\\>', '\\<TE -  L -  Corner\\>', '\\<B -  NULL -  Flat - Left\\>')



# --------------------
# Graphs
library(ggthemes)

## Cover 4
p <- pbp %>% 
  filter(CoverageScheme == 'Cover 4') %>% 
  ggplot(aes(x = EPA)) +
  geom_density(fill="#69b3a2", color="black") +
  theme_classic() + 
  geom_vline(xintercept = route_coverage_epa[11,2], linetype = 'dashed', color = 'darkorchid3', size = 2)+
  geom_text(aes(x= 2, label = paste('Average EPA = ',round(route_coverage_epa[11,2], 2), '\n', route_coverage_epa[11,1], '\nvs', colnames(route_coverage_epa)[2], '\n Count = ', route_coverage_epa[11,3])), y = .3, size = 6) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  scale_y_continuous(limits = NULL, expand = c(0,0)) + 
  xlab('Cover 4 EPA vs All Routes') +
  geom_vline(xintercept = route_coverage_epa[13,2], linetype = 'dashed', color = 'indianred3', size = 2)+
  geom_text(aes(x= -2, label = paste('Average EPA = ',round(route_coverage_epa[13,2], 2), '\n', route_coverage_epa[13,1], '\nvs', colnames(route_coverage_epa)[2], '\n Count = ', route_coverage_epa[13,3])), y = .3, size = 6)+
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background = element_rect(fill = 'transparent'))
ggsave(p, filename = "Cover 4 tp.png",  bg = "transparent")



## Cover 3
p <- pbp %>% 
  filter(CoverageScheme == 'Cover 3') %>% 
  ggplot(aes(x = EPA)) +
  geom_density(fill="#69b3a2", color="black") +
  theme_classic() + 
  geom_vline(xintercept = route_coverage_epa[15,4], linetype = 'dashed', color = 'darkorchid3', size = 2)+
  geom_text(aes(x= 2, label = paste('Average EPA = ',round(route_coverage_epa[15,4], 2), '\n', route_coverage_epa[15,1], '\nvs', colnames(route_coverage_epa)[4], '\n Count = ', route_coverage_epa[15,5])), y = .3, size = 6) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  scale_y_continuous(limits = NULL, expand = c(0,0)) + 
  xlab('Cover 3 EPA vs All Routes') +
  geom_vline(xintercept = route_coverage_epa[5,4], linetype = 'dashed', color = 'indianred3', size = 2)+
  geom_text(aes(x= -2, label = paste('Average EPA = ',round(route_coverage_epa[5,4], 2), '\n', route_coverage_epa[5,1], '\nvs', colnames(route_coverage_epa)[4], '\n Count = ', route_coverage_epa[5,5])), y = .3, size = 6)+
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background = element_rect(fill = 'transparent'))
ggsave(p, filename = "Cover 3 tp.png",  bg = "transparent")

## Cover 1
p <- pbp %>% 
  filter(CoverageScheme == 'Cover 1') %>% 
  ggplot(aes(x = EPA)) +
  geom_density(fill="#69b3a2", color="black") +
  theme_classic() + 
  geom_vline(xintercept = route_coverage_epa[5,8], linetype = 'dashed', color = 'darkorchid3', size = 2)+
  geom_text(aes(x= 2, label = paste('Average EPA = ',round(route_coverage_epa[5,8], 2), '\n', route_coverage_epa[5,1], '\nvs', colnames(route_coverage_epa)[8], '\n Count = ', route_coverage_epa[5,9])), y = .3, size = 6) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  scale_y_continuous(limits = NULL, expand = c(0,0)) + 
  xlab('Cover 1 EPA vs All Routes') +
  geom_vline(xintercept = route_coverage_epa[14,8], linetype = 'dashed', color = 'indianred3', size = 2)+
  geom_text(aes(x= -2, label = paste('Average EPA = ',round(route_coverage_epa[14,8], 2), '\n', route_coverage_epa[14,1], '\nvs', colnames(route_coverage_epa)[8], '\n Count = ', route_coverage_epa[14,9])), y = .3, size = 6) +
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background = element_rect(fill = 'transparent'))
ggsave(p, filename = "Cover 1 tp.png",  bg = "transparent")

  

## Cover 2
p <- pbp %>% 
  filter(CoverageScheme == 'Cover 2') %>% 
  ggplot(aes(x = EPA)) +
  geom_density(fill="#69b3a2", color="black") +
  theme_classic() + 
  geom_vline(xintercept = route_coverage_epa[12,14], linetype = 'dashed', color = 'darkorchid3', size = 2)+
  geom_text(aes(x= 2, label = paste('Average EPA = ',round(route_coverage_epa[12, 14], 2), '\n', route_coverage_epa[12,1], '\nvs', colnames(route_coverage_epa)[14], '\n Count = ', route_coverage_epa[12,15])), y = .3, size = 6) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  scale_y_continuous(limits = NULL, expand = c(0,0)) + 
  xlab('Cover 2 EPA vs All Routes') +
  geom_vline(xintercept = route_coverage_epa[7,14], linetype = 'dashed', color = 'indianred3', size = 2)+
  geom_text(aes(x= -2, label = paste('Average EPA = ',round(route_coverage_epa[7, 14], 2), '\n', route_coverage_epa[7,1], '\nvs', colnames(route_coverage_epa)[14], '\n Count = ', route_coverage_epa[7,15])), y = .3, size = 6)+
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background = element_rect(fill = 'transparent'))
ggsave(p, filename = "Cover 2 tp.png",  bg = "transparent")



## Cover 2 Man
p <- pbp %>% 
  filter(CoverageScheme == 'Cover 2') %>% 
  ggplot(aes(x = EPA)) +
  geom_density(fill="#69b3a2", color="black") +
  theme_classic() + 
  geom_vline(xintercept = route_coverage_epa[1,18], linetype = 'dashed', color = 'darkorchid3', size = 2)+
  geom_text(aes(x= 2, label = paste('Average EPA = ',round(route_coverage_epa[1, 18], 2), '\n', route_coverage_epa[1,1], '\nvs', colnames(route_coverage_epa)[18], '\n Count = ', route_coverage_epa[1,19])), y = .3, size = 6) +
  scale_x_continuous(limits = c(-4,4), expand = c(0,0)) +
  scale_y_continuous(limits = NULL, expand = c(0,0)) + 
  xlab('Cover 2 - Man EPA vs All Routes') +
  geom_vline(xintercept = route_coverage_epa[3,18], linetype = 'dashed', color = 'indianred3', size = 2)+
  geom_text(aes(x= -2, label = paste('Average EPA = ',round(route_coverage_epa[3, 18], 2), '\n', route_coverage_epa[3,1], '\nvs', colnames(route_coverage_epa)[18], '\n Count = ', route_coverage_epa[3,19])), y = .3, size = 6)+
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background = element_rect(fill = 'transparent'))
ggsave(p, filename = "Cover 2 Man tp.png",  bg = "transparent")



