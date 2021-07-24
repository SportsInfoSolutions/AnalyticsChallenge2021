#file to load all setup needed for complete analysis
library(tidyverse)

ginfo <- read_csv(paste0(getwd(), "/data/GameInfo.csv"))
pbp <- read_csv(paste0(getwd(), "/data/PlayByPlay.csv"))
ptp <- read_csv(paste0(getwd(), "/data/PlayerTotalPoints.csv"))
spp <- read_csv(paste0(getwd(), "/data/SkillPositionPlayers.csv"))

#need to change this in order to work across multiple platforms
source("/Users/jacksonepolk/Desktop/SIS/R/determine_wr_form.R")
source("/Users/jacksonepolk/Desktop/SIS/R/determine_fib.R")
source("/Users/jacksonepolk/Desktop/SIS/R/determine_pers.R")
source("/Users/jacksonepolk/Desktop/SIS/R/clean_routes.R")
source("/Users/jacksonepolk/Desktop/SIS/R/determine_routes.R")
source("/Users/jacksonepolk/Desktop/SIS/R/determine_pos_oti.R")
source("/Users/jacksonepolk/Desktop/SIS/R/find_top_routes.R")

form <- determine_wr_form(spp)
fib <- determine_fib(pbp, form)
pers <- determine_pers(spp)
cleaned <- clean_routes(spp)
rts <- determine_routes(spp %>% left_join(cleaned))