library(tidyverse)
library(ggplot2)
setwd("~/NYCDSA/Shiny_project/NCAA_shiny_project")

final_compare_ncaa_conf_wins_seeds_champs <- read.csv("final_compare_ncaa_conf_wins_seeds_champs.csv")

data <- final_compare_ncaa_conf_wins_seeds_champs
data <- data[-c(1263,1264), ]
data[is.na(data)] <- 0
data$season <- as.character(data$season)

tot_teams_year <- data %>% 
  group_by(season, confabbrev) %>% 
  summarise(total_teams = n())
tot_seasons <- tot_teams_year %>% 
  group_by(confabbrev) %>% 
  summarise(num_of_seasons = n())
conf_ave_teams <- tot_teams_year %>% 
  group_by(confabbrev) %>% 
  summarise(ave_num_teams = sum(total_teams)/n()) %>% 
  arrange(desc(ave_num_teams))
one_bid_confs <- conf_ave_teams %>% 
  filter(ave_num_teams < 1.5)
data_no_one_bid <- data %>% 
  filter(!confabbrev %in% one_bid_confs$confabbrev)
data_no_one_bid$confabbrev <- str_replace(data_no_one_bid$confabbrev, "pac_ten", "pac_twelve")

conferences <- read.csv('Conferences.csv')

data_no_one_bid3 <- data_no_one_bid

for (x in 1:NROW(data_no_one_bid3)) {
  data_no_one_bid3[x, 'conftour_wins'] <-
    ifelse(data_no_one_bid3[x, 'conftour_wins'] >= 3, 3, data_no_one_bid3[x, 'conftour_wins'])
}

ave_wins_confwins <- data_no_one_bid3 %>% 
  group_by(confabbrev, conftour_wins) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))

ave_wins_confwins$conftour_wins <- as.character(ave_wins_confwins$conftour_wins)

for (x in 1:NROW(ave_wins_confwins)) {
  if (ave_wins_confwins[x, 'conftour_wins'] == '0') {
    ave_wins_confwins[x, 'conftour_wins'] <- 'zero'
  } else if (ave_wins_confwins[x, 'conftour_wins'] == '1') {
    ave_wins_confwins[x, 'conftour_wins'] <- 'one'
  } else if (ave_wins_confwins[x, 'conftour_wins'] == '2') {
    ave_wins_confwins[x, 'conftour_wins'] <- 'two'
  } else {ave_wins_confwins[x, 'conftour_wins'] <- 'three +'}
}

ave_wins_confwins$conftour_wins <- factor(ave_wins_confwins$conftour_wins, 
                                          levels = c('three +', 
                                                     'two',
                                                     'one',
                                                     'zero'))

ave_wins_confwins2 <- left_join(ave_wins_confwins, conferences[, c('ConfAbbrev', 'Description')],
                                by = c('confabbrev' = 'ConfAbbrev'))
ave_wins_confwins2 <- ave_wins_confwins2 %>% 
  rename('ConfName' = 'Description')

ave_wins_confwins2$ConfName  <- str_replace(ave_wins_confwins2$ConfName, 'Conference', '')
ave_wins_confwins2$ConfName <- str_replace(ave_wins_confwins2$ConfName, 'USA', 'Conference USA')
ave_wins_confwins2$ConfName <- str_replace(ave_wins_confwins2$ConfName, 'Southeastern', 'SEC')

power_confs <- conf_ave_teams %>% 
  filter(ave_num_teams > 4)
mid_major <- conf_ave_teams %>% 
  filter(ave_num_teams > 1.5 & ave_num_teams < 4)

ave_wins_confwins2 <- ave_wins_confwins
power_ave_wins_wins <- ave_wins_confwins %>% 
  filter(confabbrev %in% power_confs$confabbrev)
power_ave_wins_wins$conf_type <- 'power'
mid_ave_wins_wins <- ave_wins_confwins %>% 
  filter(confabbrev %in% mid_major$confabbrev)
mid_ave_wins_wins$conf_type <- 'mid major'
ave_wins_wins <- rbind(power_ave_wins_wins, mid_ave_wins_wins)
ave_wins_wins$conf_type <- as.factor(ave_wins_wins$conf_type)

teams <- read_csv("MTeams.csv")








