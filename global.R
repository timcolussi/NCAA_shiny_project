library(tidyverse)
library(ggplot2)
library(DT)
library(Cairo)
options(shiny.usecairo=T)
library(htmltools)
library(plotly)


setwd("~/NYCDSA/Shiny_project/NCAA_shiny_project")

final_compare_ncaa_conf_wins_seeds_champs <- read.csv("final_compare_ncaa_conf_wins_seeds_champs.csv")

data <- final_compare_ncaa_conf_wins_seeds_champs
data <- data[-c(1263,1264), ]
data[is.na(data)] <- 0
data$season <- as.character(data$season)

by_season <- ggplot(data, aes(x = conftour_wins, y = ncaatour_wins)) + 
  geom_jitter(aes(color = season), width = .25, size = 3) + 
  geom_hline(yintercept = 5.5) + 
  geom_hline(yintercept = 4.5) + 
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 2.5) + 
  geom_hline(yintercept = 1.5) + 
  geom_hline(yintercept = 0.5) + 
  xlab("Conference Tournament Wins") + 
  ylab("NCAA Tournament Wins") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

data$confabbrev <- str_replace(data$confabbrev, "pac_ten", "pac_twelve")
sum(data$confabbrev == 'pac_ten')

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

conf_ave_teams$ave_num_teams <- round(conf_ave_teams$ave_num_teams, digits = 1)

conf_ave_teams <- left_join(conf_ave_teams, conferences[, c('ConfAbbrev', 'Description')],
                                by = c('confabbrev' = 'ConfAbbrev'))
conf_ave_teams_adj <- select(conf_ave_teams, c(3,2))

conf_ave_teams_adj <- conf_ave_teams_adj %>% 
  rename('ConfName' = 'Description')

conf_ave_teams_adj$ConfName  <- str_replace(conf_ave_teams_adj$ConfName, 'Conference', '')
conf_ave_teams_adj$ConfName <- str_replace(conf_ave_teams_adj$ConfName, 'USA', 'Conference USA')
conf_ave_teams_adj$ConfName <- str_replace(conf_ave_teams_adj$ConfName, 'Southeastern', 'SEC')
conf_ave_teams_adj <- rename(conf_ave_teams_adj, 
                             c("Conference Name" = "ConfName", 
                               "Average Number of Teams" = "ave_num_teams"))


one_bid_confs <- conf_ave_teams %>% 
  filter(ave_num_teams < 1.5)

data_no_one_bid <- data %>% 
  filter(!confabbrev %in% one_bid_confs$confabbrev)

library(stringr)
data_no_one_bid$seed_num <- as.numeric(str_extract_all(data_no_one_bid$seed, "[0-9]+"))

data_no_one_bid$conf_name <- str_replace(data_no_one_bid$conf_name, "Pacific-10 Conference", "Pacific-12 Conference")

data_no_one_bid$conf_name  <- str_replace(data_no_one_bid$conf_name, 'Conference', '')
data_no_one_bid$conf_name <- str_replace(data_no_one_bid$conf_name, 'USA', 'Conference USA')
data_no_one_bid$conf_name <- str_replace(data_no_one_bid$conf_name, 'Southeastern', 'SEC')

by_larger_conf <- ggplot(data_no_one_bid, aes(x = conftour_wins, y = ncaatour_wins)) + 
  geom_jitter(aes(color = conf_name), width = .1) + 
  geom_hline(yintercept = 5.5) + 
  geom_hline(yintercept = 4.5) + 
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 2.5) + 
  geom_hline(yintercept = 1.5) + 
  geom_hline(yintercept = 0.5) + 
  xlab("Conference Tournament Wins") + 
  ylab("NCAA Tournament Wins") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

by_larger_conf + facet_wrap(~ conf_name) + theme(legend.position = "none")

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

wins_byconf_faceted <- ggplot(ave_wins_confwins2, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity', position = 'dodge') +
  facet_wrap(~ ConfName) +
  xlab("Conference Tournament Wins") +
  ylab("Average NCAA Tournament Wins") +
  theme_bw() +
  theme(legend.position = "none")

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

wins_by_conftype <- ggplot(ave_wins_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conf_type), stat = 'identity', position = 'dodge') +
  xlab("Conference Tournament Wins") +
  ylab("Average NCAA Tournament Wins") +
  guides(fill=guide_legend(title="Conference Type"))




teams <- read_csv("MTeams.csv")

select_confs <- rbind(power_confs, mid_major)
#select_confs <- left_join(select_confs, conferences[, c('ConfAbbrev', 'Description')], 
                          #by = c('confabbrev' = 'ConfAbbrev'))
select_confs <- rename(select_confs, 'ConfName' = 'Description')

select_confs$ConfName <- str_replace(select_confs$ConfName, 'Conference', '')
select_confs$ConfName <- str_replace(select_confs$ConfName, 'USA', 'Conference USA')
select_confs$ConfName <- str_replace(select_confs$ConfName, 'Southeastern', 'SEC')

conf_choices <- select_confs$ConfName

acc <- data_no_one_bid %>% 
  filter(confabbrev == 'acc')
acc_appearances <- acc %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
acc_appearances <- left_join(acc_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
acc_ave_wins <- acc %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
acc_apps_wins <- left_join(acc_appearances, acc_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
acc_apps_wins <- acc_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
acc_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'acc')
acc_plot <- ggplot(acc_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

a_ten <- data_no_one_bid %>% 
  filter(confabbrev == 'a_ten')
a_ten_appearances <- a_ten %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
a_ten_appearances <- left_join(a_ten_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
a_ten_ave_wins <- a_ten %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
a_ten_apps_wins <- left_join(a_ten_appearances, a_ten_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
a_ten_apps_wins <- a_ten_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
a_ten_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'a_ten')
a_ten_plot <- ggplot(a_ten_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")
















