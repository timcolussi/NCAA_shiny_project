library(tidyverse)
library(ggplot2)

final_compare_ncaa_conf_wins_seeds_champs <- read_csv("/NYCDSA/Shiny_project/NCAA_shiny_project/final_compare_ncaa_conf_wins_seeds_champs.csv")
  cols(
    season = col_double(),
    teamid = col_double(),
    teamname = col_character(),
    ncaatour_wins = col_double(),
    conftour_wins = col_double(),
    confabbrev = col_character(),
    conf_name = col_character(),
    seed = col_character(),
    conf_tourney_champ = col_double()
  )

final_compare_ncaa_conf_wins_seeds_champs <- read_csv("final_compare_ncaa_conf_wins_seeds_champs.csv")  
  
data <- final_compare_ncaa_conf_wins_seeds_champs

data <- data[-c(1263,1264), ]
sum(is.na(data$conftour_wins))
data[is.na(data)] <- 0

data$season <- as.character(data$season)
  
by_season <- ggplot(data, aes(x = conftour_wins, y = ncaatour_wins)) + 
  geom_jitter(aes(color = season), width = .25) + 
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

data$conf_tourney_champ <- as.factor(data$conf_tourney_champ)

by_conf_champs <- ggplot(data, aes(x = conftour_wins, y = ncaatour_wins)) + 
  geom_jitter(aes(color = conf_tourney_champ), width = .25) + 
  geom_hline(yintercept = 5.5) + 
  geom_hline(yintercept = 4.5) + 
  geom_hline(yintercept = 3.5) + 
  geom_hline(yintercept = 2.5) + 
  geom_hline(yintercept = 1.5) + 
  geom_hline(yintercept = 0.5) + 
  labs(x = "Conference Tournament Wins", y = "NCAA Tournament Wins") +
  scale_color_discrete(name="Conf. Tourney Champs",
                       labels = c("No", "Yes")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

data2 <- data

tot_teams_year <- data2 %>% 
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

data_no_one_bid <- data2 %>% 
  filter(!confabbrev %in% one_bid_confs$confabbrev)
  
library(stringr)
data_no_one_bid$seed_num <- as.numeric(str_extract_all(data_no_one_bid$seed, "[0-9]+"))

data_no_one_bid$confabbrev <- str_replace(data_no_one_bid$confabbrev, "pac_ten", "pac_twelve")


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

data_no_one_bid %>% 
  filter(confabbrev == "acc" & ncaatour_wins == 6)

data_no_one_bid %>% 
  filter(ncaatour_wins == 6) %>% 
  group_by(confabbrev) %>% 
  summarise(n())

final_four <- data_no_one_bid %>% 
  filter(ncaatour_wins >= 4)

ggplot(final_four, aes(x = conftour_wins, y = ncaatour_wins)) + 
  geom_jitter(width = .1) + 
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

final_four_conf_wins <- final_four %>% 
  group_by(conftour_wins) %>% 
  summarise(count = n())


final_four_conf_wins$conftour_wins <- as.factor(final_four_conf_wins$conftour_wins)

ggplot(final_four_conf_wins, aes(x="", y=count, fill=conftour_wins)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0) + 
  theme_void()

data_no_one_bid$seed_num <- as.factor(data_no_one_bid$seed_num)

ggplot(data_no_one_bid, aes(x = conftour_wins, y = ncaatour_wins)) + 
  geom_jitter(aes(color = seed_num), width = .1) + 
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

data_no_one_bid %>% 
  group_by(seed_num) %>% 
  summarise(n())

data_no_one_bid %>% 
  filter(conftour_wins >= 4) %>% 
  group_by(seed_num) %>% 
  summarise(n())

conf_champs <- data_no_one_bid %>% 
  filter(conf_tourney_champ == 1)
  
conf_champs_wins <- conf_champs %>% 
  group_by(season, confabbrev) %>% 
  summarise(tournament_wins = sum(ncaatour_wins))
  
conf_champs_ave_wins <- conf_champs_wins %>% 
  group_by(confabbrev) %>% 
  summarise(average_wins = mean(tournament_wins))

conferences <- read.csv('Conferences.csv')

ggplot(conf_champs_ave_wins, aes(x = confabbrev, y = average_wins)) +
  geom_bar(aes(fill = confabbrev), stat = "Identity") +
  xlab("Conferences") + 
  ylab("Average Wins") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

zero_conf_wins <- data_no_one_bid %>% 
  filter(conftour_wins == 0)
one_conf_win <- data_no_one_bid %>% 
  filter(conftour_wins == 1)
two_conf_wins <- data_no_one_bid %>% 
  filter(conftour_wins == 2)
three_or_more_conf_wins <- data_no_one_bid %>% 
  filter(conftour_wins >= 3)

zero_conf_tourwins <- zero_conf_wins %>% 
  group_by(season, confabbrev) %>% 
  summarise(tournament_wins = sum(ncaatour_wins))
one_conf_tourwins <- one_conf_win %>% 
  group_by(season, confabbrev) %>% 
  summarise(tournament_wins = sum(ncaatour_wins))
two_conf_tourwins <- two_conf_wins %>% 
  group_by(season, confabbrev) %>% 
  summarise(tournament_wins = sum(ncaatour_wins))
three_or_more_conf_tourwins <- three_or_more_conf_wins %>% 
  group_by(season, confabbrev) %>% 
  summarise(tournament_wins = sum(ncaatour_wins))
  
zero_conf_ave_wins <- zero_conf_tourwins %>% 
  group_by(confabbrev) %>% 
  summarise(average_wins = mean(tournament_wins))
one_conf_ave_wins <- one_conf_tourwins %>% 
  group_by(confabbrev) %>% 
  summarise(average_wins = mean(tournament_wins))
two_conf_ave_wins <- two_conf_tourwins %>% 
  group_by(confabbrev) %>% 
  summarise(average_wins = mean(tournament_wins))
three_or_more_conf_ave_wins <- three_or_more_conf_tourwins %>% 
  group_by(confabbrev) %>% 
  summarise(average_wins = mean(tournament_wins))

zero_conf_ave_wins <- zero_conf_ave_wins %>% rename(zero_ave_wins = average_wins)
one_conf_ave_wins <- one_conf_ave_wins %>% rename(one_ave_wins = average_wins)
two_conf_ave_wins <- two_conf_ave_wins %>% rename(two_ave_wins = average_wins)
three_or_more_conf_ave_wins <- three_or_more_conf_ave_wins %>% rename(three_plus_ave_wins = average_wins)

ave_wins <- left_join(zero_conf_ave_wins, one_conf_ave_wins, by = 'confabbrev') %>% 
  left_join(., two_conf_ave_wins, by = 'confabbrev') %>% 
  left_join(., three_or_more_conf_ave_wins, by = 'confabbrev')

ave_wins_long <- gather(ave_wins, key = conf_wins, value = ave_ncaa_wins, -confabbrev)

ggplot(ave_wins_long, aes(x = confabbrev, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conf_wins), stat = "identity", position = "dodge") 

ave_wins_long$conf_wins <- str_replace_all(ave_wins_long$conf_wins, c('zero_ave_wins' = 'zero', 
                                  'one_ave_wins' = 'one', 
                                  'two_ave_wins' = 'two', 
                                  'three_plus_ave_wins' = 'three +'))
ave_wins_long$conf_wins <- factor(ave_wins_long$conf_wins, levels = c('three +',
                                                                         'two',
                                                                         'one', 
                                                                         'zero'))


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

aac <- data_no_one_bid %>% 
  filter(confabbrev == 'aac')
aac_appearances <- aac %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
aac_appearances <- left_join(aac_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
aac_ave_wins <- aac %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
aac_apps_wins <- left_join(aac_appearances, aac_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
aac_apps_wins <- aac_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
aac_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'aac')
aac_plot <- ggplot(aac_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

bigeast <- data_no_one_bid %>% 
  filter(confabbrev == 'big_east')
bigeast_appearances <- bigeast %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
bigeast_appearances <- left_join(bigeast_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
bigeast_ave_wins <- bigeast %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
bigeast_apps_wins <- left_join(bigeast_appearances, bigeast_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
bigeast_apps_wins <- bigeast_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
bigeast_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'big_east')
bigeast_plot <- ggplot(bigeast_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

bigten <- data_no_one_bid %>% 
  filter(confabbrev == 'big_ten')
bigten_appearances <- bigten %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
bigten_appearances <- left_join(bigten_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
bigten_ave_wins <-  bigten %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
bigten_apps_wins <- left_join(bigten_appearances, bigten_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
bigten_apps_wins <- bigten_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
bigten_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'big_ten')
bigten_plot <- ggplot(bigten_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

bigtwelve <- data_no_one_bid %>% 
  filter(confabbrev == 'big_twelve')
bigtwelve_appearances <- bigtwelve %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
bigtwelve_appearances <- left_join(bigtwelve_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
bigtwelve_ave_wins <-  bigtwelve %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
bigtwelve_apps_wins <- left_join(bigtwelve_appearances, bigtwelve_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
bigtwelve_apps_wins <- bigtwelve_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
bigtwelve_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'big_twelve')
bigtwelve_plot <- ggplot(bigtwelve_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

cusa <- data_no_one_bid %>% 
  filter(confabbrev == 'cusa')
cusa_appearances <- cusa %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
cusa_appearances <- left_join(cusa_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
cusa_ave_wins <-  cusa %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
cusa_apps_wins <- left_join(cusa_appearances, cusa_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
cusa_apps_wins <- cusa_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
cusa_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'cusa')
cusa_plot <- ggplot(cusa_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

mvc <- data_no_one_bid %>% 
  filter(confabbrev == 'mvc')
mvc_appearances <- mvc %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
mvc_appearances <- left_join(mvc_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
mvc_ave_wins <-  mvc %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
mvc_apps_wins <- left_join(mvc_appearances, mvc_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
mvc_apps_wins <- mvc_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
mvc_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'mvc')
mvc_plot <- ggplot(mvc_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

mwc <- data_no_one_bid %>% 
  filter(confabbrev == 'mwc')
mwc_appearances <- mwc %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
mwc_appearances <- left_join(mwc_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
mwc_ave_wins <-  mwc %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
mwc_apps_wins <- left_join(mwc_appearances, mwc_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
mwc_apps_wins <- mwc_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
mwc_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'mwc')
mwc_plot <- ggplot(mwc_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

pactwelve <- data_no_one_bid %>% 
  filter(confabbrev == 'pac_twelve')
pactwelve_appearances <- pactwelve %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
pactwelve_appearances <- left_join(pactwelve_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
pactwelve_ave_wins <-  pactwelve %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
pactwelve_apps_wins <- left_join(pactwelve_appearances, pactwelve_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
pactwelve_apps_wins <- pactwelve_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
pactwelve_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'pac_twelve')
pactwelve_plot <- ggplot(pactwelve_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

sec <- data_no_one_bid %>% 
  filter(confabbrev == 'sec')
sec_appearances <- sec %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
sec_appearances <- left_join(sec_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
sec_ave_wins <-  sec %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
sec_apps_wins <- left_join(sec_appearances, sec_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
sec_apps_wins <- sec_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
sec_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'sec')
sec_plot <- ggplot(sec_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")

wcc <- data_no_one_bid %>% 
  filter(confabbrev == 'wcc')
wcc_appearances <- wcc %>% 
  group_by(teamid) %>% 
  summarise(appearances = n()) %>% 
  arrange(desc(appearances))
wcc_appearances <- left_join(wcc_appearances, teams[, c('TeamID', 'TeamName')], by = c('teamid' = 'TeamID'))
wcc_ave_wins <-  wcc %>% 
  group_by(teamid) %>% 
  summarise(ave_ncaa_wins = mean(ncaatour_wins))
wcc_apps_wins <- left_join(wcc_appearances, wcc_ave_wins[, c('teamid', 'ave_ncaa_wins')], by = 'teamid')
wcc_apps_wins <- wcc_apps_wins[, c('TeamName', 'appearances', 'ave_ncaa_wins')]
wcc_wins <- ave_wins_confwins %>% 
  filter(confabbrev == 'wcc')
wcc_plot <- ggplot(wcc_wins, aes(x = conftour_wins, y = ave_ncaa_wins)) +
  geom_bar(aes(fill = conftour_wins), stat = 'identity') +
  xlab("Conference Tournament Wins") + 
  ylab("Average NCAA Tournament Wins") + 
  theme_bw() +
  theme(legend.position = "none")






