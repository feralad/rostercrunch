# Setup & Importing Data --------------------------------------------------

# install and load packages
install.packages("rmarkdown")

install.packages("tidyverse")

library(tidyverse)

library(readr)

# import main dataset
retro_final_PA_1990_2020d <-
  read_csv("~/Downloads/retro_final_PA_1990-2020d.csv")

# get dataset in more workable format
game_data <- as_tibble(retro_final_PA_1990_2020d)

# install and load more packages
install.packages(c("here","janitor","skimr"))

library(here)

library(janitor)

library(skimr)

# turn off scientific notation
options(scipen=999)

# Validating Data ---------------------------------------------------------

# explore data
skim_without_charts(game_data)

# isolate errors in the dataset
## five run events don't seem possible since only four runs can score at once
## this field will not be directly used for the analysis
five_run_events <- 
  as_tibble(game_data %>% 
              filter(EVENT_RUNS == "5"))

# import another dataset (wOBA weights) to help validate accuracy of main dataset
FanGraphs_wOBA_season_weights <-
  read_csv("Downloads/FanGraphs_wOBA_season_weights.csv",
  +     col_types = cols(Season = col_date(format = "%Y")))

# get dataset in more workable format
wOBA_weights <- as_tibble(FanGraphs_wOBA_season_weights)

# filter dataset for appropriate seasons
wOBA_weights_1990_2020 <- wOBA_weights %>% 
  filter(Season >= as.Date("1990-01-01") & Season <= as.Date("2020-01-01"))

# calculate min/max wOBA values to be used for data validation
wOBA_weights_1990_2020_min_max <- wOBA_weights_1990_2020 %>%
  summarise(
    MinBB = min(wBB),
    MaxBB = max(wBB),
    MinHBP = min(wHBP),
    MaxHBP = max(wHBP),
    Min1B = min(w1B),
    Max1B = max(w1B),
    Min2B = min(w2B),
    Max2B = max(w2B),
    Min3B = min(w3B),
    Max3B = max(w3B),
    MinHR = min(wHR),
    MaxHR = max(wHR),
  )

# validate that wOBA values don't fall outside corresponding HIT VALUE
game_data_wOBA_min_max <- game_data %>%
  group_by(HIT_VAL) %>%
  filter(EVENT_WOBA != 0) %>%
  summarise(
    Min = min(EVENT_WOBA),
    Max = max(EVENT_WOBA)
  )

# transform data to validate that HIT VALUES were properly coded
event_data <- as.tibble(game_data) %>%
  select("EVENT_TX", "HIT_VAL", "EVENT_WOBA") %>%
  mutate(first_char = substr(event_data$EVENT_TX, 1, 1)) %>%
  mutate(first_second_char = substr(event_data$EVENT_TX, 1, 2))

# validate that each EVENT has proper corresponding HIT VALUE and wOBA
event_data_grouped <- as.tibble(event_data) %>%
  group_by_at("first_char") %>%
  count(HIT_VAL)

# manually cycle through the different event types
event_data_calc <- event_data %>%
  filter(first_char == "W") %>%
  group_by_at("first_second_char") %>%
  count(HIT_VAL)

event_data_I <- event_data %>%
  filter(first_char == "W") %>%
  group_by(first_char) %>%
  summarise(
    Min = min(EVENT_WOBA),
    Max = max(EVENT_WOBA)
  )

# Importing & Cleaning Additional Dataset: Biographical Info --------------

# import dataset of biographical information, namely player date of birth
biofile <- read_csv("Downloads/biofile.csv", 
  col_types = cols(BIRTHDATE = col_date(format = "%m/%d/%Y"), 
      PLAY.DEBUT = col_date(format = "%m/%d/%Y"), 
      PLAY.LASTGAME = col_date(format = "%m/%d/%Y"), 
      MGR.DEBUT = col_date(format = "%m/%d/%Y"), 
      MGR.LASTGAME = col_date(format = "%m/%d/%Y"), 
      COACH.DEBUT = col_date(format = "%m/%d/%Y"), 
      COACH.LASTGAME = col_date(format = "%m/%d/%Y"), 
      UMP.DEBUT = col_date(format = "%m/%d/%Y"), 
      UMP.LASTGAME = col_date(format = "%m/%d/%Y"), 
      DEATHDATE = col_date(format = "%m/%d/%Y"), 
      HEIGHT = col_character()))

# detect problems with dataset (incomplete date of births)
problems(biofile)

# filter for players who debuted 1990 or later
current_players <- biofile %>%
  filter(PLAY.DEBUT >= as.Date('1990-01-01')) %>%
  arrange(desc(PLAY.LASTGAME))

# look for other issues with the dataset
skim_without_charts(current_players)

min(current_players$BIRTHDATE)

max(current_players$BIRTHDATE)

# fix blank LAST GAME data with a simple online search for one player
current_players_cleaned <- current_players %>%
  mutate(PLAY.LASTGAME = replace(PLAY.LASTGAME, 
                                 is.na(PLAY.LASTGAME), "2020-10-05")) %>%
  arrange(PLAY.LASTGAME)

# Preparing & Processing Data ---------------------------------------------

# sort data in order to apply ordered plate appearance numbers to each event
ordered_game_data <- game_data %>%
  filter(WOBA_APP == "TRUE") %>%
  arrange(ymd(DATE),row_idx)

# add column with career plate appearance and batters faced number
career_wOBA_log <- ordered_game_data %>%
  group_by(BAT_ID) %>%
  mutate(career_PA_wOBA_num = 1:n()) %>%
  mutate(career_total_PA_wOBA = max(career_PA_wOBA_num)) %>%
  group_by(PIT_ID) %>%
  mutate(career_BF_wOBA_num = 1:n()) %>%
  mutate(career_total_BF_wOBA = max(career_BF_wOBA_num))

# add Total Plate Appearances, Batters Faced, and Career wOBA columns
career_wOBA_totals_grouped_hit <- career_wOBA_log %>%
  group_by(BAT_ID) %>%
  summarise('TOTAL_PA_WOBA' = max(career_PA_wOBA_num),
            'CAREER_WOBA_BAT' = mean(EVENT_WOBA)) %>%
  ungroup()

career_wOBA_totals_grouped_pit <- career_wOBA_log %>%
  group_by(PIT_ID) %>%
  summarise('TOTAL_BF_WOBA' = max(career_BF_wOBA_num),
            'CAREER_WOBA_PIT' = mean(EVENT_WOBA)) %>%
  ungroup()

# distinguish batters from pitchers and vice versa
batter_careers <- career_wOBA_totals_grouped_hit %>%
  rename(PLAYER_ID = BAT_ID)

pitcher_careers <- career_wOBA_totals_grouped_pit %>%
  rename(PLAYER_ID = PIT_ID)

# calculate the ratio of plate appearances to batters faced
## this reveals whether the player is a batter or pitcher (or two-way)
batters_pitchers_joined <- 
  full_join(batter_careers, pitcher_careers, by="PLAYER_ID") %>%
  mutate(PA_BF_RATIO = TOTAL_PA_WOBA/TOTAL_BF_WOBA) %>%
  mutate(PA_BF_RATIO = round(PA_BF_RATIO, 2)) %>%
  mutate(across(c('CAREER_WOBA_BAT', 'CAREER_WOBA_PIT'), round, 3)) %>%
  mutate(PLAYER_TYPE = 
           case_when(is.na(TOTAL_BF_WOBA) ~ "Batter",
                     is.na(TOTAL_PA_WOBA) ~ "Pitcher",
                     PA_BF_RATIO < .8 ~ "Pitcher",
                     PA_BF_RATIO >= .8 & PA_BF_RATIO <= 4.25 ~ "Two-Way",
                     PA_BF_RATIO > 4.25 ~ "Batter"))

batters_with_filter_column <- batters_pitchers_joined %>%
  filter(TOTAL_PA_WOBA >= 1) %>%
  mutate(BATTER_IS_PITCHER = case_when(PLAYER_TYPE == "Pitcher" ~ TRUE,
                                       PLAYER_TYPE == "Two-Way" ~ FALSE,
                                       PLAYER_TYPE == "Batter" ~ FALSE)) %>%
  rename(BAT_ID = PLAYER_ID)

pitchers_with_filter_column <- batters_pitchers_joined %>%
  filter(TOTAL_BF_WOBA >= 1) %>%
  mutate(PITCHER_IS_BATTER = case_when(PLAYER_TYPE == "Pitcher" ~ FALSE,
                                       PLAYER_TYPE == "Two-Way" ~ FALSE,
                                       PLAYER_TYPE == "Batter" ~ TRUE)) %>%
  rename(PIT_ID = PLAYER_ID)

career_wOBA_log_joined <- batters_with_filter_column %>%
  select(BAT_ID, BATTER_IS_PITCHER) %>%
  merge(career_wOBA_log, batters_with_filter_column, 
        by.x = "BAT_ID", by.y = "BAT_ID") %>%
  relocate(BAT_ID, .after = BAT_HOME_IND) %>%
  relocate(BATTER_IS_PITCHER, .after = career_total_PA_wOBA) %>%
  arrange(ymd(DATE),row_idx)

career_wOBA_log_joined_2 <- pitchers_with_filter_column %>%
  select(PIT_ID, PITCHER_IS_BATTER) %>%
  merge(career_wOBA_log_joined, pitchers_with_filter_column, 
        by.x = "PIT_ID", by.y = "PIT_ID") %>%
  relocate(PIT_ID, .after = FIELD_POS) %>%
  relocate(PITCHER_IS_BATTER, .after = career_total_BF_wOBA) %>%
  arrange(ymd(DATE),row_idx)

# bring player birthdates, debut and last game dates into main dataset
current_players_only_dates <- current_players_cleaned %>%
  select(PLAYERID, BIRTHDATE, PLAY.DEBUT, PLAY.LASTGAME)

career_wOBA_log_w_biodates <- 
  left_join(career_wOBA_log_joined_2, 
            current_players_only_dates, 
            by=c("BAT_ID" = "PLAYERID")) %>%
  relocate(BIRTHDATE, .after = BAT_NAME) %>%
  relocate(PLAY.DEBUT, .after = BIRTHDATE) %>%
  relocate(PLAY.LASTGAME, .after = PLAY.DEBUT)

colnames(career_wOBA_log_w_biodates)[12:14] <- 
  c("BAT_BIRTHDATE", "BAT_DEBUT", "BAT_LASTGAME")
  
career_wOBA_log_w_biodates <-
  left_join(career_wOBA_log_w_biodates, 
            current_players_only_dates, 
            by=c("PIT_ID" = "PLAYERID")) %>%
  relocate(BIRTHDATE, .after = PIT_NAME) %>%
  relocate(PLAY.DEBUT, .after = BIRTHDATE) %>%
  relocate(PLAY.LASTGAME, .after = PLAY.DEBUT)

colnames(career_wOBA_log_w_biodates)[19:21] <- 
  c("PIT_BIRTHDATE", "PIT_DEBUT", "PIT_LASTGAME")

# need two columns, one for batter age at PA, one for pitcher age at BF
career_wOBA_log_w_biodates <- career_wOBA_log_w_biodates %>%
  mutate(BAT_AGE_AT_PA_WKS =
           as.numeric(difftime(DATE, BAT_BIRTHDATE, units = "weeks"))) %>%
  relocate(BAT_AGE_AT_PA_WKS, .after = BAT_LASTGAME) %>%
  mutate(BAT_AGE_AT_PA_YRS = BAT_AGE_AT_PA_WKS/52) %>%
  relocate(BAT_AGE_AT_PA_YRS, .after = BAT_AGE_AT_PA_WKS)

career_wOBA_log_w_biodates <- career_wOBA_log_w_biodates %>%
  mutate(PIT_AGE_AT_BF_WKS =
           as.numeric(difftime(DATE, PIT_BIRTHDATE, units = "weeks"))) %>%
  relocate(PIT_AGE_AT_BF_WKS, .after = PIT_LASTGAME) %>%
  mutate(PIT_AGE_AT_BF_YRS = PIT_AGE_AT_BF_WKS/52) %>%
  relocate(PIT_AGE_AT_BF_YRS, .after = PIT_AGE_AT_BF_WKS)

# Formatting & Transforming Data ------------------------------------------

# calculate rolling wOBA for batters
bat_wOBA_log_debut_1990_on <- career_wOBA_log_w_biodates %>%
  group_by(BAT_ID) %>%
  filter(BAT_DEBUT > "1990-04-08") %>%
  mutate(ROLLING_WOBA_BAT = cumsum(EVENT_WOBA) / seq_along(EVENT_WOBA)) %>%
  relocate(ROLLING_WOBA_BAT, .after = career_PA_wOBA_num) %>%
  ungroup()

# check to see if function is working properly for a batter with few PAs
bat_wOBA_log_debut_1990_on %>% 
  select(BAT_ID, HIT_VAL, EVENT_WOBA, career_PA_wOBA_num, ROLLING_WOBA_BAT) %>%
  filter(BAT_ID == "bellm001") %>%
  print(n = Inf)

# calculate rolling wOBA for pitchers
pit_wOBA_log_debut_1990_on <- career_wOBA_log_w_biodates %>%
  group_by(PIT_ID) %>%
  filter(PIT_DEBUT > "1990-04-08") %>%
  mutate(ROLLING_WOBA_PIT = cumsum(EVENT_WOBA) / seq_along(EVENT_WOBA)) %>%
  relocate(ROLLING_WOBA_PIT, .after = career_BF_wOBA_num) %>%
  ungroup()

# create a column with career wOBA
bat_wOBA_log_debut_1990_on <- bat_wOBA_log_debut_1990_on %>%
  group_by(BAT_ID) %>%
  mutate(CAREER_WOBA_BAT = mean(EVENT_WOBA)) %>%
  relocate(CAREER_WOBA_BAT, .after = ROLLING_WOBA_BAT) %>%
  ungroup()

pit_wOBA_log_debut_1990_on <- pit_wOBA_log_debut_1990_on %>%
  group_by(PIT_ID) %>%
  mutate(CAREER_WOBA_PIT = mean(EVENT_WOBA)) %>%
  relocate(CAREER_WOBA_PIT, .after = ROLLING_WOBA_PIT) %>%
  ungroup()

# filter out pitcher plate appearances
bat_wOBA_log_debut_1990_on_no_pit <- bat_wOBA_log_debut_1990_on %>%
  filter(BATTER_IS_PITCHER == "FALSE")

# create summary dataframe for all batters (players who debuted 1990 or later)
grouped_bat_1990_on_no_pit <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  summarise(BAT_ID = last(BAT_ID),
            BAT_NAME = last(BAT_NAME),
            BAT_DEBUT = last(BAT_DEBUT),
            BAT_LASTGAME = last(BAT_LASTGAME),
            CAREER_WOBA_BAT = last(CAREER_WOBA_BAT),
            career_total_PA_wOBA = last(career_total_PA_wOBA))

# filter out batter pitching appearances
pit_wOBA_log_debut_1990_on_no_bat <- pit_wOBA_log_debut_1990_on %>%
  filter(PITCHER_IS_BATTER == "FALSE")

# create summary dataframe for all pitchers (players who debuted 1990 or later)
grouped_pit_1990_on_no_bat <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  summarise(PIT_ID = last(PIT_ID),
            PIT_NAME = last(PIT_NAME),
            PIT_DEBUT = last(PIT_DEBUT),
            PIT_LASTGAME = last(PIT_LASTGAME),
            CAREER_WOBA_PIT = last(CAREER_WOBA_PIT),
            CAREER_TOTAL_BF = last(career_total_BF_wOBA),
            AVG_PIT_COUNT = mean(PITCH_COUNT_FINAL),
            IP_MIN = min(INNING),
            AVG_DAYS_REST = mean(PIT_REST),
            IP_MEAN = mean(INNING))

# calculate wOBA at specific plate appearance milestones
quartile_one_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 1:150) %>%
  summarise(QUARTILE_ONE_WOBA = mean(EVENT_WOBA),
            QUARTILE_ONE_AGE = mean(BAT_AGE_AT_PA_YRS))

quartile_two_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 151:300) %>%
  summarise(QUARTILE_TWO_WOBA = mean(EVENT_WOBA),
            QUARTILE_TWO_AGE = mean(BAT_AGE_AT_PA_YRS))

quartile_three_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 301:450) %>%
  summarise(QUARTILE_THREE_WOBA = mean(EVENT_WOBA),
            QUARTILE_THREE_AGE = mean(BAT_AGE_AT_PA_YRS))

quartile_four_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 451:600) %>%
  summarise(QUARTILE_FOUR_WOBA = mean(EVENT_WOBA),
            QUARTILE_FOUR_AGE = mean(BAT_AGE_AT_PA_YRS))

quartiles_combined_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 1:600) %>%
  summarise(FIRST_600_WOBA = mean(EVENT_WOBA),
            FIRST_600_MEAN_AGE = mean(BAT_AGE_AT_PA_YRS))

post_150_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num > 150) %>%
  summarise(POST_150_WOBA = mean(EVENT_WOBA))

post_600_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num > 600) %>%
  summarise(POST_600_WOBA = mean(EVENT_WOBA))

all_batter_career_data_by_ID <- grouped_bat_1990_on_no_pit %>%
  left_join(quartile_one_woba_bat, by = "BAT_ID") %>%
  left_join(quartile_two_woba_bat, by = "BAT_ID") %>%
  left_join(quartile_three_woba_bat, by = "BAT_ID") %>%
  left_join(quartile_four_woba_bat, by = "BAT_ID") %>%
  left_join(quartiles_combined_woba_bat, by = "BAT_ID") %>%
  left_join(post_150_woba_bat, by = "BAT_ID") %>%
  left_join(post_600_woba_bat, by = "BAT_ID")

# calculate wOBA at additional milestones
## realized I needed these later on
first_300_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 1:300) %>%
  summarise(FIRST_300_WOBA = mean(EVENT_WOBA))

first_450_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 1:450) %>%
  summarise(FIRST_450_WOBA = mean(EVENT_WOBA))

first_900_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num %in% 1:900) %>%
  summarise(FIRST_900_WOBA = mean(EVENT_WOBA))

post_300_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num > 300) %>%
  summarise(POST_300_WOBA = mean(EVENT_WOBA))

post_450_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num > 450) %>%
  summarise(POST_450_WOBA = mean(EVENT_WOBA))

post_900_woba_bat <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>%
  filter(career_PA_wOBA_num > 900) %>%
  summarise(POST_900_WOBA = mean(EVENT_WOBA))

all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  left_join(first_300_woba_bat, by = "BAT_ID") %>%
  left_join(first_450_woba_bat, by = "BAT_ID") %>% 
  left_join(first_900_woba_bat, by = "BAT_ID") %>% 
  left_join(post_300_woba_bat, by = "BAT_ID") %>%
  left_join(post_450_woba_bat, by = "BAT_ID") %>% 
  left_join(post_900_woba_bat, by = "BAT_ID") 

all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  relocate(FIRST_300_WOBA, .after = QUARTILE_FOUR_AGE) %>%
  relocate(FIRST_450_WOBA, .after = FIRST_300_WOBA) %>%
  relocate(POST_300_WOBA, .after = FIRST_POST_150_WOBA_DIFF) %>%
  relocate(POST_450_WOBA, .after = POST_300_WOBA) %>% 
  relocate(FIRST_900_WOBA, .before = POST_150_WOBA) %>% 
  relocate(POST_900_WOBA, .before = PLAYER_STATUS)

# round columns in batter dataset
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(across(c("CAREER_WOBA_BAT", "QUARTILE_ONE_WOBA", "QUARTILE_TWO_WOBA", 
                  "QUARTILE_THREE_WOBA", "QUARTILE_FOUR_WOBA", "FIRST_300_WOBA", 
                  "FIRST_450_WOBA", "FIRST_600_WOBA", "POST_150_WOBA", 
                  "FIRST_POST_150_WOBA_DIFF", "POST_300_WOBA", "POST_450_WOBA", 
                  "POST_600_WOBA", "FIRST_POST_600_WOBA_DIFF", "FIRST_900_WOBA", 
                  "POST_900_WOBA"), round, 3))

# round age columns - one decimal
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(across(c("QUARTILE_ONE_AGE", "QUARTILE_TWO_AGE",
                  "QUARTILE_THREE_AGE", "QUARTILE_FOUR_AGE",
                  "FIRST_600_MEAN_AGE"), round, 1))

# create batter wOBA "grades" columns for 150 PA, Post 150, 600, Post 600

# first ensure batters who didn't reach thresholds show up as Did Not Qualify
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(FIRST_150_WOBA_GRADE = case_when(CAREER_TOTAL_PA < 150 ~ 0)) %>%
  relocate(FIRST_150_WOBA_GRADE, .after = QUARTILE_ONE_WOBA)

all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(FIRST_600_WOBA_GRADE = case_when(CAREER_TOTAL_PA < 600 ~ 0)) %>%
  relocate(FIRST_600_WOBA_GRADE, .after = FIRST_600_WOBA)

all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(POST_150_WOBA_GRADE = case_when(CAREER_TOTAL_PA < 600 ~ 0)) %>%
  relocate(POST_150_WOBA_GRADE, .after = POST_150_WOBA)

# threshold here set for 3x initial plate appearances (600 x 3 = 1800)
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(POST_600_WOBA_GRADE = case_when(CAREER_TOTAL_PA < 1800 ~ 0)) %>%
  relocate(POST_600_WOBA_GRADE, .after = POST_600_WOBA)

# first 150 PA
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(FIRST_150_WOBA_GRADE = case_when(
    FIRST_150_WOBA_GRADE == 0 ~ "DNQ",
    QUARTILE_ONE_WOBA >= .400 ~ "Elite",
    between(QUARTILE_ONE_WOBA, .370, .399) ~ "Great",
    between(QUARTILE_ONE_WOBA, .340, .369) ~ "Good",
    between(QUARTILE_ONE_WOBA, .320, .339) ~ "Average",
    between(QUARTILE_ONE_WOBA, .310, .319) ~ "Below Average",
    between(QUARTILE_ONE_WOBA, .291, .309) ~ "Poor",
    QUARTILE_ONE_WOBA <= .290 ~ "Awful"),
    FIRST_150_WOBA_GRADE = factor(FIRST_150_WOBA_GRADE, levels = 
                                  c("Elite", "Great", "Good", "Average",
                                    "Below Average", "Poor", "Awful", "DNQ")))

# first 600 PA
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(FIRST_600_WOBA_GRADE = case_when(
    FIRST_600_WOBA_GRADE == 0 ~ "DNQ",
    FIRST_600_WOBA >= .400 ~ "Elite",
    between(FIRST_600_WOBA, .370, .399) ~ "Great",
    between(FIRST_600_WOBA, .340, .369) ~ "Good",
    between(FIRST_600_WOBA, .320, .339) ~ "Average",
    between(FIRST_600_WOBA, .310, .319) ~ "Below Average",
    between(FIRST_600_WOBA, .291, .309) ~ "Poor",
    FIRST_600_WOBA <= .290 ~ "Awful"),
    FIRST_600_WOBA_GRADE = factor(FIRST_600_WOBA_GRADE, levels = 
                                  c("Elite", "Great", "Good", "Average",
                                    "Below Average", "Poor", "Awful", "DNQ")))

# post 150 PA
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(POST_150_WOBA_GRADE = case_when(
    POST_150_WOBA_GRADE == 0 ~ "DNQ",
    POST_150_WOBA >= .400 ~ "Elite",
    between(POST_150_WOBA, .370, .399) ~ "Great",
    between(POST_150_WOBA, .340, .369) ~ "Good",
    between(POST_150_WOBA, .320, .339) ~ "Average",
    between(POST_150_WOBA, .310, .319) ~ "Below Average",
    between(POST_150_WOBA, .291, .309) ~ "Poor",
    POST_150_WOBA <= .290 ~ "Awful"),
    POST_150_WOBA_GRADE = factor(POST_150_WOBA_GRADE, levels =
                                  c("Elite", "Great", "Good", "Average",
                                    "Below Average", "Poor", "Awful", "DNQ")))

# post 600 PA
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(POST_600_WOBA_GRADE = case_when(
    POST_600_WOBA_GRADE == 0 ~ "DNQ",
    POST_600_WOBA >= .400 ~ "Elite",
    between(POST_600_WOBA, .370, .399) ~ "Great",
    between(POST_600_WOBA, .340, .369) ~ "Good",
    between(POST_600_WOBA, .320, .339) ~ "Average",
    between(POST_600_WOBA, .310, .319) ~ "Below Average",
    between(POST_600_WOBA, .291, .309) ~ "Poor",
    POST_600_WOBA <= .290 ~ "Awful"),
    POST_600_WOBA_GRADE = factor(POST_600_WOBA_GRADE, levels =
                                   c("Elite", "Great", "Good", "Average",
                                     "Below Average", "Poor", "Awful", "DNQ")))

# create column to distinguish active and inactive batters as of end of dataset
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(PLAYER_STATUS = case_when
         (BAT_LASTGAME > as.Date("2020-09-28") ~ "ACTIVE",
          BAT_LASTGAME < as.Date("2020-09-28") ~ "INACTIVE"))

# create column to distinguish active and inactive pitchers
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(PLAYER_STATUS = case_when
         (PIT_LASTGAME > as.Date("2020-09-28") ~ "ACTIVE",
           PIT_LASTGAME < as.Date("2020-09-28") ~ "INACTIVE"))

# creating column to distinguish starters and relievers
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(PITCHER_TYPE = case_when
         (AVG_PIT_COUNT >= 70 ~ "STARTER",
           AVG_PIT_COUNT <= 40 ~ "RELIEVER",
           IP_MIN > 1 ~ "RELIEVER",
           IP_MEAN > 5.5 ~ "RELIEVER",
           (is.na(PITCHER_TYPE) ~ "OTHER")))

# rename column
colnames(all_batter_career_data_by_ID)[6] = "CAREER_TOTAL_PA"

# first let's see the equivalency of batters faced and innings pitched
## import dataset from Fangraphs
## includes all pitchers (not just ones that debuted 1990 or later)
Pitchers_1990_2020_Fangraphs_Leaderboard <- 
  read_csv("Original Datasets/Pitchers_1990_2020_Fangraphs_Leaderboard.csv")

TBF_SUM <- sum(Pitchers_1990_2020_Fangraphs_Leaderboard$TBF)

IP_SUM <- sum(Pitchers_1990_2020_Fangraphs_Leaderboard$IP)

# calculate the average batters faced per inning
AVG_BF_PER_IP <- TBF_SUM/IP_SUM

# set batters faced thresholds for pitchers to roughly match batter thresholds

BF_50_IP_AVG <- AVG_BF_PER_IP*50

BF_200_IP_AVG <- AVG_BF_PER_IP*200

# calculate pitcher wOBA against/allowed at specific milestones
quartile_one_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 1:215) %>%
  summarise(QUARTILE_ONE_WOBA = mean(EVENT_WOBA),
            QUARTILE_ONE_AGE = mean(PIT_AGE_AT_BF_YRS))

quartile_two_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 216:430) %>%
  summarise(QUARTILE_TWO_WOBA = mean(EVENT_WOBA),
            QUARTILE_TWO_AGE = mean(PIT_AGE_AT_BF_YRS))

quartile_three_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 431:645) %>%
  summarise(QUARTILE_THREE_WOBA = mean(EVENT_WOBA),
            QUARTILE_THREE_AGE = mean(PIT_AGE_AT_BF_YRS))

quartile_four_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 646:860) %>%
  summarise(QUARTILE_FOUR_WOBA = mean(EVENT_WOBA),
            QUARTILE_FOUR_AGE = mean(PIT_AGE_AT_BF_YRS))

quartiles_combined_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 1:860) %>%
  summarise(FIRST_860_WOBA = mean(EVENT_WOBA),
            FIRST_860_MEAN_AGE = mean(PIT_AGE_AT_BF_YRS))

post_215_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num > 215) %>%
  summarise(POST_215_WOBA = mean(EVENT_WOBA))

post_860_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num > 860) %>%
  summarise(POST_860_WOBA = mean(EVENT_WOBA))

all_pitcher_career_data_by_ID <- grouped_pit_1990_on_no_bat %>%
  left_join(quartile_one_woba_pit, by = "PIT_ID") %>%
  left_join(quartile_two_woba_pit, by = "PIT_ID") %>%
  left_join(quartile_three_woba_pit, by = "PIT_ID") %>%
  left_join(quartile_four_woba_pit, by = "PIT_ID") %>%
  left_join(quartiles_combined_woba_pit, by = "PIT_ID") %>%
  left_join(post_215_woba_pit, by = "PIT_ID") %>%
  left_join(post_860_woba_pit, by = "PIT_ID")

# calculating wOBA against at additional milestones
## realized I needed these later on
first_430_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 1:430) %>%
  summarise(FIRST_430_WOBA = mean(EVENT_WOBA))

first_645_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 1:645) %>%
  summarise(FIRST_645_WOBA = mean(EVENT_WOBA))

first_1290_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num %in% 1:1290) %>%
  summarise(FIRST_1290_WOBA = mean(EVENT_WOBA))

post_430_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num > 430) %>%
  summarise(POST_430_WOBA = mean(EVENT_WOBA))

post_645_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num > 645) %>%
  summarise(POST_645_WOBA = mean(EVENT_WOBA))

post_1290_woba_pit <- pit_wOBA_log_debut_1990_on_no_bat %>%
  group_by(PIT_ID) %>%
  filter(career_BF_wOBA_num > 1290) %>%
  summarise(POST_1290_WOBA = mean(EVENT_WOBA))

all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  left_join(first_430_woba_pit, by = "PIT_ID") %>%
  left_join(first_645_woba_pit, by = "PIT_ID") %>%
  left_join(post_430_woba_pit, by = "PIT_ID") %>%
  left_join(post_645_woba_pit, by = "PIT_ID") %>% 
  left_join(first_1290_woba_pit, by = "PIT_ID") %>%
  left_join(post_1290_woba_pit, by = "PIT_ID")

all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  relocate(FIRST_430_WOBA, .after = QUARTILE_FOUR_AGE) %>%
  relocate(FIRST_645_WOBA, .after = FIRST_430_WOBA) %>%
  relocate(POST_430_WOBA, .after = FIRST_POST_215_WOBA_DIFF) %>%
  relocate(POST_645_WOBA, .after = POST_430_WOBA) %>% 
  relocate(FIRST_1290_WOBA, .before = POST_215_WOBA) %>% 
  relocate(POST_1290_WOBA, .before = PLAYER_STATUS)

# round columns in pitcher dataset
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(across(c("CAREER_WOBA_PIT", "QUARTILE_ONE_WOBA", "QUARTILE_TWO_WOBA", 
                  "QUARTILE_THREE_WOBA", "QUARTILE_FOUR_WOBA", "FIRST_430_WOBA", 
                  "FIRST_645_WOBA", "FIRST_860_WOBA", "POST_215_WOBA", 
                  "FIRST_POST_215_WOBA_DIFF", "POST_430_WOBA", "POST_645_WOBA", 
                  "POST_860_WOBA", "FIRST_POST_860_WOBA_DIFF", "FIRST_1290_WOBA", 
                  "POST_1290_WOBA"), round, 3))

# round age columns - one decimal
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(across(c("AVG_PIT_COUNT", "AVG_DAYS_REST", "IP_MEAN", 
                  "QUARTILE_ONE_AGE", "QUARTILE_TWO_AGE",
                  "QUARTILE_THREE_AGE", "QUARTILE_FOUR_AGE",
                  "FIRST_860_MEAN_AGE"), round, 1))

# create pitcher wOBA "grades" columns for 215 BF, Post 215, 860, Post 860
## the scale is an inverted scale of batter wOBA (wOBA-against or wOBA-allowed)

# ensure pitchers who didn't reach thresholds equal Did Not Qualify
# ensure non-SP pitchers who are out-of-scope for certain grades equal NA
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(FIRST_215_WOBA_GRADE = case_when(CAREER_TOTAL_BF < 215 ~ 0)) %>%
  relocate(FIRST_215_WOBA_GRADE, .after = QUARTILE_ONE_WOBA)

## -100 will become NA
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(FIRST_860_WOBA_GRADE_SP = case_when(
    PITCHER_TYPE == "STARTER" & CAREER_TOTAL_BF < 860 ~ 0,
    PITCHER_TYPE == "RELIEVER" ~ -100, 
    PITCHER_TYPE == "OTHER" ~ -100)) %>%
  relocate(FIRST_860_WOBA_GRADE_SP, .after = FIRST_860_WOBA)

# set different batters faced thresholds for starters and non-starters
## threshold here set at 860 for starters, 430 for non-starters
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(POST_215_WOBA_GRADE_SP = case_when(
    PITCHER_TYPE == "STARTER" & CAREER_TOTAL_BF < 860 ~ 0,
    PITCHER_TYPE == "RELIEVER" ~ -100, 
    PITCHER_TYPE == "OTHER" ~ -100)) %>% 
  mutate(POST_215_WOBA_GRADE_NON_SP = case_when(
    PITCHER_TYPE == "STARTER" ~ -100, 
    PITCHER_TYPE == "RELIEVER" & CAREER_TOTAL_BF < 430 ~ 0,
    PITCHER_TYPE == "OTHER" & CAREER_TOTAL_BF < 430 ~ 0)) %>%
  relocate(POST_215_WOBA_GRADE_SP, .after = POST_215_WOBA) %>%
  relocate(POST_215_WOBA_GRADE_NON_SP, .after = POST_215_WOBA_GRADE_SP)

# threshold here set at 2580 for starters, non-starters will be NA
## 2580 is 860 * 3, which aligns with batters (600 * 3 = 1800)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(POST_860_WOBA_GRADE_SP = case_when(
    PITCHER_TYPE == "STARTER" & CAREER_TOTAL_BF < 2580 ~ 0,
    PITCHER_TYPE == "RELIEVER" ~ -100, 
    PITCHER_TYPE == "OTHER" ~ -100)) %>% 
  relocate(POST_860_WOBA_GRADE_SP, .after = POST_860_WOBA)

# first 215 BF
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(FIRST_215_WOBA_GRADE = case_when(
    FIRST_215_WOBA_GRADE == 0 ~ "DNQ", 
    QUARTILE_ONE_WOBA <= .290 ~ "Elite", 
    between(QUARTILE_ONE_WOBA, .291, .309) ~ "Great", 
    between(QUARTILE_ONE_WOBA, .310, .319) ~ "Good", 
    between(QUARTILE_ONE_WOBA, .320, .339) ~ "Average", 
    between(QUARTILE_ONE_WOBA, .340, .369) ~ "Below Average", 
    between(QUARTILE_ONE_WOBA, .370, .399) ~ "Poor", 
    QUARTILE_ONE_WOBA >= .400 ~ "Awful"),
    FIRST_215_WOBA_GRADE = factor(FIRST_215_WOBA_GRADE, levels = 
                                    c("Elite", "Great", "Good", "Average", 
                                      "Below Average", "Poor", "Awful", "DNQ")))

# first 860 BF (starters)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(FIRST_860_WOBA_GRADE_SP = case_when(
    FIRST_860_WOBA_GRADE_SP == -100 ~ NA_character_,
    FIRST_860_WOBA_GRADE_SP == 0 ~ "DNQ", 
    FIRST_860_WOBA <= .290 ~ "Elite", 
    between(FIRST_860_WOBA, .291, .309) ~ "Great", 
    between(FIRST_860_WOBA, .310, .319) ~ "Good", 
    between(FIRST_860_WOBA, .320, .339) ~ "Average", 
    between(FIRST_860_WOBA, .340, .369) ~ "Below Average", 
    between(FIRST_860_WOBA, .370, .399) ~ "Poor", 
    FIRST_860_WOBA >= .400 ~ "Awful"),
    FIRST_860_WOBA_GRADE_SP = factor(FIRST_860_WOBA_GRADE_SP, levels = 
                                    c("Elite", "Great", "Good", "Average", 
                                      "Below Average", "Poor", "Awful", "DNQ")))

# post 215 BF (starters)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(POST_215_WOBA_GRADE_SP = case_when(
    POST_215_WOBA_GRADE_SP == -100 ~ NA_character_,
    POST_215_WOBA_GRADE_SP == 0 ~ "DNQ", 
    POST_215_WOBA <= .290 ~ "Elite", 
    between(POST_215_WOBA, .291, .309) ~ "Great", 
    between(POST_215_WOBA, .310, .319) ~ "Good", 
    between(POST_215_WOBA, .320, .339) ~ "Average", 
    between(POST_215_WOBA, .340, .369) ~ "Below Average", 
    between(POST_215_WOBA, .370, .399) ~ "Poor", 
    POST_215_WOBA >= .400 ~ "Awful"),
    POST_215_WOBA_GRADE_SP = factor(POST_215_WOBA_GRADE_SP, levels = 
                                    c("Elite", "Great", "Good", "Average", 
                                      "Below Average", "Poor", "Awful", "DNQ")))

# post 215 BF (non-starters)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(POST_215_WOBA_GRADE_NON_SP = case_when(
    POST_215_WOBA_GRADE_NON_SP == -100 ~ NA_character_,
    POST_215_WOBA_GRADE_NON_SP == 0 ~ "DNQ", 
    POST_215_WOBA <= .290 ~ "Elite", 
    between(POST_215_WOBA, .291, .309) ~ "Great", 
    between(POST_215_WOBA, .310, .319) ~ "Good", 
    between(POST_215_WOBA, .320, .339) ~ "Average", 
    between(POST_215_WOBA, .340, .369) ~ "Below Average", 
    between(POST_215_WOBA, .370, .399) ~ "Poor", 
    POST_215_WOBA >= .400 ~ "Awful"),
    POST_215_WOBA_GRADE_NON_SP = factor(POST_215_WOBA_GRADE_NON_SP, levels = 
                                    c("Elite", "Great", "Good", "Average", 
                                      "Below Average", "Poor", "Awful", "DNQ")))

# post 860 BF (starters)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(POST_860_WOBA_GRADE_SP = case_when(
    POST_860_WOBA_GRADE_SP == -100 ~ NA_character_,
    POST_860_WOBA_GRADE_SP == 0 ~ "DNQ", 
    POST_860_WOBA <= .290 ~ "Elite", 
    between(POST_860_WOBA, .291, .309) ~ "Great", 
    between(POST_860_WOBA, .310, .319) ~ "Good", 
    between(POST_860_WOBA, .320, .339) ~ "Average", 
    between(POST_860_WOBA, .340, .369) ~ "Below Average", 
    between(POST_860_WOBA, .370, .399) ~ "Poor", 
    POST_860_WOBA >= .400 ~ "Awful"),
    POST_860_WOBA_GRADE_SP = factor(POST_860_WOBA_GRADE_SP, levels = 
                                    c("Elite", "Great", "Good", "Average", 
                                      "Below Average", "Poor", "Awful", "DNQ")))

# calculate batter disparities between initial 600 PA & post 600 wOBA
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(FIRST_POST_600_WOBA_DIFF = POST_600_WOBA-FIRST_600_WOBA) %>%
  relocate(FIRST_POST_600_WOBA_DIFF, .after = POST_600_WOBA)

# calculate batter disparities between initial 150 PA & post 150 wOBA
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(FIRST_POST_150_WOBA_DIFF = POST_150_WOBA-QUARTILE_ONE_WOBA) %>%
  relocate(FIRST_POST_150_WOBA_DIFF, .after = POST_150_WOBA)

# calculate pitcher disparities between initial 860 BF & post 860 wOBA
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(FIRST_POST_860_WOBA_DIFF = POST_860_WOBA-FIRST_860_WOBA) %>%
  relocate(FIRST_POST_860_WOBA_DIFF, .after = POST_860_WOBA)

# calculate pitcher disparities between initial 215 BF & post 215 wOBA
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(FIRST_POST_215_WOBA_DIFF = POST_215_WOBA-QUARTILE_ONE_WOBA) %>%
  relocate(FIRST_POST_215_WOBA_DIFF, .after = POST_215_WOBA)

# Importing & Cleaning Additional Dataset: Prospect Ranks  ----------------

# Goal: Create column(s) in summary dataframes to display players' prospect rank

# Challenge: Prospect rank dataset is not clean and does not contain player IDs

# first check for duplicate names in current players dataframe
## this step should have been done previously with the biofile dataframe
### hard-coded prospect ranks for these duplicate name players prior to importing
duplicate_name_players <- current_players_cleaned %>%
  select(PLAYERID, NICKNAME, LAST, PLAY.DEBUT) %>%
  unite(col = "NAME", c("NICKNAME", "LAST"), sep = " ") %>%
  group_by(NAME) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  print(n = Inf)

# import prospect dataset
BA_Top_100_Prospects_1990_2020_Prospects_Raw <- read_csv(
  "Original Datasets/BA_Top_100_Prospects_1990_2020 - Prospects_Raw.csv",
    col_types = cols(POSITION_4 = col_double(), POSITION_5 = col_double()))

# arrange alphabetically
top_prospect_rankings <- BA_Top_100_Prospects_1990_2020_Prospects_Raw %>%
  arrange(PLAYER_NAME)

# manually check for spelling errors
top_prospect_rankings %>%
  group_by(PLAYER_NAME) %>%
  summarize(count = n()) %>%
  print(n = Inf)

# check class of objects
str(top_prospect_rankings)

# change two misspellings (fun players!): Carlos Gonzales(z), Miguel Teje(a)da
# change all caps TEAM to Team
# change YEAR (Number class) to year with date (Date class)
top_prospect_rankings <- top_prospect_rankings %>%
  mutate("PLAYER_NAME" = str_replace(PLAYER_NAME, "Carlos Gonzales", 
                                    "Carlos Gonzalez"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Miguel Tejeda",
                                     "Miguel Tejada"),
         "TEAM" = str_to_title(top_prospect_rankings$TEAM),
         "YEAR" = lubridate::ymd(YEAR, truncated = 2))
  
# bring ID over from summary dataframes to prospect dataframe for players w/o ID
## in hindsight, I should have brought this over from retrosheet's biofile
top_prospect_rankings_2 <- top_prospect_rankings %>%
  filter(is.na(ID)) %>%
  left_join(select(all_batter_career_data_by_ID, BAT_ID,
                   BAT_NAME), by = c("PLAYER_NAME" = "BAT_NAME"),
            relationship = "many-to-many") %>%
  mutate(ID = coalesce(ID, BAT_ID)) %>%
  left_join(select(all_pitcher_career_data_by_ID, PIT_ID,
                    PIT_NAME), by = c("PLAYER_NAME" = "PIT_NAME"),
             relationship = "many-to-many") %>%
  mutate(ID = coalesce(ID, PIT_ID))

# some players are still without an ID because...
## some prospects debuted before 1990 and after 2021 or never debuted in MLB
## names didn't match (spelling, punctuation, etc.)
### how many players?
top_prospect_rankings_2 %>% 
  filter(is.na(ID)) %>%
  group_by(PLAYER_NAME) %>%
  summarize(count = n()) %>%
  print(n = Inf)

# compare this list to biofile to filter out players w/debuts outside 1990/2020
# with remaining players withouth ID, hard-code to verify names are spelled right

# create subset of biofile with less columns 
biofile_prospects <- biofile %>%
  select(PLAYERID, NICKNAME, LAST, PLAY.DEBUT) %>%
  unite(col = "PLAYER_NAME", c("NICKNAME", "LAST"), sep = " ") %>%
  arrange(PLAYER_NAME)

# create another subset of biofile for only players who debuted in 2021 or later
biofile_new_prospects <- biofile_prospects %>%
  filter(PLAY.DEBUT > as.Date('2021-01-01')) %>%
  select(PLAYERID, PLAYER_NAME, PLAY.DEBUT)

# join prospect rankings with biofile of players who debuted in 2021 or later
top_prospect_rankings_3 <- top_prospect_rankings_2 %>%
  filter(is.na(ID)) %>%
  left_join(select(biofile_new_prospects, PLAYERID,
                   PLAYER_NAME), by = "PLAYER_NAME") %>%
  mutate(ID = coalesce(ID, PLAYERID))

# create updated list of prospects with no player ID
top_prospect_rankings_3 %>%
  filter(is.na(ID)) %>%
  select(ID, PLAYER_NAME) %>%
  group_by(PLAYER_NAME) %>%
  summarize(count = n()) %>%
  print(n = Inf)

# hard-code names that had spelling or capitalization issues or differences
top_prospect_rankings_4 <- top_prospect_rankings_3 %>%
  filter(is.na(ID)) %>%
  mutate("PLAYER_NAME" = str_replace(PLAYER_NAME, "Adam Laroche", 
                                     "Adam LaRoche"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Albert Almora",
                                     "Albert Almora Jr."),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Alexis Rios",
                                     "Alex Rios"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Andy Laroche", 
                                     "Andy LaRoche"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Andy Oliver",
                                     "Andrew Oliver"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Andy Sisco",
                                     "Andrew Sisco"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Angelo Jimenez", 
                                     "D'Angelo Jimenez"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Blake Dewitt",
                                     "Blake DeWitt"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "CJ Abrams",
                                     "C.J. Abrams"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "CJ Nitkowski", 
                                     "C.J. Nitkowski"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Dan Hudson",
                                     "Daniel Hudson"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Daniel Cortes",
                                     "Dan Cortes"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Den Peltier",
                                     "Dan Peltier"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Dennis Reyes", 
                                     "Dennys Reyes"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Dermal Brown",
                                     "Dee Brown"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Donald Veal",
                                     "Donnie Veal"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Edison Volquez",
                                     "Edinson Volquez"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Eric Dubose", 
                                     "Eric DuBose"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Frank Rodriguez",
                                     "Frankie Rodriguez"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Garrett Anderson",
                                     "Garret Anderson"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Jacob Peavy",
                                     "Jake Peavy"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Jennry Mejia", 
                                     "Jenrry Mejia"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "John Vanbenschoten",
                                     "John Van Benschoten"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Johnvan Benschoten",
                                     "John Van Benschoten"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Jon Vanbenschoten",
                                     "John Van Benschoten"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Jon Hurst",
                                     "Jonathan Hurst"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Jon Papelbon",
                                     "Jonathan Papelbon"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Jonathan Singleton", 
                                     "Jon Singleton"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Joshua James",
                                     "Josh James"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Kendry Morales",
                                     "Kendrys Morales"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Latroy Hawkins",
                                     "LaTroy Hawkins"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Mackenzie Gore", 
                                     "MacKenzie Gore"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Makato Suzuki",
                                     "Mac Suzuki"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Manuel Aybar",
                                     "Manny Aybar"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Matt Laporta",
                                     "Matt LaPorta"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Matt Lecroy", 
                                     "Matt LeCroy"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Maurice Vaughn",
                                     "Mo Vaughn"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Mike Macdougal",
                                     "Mike MacDougal"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Philip Hughes",
                                     "Phil Hughes"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Ron Belliard",
                                     "Ronnie Belliard"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Russ Branyan",
                                     "Russell Branyan"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Shawn Gree", 
                                     "Shawn Green"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Stephen Cooke",
                                     "Steve Cooke"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Sun Woo Kim",
                                     "Sun-Woo Kim"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Tom Howard", 
                                     "Thomas Howard"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Travis D'Arnaud",
                                     "Travis d'Arnaud"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Wilfredo Cordero",
                                     "Wil Cordero"),
         "PLAYER_NAME" = str_replace(PLAYER_NAME, "Zach Britton",
                                     "Zack Britton")
  )

# eliminate duplicate ID fields from joins
top_prospect_rankings_4 <- top_prospect_rankings_4 %>%
  select(-PLAYERID)

top_prospect_rankings_2 <- top_prospect_rankings_2 %>%
  select(-c(BAT_ID, PIT_ID))

top_prospect_rankings_3 <- top_prospect_rankings_3 %>%
  select(-c(BAT_ID, PIT_ID, PLAYERID))

# create another subset of biofile to capture players who debuted in late 80s
biofile_1987_on <- biofile_prospects %>%
  filter(PLAY.DEBUT > as.Date('1987-01-01')) %>%
  select(PLAYERID, PLAYER_NAME, PLAY.DEBUT)

top_prospect_rankings_4 <- top_prospect_rankings_4 %>%
  left_join(select(biofile_1987_on, PLAYERID,
                   PLAYER_NAME), by = "PLAYER_NAME") %>%
  mutate(ID = coalesce(ID, PLAYERID))

# hard-code remaining IDs that weren't brought over due to name differences
top_prospect_rankings_4 <- top_prospect_rankings_4 %>%
  mutate(ID = case_when(PLAYER_NAME == "Albert Almora Jr." ~ "almoa002",
                        PLAYER_NAME == "Andrew Oliver" ~ "oliva002",
                        PLAYER_NAME == "Andrew Sisco" ~ "sisca001",
                        PLAYER_NAME == "C.J. Abrams" ~ "abrac001",
                        PLAYER_NAME == "Geraldo Perdomo" ~ "perdg001",
                        PLAYER_NAME == "Jesus E. Sanchez" ~ "sancj003",
                        PLAYER_NAME == "Sandy Alomar Jr." ~ "aloms001")
  )
                        
# subset each prospect dataframe to filter out prospects with no player ID
top_prospect_rankings_1 <- top_prospect_rankings

top_prospect_rankings_1_no_na <- top_prospect_rankings_1 %>%
  filter(!is.na(ID))

top_prospect_rankings_2_no_na <- top_prospect_rankings_2 %>%
  filter(!is.na(ID))

top_prospect_rankings_3_no_na <- top_prospect_rankings_3 %>%
  filter(!is.na(ID))

# append subsetted prospect dataframes with no NA player IDs
top_prospect_rankings_final <- bind_rows(top_prospect_rankings_1_no_na,
                                         top_prospect_rankings_2_no_na,
                                         top_prospect_rankings_3_no_na,
                                         top_prospect_rankings_4)

# arrange final prospect dataframe alphabetically
top_prospect_rankings_final <- top_prospect_rankings_final %>%
  arrange(PLAYER_NAME)

# detect duplicates
top_prospect_rankings_final %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  ungroup()

# remove duplicates
top_prospect_rankings_final_2 <- top_prospect_rankings_final %>%
  distinct()

# there are still more observations (3107) than the original dataset (3100)
## show which prospect ranks (1 to 100)  have more players than possible years
top_prospect_rankings_final_2 %>%
  group_by(PROSPECT_RANK) %>%
  filter(n() > 31) %>%
  print(n = Inf)

# when joining w/biofile, additional duplicates were created w/different ID
## remove/update those rows
top_prospect_rankings_final_3 <- top_prospect_rankings_final_2 %>%
  mutate(ID_TRUE_FALSE = case_when(ID == "garcl002" ~ 1,
                                   ID == "garcl003" ~ 1,
                                   ID == "andeb006" ~ 1)) %>%
  filter(is.na(ID_TRUE_FALSE)) %>%
  select(-c(ID_TRUE_FALSE))

# fix additional data errors
## Brian Anderson (Angels pitcher, andeb003 ~ andeb002)
## Luis Garcia (Phillies prospect, garcl006 ~ NA) 
## Chad Green (Brewers prospect, greec003 ~ NA)
## Bobby Bradley (Pirates prospect, bradb002 ~ NA)
## Alex Sanchez (Blue Jays pitcher, sanca003 ~ sanca002)
## Javier - Javy Lopez (Braves, lopej002 ~ lopej001)
## Jose - Javier Valentin (valej003 ~ valej004)
## Johan - Ervin Santana (santj003 ~ sante001)
## Carlos Hernandez (hernc003 ~ hernc004)
## Matt White (Rays - 1997 - 2001 - whitm003) should be NA (minor leaguer)
top_prospect_rankings_final_fix <- top_prospect_rankings_final_3 %>%
  filter(ID %in% c("andeb003", "garcl006", "greec003", "bradb002",
                   "sanca003", "lopej002", "valej003", "santj003",
                   "hernc003", "whitm003")) %>%
  mutate(ID = case_when(ID == "andeb003" & POSITION_1 == 7 ~ "andeb003",
                        ID == "andeb003" & POSITION_1 == 1 ~ "andeb002",
                        ID == "garcl006" & TEAM == "Nationals" ~ "garcl006",
                        ID == "garcl006" & TEAM == "Philles" ~ NA_character_,
                        ID == "greec003" ~ NA_character_,
                        ID == "bradb002" & TEAM == "Indians" ~ "bradb002",
                        ID == "bradb002" & TEAM == "Pirates" ~ NA_character_,
                        ID == "sanca003" ~ "sanca002",
                        ID == "lopej002" ~ "lopej001",
                        ID == "valej003" ~ "valej004",
                        ID == "santj003" ~ "sante001",
                        ID == "hernc003" ~ "hernc004",
                        ID ==  "whitm003" ~ NA_character_)) %>%
  mutate(PLAYER_NAME = case_when(
                        PLAYER_NAME == "Javier Lopez" ~ "Javy Lopez",
                        PLAYER_NAME == "Jose Valentin" ~ "Javier Valentin",
                        PLAYER_NAME == "Johan Santana" ~ "Ervin Santana",
                        PLAYER_NAME == "Brian Anderson" ~ "Brian Anderson",
                        PLAYER_NAME == "Luis Garcia" ~ "Luis Garcia",
                        PLAYER_NAME == "Chad Green" ~ "Chad Green",
                        PLAYER_NAME == "Bobby Bradley" ~ "Bobby Bradley",
                        PLAYER_NAME == "Alex Sanchez" ~ "Alex Sanchez",
                        PLAYER_NAME == "Carlos Hernandez" ~ "Carlos Hernandez",
                        PLAYER_NAME == "Matt White" ~ "Matt White"))

# fix last data errors
## went through prospects one by one to manually verify correct information
## detected some last errors that would be hard to catch other than manually
### Angel Martinez (Blue Jays - 1995 - NA) is Sandy Martinez (Major Leaguer)
### Calvin Reese (Reds - 1992, 1993 - NA) is Pokey Reese (Major Leaguer)
### Edgard Velasquez (Rockies - 1997 - NA) is Edgard Clemente (Major Leaguer)
### Jacob McGee (Rays - 2007 - NA) is Jake McGee (Major Leaguer)
### Matthew Lecroy (Twins - 2001 - NA) is Matt LeCroy (Major Leaguer)
### Michael Inoa (Athletics - 2009 - NA) is Michael Ynoa (Major Leaguer)
### Robert Smith (Braves - 1996 - NA) is Bob Smith (Major Leaguer)
### Jesus E. Sanchez and Jesus Sanchez (same player - make name consistent)
top_prospect_rankings_final_fix_2 <- top_prospect_rankings_final_3 %>%
  filter(PLAYER_NAME %in% c("Angel Martinez", "Calvin Reese",
                            "Edgard Velasquez", "Jacob McGee",
                            "Matthew Lecroy", "Michael Inoa",
                            "Robert Smith", "Jesus E. Sanchez")) %>%
  mutate(ID = case_when(PLAYER_NAME == "Angel Martinez" ~ "marta002",
                        PLAYER_NAME == "Calvin Reese" ~ "reesp001",
                        PLAYER_NAME == "Edgard Velasquez" ~ "cleme001",
                        PLAYER_NAME == "Jacob McGee" ~ "mcgej001",
                        PLAYER_NAME == "Matthew Lecroy" ~ "lecrm001",
                        PLAYER_NAME == "Michael Inoa" ~ "ynoam001",
                        PLAYER_NAME == "Robert Smith" ~ "smitr003",
                        PLAYER_NAME == "Jesus E. Sanchez" ~ "sancj003")) %>%
  mutate(PLAYER_NAME = case_when(
                        PLAYER_NAME == "Angel Martinez" ~ "Sandy Martinez",
                        PLAYER_NAME == "Calvin Reese" ~ "Pokey Reese",
                        PLAYER_NAME == "Edgard Velasquez" ~ "Edgard Clemente",
                        PLAYER_NAME == "Jacob McGee" ~ "Jake McGee",
                        PLAYER_NAME == "Matthew Lecroy" ~ "Matt LeCroy",
                        PLAYER_NAME == "Michael Inoa" ~ "Michael Ynoa",
                        PLAYER_NAME == "Robert Smith" ~ "Bob Smith",
                        PLAYER_NAME == "Jesus E. Sanchez" ~ "Jesus Sanchez"))

# filter out all those players from main prospect dataset
top_prospect_rankings_final_4 <- top_prospect_rankings_final_3 %>%
  filter(is.na(ID) | !ID %in% c("andeb003", "garcl006", "greec003",
                                  "bradb002", "sanca003", "lopej002",
                                  "valej003", "santj003", "hernc003")) %>%
  filter(!PLAYER_NAME %in% c("Matt White", "Angel Martinez",
                             "Calvin Reese", "Edgard Velasquez",
                             "Jacob McGee", "Matthew Lecroy",
                             "Michael Inoa", "Robert Smith",
                             "Jesus E. Sanchez"))
  
# append the main prospect dataset with the fixed players
top_prospect_rankings_last <- bind_rows(
  top_prospect_rankings_final_4,
  top_prospect_rankings_final_fix,
  top_prospect_rankings_final_fix_2) %>%
  arrange(PLAYER_NAME, YEAR)

# join biofile to prospect dataset to pull in debut date
biofile_1987_on <- biofile_1987_on %>%
  select(-c(PLAYER_NAME))

top_prospect_rankings_last <-
  left_join(top_prospect_rankings_last, 
            biofile_1987_on, 
            by = c("ID" = "PLAYERID"))

# create column to total the amount of time between the rank year and debut date
## this helped check data accuracy (large outliers indicated)
## went back and added additional fixes and manually verified each prospect
## prospect teams are not 100% accurate but were NOT corrected
## example: Nick Castellanos, 2014, Royals
top_prospect_rankings_last <- top_prospect_rankings_last %>%
  mutate(RNK_DBT_DIFF =
           as.numeric(difftime(YEAR, PLAY.DEBUT, units = "weeks")))

# change col names to make formatting consistent and clear, remove unused col
top_prospect_rankings_last <- top_prospect_rankings_last %>%
  rename(PLAY_DEBUT = PLAY.DEBUT,
         RANK_DEBUT_DATEDIFF_WKS = RNK_DBT_DIFF) %>%
  select(-MLB_DEBUT)

# export prospect dataframe as csv
write_csv(top_prospect_rankings_last,
          "BA_top100_prospect_ranks_1990-2020_final")

# filter for last prospect rank for each player
## filter for prospects who did not fall off the top 100 list prior to debut
top_prospect_rankings_last_max <- top_prospect_rankings_last %>%
  filter(!(is.na(ID))) %>%
  group_by(ID) %>%
  filter(RANK_DEBUT_DATEDIFF_WKS == max(RANK_DEBUT_DATEDIFF_WKS)) %>%
  filter(RANK_DEBUT_DATEDIFF_WKS > -52)

# join with all_batter and all_pitcher datasets based on ID
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  left_join(select(top_prospect_rankings_last_max, ID,
                   PROSPECT_RANK), by = c("BAT_ID" = "ID")) %>%
  relocate(PROSPECT_RANK, .after = BAT_LASTGAME)

all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  left_join(select(top_prospect_rankings_last_max, ID,
                   PROSPECT_RANK), by = c("PIT_ID" = "ID")) %>%
  relocate(PROSPECT_RANK, .after = PIT_LASTGAME)

# create column to differentiate Top 100 and Non-Top 100 Prospects
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>% 
  mutate(TOP_100_PROSPECT = case_when(PROSPECT_RANK %in% 1:100 ~ "Y", 
                                      is.na(PROSPECT_RANK) ~ "N")) %>%
  relocate(TOP_100_PROSPECT, .after = PROSPECT_RANK)

all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>% 
  mutate(TOP_100_PROSPECT = case_when(PROSPECT_RANK %in% 1:100 ~ "Y", 
                                      is.na(PROSPECT_RANK) ~ "N")) %>%
  relocate(TOP_100_PROSPECT, .after = PROSPECT_RANK)

# Final Transformations ----------------------------------------------------

# create column for simple batter wOBA grades (First & Post 150 PAs)
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>% 
  mutate(FIRST_150_WOBA_GRADE_SIMPLE = 
           fct_collapse(FIRST_150_WOBA_GRADE, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful"), 
                        DNQ = "DNQ")) %>% 
  relocate(FIRST_150_WOBA_GRADE_SIMPLE, .after = FIRST_150_WOBA_GRADE) %>%
  mutate(POST_150_WOBA_GRADE_SIMPLE = 
           fct_collapse(POST_150_WOBA_GRADE, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful", "DNQ"))) %>% 
  relocate(POST_150_WOBA_GRADE_SIMPLE, .after = POST_150_WOBA_GRADE)

# create column for simple batter wOBA grades (First & Post 600 PAs)
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>% 
  mutate(FIRST_600_WOBA_GRADE_SIMPLE = 
           fct_collapse(FIRST_600_WOBA_GRADE, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful"), 
                        DNQ = "DNQ")) %>% 
  relocate(FIRST_600_WOBA_GRADE_SIMPLE, .after = FIRST_600_WOBA_GRADE) %>%
  mutate(POST_600_WOBA_GRADE_SIMPLE = 
           fct_collapse(POST_600_WOBA_GRADE, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful", "DNQ"))) %>% 
  relocate(POST_600_WOBA_GRADE_SIMPLE, .after = POST_600_WOBA_GRADE)

# create column for simple pitcher wOBA grades (First & Post 215 BFs)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>% 
  mutate(FIRST_215_WOBA_GRADE_SIMPLE = 
           fct_collapse(FIRST_215_WOBA_GRADE, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful"), 
                        DNQ = "DNQ")) %>% 
  relocate(FIRST_215_WOBA_GRADE_SIMPLE, 
           .after = FIRST_215_WOBA_GRADE) %>%
  mutate(POST_215_WOBA_GRADE_SP_SIMPLE = 
           fct_collapse(POST_215_WOBA_GRADE_SP, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful", "DNQ"))) %>% 
  relocate(POST_215_WOBA_GRADE_SP_SIMPLE, 
           .after = POST_215_WOBA_GRADE_SP) %>% 
  mutate(POST_215_WOBA_GRADE_NON_SP_SIMPLE = 
           fct_collapse(POST_215_WOBA_GRADE_NON_SP, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful", "DNQ"))) %>% 
  relocate(POST_215_WOBA_GRADE_NON_SP_SIMPLE, 
           .after = POST_215_WOBA_GRADE_NON_SP)

# create column for simple starting pitcher wOBA grades (First & Post 860 BFs)
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>% 
  mutate(FIRST_860_WOBA_GRADE_SP_SIMPLE = 
           fct_collapse(FIRST_860_WOBA_GRADE_SP, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful"), 
                        DNQ = "DNQ")) %>% 
  relocate(FIRST_860_WOBA_GRADE_SP_SIMPLE, .after = FIRST_860_WOBA_GRADE_SP) %>%
  mutate(POST_860_WOBA_GRADE_SP_SIMPLE = 
           fct_collapse(POST_860_WOBA_GRADE_SP, 
                        Good = c("Elite", "Great", "Good"), 
                        Average = "Average", 
                        Bad = c("Below Average", "Poor", "Awful", "DNQ"))) %>% 
  relocate(POST_860_WOBA_GRADE_SP_SIMPLE, .after = POST_860_WOBA_GRADE_SP)

# create column to show prospect rank group for batters
all_batter_career_data_by_ID <- all_batter_career_data_by_ID %>%
  mutate(PROSPECT_RANK_GROUP = case_when(PROSPECT_RANK %in% 1:25 ~ "1 - 25", 
                                         PROSPECT_RANK %in% 26:50 ~ "26 - 50", 
                                         PROSPECT_RANK %in% 51:75 ~ "51 - 75", 
                                         PROSPECT_RANK %in% 76:100 ~ "76 - 100", 
                                         is.na(PROSPECT_RANK) ~ "Unranked")) %>% 
  relocate(PROSPECT_RANK_GROUP, .after = PROSPECT_RANK)

# create column to show prospect rank group for pitchers
all_pitcher_career_data_by_ID <- all_pitcher_career_data_by_ID %>%
  mutate(PROSPECT_RANK_GROUP = case_when(PROSPECT_RANK %in% 1:25 ~ "1 - 25", 
                                         PROSPECT_RANK %in% 26:50 ~ "26 - 50", 
                                         PROSPECT_RANK %in% 51:75 ~ "51 - 75", 
                                         PROSPECT_RANK %in% 76:100 ~ "76 - 100", 
                                         is.na(PROSPECT_RANK) ~ "Unranked")) %>% 
  relocate(PROSPECT_RANK_GROUP, .after = PROSPECT_RANK)

# Analysis & Visualization: Simple wOBA Grades Stacked Bar Charts ----------

# install and load hrbrthemes package for more aesthetic plots
install.packages("hrbrthemes")

library(hrbrthemes)

# 150 PA
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01") %>% 
  group_by(FIRST_150_WOBA_GRADE_SIMPLE, POST_150_WOBA_GRADE_SIMPLE) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  complete(FIRST_150_WOBA_GRADE_SIMPLE, POST_150_WOBA_GRADE_SIMPLE, fill = list(n = 0)) %>%
  mutate(PERCENT_OF_BATTERS = n / sum(n)) %>% 
  ggplot(aes(x = fct_rev(FIRST_150_WOBA_GRADE_SIMPLE), 
             y = PERCENT_OF_BATTERS, 
             fill = POST_150_WOBA_GRADE_SIMPLE)) + 
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() +
  labs(title = "Batter Simple wOBA Grades", 
       subtitle = "First 150 Plate Appearances (Initial); Post 150 PA (Post)", 
       caption = "Percentages are rounded | Number of Batters = 1,891") + 
  xlab(label = "Initial wOBA Grade") + 
  ylab(label = "% of Batters") +
  scale_fill_manual(name = "Post wOBA Grade", 
                    values = c("blue", "darkgrey", "darkred")) +
  scale_y_continuous(labels = percent, 
                     limits = c(0, .7)) +
  geom_text(aes(label = if_else(
    PERCENT_OF_BATTERS > .035, 
    (paste0(round(PERCENT_OF_BATTERS * 100, 0), "%")), NA)), 
    position = position_stack(vjust = .5),
    color = "white") + 
  theme_ipsum() + 
  update_geom_font_defaults(family = "Arial Narrow", face = "plain",
                            size = 3.5, color = "#2b2b2b") +
  theme(legend.position = "bottom")

# 150 PA (faceted by prospect rank)
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01") %>% 
  group_by(FIRST_150_WOBA_GRADE_SIMPLE, POST_150_WOBA_GRADE_SIMPLE, 
           PROSPECT_RANK_GROUP) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  complete(FIRST_150_WOBA_GRADE_SIMPLE, POST_150_WOBA_GRADE_SIMPLE, 
           PROSPECT_RANK_GROUP, fill = list(n = 0)) %>% 
  group_by(PROSPECT_RANK_GROUP) %>%
  mutate(PERCENT_OF_BATTERS = n / sum(n)) %>% 
  ggplot(aes(x = fct_rev(FIRST_150_WOBA_GRADE_SIMPLE), 
             y = PERCENT_OF_BATTERS, 
             fill = POST_150_WOBA_GRADE_SIMPLE)) + 
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() +
  facet_wrap(~PROSPECT_RANK_GROUP, scales = "free_x") +
  labs(title = "Batter Simple wOBA Grades by Prospect Rank", 
       subtitle = "First 150 Plate Appearances (Initial); Post 150 PA (Post)") + 
  xlab(label = "Initial wOBA Grade") + 
  ylab(label = "% of Batters") +
  scale_fill_manual(name = "Post wOBA Grade", 
                    values = c("blue", "darkgrey", "darkred")) +
  scale_y_continuous(labels = percent, 
                     limits = c(0, .7), 
                     breaks = c(0, .3, .6)) +
  theme_ipsum() + 
  update_geom_font_defaults(family = "Arial Narrow", face = "plain",
                            size = 3.5, color = "#2b2b2b") +
  theme(legend.position = "bottom")

# Analysis & Visualization: wOBA Grades Crosstabs --------------------------

# using janitor package here

# create list of tabyls for batters by prospect group (150 PA)
batter_woba_grade_crosstabs_150_list <- all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01") %>% 
  tabyl(FIRST_150_WOBA_GRADE, POST_150_WOBA_GRADE, PROSPECT_RANK_GROUP) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row",,Elite:DNQ)

# create list of tabyls for batters by prospect group (600 PA)
batter_woba_grade_crosstabs_600_list <- all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01") %>% 
  tabyl(FIRST_600_WOBA_GRADE, POST_600_WOBA_GRADE, PROSPECT_RANK_GROUP) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row",,Elite:DNQ)

# create list of tabyls for pitchers by prospect group (215 BF)
pitcher_woba_grade_crosstabs_215_list <- all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & PITCHER_TYPE == "STARTER") %>% 
  tabyl(FIRST_215_WOBA_GRADE, POST_215_WOBA_GRADE_SP, PROSPECT_RANK_GROUP) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row",,Elite:DNQ)

# create list of tabyls for pitchers by prospect group (860 BF)
pitcher_woba_grade_crosstabs_860_list <- all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & PITCHER_TYPE == "STARTER") %>% 
  tabyl(FIRST_860_WOBA_GRADE_SP, POST_860_WOBA_GRADE_SP, PROSPECT_RANK_GROUP) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row",,Elite:DNQ)

# install and load gt package
install.packages("gt")

library(gt)

# create gt table from tabyl (example)
batter_woba_grade_crosstabs_150_list$`1 - 25` %>% 
  gt(rowname_col = "FIRST_150_WOBA_GRADE", groupname_col = "PROSPECT_RANK_GROUP") %>% 
  tab_header(title = md("**Batter wOBA Grades by Prospect Rank: 1 to 25**"), 
             subtitle = "First & Post 150 Plate Appearances") %>%
  tab_footnote(
    footnote = "*Percentages are rounded and may not equal to exactly 100% across rows.") %>% 
  tab_stubhead(
    label = "Initial wOBA") %>% 
  tab_spanner(
    label = "Remaining Career wOBA", columns = 2:9) %>% 
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_stub(rows = 9)) %>% 
  tab_style(
    style = cell_text(style = "italic"), 
    locations = list(cells_body(columns = Total), cells_column_labels(columns = Total))) %>% 
  cols_label(
    `Below Average` = "Below", 
    Average = "Avg.") %>%
  cols_align(
    align = "left",
    columns = 1) %>% 
  cols_align(
    align = "center",
    columns = 2:10) %>% 
  cols_width(
    1 ~ px(130),
    c(2:10) ~ px(50)) %>% 
  sub_missing(
    missing_text = "---") %>%
  data_color(
    rows = everything(), 
    direction = "row", 
    columns = 2:9,
    method = "numeric", 
    palette = "plasma") %>% 
  fmt_percent(
    columns = 2:9,
    rows = everything(), 
    drop_trailing_zeros = TRUE) %>% 
  fmt_percent(
    columns = c(!starts_with("Total") & where(~ is.numeric(.))),
    decimals = 0) %>% 
  tab_options(
    table.align = "left", 
    table.margin.left = px(50),
    quarto.disable_processing = TRUE)

# create gt theme with color function
gt_crosstab_theme_fn_data_color <- function(gt_tbl) {
  gt_tbl %>% 
    tab_footnote(
      footnote = "*Percentages are rounded and may not equal to exactly 100% across rows.") %>% 
    tab_stubhead(
      label = "Initial wOBA") %>% 
    tab_spanner(
      label = "Remaining Career wOBA", columns = 2:9) %>% 
    tab_style(
      style = cell_text(weight = "bold"), 
      locations = cells_stub(rows = 9)) %>% 
    tab_style(
      style = cell_text(style = "italic"), 
      locations = list(cells_body(columns = Total), cells_column_labels(columns = Total))) %>% 
    cols_label(
      `Below Average` = "Below", 
      Average = "Avg.") %>%
    cols_align(
      align = "left",
      columns = 1) %>% 
    cols_align(
      align = "center",
      columns = 2:10) %>% 
    cols_width(
      1 ~ px(130),
      c(2:10) ~ px(50)) %>% 
    sub_missing(
      missing_text = "---") %>%
    data_color(
      rows = everything(), 
      direction = "row", 
      columns = 2:9,
      method = "numeric", 
      palette = "plasma") %>% 
    fmt_percent(
      columns = 2:9,
      rows = everything(), 
      drop_trailing_zeros = TRUE) %>% 
    fmt_percent(
      columns = c(!starts_with("Total") & where(~ is.numeric(.))),
      decimals = 0) %>% 
    tab_options(
      table.align = "left", 
      table.margin.left = px(50),
      quarto.disable_processing = TRUE)
}

# create gt theme no color function
gt_crosstab_theme_fn_no_color <- function(gt_tbl) {
  gt_tbl %>% 
    tab_footnote(
      footnote = "*Percentages are rounded and may not equal to exactly 100% across rows.") %>% 
    tab_stubhead(
      label = "Initial wOBA") %>% 
    tab_spanner(
      label = "Remaining Career wOBA", columns = 2:9) %>% 
    tab_style(
      style = cell_text(weight = "bold"), 
      locations = cells_stub(rows = 9)) %>% 
    tab_style(
      style = cell_text(style = "italic"), 
      locations = list(cells_body(columns = Total), cells_column_labels(columns = Total))) %>% 
    cols_label(
      `Below Average` = "Below", 
      Average = "Avg.") %>%
    cols_align(
      align = "left",
      columns = 1) %>% 
    cols_align(
      align = "center",
      columns = 2:10) %>% 
    cols_width(
      1 ~ px(130),
      c(2:10) ~ px(50)) %>% 
    sub_missing(
      missing_text = "---") %>%
    fmt_percent(
      columns = 2:9,
      rows = everything(), 
      drop_trailing_zeros = TRUE) %>% 
    fmt_percent(
      columns = c(!starts_with("Total") & where(~ is.numeric(.))),
      decimals = 0) %>% 
    tab_options(
      table.align = "left", 
      table.margin.left = px(50),
      quarto.disable_processing = TRUE)
}
  
# code chunk to update manually for tabyls with NAs
data_color(
  columns = 2:9,
  rows = everything(), 
  direction = "row", 
  method = "numeric", 
  palette = "plasma")

# test with gt theme color function
batter_woba_grade_crosstabs_600_list$`1 - 25` %>% 
  gt(rowname_col = "FIRST_600_WOBA_GRADE", groupname_col = "PROSPECT_RANK_GROUP") %>% 
  tab_header(title = md("**Percentage of Batters by wOBA Grades**"), 
             subtitle = "Prospect Rank: 1 to 25 | First & Post 600 Plate Appearances") %>% 
  gt_crosstab_theme_fn_data_color()

# test with gt theme no color function
batter_woba_grade_crosstabs_600_list$`51 - 75` %>% 
  gt(rowname_col = "FIRST_600_WOBA_GRADE", groupname_col = "PROSPECT_RANK_GROUP") %>% 
  tab_header(title = md("**Percentage of Batters by wOBA Grades**"), 
             subtitle = "Prospect Rank: 51 to 75 | First & Post 600 Plate Appearances") %>% 
  data_color(
    columns = 2:9,
    rows = 2:9, 
    direction = "row", 
    method = "numeric", 
    palette = "plasma") %>% 
  gt_crosstab_theme_fn_no_color()

# missing rows
## batter_woba_grade_crosstabs_600_list$`51 - 75`, 1 (Elite)
## batter_woba_grade_crosstabs_600_list$`76 - 100`, 1 (Elite)
## pitcher_woba_grade_crosstabs_215_list$`1 - 25`, 8 (DNQ)
## pitcher_woba_grade_crosstabs_860_list$`1 - 25`, 7 (Awful)
## pitcher_woba_grade_crosstabs_860_list$`26 - 50`, 1 (Elite)
## pitcher_woba_grade_crosstabs_860_list$`51 - 75`, 7 (Awful)
## pitcher_woba_grade_crosstabs_860_list$`76 - 100`, 7 (Awful)
## pitcher_woba_grade_crosstabs_860_list$`Unranked`, 7 (Awful)

# Analysis & Visualization: wOBA Summary & Difference Tables --------------

# create wOBA summary & difference tables for batters & pitchers at PA/BF milestones

# batter wOBA 150
woba_summary_table_bat_150 <- all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01" & CAREER_TOTAL_PA >= 600) %>% 
  group_by(PROSPECT_RANK_GROUP) %>% 
  summarise(FIRST_150_WOBA_MEAN = round(mean(QUARTILE_ONE_WOBA, na.rm = TRUE), 3), 
            POST_150_WOBA_MEAN = round(mean(POST_150_WOBA, na.rm = TRUE), 3),
            NUMBER_OF_BATTERS_600_PA = n()) %>% 
  rowwise() %>% 
  mutate(WOBA_DIFFERENCE_150 = round(POST_150_WOBA_MEAN - FIRST_150_WOBA_MEAN, 3)) %>% 
  relocate(WOBA_DIFFERENCE_150, .after = POST_150_WOBA_MEAN)

# batter wOBA 600
woba_summary_table_bat_600 <- all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT <  "2011-01-01" & CAREER_TOTAL_PA >= 1800) %>% 
  group_by(PROSPECT_RANK_GROUP) %>% 
  summarise(FIRST_600_WOBA_MEAN = round(mean(FIRST_600_WOBA, na.rm = TRUE), 3), 
            POST_600_WOBA_MEAN = round(mean(POST_600_WOBA, na.rm = TRUE), 3),
            NUMBER_OF_BATTERS_1800_PA = n()) %>% 
  rowwise() %>% 
  mutate(WOBA_DIFFERENCE_600 = round(POST_600_WOBA_MEAN - FIRST_600_WOBA_MEAN, 3)) %>% 
  relocate(WOBA_DIFFERENCE_600, .after = POST_600_WOBA_MEAN)

# join wOBA bat tables together
woba_summary_table_bat_all <- 
  left_join(woba_summary_table_bat_150, 
            woba_summary_table_bat_600, 
            by = "PROSPECT_RANK_GROUP") %>% 
  relocate(6:8, .before = 5) %>% 
  rowwise() %>% 
  mutate(PERCENT_BATTERS_REMAINING = 
           (round(NUMBER_OF_BATTERS_1800_PA / NUMBER_OF_BATTERS_600_PA, 2) * 100))

# split wOBA bat tables into three for presentation
woba_summary_table_bat_150_final <- woba_summary_table_bat_all %>% 
  select(1:4)

woba_summary_table_bat_600_final <- woba_summary_table_bat_all %>% 
  select(c(1, 5:7))

woba_summary_table_bat_count_final <- woba_summary_table_bat_all %>% 
  select(c(1, 8:10))

# pitcher wOBA against 215 BF
woba_summary_table_pit_215 <- all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_BF >= 860 & 
           PITCHER_TYPE == "STARTER") %>% 
  group_by(PROSPECT_RANK_GROUP) %>% 
  summarise(FIRST_215_WOBA_MEAN = round(mean(QUARTILE_ONE_WOBA, na.rm = TRUE), 3), 
            POST_215_WOBA_MEAN = round(mean(POST_215_WOBA, na.rm = TRUE), 3),
            NUMBER_OF_PITCHERS_860_BF = n()) %>% 
  rowwise() %>% 
  mutate(WOBA_DIFFERENCE_215 = round(POST_215_WOBA_MEAN - FIRST_215_WOBA_MEAN, 3)) %>% 
  relocate(WOBA_DIFFERENCE_215, .after = POST_215_WOBA_MEAN)

# pitcher wOBA against 860 BF
woba_summary_table_pit_860 <- all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_BF >= 2580 & 
           PITCHER_TYPE == "STARTER") %>% 
  group_by(PROSPECT_RANK_GROUP) %>% 
  summarise(FIRST_860_WOBA_MEAN = round(mean(FIRST_860_WOBA, na.rm = TRUE), 3), 
            POST_860_WOBA_MEAN = round(mean(POST_860_WOBA, na.rm = TRUE), 3),
            NUMBER_OF_PITCHERS_2580_BF = n()) %>% 
  rowwise() %>% 
  mutate(WOBA_DIFFERENCE_860 = round(POST_860_WOBA_MEAN - FIRST_860_WOBA_MEAN, 3)) %>% 
  relocate(WOBA_DIFFERENCE_860, .after = POST_860_WOBA_MEAN)

# join pitcher tables together
woba_summary_table_pit_all <- 
  left_join(woba_summary_table_pit_215, 
            woba_summary_table_pit_860, 
            by = "PROSPECT_RANK_GROUP") %>% 
  relocate(6:8, .before = 5) %>% 
  rowwise() %>% 
  mutate(PERCENT_PITCHERS_REMAINING = 
           (round(NUMBER_OF_PITCHERS_2580_BF / NUMBER_OF_PITCHERS_860_BF, 2) * 100))

# split wOBA pit tables into three for presentation
woba_summary_table_pit_215_final <- woba_summary_table_pit_all %>% 
  select(1:4)

woba_summary_table_pit_860_final <- woba_summary_table_pit_all %>% 
  select(c(1, 5:7))

woba_summary_table_pit_count_final <- woba_summary_table_pit_all %>% 
  select(c(1, 8:10))

# create gt table
## not saving as theme due to variety of changes needed between tables

# 150 PA
woba_summary_table_bat_150_final %>% 
  gt(rowname_col = "PROSPECT_RANK_GROUP") %>% 
  tab_header(title = md("**Mean wOBA for Batters by Prospect Rank**"), 
             subtitle = "First & Post 150 Plate Appearances") %>%
  tab_footnote(
    footnote = "*Minimum Career Plate Appearances = 600") %>% 
  tab_stubhead(
    label = "Prospect Rank") %>% 
  cols_align(
    align = "center",
    columns = 2:4) %>% 
  cols_label(
    FIRST_150_WOBA_MEAN = "First 150 wOBA", 
    POST_150_WOBA_MEAN = "Post 150 wOBA", 
    WOBA_DIFFERENCE_150 = "Difference") %>% 
  cols_width(
    1 ~ px(150),
    c(2:4) ~ px(150)) %>%
  data_color(
    rows = everything(), 
    direction = "column", 
    columns = 2:4,
    method = "numeric", 
    reverse = FALSE,
    palette = c(
      "#2700D1FF", 
      "#6B58EFFF", 
      "#8888FFFF", 
      "#C7C1FFFF",
      "#D5D5FFFF", 
      "#FFC0E5FF", 
      "#FF8989FF", 
      "#FF7080FF", 
      "#FF5A5AFF", 
      "#EF4040FF")) %>% 
  tab_options(
    quarto.disable_processing = TRUE)

# batters count
woba_summary_table_bat_count_final %>% 
  gt(rowname_col = "PROSPECT_RANK_GROUP") %>% 
  tab_header(title = md("**Total Batters by Prospect Rank**"), 
             subtitle = "Number & Percentage of Batters with 600 & 1,800 Plate Appearances") %>%
  tab_stubhead(
    label = "Prospect Rank") %>% 
  tab_footnote(
    footnote = "*Batters with less than 600 career Plate Appearances are not included.") %>% 
  cols_align(
    align = "center",
    columns = 2:4) %>% 
  cols_label(
    NUMBER_OF_BATTERS_600_PA = ">= 600 PA",
    NUMBER_OF_BATTERS_1800_PA = ">= 1,800 PA",
    PERCENT_BATTERS_REMAINING = "% of Batters") %>% 
  cols_width(
    1 ~ px(150),
    c(2:4) ~ px(150)) %>%
  data_color(
    rows = everything(), 
    direction = "column", 
    columns = 4,
    method = "numeric", 
    reverse = FALSE,
    palette = c(
      "#2700D1FF", 
      "#6B58EFFF", 
      "#8888FFFF", 
      "#C7C1FFFF",
      "#D5D5FFFF", 
      "#FFC0E5FF", 
      "#FF8989FF", 
      "#FF7080FF", 
      "#FF5A5AFF", 
      "#EF4040FF")) %>% 
  fmt_percent(
    columns = 4,
    rows = everything(),
    scale_values = FALSE, 
    decimals = 0)  %>% 
  tab_options(
    quarto.disable_processing = TRUE)

# Analysis & Visualization: wOBA Movement Plots ----------------------------

# plot distribution of player wOBA difference between initial and post PA/BF
## using a density plot to show distribution

# 150 PA
all_batter_career_data_by_ID %>% 
  mutate(TOP_100_PROSPECT = case_when(TOP_100_PROSPECT == "Y" ~ "YES", 
                                      TOP_100_PROSPECT == "N" ~ "NO")) %>%
  filter(BAT_DEBUT <  "2011-01-01" & 
           !is.na(POST_150_WOBA) &
           CAREER_TOTAL_PA >= 600) %>% 
  ggplot(aes(FIRST_POST_150_WOBA_DIFF, fill = TOP_100_PROSPECT)) +
  geom_density(alpha = .5) + 
  scale_fill_manual(values = c("#1AFF1A", "#4B0092")) + 
  scale_y_continuous(limits = c(0, 17.5)) + 
  scale_x_continuous(limits = c(-.15, .15)) +
  labs(
    title = "Batter wOBA Difference First & Post 150 PA", 
    x = "wOBA Difference", 
    y = "Density", 
    caption = "Minimum Career Plate Appearances = 600 | Number of Batters = 970") + 
  guides(
    fill = 
      guide_legend(
        title = "Top 100 Prospect",
        reverse = TRUE)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = .1, color = "blue") +
  geom_vline(xintercept = -.1, color = "red") +
  theme_ipsum() +
  theme(legend.position = "bottom")

# Analysis & Visualization: Career wOBA Comparison Plots  ------------------

# plot distribution of player career wOBA and wOBA against values and grades
## using density plots and population pyramids to show distribution

# batters - density
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT <  "2011-01-01" & CAREER_TOTAL_PA >= 150) %>% 
  ggplot(aes(CAREER_WOBA_BAT, color = PROSPECT_RANK_GROUP)) +
  geom_line(stat = "density", position = "identity", size = .75) + 
  scale_x_continuous(labels = signs::signs_format(accuracy = .001, 
                                                  trim_leading_zeros = TRUE)) +
  scale_color_manual(values = c("#1192e8", "#fa4d56", "#002d9c", "#009d9a", "#a56eff")) +
  labs(
    title = "Batter Career wOBA Distribution", 
    x = "Career wOBA", 
    y = "Density", 
    caption = "Minimum Career Plate Appearances = 150 | Number of Batters = 1,316") + 
  guides(
    color = 
      guide_legend(
        title = "Prospect Rank")) +
  theme_ipsum() + 
  theme(legend.position = "bottom")

# batters - "population" pyramid
all_batter_career_data_by_ID %>% 
  mutate(CAREER_WOBA_GRADE = case_when(
    CAREER_TOTAL_PA < 1800 ~ "DNQ",
    CAREER_TOTAL_PA >= 1800 & CAREER_WOBA_BAT >= .400 ~ "Elite",
    CAREER_TOTAL_PA >= 1800 & between(CAREER_WOBA_BAT, .370, .399) ~ "Great",
    CAREER_TOTAL_PA >= 1800 & between(CAREER_WOBA_BAT, .340, .369) ~ "Good",
    CAREER_TOTAL_PA >= 1800 & between(CAREER_WOBA_BAT, .320, .339) ~ "Average",
    CAREER_TOTAL_PA >= 1800 & between(CAREER_WOBA_BAT, .310, .319) ~ "Below Average",
    CAREER_TOTAL_PA >= 1800 & between(CAREER_WOBA_BAT, .291, .309) ~ "Poor",
    CAREER_TOTAL_PA >= 1800 & CAREER_WOBA_BAT <= .290 ~ "Awful"),
    CAREER_WOBA_GRADE = factor(CAREER_WOBA_GRADE, levels = 
                                 c("Elite", "Great", "Good", "Average", 
                                   "Below Average", "Poor", "Awful", "DNQ"))) %>%
  mutate(TOP_100_PROSPECT = case_when(TOP_100_PROSPECT == "Y" ~ "YES", 
                                      TOP_100_PROSPECT == "N" ~ "NO")) %>%
  filter(BAT_DEBUT <  "2011-01-01" & CAREER_TOTAL_PA >= 150) %>% 
  group_by(CAREER_WOBA_GRADE, TOP_100_PROSPECT) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = ifelse(TOP_100_PROSPECT == "YES", -count, count), 
             y = fct_rev(CAREER_WOBA_GRADE), 
             fill = TOP_100_PROSPECT)) +
  geom_col() + 
  scale_x_continuous(labels = abs, 
                     limits = c(-250, 650)) +
  scale_fill_manual(name = "TOP_100_PROSPECT",
                    values = c("#fa4d56", "#1192e8")) +
  geom_text(aes(
    label = if_else(ifelse(TOP_100_PROSPECT == "YES", -count, count) < -0, 
                    ifelse(TOP_100_PROSPECT == "YES", -count, count) * -1, 
                    ifelse(TOP_100_PROSPECT == "YES", -count, count)), 
    hjust = if_else(TOP_100_PROSPECT == "YES", 1.25, -.25)),
    position = position_dodge()) + 
  labs(title = "Number of Batters per Career wOBA Grade", 
       subtitle = "Includes All Batters with At Least 150 Career Plate Appearances",
       caption = "Batters who failed to reach 600 PA are listed as DNQ") + 
  xlab(label = "Number of Batters") + 
  ylab(label = "Career wOBA Grade") + 
  guides(
    fill = 
      guide_legend(
        title = "Top 100 Prospect",
        reverse = TRUE)) + 
  theme_ipsum() + 
  update_geom_font_defaults(family = "Arial Narrow", face = "plain",
                            size = 3.5, color = "#2b2b2b") +
  theme(legend.position = "bottom")

# Analysis & Visualization: Top Ranked & Unranked Prospect Tables ----------

# install and load knitr package
install.packages("knitr")

library(knitr)

# batters - ranked prospects
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_PA >= 1800 & 
           TOP_100_PROSPECT == "Y") %>% 
  select(BAT_NAME, CAREER_WOBA_BAT, PROSPECT_RANK) %>% 
  arrange(desc(CAREER_WOBA_BAT)) %>% 
  head(n = 100) %>%
  kable(col.names = c("Name", "Career wOBA", "Prospect Rank"), 
        align = "lccc")

# batters - unranked prospects
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_PA >= 1800 & 
           TOP_100_PROSPECT == "N") %>% 
  select(BAT_NAME, CAREER_WOBA_BAT, PROSPECT_RANK) %>% 
  arrange(desc(CAREER_WOBA_BAT)) %>% 
  head(n = 100) %>% 
  select(BAT_NAME, CAREER_WOBA_BAT, PROSPECT_RANK) %>%
  kable(col.names = c("Name", "Career wOBA", "Prospect Rank"), 
        align = "lccc") 

# Analysis & Visualization: Correlation & Regression -----------------------

# install and load signs package for number formatting tools
install.packages("signs")

library(signs)

# install and load ggpmisc extension package to annotate values for each plot
install.packages("ggpmisc")

library(ggpmisc)

# plot initial/post wOBA/wOBA Against as scatterplot with linear model

# 150 PA
all_batter_career_data_by_ID %>%
  filter(BAT_DEBUT <  "2011-01-01" & CAREER_TOTAL_PA >= 1800) %>%
  ggplot(aes(y = POST_150_WOBA, x = QUARTILE_ONE_WOBA)) + 
  geom_point(aes(color = PROSPECT_RANK_GROUP)) + 
  geom_smooth(method = "lm", color = "black") + 
  stat_correlation(use_label(c("R", "R2", "P"))) + 
  scale_x_continuous(labels = signs::signs_format(accuracy = .001, 
                                                  trim_leading_zeros = TRUE)) + 
  scale_y_continuous(labels = signs::signs_format(accuracy = .001, 
                                                  trim_leading_zeros = TRUE)) + 
  scale_color_manual(values = c("#1192e8", "#fa4d56", "#002d9c", "#009d9a", "#a56eff")) + 
  labs(title = "wOBA Correlation & Regression First & Post 150 PA", 
       subtitle = "Includes All Batters with At Least 1,800 Career Plate Appearances", 
       caption = "Number of Batters = 636") + 
  ylab(label = "Post 150 PA wOBA") + 
  xlab(label = "First 150 PA wOBA") + 
  guides(
    color = 
      guide_legend(
        title = "Prospect Rank Group")) + 
  theme_ipsum() + 
  theme(legend.position = "bottom")

# Analysis: Fun Facts (Game Endings) --------------------------------------

# how many/which games from 1990 through 2020 ended in back to back home runs?
back_to_back_home_run_game_endings <- game_data %>% 
  select(row_idx, GAME_ID, PARK, DATE, HOME_TEAM_ID, AWAY_TEAM_ID, INNING, 
         BAT_NAME, PIT_NAME, EVENT_TX, HIT_VAL, EVENT_RUNS, PA_IND) %>% 
  arrange(ymd(DATE), row_idx) %>% 
  filter(PA_IND = TRUE) %>% 
  group_by(GAME_ID) %>% 
  slice_tail(n = 2) %>% 
  filter(HIT_VAL == 4) %>% 
  group_by(GAME_ID) %>% 
  add_count(GAME_ID) %>% 
  filter(n != 1) %>% 
  arrange(ymd(DATE), row_idx) %>%
  select(-PA_IND)

# create "kable" table
back_to_back_home_run_game_endings %>% 
  select(-c("PARK", "HIT_VAL", "EVENT_RUNS")) %>% 
  kable(col.names = c("Row ID", "Game ID", "Date", "Home Team", "Away Team", 
                      "Inning", "Batter", "Pitcher", "Event"), 
        align = "lllccclll")

# number of all games from 1990 through 2020 (71,080)
game_data %>% 
  distinct(GAME_ID) %>% 
  count()

# how rare is it to see a game with back to back home runs? (1 out of every 2632)
(27/71080)*100

71080/27

# doesn't appear that any games ended in back to back to back home runs!
## adjust slice_tail to (n = 3) and filter(n == 3)

# Quarto Extras -----------------------------------------------------------

# more efficient code to bring wOBA values into summary dataset
all_batter_career_data_by_ID_test <- bat_wOBA_log_debut_1990_on_no_pit %>%
  group_by(BAT_ID) %>% 
  summarise(FIRST_900_WOBA_test = mean(EVENT_WOBA[career_PA_wOBA_num <= 900]), 
            POST_900_WOBA_test = mean(EVENT_WOBA[career_PA_wOBA_num > 900])) %>% 
  left_join(all_batter_career_data_by_ID_test, 
            ., 
            by = "BAT_ID")

# biggest starting pitcher "risers" (first/post 215 BF)
all_pitcher_career_data_by_ID %>%
  filter(POST_215_WOBA != is.na(POST_215_WOBA) &
           PITCHER_TYPE == "STARTER" & 
           PIT_DEBUT < "2011-01-01" &
           CAREER_TOTAL_BF >= 860) %>%
  select(PIT_NAME, CAREER_WOBA_PIT, CAREER_TOTAL_BF, QUARTILE_ONE_WOBA,
         POST_215_WOBA, FIRST_POST_215_WOBA_DIFF, PLAYER_STATUS,
         PITCHER_TYPE) %>%
  arrange(FIRST_POST_215_WOBA_DIFF) %>%
  print(n = 20)

# calculate total innings pitched for top 50 pitchers (ranked and unranked)
all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_BF >= 2580 & 
           TOP_100_PROSPECT == "Y" & 
           PITCHER_TYPE == "STARTER") %>% 
  arrange(CAREER_WOBA_PIT) %>% 
  head(n = 50) %>% 
  summarise(TOTAL_CAREER_BF = sum(CAREER_TOTAL_BF))

# calculate median, mean prospect rank for best batting prospects by career wOBA
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_PA >= 1800 & 
           TOP_100_PROSPECT == "Y") %>% 
  select(BAT_NAME, CAREER_WOBA_BAT, PROSPECT_RANK) %>% 
  arrange(desc(CAREER_WOBA_BAT)) %>% 
  head(n = 100) %>% 
  summarise(median(PROSPECT_RANK),
            mean(PROSPECT_RANK))

# calculate median, mean prospect rank for best pitching prospects by career wOBA Against
all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & 
           CAREER_TOTAL_BF >= 2580 & 
           TOP_100_PROSPECT == "Y" & 
           PITCHER_TYPE == "STARTER") %>% 
  select(PIT_NAME, CAREER_WOBA_PIT, PROSPECT_RANK) %>% 
  arrange(CAREER_WOBA_PIT) %>% 
  head(n = 50) %>% 
  summarise(median(PROSPECT_RANK), 
            mean(PROSPECT_RANK))

# calculate number of players in the years of focus, including how many are active

## batters
all_batter_career_data_by_ID %>% 
  filter(BAT_DEBUT < "2011-01-01") %>%
  group_by(PLAYER_STATUS) %>% 
  summarise(n = n())

## active + inactive
52 + 1839

## pitchers
all_pitcher_career_data_by_ID %>% 
  filter(PIT_DEBUT < "2011-01-01" & PITCHER_TYPE == "STARTER") %>% 
  group_by(PLAYER_STATUS) %>% 
  summarise(n = n())

## active + inactive
30 + 687

# calculate percentage of prospects who make it to the major leagues

# first double-check if any players without IDs are duplicates
top_prospect_rankings_last %>%
  filter((is.na(ID))) %>%
  group_by(PLAYER_NAME, TEAM) %>% 
  summarize(PLAYER_NAME = first(PLAYER_NAME)) %>% 
  group_by(PLAYER_NAME) %>% 
  add_count(PLAYER_NAME) %>% 
  filter(n > 1)

# find most recent prospect rank for players who did not debut in MLB
# combine that dataframe with the dataframe of distinct prospects who did debut
top_prospect_rankings_last_max_all <- top_prospect_rankings_last %>%
  filter((is.na(ID))) %>%
  group_by(PLAYER_NAME) %>% 
  mutate(CURRENT_YEAR = as.Date("2024-01-01")) %>% 
  mutate(RANK_YEAR_DATEDIFF = as.numeric(difftime(CURRENT_YEAR, YEAR, units = "weeks"))) %>% 
  group_by(PLAYER_NAME) %>% 
  filter(RANK_YEAR_DATEDIFF == min(RANK_YEAR_DATEDIFF)) %>% 
  ungroup() %>% 
  select(1:12) %>% 
  bind_rows(., top_prospect_rankings_last_max)

# create summary table to show prospect debut rate for batters
## only for prospects ranked in years 1990 to 2010
batting_prospect_mlb_debut_rate <- top_prospect_rankings_last_max_all %>% 
  mutate(PROSPECT_RANK_GROUP = case_when(PROSPECT_RANK %in% 1:25 ~ "1 - 25", 
                                         PROSPECT_RANK %in% 26:50 ~ "26 - 50", 
                                         PROSPECT_RANK %in% 51:75 ~ "51 - 75", 
                                         PROSPECT_RANK %in% 76:100 ~ "76 - 100", 
                                         is.na(PROSPECT_RANK) ~ "Unranked")) %>% 
  relocate(PROSPECT_RANK_GROUP, .after = PROSPECT_RANK) %>% 
  mutate(MLB_PLAYER = if_else(!is.na(PLAY_DEBUT), "Yes", "No"), 
         MLB_PLAYER = factor(MLB_PLAYER, levels = c("Yes", "No"))) %>%
  filter(YEAR < "2011-01-01" & !POSITION_1 == 1) %>% 
  group_by(PROSPECT_RANK_GROUP, MLB_PLAYER) %>% 
  tabyl(PROSPECT_RANK_GROUP, MLB_PLAYER) %>% 
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns()

## create summary table for pitchers
pitching_prospect_mlb_debut_rate <- top_prospect_rankings_last_max_all %>% 
  mutate(PROSPECT_RANK_GROUP = case_when(PROSPECT_RANK %in% 1:25 ~ "1 - 25", 
                                         PROSPECT_RANK %in% 26:50 ~ "26 - 50", 
                                         PROSPECT_RANK %in% 51:75 ~ "51 - 75", 
                                         PROSPECT_RANK %in% 76:100 ~ "76 - 100", 
                                         is.na(PROSPECT_RANK) ~ "Unranked")) %>% 
  relocate(PROSPECT_RANK_GROUP, .after = PROSPECT_RANK) %>% 
  mutate(MLB_PLAYER = if_else(!is.na(PLAY_DEBUT), "Yes", "No"), 
         MLB_PLAYER = factor(MLB_PLAYER, levels = c("Yes", "No"))) %>%
  filter(YEAR < "2011-01-01" & POSITION_1 == 1) %>% 
  group_by(PROSPECT_RANK_GROUP, MLB_PLAYER) %>% 
  tabyl(PROSPECT_RANK_GROUP, MLB_PLAYER) %>% 
  adorn_totals(c("col", "row")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns()

# Transforming & Exporting Final Datasets ----------------------------------

# summary batter and pitcher dataframes

# duplicate and rename the dataframes
batter_info_and_stats_1990_2020_debuted_1990_2010 <- all_batter_career_data_by_ID

pitcher_info_and_stats_1990_2020_debuted_1990_2010 <- all_pitcher_career_data_by_ID

# batters

# show column names
colnames(batter_info_and_stats_1990_2020_debuted_1990_2010)

# rename QUARTILE_ONE_WOBA
batter_info_and_stats_1990_2020_debuted_1990_2010 <- 
  batter_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  rename(FIRST_150_WOBA = QUARTILE_ONE_WOBA)

# remove unwanted columns
batter_info_and_stats_1990_2020_debuted_1990_2010 <- 
  batter_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  select(-c(13:19, 25))

# relocate columns
batter_info_and_stats_1990_2020_debuted_1990_2010 <- 
  batter_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  relocate(PLAYER_STATUS, .after = CAREER_TOTAL_PA) %>% 
  relocate(FIRST_900_WOBA, .after = FIRST_600_WOBA) %>% 
  relocate(FIRST_150_WOBA_GRADE:FIRST_150_WOBA_GRADE_SIMPLE, 
           .after = FIRST_900_WOBA) %>% 
  relocate(POST_900_WOBA, .after = POST_600_WOBA) %>% 
  relocate(POST_150_WOBA_GRADE:POST_150_WOBA_GRADE_SIMPLE, 
           .after = POST_900_WOBA) %>% 
  relocate(FIRST_POST_150_WOBA_DIFF, .before = FIRST_POST_600_WOBA_DIFF)

# filter to only show batters who debuted between 1990 and 2010
batter_info_and_stats_1990_2020_debuted_1990_2010 <- 
  batter_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  filter(BAT_DEBUT < "2011-01-01")

# pitchers

# show column names
colnames(pitcher_info_and_stats_1990_2020_debuted_1990_2010)

# rename QUARTILE_ONE_WOBA
pitcher_info_and_stats_1990_2020_debuted_1990_2010 <- 
  pitcher_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  rename(FIRST_215_WOBA = QUARTILE_ONE_WOBA)

# remove unwanted columns
pitcher_info_and_stats_1990_2020_debuted_1990_2010 <- 
  pitcher_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  select(-c(17:23, 29))

# relocate columns
pitcher_info_and_stats_1990_2020_debuted_1990_2010 <- 
  pitcher_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  relocate(PLAYER_STATUS:PITCHER_TYPE, .after = IP_MEAN) %>% 
  relocate(FIRST_1290_WOBA, .after = FIRST_860_WOBA) %>%
  relocate(FIRST_215_WOBA_GRADE:FIRST_215_WOBA_GRADE_SIMPLE, 
           .after = FIRST_1290_WOBA) %>% 
  relocate(POST_1290_WOBA, .after = POST_860_WOBA) %>%
  relocate(POST_215_WOBA_GRADE_SP:POST_215_WOBA_GRADE_NON_SP_SIMPLE, 
           .after = POST_1290_WOBA) %>%
  relocate(FIRST_POST_215_WOBA_DIFF, .before = FIRST_POST_860_WOBA_DIFF)

# filter to only show pitchers who debuted between 1990 and 2010
pitcher_info_and_stats_1990_2020_debuted_1990_2010 <- 
  pitcher_info_and_stats_1990_2020_debuted_1990_2010 %>% 
  filter(PIT_DEBUT < "2011-01-01")

# prospect dataset

# import most recent biofile from Retrosheet
updated_biofile_2024 <- read_csv("~/Downloads/updated_biofile.csv", 
                                 col_types = cols(PLAY.DEBUT = col_date(format = "%m/%d/%Y")))

# subset by debut, shorten player name, select necessary columns
updated_biofile_2024 <- updated_biofile_2024 %>% 
  filter(PLAY.DEBUT > "2020-01-01") %>% 
  unite(col = "PLAYER_NAME", c("NICKNAME", "LAST"), sep = " ") %>% 
  select(PLAYERID, PLAYER_NAME, PLAY.DEBUT)

# show players who recently debuted but were not updated in prospect dataframe
## Luis Garcia is a duplicated name (removed)
recent_prospects_na_subset <- top_prospect_rankings_last %>% 
  filter(is.na(ID) & YEAR > "2005-01-01") %>% 
  group_by(PLAYER_NAME) %>% 
  summarise() %>% 
  left_join(updated_biofile_2024, by = "PLAYER_NAME") %>% 
  filter(!is.na(PLAYERID) & !PLAYER_NAME == "Luis Garcia")

# join back to newly created top prospect rankings dataframe
ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024 <- 
  left_join(top_prospect_rankings_last, 
            recent_prospects_na_subset, 
            by = "PLAYER_NAME") %>% 
  mutate(ID = coalesce(ID, PLAYERID), 
         PLAY_DEBUT = coalesce(PLAY_DEBUT, PLAY.DEBUT)) %>% 
  select(-c("PLAYERID", "PLAY.DEBUT"))

# recalculate time difference between ranking date and debut date
ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024 <- 
  ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024 %>%
  mutate(RANK_DEBUT_DATEDIFF_WKS =
          as.numeric(difftime(YEAR, PLAY_DEBUT, units = "weeks")))

# if PLAY_DEBUT is 2021 or more recent, RANK_DEBUT_DATEDIFF_WKS should be NA
## this is because a prospect may have been ranked on the 2021, etc., list
ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024 <- 
  ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024 %>%
  mutate(RANK_DEBUT_DATEDIFF_WKS = if_else(PLAY_DEBUT > "2021-01-01", 
                                           NA, 
                                           RANK_DEBUT_DATEDIFF_WKS))

# export final datasets as csv files
write_csv(batter_info_and_stats_1990_2020_debuted_1990_2010,
          "batter_info_and_stats_1990_2020_debuted_1990_2010.csv")

write_csv(pitcher_info_and_stats_1990_2020_debuted_1990_2010,
          "pitcher_info_and_stats_1990_2020_debuted_1990_2010.csv")

write_csv(ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024,
          "ba_1990_2020_top_100_preseason_prospect_rankings_id_debut_updates_may_2024.csv")
