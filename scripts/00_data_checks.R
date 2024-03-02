# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)
library(tidymodels)

# load data ----
data_2023 <- load_pbp(2023) %>% # only run once
  filter(season_type == "REG") # only run once

write_csv(data_2023, file = here("data/raw_data.csv")) # only run once

data_2023 <- read_csv(here("data/raw_data.csv"))

# individuality check ----
data_2023 %>%
  filter(play_type %in% c("pass", "run")) %>%
  summarize(pass_percentage = sum(pass) /n() * 100,
            .by = posteam) %>%
  arrange(desc(pass_percentage)) %>%
  left_join(team_data, by = join_by(posteam == team_abbr)) %>%
  ggplot(aes(x = posteam, y = pass_percentage)) +
  geom_col(position = "stack")

# data selection ----
modeling_data <- data_2023 %>%
  
  # Only want run/pass plays
  # Avoiding Week 18 because of starters resting
  filter(between(week, 1, 17),
         !is.na(down),
         play_type %in% c("pass", "run"),
         aborted_play == 0) %>% # Play must count
  
  mutate(
  
  ) %>%
  
  # last play call and its success  
  group_by(posteam, game_id) %>%
  mutate(
    cum_epa = lag(cumsum(epa)),
    game_rush_epa = 
    
    last_play = lag(play_type),
    last_play_success = lag(epa),
    
    n_pass = cumsum(pass),
    n_run = cumsum(rush),
    
    game_3rd_conversion = cumsum(third_down_converted) / (cumsum(third_down_converted) + cumsum(third_down_failed))
  ) %>%
  ungroup()
  
  # rolling play call ratio + for the season


  
  # avg play call success

  
  # Choosing variables to work with
  select(posteam, defteam, posteam_type, posteam_score, defteam_score, yardline_100, down, ydstogo,
         goal_to_go, qtr, half_seconds_remaining, score_differential, fg_prob, safety_prob, td_prob,
         epa, total_home_rush_epa, total_home_pass_epa, total_away_rush_epa, total_away_pass_epa, drive,
         posteam_timeouts_remaining, third_down_converted, third_down_failed, wp, game_date, xpass, pass, play_type) %>%
    # fix variable types
  mutate(qtr = factor(qtr),   
         down = factor(down),
         month = factor(month(game_date))) %>%
  select(-game_date)
    
# initial split ---- 

# This is not the format this should be completed in
# Folds/Split will have to occur again later
# Only for null and baseline model

set.seed(1703)
data_split <- initial_split(modeling_data, prop = 0.80, strata = play_type)

training_data <- training(data_split)
testing_data <- testing(data_split)

data_folds <- vfold_cv(training_data, v = 10, repeats = 3, strata = play_type)

# save out files
save(training_data, file = here("data/training_data.rda"))
save(testing_data, file = here("data/testing_data.rda"))
save(data_folds, file = here("data/data_folds.rda"))

# id null results ----
modeling_data %>%
  mutate(error = abs(pass-xpass),
         bingo = if_else(pass == 1 & xpass > 0.50 | pass == 0 & xpass < 0.50, 1, 0)) %>%
  summarize(x_avg = mean(xpass),
            avg = mean(pass),
            accuracy = mean(bingo),
            abs_mean_err = mean(error))

# checking qb scrambles ----
data_2023 %>%
  filter(posteam == "BAL") %>%
  filter(pass == 1 & qb_scramble == 1) %>% view()

# qb scrambles are pass plays
# does not include designed runs

# graph for first memo ----
modeling_data %>% count(play_type)

modeling_data %>%
  ggplot(aes(x = play_type, fill = play_type)) +
  geom_bar() +
  labs(x = "Play Type", y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

ggsave(filename = "play_type.jpg", path = here("plots/"))


# 4 recipes
# - linear model
#  + variant
# - tree model
#  + variant


