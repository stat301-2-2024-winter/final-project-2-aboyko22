# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)
library(naniar)

# load data ----
data_2023 <- nflfastR::load_pbp(2023) %>% # only run once
  filter(season_type == "REG") # only run once

save(data_2023, file = here("data/raw_data/raw_data.rda")) # only run once

data_2023 <- read_csv(here("data/raw_data/raw_data.csv"))

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
         aborted_play == 0) %>%

  # game stats
  group_by(posteam, game_id) %>%
  mutate(
    rush_epa = if_else(rush == 1, epa, 0),
    pass_epa = if_else(pass == 1, epa, 0),
    
    game_rush_epa = cumsum(rush_epa),
    game_pass_epa = cumsum(pass_epa),
    
    last_play = lag(play_type),
    last_play_success = lag(epa),
    
    n_pass = cumsum(pass),
    n_run = cumsum(rush),
    
    game_3rd_conversion = cumsum(third_down_converted) / (cumsum(third_down_converted) + cumsum(third_down_failed)),
    
    rush_success = game_rush_epa / n_run,
    pass_success = game_pass_epa / n_pass) %>%
  ungroup() %>%
  
  # season stats
  group_by(posteam) %>%
  mutate(pass_ratio = cumsum(pass) / (cumsum(pass) + cumsum(rush)),
         avg_season_success = cumsum(epa) / cumsum(play),
         season_3rd = cumsum(third_down_converted) / (cumsum(third_down_converted) + cumsum(third_down_failed))) %>%
  ungroup() %>%

  # Choosing variables to work with
  select(posteam, defteam, posteam_type, posteam_score, defteam_score, score_differential, down,
         ydstogo, goal_to_go, yardline_100, qtr, half_seconds_remaining, drive, posteam_timeouts_remaining,
         fg_prob, safety_prob, td_prob, rush_epa, pass_epa, game_rush_epa, game_pass_epa, last_play,
         last_play_success, game_3rd_conversion, pass_ratio, avg_season_success, season_3rd, wp, game_date,
         xpass, pass, play_type) %>%
    
    # fix variable types
  mutate(posteam_type = factor(posteam_type),
         qtr = factor(qtr),   
         down = factor(down),
         month = factor(month(game_date)),
         play_type = factor(play_type)) %>%
  select(-game_date)

naniar::miss_var_summary(modeling_data) %>% print(n = 32)

# 3 new variables creating missing values
# 3rd down and last play variables
# Rather than impute later, fix these now
# One edge case (fumble on first play of game nyj vs. atl)

modeling_data <- modeling_data %>%
  mutate(last_play = factor(if_else(is.na(last_play), "drive_start", last_play)),
         last_play_success = if_else(is.na(last_play_success), avg_season_success, last_play_success),
         season_3rd = if_else(is.na(season_3rd), 0.388, season_3rd), # nfl average 2023
         game_3rd_conversion = if_else(is.na(game_3rd_conversion), season_3rd, game_3rd_conversion))

skimr::skim_without_charts(modeling_data)
    
# save out cleaned data
save(modeling_data, file = here("data/cleaned_data/cleaned_data.rda"))

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
