# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)

# load data ----
data_2023 <- load_pbp(2023) %>%
  filter(season_type == "REG")

write_csv(data_2023, file = here("data/raw_data.csv"))

data_2023 <- read_csv(here("data/raw_data.csv"))
team_data <- nflreadr::load_teams()

# individuality check ----
data_2023 %>%
  filter(play_type %in% c("pass", "run")) %>%
  summarize(pass_percentage = sum(pass) /n() * 100,
            .by = posteam) %>%
  arrange(desc(pass_percentage)) %>%
  left_join(team_data, by = join_by(posteam == team_abbr)) %>%
  ggplot(aes(x = posteam, y = pass_percentage)) +
  geom_col(position = "stack")

# graph for first memo ----
modeling_data %>% count(play_type)

modeling_data %>%
  ggplot(aes(x = play_type, fill = play_type)) +
  geom_bar() +
  labs(x = "Play Type", y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

ggsave(filename = "play_type.jpg", path = here("plots/"))

# data selection ----
modeling_data <- data_2023 %>%
  
  # Only want run/pass plays
  # Avoiding Week 18 because of starters resting
  filter(between(week, 1, 17),
         !is.na(down),
         play_type %in% c("pass", "run"),
         aborted_play == 0) %>%
  
  # Choosing variables to work with
  select(home_team, posteam, posteam_type, defteam, yardline_100, game_date, half_seconds_remaining,
         qtr, down, ydstogo, play_type, score_differential_post, fg_prob, safety_prob, td_prob, epa,
         total_home_rush_epa, total_home_pass_epa, total_away_rush_epa, total_away_pass_epa, drive)
  
  # where to go from here
    # create game variables and rolling success
    # fix variable types (rename for graphs)
    # initial split
    # save out files








