# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)
library(tidymodels)

# load data ----
data_2022 <- nflfastR::load_pbp(2022) %>%
  filter(season_type == "REG",
         !is.na(down),
         play_type %in% c("pass", "run"))

data_dictionary <- nflreadr::dictionary_pbp
team_data <- nflreadr::load_teams()

save(data_2022, file = here("data/raw_data/eda_data.rda"))
save(data_dictionary, file = here("data/data_dictionary.rda"))

# this data is not for the same season as the modeling process
# and is not a part of the same 'sample'
# i'm using those to look for some general trends and create
# graphics that might be helpful for the final output
# but none of the claims about individual teams will be representative

# Play call variation by ----
# making a function
col_chart <- function(var) {
  filename <- rlang::englue("play_calls_by_{var}.jpg")
  
  plot <- data_2022 %>%
    filter(!is.na(var)) %>%
    summarize(pass_percentage = sum(pass) / n() * 100,
              .by = var) %>%
    ggplot(aes(x = !!rlang::sym(var), y = pass_percentage)) +
    geom_col(position = "stack", fill = "darkblue")
  
  ggsave(filename = filename, plot = plot, path = here("plots/"))
}

# loop the function
loop_vars <- c("week", "posteam", "defteam", "start_time", "roof", "surface", "total_line")

for (var in loop_vars) {
  print(col_chart(var))
}

data_2022 %>%
  count(start_time)

# results from this:
# offensive team has a big impact, lots of variability
# defense matters, but to a lesser extent
# the environmental factors are not super relevant
# vegas spread has a relationship, but definitely because of confounding
# week may matter (run in december), but low variability

# is unpredictability a good thing?
# answer: basically no relationship, but interesting

results_2022 <- nflreadr::load_schedules(seasons = 2022) %>%
  filter(game_type == "REG") %>%
  pivot_longer(cols = c(away_team, home_team),
               values_to = "team") %>%
  select(team, name, away_score, home_score, result) %>%
  mutate(win = 
           if_else(result == 0, 0.5,
    if_else(name == "home_team" & result > 0 | name == "away_team" & result < 0, 1, 0))) %>%
  summarize(win_percentage = mean(win), .by = team)
  
# check if unpredictability effects success -> record
# basically no relationship once again

data_2022 %>%
  mutate(play_call_unpredictability = (pass_oe)^2) %>%
  summarize(unpredictability = sum(play_call_unpredictability),
            success = sum(epa),
            .by = posteam) %>%
  mutate(unpredictability = unpredictability / max(unpredictability),
         success = success / max(success)) %>%
  left_join(results_2022, by = join_by(posteam == team)) %>%
  ggplot(aes(x = unpredictability, y = success, label = posteam, color = win_percentage)) +
  geom_vline(xintercept = 0.854, lty = 2, color = "red") + # calculated mean
  geom_hline(yintercept = -0.0431, lty = 2, color = "red") + # calculated mean
  geom_smooth(method = "lm") +
  geom_label()



data_2022 %>%
  summarize(pass_percentage = sum(pass) /n() * 100, .by = c(posteam_type, posteam)) %>%
  pivot_wider(names_from = posteam_type, values_from = pass_percentage) %>%
  mutate(diff = home - away) %>% view() # defteam similar results
  # I'll include it, but test in one recipe because my gut says this is noise

