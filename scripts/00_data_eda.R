# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)

# load data ----
data_2022 <- load_pbp(2022) %>%
  filter(season_type == "REG",
         !is.na(down),
         play_type %in% c("pass", "run"))

data_dictionary <- nflreadr::dictionary_pbp
save(data_dictionary, file = here("data/data_dictionary.rda"))

# this data is not for the same season as the modeling process
# and is not a part of the same 'sample'
# i'm using those to look for some general trends and create
# graphics that might be helpful for the final output
# but none of the claims about individual teams will be representative

# data narrowing (LATER)

# Play call variation by ----

## week ----
# not super important, small trend towards less later
data_2022 %>%
  filter(play_type %in% c("pass", "run")) %>%
  summarize(pass_percentage = sum(pass) / n() * 100,
            .by = week) %>%
  ggplot(aes(x = week, y = pass_percentage)) +
  geom_col(position = "stack", color = "blue")

## team ----
# makes a big difference
data_2022 %>%
  filter(play_type %in% c("pass", "run")) %>%
  summarize(pass_percentage = sum(pass) / n() * 100,
            .by = posteam) %>%
  ggplot(aes(x = posteam, y = pass_percentage)) +
  geom_col(position = "stack")

## defense ----
# important, but less important
data_2022 %>%
  filter(play_type %in% c("pass", "run")) %>%
  summarize(pass_percentage = sum(pass) / n() * 100,
            .by = defteam) %>%
  ggplot(aes(x = defteam, y = pass_percentage)) +
  geom_col(position = "stack")

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

# function works
# yay

# loop the function

loop_vars <- c("week", "posteam", "defteam", "start_time", "roof", "surface", "total_line")

for (var in loop_vars) {
  print(col_chart(var))
}









