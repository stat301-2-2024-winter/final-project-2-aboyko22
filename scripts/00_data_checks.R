# load packages ----
library(tidyverse)
library(nflverse)

data2023 <- load_pbp(2023) %>%
  filter(season_type == "REG")

data2023 %>%
  ggplot(aes(x = wpa, fill = play_type)) +
  geom_histogram(bins = 100) +
  facet_wrap(~play_type)

