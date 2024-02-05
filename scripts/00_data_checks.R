# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)

data_2023 <- load_pbp(2023) %>%
  filter(season_type == "REG")

write_csv(data_2023, file = here("data/raw_data.csv"))

# data selection
modeling_data <- data_2023 %>%
  filter(between(week, 2, 17),
         !is.na(down),
         play_type %in% c("pass", "run"))

modeling_data %>% count(play_type)

modeling_data %>%
  ggplot(aes(x = play_type, fill = play_type)) +
  geom_bar() +
  labs(x = "Play Type", y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

ggsave(filename = "play_type.jpg", path = here("plots/"))
