# load packages ----
library(tidyverse)
library(nflverse)
library(scales)
library(here)
library(tidymodels)

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
# making a function ----
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

data_2022 %>%
  mutate(play_call_unpredictability = (pass_oe)^2) %>%
  summarize(unpredictability = sum(play_call_unpredictability),
            success = sum(epa),
            .by = posteam) %>%
  mutate(unpredictability = unpredictability / max(unpredictability),
         success = success / max(success)) %>%
  ggplot(aes(x = unpredictability, y = success, label = posteam)) +
  geom_vline(xintercept = 0.854, lty = 2, color = "red") + # calculated mean
  geom_hline(yintercept = -0.0431, lty = 2, color = "red") + # calculated mean
  geom_smooth(method = "lm") +
  geom_label()


# cor matrix (LATER)


