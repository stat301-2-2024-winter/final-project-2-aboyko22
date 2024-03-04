## Post-Production Script
## Purpose: Create files for report & full data EDA

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(janitor)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/cleaned_data/cleaned_data.rda"))

# Identifying Example Team
modeling_data %>%
  summarize(pass_percent = mean(pass) * 100,
            success = mean(last_play_success),
            .by = posteam) %>%
  adorn_totals(name = "mean") %>%
  mutate(across(where(is.numeric), 
                ~ replace(., n(), .[n()]/(n()-1)))) %>%
  ggplot(aes(x = pass_percent, y = success, label = posteam)) +
  geom_label() +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Pass Percentage", y = "Success (EPA/play)")

# Choosing 2023 Houston Texans
texans_data <- modeling_data %>%
  filter(posteam == "HOU")


# To Do List ----