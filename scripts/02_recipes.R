## Recipe Script
## Purpose: Create recipes for all model types

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load data ----
load(here("data/training_data.rda"))

# Null Recipe (To be updated)
null_recipe <- recipe(training_data, play_type ~.) %>%
  step_rm(pass, xpass) %>%
  step_dummy(posteam, defteam, posteam_type, down, qtr, month)

# Basic Recipe
basic_recipe <- recipe(training_data %>% select(down, ydstogo, score_differential, qtr, play_type), 
                       play_type ~.) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv() %>%
  step_normalize()

basic_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% view()

# To Do List ----