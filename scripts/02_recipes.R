## Recipe Script
## Purpose: Create recipes for all model types

# With teams, may be to hard to compute
# May need remove posteam and defteam
# If so, try to approximate defense somehow

# Load Packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# Load Data ----
load(here("data/data_split/training_data.rda"))

# Null Recipe
null_recipe <- recipe(training_data, play_type ~.) %>%
  step_rm(pass, xpass) %>%
  step_dummy(all_nominal_predictors())

# Basic Recipe (Casual Fan)
# 9 total predictor columns w/dummies
basic_recipe <- recipe(training_data %>% select(down, ydstogo, score_differential, qtr, play_type), 
                       play_type ~.) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv() %>%
  step_normalize()

# basic_recipe %>%
#   prep() %>%
#   bake(new_data = NULL) %>% glimpse()

# Standard Recipe (The Coach) version 1
# 83 total predictor columns w/dummies, 16 otherwise
standard_recipe <- training_data %>%
  select(posteam, defteam, posteam_type, score_differential, down, ydstogo, goal_to_go,
         yardline_100, qtr, half_seconds_remaining, drive, posteam_timeouts_remaining,
         last_play, game_3rd_conversion, posteam_score, defteam_score, play_type) %>%
  recipe(play_type ~.) %>%
  step_zv() %>%
  step_normalize()

standard_knn_recipe <- training_data %>%
  select(posteam, defteam, posteam_type, score_differential, down, ydstogo, goal_to_go,
         yardline_100, qtr, half_seconds_remaining, drive, posteam_timeouts_remaining,
         last_play, game_3rd_conversion, posteam_score, defteam_score, play_type) %>%
  recipe(play_type ~.) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv() %>%
  step_normalize()
 
# Ideas for version 2
#   - check if need for log/splining
#   - ideas for interactions

# standard_recipe %>%
#   prep() %>%
#   bake(new_data = NULL) %>% glimpse()

# Kitchen Sink Recipe (The Computer)
# 97 total predictor columns w/dummies, 29 otherwise
full_recipe <- recipe(training_data, play_type ~.) %>%
    update_role(pass, new_role = "id") %>%
    update_role(xpass, new_role = "id") %>%
  step_zv() %>%
  step_normalize()

full_knn_recipe <- recipe(training_data, play_type ~.) %>%
  update_role(pass, new_role = "id") %>%
  update_role(xpass, new_role = "id") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv() %>%
  step_normalize()

# full_recipe %>%
#   prep() %>%
#   bake(new_data = NULL) %>% glimpse()

# Write out recipes
save(null_recipe, file = here("recipes/null_recipe.rda"))
save(basic_recipe, file = here("recipes/basic_recipe.rda"))
save(standard_recipe, file = here("recipes/standard_recipe.rda"))
save(standard_knn_recipe, file = here("recipes/standard_knn_recipe.rda"))
save(full_recipe, file = here("recipes/full_recipe.rda"))
save(full_knn_recipe, file = here("recipes/full_knn_recipe.rda"))


