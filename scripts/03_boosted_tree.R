## Boosted Tree Script
## Purpose: Create, tune, and fit boosted tree model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/data_split/data_folds.rda"))

load(here("recipes/standard_recipe.rda"))
load(here("recipes/full_recipe.rda"))

# Defining Boosted Tree
boosted_spec <- boost_tree(min_n = tune(),
                           mtry = tune(),
                           learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

standard_boosted <- workflow() %>%
  add_model(boosted_spec) %>%
  add_recipe(standard_recipe)

full_boosted <- workflow() %>%
  add_model(boosted_spec) %>%
  add_recipe(full_recipe)

# Hyperparameter Tuning
extract_parameter_set_dials(boosted_spec)

standard_boosted_parameters <- parameters(boosted_spec) %>%
  update(mtry = mtry(c(1, 8)),
         learn_rate = learn_rate(c(-2, -0.2)))

full_boosted_parameters <- parameters(boosted_spec) %>%
  update(mtry = mtry(c(1, 15)),
         learn_rate = learn_rate(c(-2, -0.2)))

standard_boosted_grid <- grid_regular(standard_boosted_parameters, levels = 5)
full_boosted_grid <- grid_regular(full_boosted_parameters, levels = 5)

# Fitting the Models
set.seed(34048)
standard_boosted_fit <- standard_boosted %>%
  tune_grid(data_folds, grid = standard_boosted_grid,
            control = control_grid(save_workflow = FALSE,
                                   save_pred = FALSE))

set.seed(42131)
full_boosted_fit <- full_boosted %>%
  tune_grid(data_folds, grid = full_boosted_grid,
            control = control_grid(save_workflow = FALSE,
                                   save_pred = FALSE))

# Note: Saving predictions is ideal, but computationally hard
# For the final model, I would like to have them for analysis
# But for now, I am choosing not to save them to save time/effort

# Save out fits
save(standard_boosted_fit, file = here("results/standard_boosted_fit.rda"))
save(full_boosted_fit, file = here("results/full_boosted_fit.rda"))

