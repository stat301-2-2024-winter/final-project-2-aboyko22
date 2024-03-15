## Boosted Tree Script
## Purpose: Create, tune, and fit boosted tree model

# 10:16 computation time

# Load Packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# Load Data ----
load(here("data/data_split/data_folds.rda"))
load(here("recipes/full_dummy_recipe.rda"))

# Defining Boosted Tree ----
boosted_spec <- boost_tree(min_n = tune(),
                           mtry = tune(),
                           learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

full_boosted <- workflow() %>%
  add_model(boosted_spec) %>%
  add_recipe(full_dummy_recipe)

# Hyperparameter Tuning ----
extract_parameter_set_dials(boosted_spec)

full_boosted_parameters <- parameters(boosted_spec) %>%
  update(mtry = mtry(c(1, 15)),
         learn_rate = learn_rate(c(-2, -0.2)))

full_boosted_grid <- grid_regular(full_boosted_parameters, levels = 5)

# Fitting The Models ----
set.seed(42131)
full_boosted_fit <- full_boosted %>%
  tune_grid(data_folds, grid = full_boosted_grid,
            control = control_grid(save_workflow = TRUE,
                                   save_pred = TRUE))
# Save Out Fits ----
save(full_boosted_fit, file = here("results/full_boosted_fit.rda"))
