## Random Forest Script
## Purpose: Create, tune, and fit random forest model

# Computation time: 9:51:22

# Load Packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# Load Data ----
load(here("data/data_split/data_folds.rda"))
load(here("recipes/full_recipe.rda"))

# Defining Random Forest ----
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

full_rf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(full_recipe)

# Hyperparameter Tuning ----
extract_parameter_set_dials(rf_spec)

full_rf_parameters <- parameters(rf_spec) %>%
  update(mtry = mtry(c(4, 18)),
         min_n = min_n(c(4, 20)))

full_rf_grid <- grid_regular(full_rf_parameters, levels = 4)

# Fitting The Models ----
set.seed(32011)
full_forest_fit <- full_rf %>%
  tune_grid(data_folds, grid = full_rf_grid,
            control = control_grid(save_workflow = TRUE,
                                   save_pred = TRUE))
# Save Out Fits ----
save(full_forest_fit, file = here("results/full_forest_fit.rda"))