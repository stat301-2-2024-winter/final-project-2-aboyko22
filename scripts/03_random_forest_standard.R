## Random Forest Script
## Purpose: Create, tune, and fit random forest model

# Computation time: 7:40:19

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

# Defining Random Forest
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

standard_rf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(standard_recipe)

# Hyperparameter Tuning
extract_parameter_set_dials(rf_spec)

standard_rf_parameters <- parameters(rf_spec) %>%
  update(mtry = mtry(c(2, 10)),
    min_n = min_n(c(4, 20)))

standard_rf_grid <- grid_regular(standard_rf_parameters, levels = 5)

# Fitting the Models
set.seed(10440)
standard_forest_fit <- standard_rf %>%
  tune_grid(data_folds, grid = standard_rf_grid,
            control = control_grid(save_workflow = TRUE,
                                   save_pred = TRUE))
# Save out fits
save(standard_forest_fit, file = here("results/standard_forest_fit.rda"))
