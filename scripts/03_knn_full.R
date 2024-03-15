## K-Nearest Neighbors Script
## Purpose: Create, tune, and fit knn model

# Computation Time: 24:32

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

# Defining KNN ----
knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

full_knn <- workflow() %>%
  add_model(knn_spec) %>%
  add_recipe(full_dummy_recipe)

# Hyperparameter Tuning ----
extract_parameter_set_dials(knn_spec)

full_knn_parameters <- parameters(knn_spec) %>%
  update(neighbors = neighbors(c(10, 100)))

full_knn_grid <- grid_regular(full_knn_parameters, levels = 10)

# Fitting The Models ----
set.seed(12373)
full_knn_fit <- full_knn %>%
  tune_grid(data_folds, grid = full_knn_grid,
            control = control_grid(save_workflow = TRUE,
                                   save_pred = TRUE))
# Save Out Fits ----
save(full_knn_fit, file = here("results/full_knn_fit.rda"))
