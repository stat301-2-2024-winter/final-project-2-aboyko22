## K-Nearest Neighbors Script
## Purpose: Create, tune, and fit knn model

# Computation Time: 18:33

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/data_split/data_folds.rda"))
load(here("recipes/standard_dummy_recipe.rda"))

# Defining KNN
knn_spec <- nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

standard_knn <- workflow() %>%
  add_model(knn_spec) %>%
  add_recipe(standard_dummy_recipe)

# Hyperparameter Tuning
extract_parameter_set_dials(knn_spec)

standard_knn_parameters <- parameters(knn_spec) %>%
  update(neighbors = neighbors(c(10, 100)))

standard_knn_grid <- grid_regular(standard_knn_parameters, levels = 10)

# Fitting the Models
set.seed(22077)
standard_knn_fit <- standard_knn %>%
  tune_grid(data_folds, grid = standard_knn_grid,
            control = control_grid(save_workflow = TRUE,
                                   save_pred = TRUE))
# Save out fits
save(standard_knn_fit, file = here("results/standard_knn_fit.rda"))
