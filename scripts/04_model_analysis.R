## Model Analysis Script
## Purpose: Check initial metrics and choose final model

# Load Packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# Load Data ----
load(here("results/null_fit.rda"))
load(here("results/basic_fit.rda"))
load(here("results/standard_boosted_fit.rda"))
load(here("results/full_boosted_fit.rda"))
load(here("results/standard_knn_fit.rda"))
load(here("results/full_knn_fit.rda"))
load(here("results/standard_forest_fit.rda"))
load(here("results/full_forest_fit.rda"))

# To Do List ----
best_models <- as_workflow_set(
  null_model = null_fit,
  casual_fan = basic_fit,
  
  knn_coach = standard_knn_fit,
  knn_computer = full_knn_fit,
  
  boosted_coach = standard_boosted_fit,
  boosted_computer = full_boosted_fit,
  
  forest_coach = standard_forest_fit,
  forest_computer = full_forest_fit) %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  slice_max(mean, by = wflow_id) %>%
  select(wflow_id, mean, std_err)

save(best_models, file = here("results/best_models.rda"))

full_forest_fit %>%
  select_best(metric = "accuracy")
