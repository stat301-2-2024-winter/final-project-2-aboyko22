## Model Analysis Script
## Purpose: Check initial metrics and choose final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load data ----
load(here("results/standard_boosted_fit.rda"))
load(here("results/full_boosted_fit.rda"))
load(here("results/standard_knn_fit.rda"))
load(here("results/full_knn_fit.rda"))
load(here("results/full_forest_fit.rda"))

# To Do List ----
best_models <- as_workflow_set(
  f_bt = full_boosted_fit,
  s_bt = standard_boosted_fit,
  f_knn = full_knn_fit,
  s_knn = standard_knn_fit,
  f_rf = full_forest_fit) %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  slice_max(mean, by = wflow_id)

full_forest_fit %>%
  select_best(metric = "accuracy")
