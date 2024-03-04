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

# To Do List ----
best_models <- as_workflow_set(
  f_bt = full_boosted_fit,
  s_bt = standard_boosted_fit) %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  slice_max(mean, by = wflow_id)

# full model is WAY too accurate (97%), must be semi ID variable
# make sure rush_epa, pass_epa etc. are lagged

