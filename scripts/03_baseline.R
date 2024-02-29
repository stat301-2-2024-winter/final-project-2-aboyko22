## Baseline Model Script
## Purpose: Develop baseline models (null & basic)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# NOTE ----
# None of this is saved out because the folds will be rewritten
# Not traditional practice

# load data ----
load(here("data/data_folds.rda"))

# Defining Null
null_spec <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

null_wflow <- workflow() %>%
  add_recipe(null_recipe) %>%
  add_model(null_spec)

null_fit <- fit_resamples(null_wflow, data_folds, control = control_resamples(save_workflow = FALSE))

null_fit %>%
  collect_metrics(metric = "accuracy")

# Defining Basic Recipe
basic_spec <-


# To Do List ----