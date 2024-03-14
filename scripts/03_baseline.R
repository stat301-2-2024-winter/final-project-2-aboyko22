## Baseline Model Script
## Purpose: Develop baseline models (null & basic)

# For future reference
# Not saving predictions during this stage
# Computation

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/data_split/data_folds.rda"))

load(here("recipes/null_recipe.rda"))
load(here("recipes/basic_recipe.rda"))

# Defining Null
null_spec <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

null_wflow <- workflow() %>%
  add_recipe(null_recipe) %>%
  add_model(null_spec)

null_fit <- fit_resamples(null_wflow, data_folds,
                          control = control_resamples(save_workflow = TRUE,
                                                      save_pred = TRUE))

# Defining Basic Recipe
basic_spec <- logistic_reg(penalty = 0.01) %>%
  set_engine('glmnet') %>%
  set_mode("classification")

basic_wflow <- workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(basic_spec)

set.seed(9554)
basic_fit <- fit_resamples(basic_wflow, data_folds,
                           control = control_resamples(save_workflow = TRUE,
                                                       save_pred = TRUE))
# Save out fits  
save(null_fit, file = here("results/null_fit.rda"))
save(basic_fit, file = here("results/basic_fit.rda"))
