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

# Defining Basic Recipe
basic_spec <- logistic_reg(penalty = 0.01) %>%
  set_engine('glmnet') %>%
  set_mode("classification")

basic_wflow <- workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(basic_spec)

basic_fit <- fit_resamples(basic_wflow, data_folds, control = control_resamples(save_workflow = FALSE))

id <- tibble(id = c('null', "basic"))
null_results <- collect_metrics(null_fit) %>% select(.metric, mean, n, std_err)      
basic_results <- collect_metrics(basic_fit) %>% select(.metric, mean, n, std_err)  

baseline_results <- rbind(null_results, basic_results) %>%
  filter(.metric == "accuracy") %>%
  bind_cols(id) %>% relocate(id)
  

save(baseline_results, file = here("memos/memo_files/baseline_results.rda"))


# To Do List ----