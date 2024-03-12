## Final Model Script
## Purpose: Create, tune, and fit final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/data_split/training_data.rda"))
load(here("results/full_forest_fit.rda")) # Best model fit, check standard too

# Defining Final Model
final_wflow <- extract_workflow(full_forest_fit) %>%
  finalize_workflow(select_best(full_forest_fit, metric = "accuracy"))

set.seed(12175)
final_fit <- fit(final_wflow, training_data)

# Save out final results
save(final_fit, file = here("results/final_fit.rda"))