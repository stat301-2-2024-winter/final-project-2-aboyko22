## Model Analysis Script
## Purpose: Check initial metrics and choose final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load data ----
load(here("results/full_boosted_fit.rda"))

# To Do List ----
full_boosted_fit %>%
  select_best(metric = "accuracy")
