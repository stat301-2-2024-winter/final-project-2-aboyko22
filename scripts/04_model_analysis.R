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
as_workflow_set(
  f_bt = full_boosted_fit,
  s_bt = stamd
)

