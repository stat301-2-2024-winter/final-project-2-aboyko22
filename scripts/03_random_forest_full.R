## Random Forest Script
## Purpose: Create, tune, and fit random forest model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/data_split/data_folds.rda"))
load(here("recipes/full_recipe.rda"))

# To Do List ----

# Create spec and workflow
# Define tuning parameters
# Fit to resamples
# Save out files