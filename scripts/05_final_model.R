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
load(here("")) # Best model fit

# To Do List ----
