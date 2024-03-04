## Final Assessment Script
## Purpose: Compare final predictions to the test set

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load data ----
load(here("data/data_split/testing_data.rda"))

# To Do List ----