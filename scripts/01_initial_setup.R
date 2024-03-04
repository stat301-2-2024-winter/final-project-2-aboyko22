## Initial Setup Script
## Purpose: Transform cleaned data for modeling

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load data ----
load(here("data/cleaned_data/cleaned_data.rda"))

# initial split ---- 
set.seed(1703)
data_split <- initial_split(modeling_data, prop = 0.80, strata = play_type)

training_data <- training(data_split) # 25459 observations
testing_data <- testing(data_split) # 6366 observations

data_folds <- vfold_cv(training_data, v = 10, repeats = 4, strata = play_type)

# save out files
save(training_data, file = here("data/data_split/training_data.rda"))
save(testing_data, file = here("data/data_split/testing_data.rda"))
save(data_folds, file = here("data/data_split/data_folds.rda"))
