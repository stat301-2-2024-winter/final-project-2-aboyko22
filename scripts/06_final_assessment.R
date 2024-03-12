## Final Assessment Script
## Purpose: Compare final predictions to the test set

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

tidymodels_prefer()

# load data ----
load(here("data/data_split/testing_data.rda"))
load(here("results/final_fit.rda"))

# attach to testing data ----
predictions <- bind_cols(predict(final_fit, testing_data), testing_data)
prob_predictions <- bind_cols(predict(final_fit, testing_data, type = "prob"),
                         testing_data)

class_metrics <- metric_set(accuracy)

class_results <- predictions %>%
  select(.pred_class, play_type) %>%
  class_metrics(truth = play_type, estimate = .pred_class)

# new xpass
prob_metrics <- metric_set(rmse, mae, mape, rsq)

prob_results <- prob_predictions %>%
  select(.pred_pass, pass) %>%
  prob_metrics(truth = pass, estimate = .pred_pass)

xpass_results <- prob_predictions %>%
  select(xpass, pass) %>%
  prob_metrics(truth = pass, estimate = xpass)

# graphing results ----
prob_predictions %>%
  ggplot(aes(x = xpass, y = .pred_pass, color = play_type)) +
  geom_point(alpha = 0.3) +
  geom_abline(lty = 2) +
  coord_obs_pred() +
  scale_color_brewer(palette = "Set1")

prob_predictions %>%
  ggplot(aes(xpass)) +
  geom_density() +
  facet_wrap(~play_type)

prob_predictions %>%
  ggplot(aes(.pred_pass)) +
  geom_density() +
  facet_wrap(~play_type)

# tabular results ----
predictions %>%
  mutate(predicted_pass = if_else(xpass >= 0.50, 1, 0),
         correct_guess = if_else(predicted_pass == pass, 1, 0),
         .keep = "used") %>%
  summarize(accuracy = mean(correct_guess))

  


