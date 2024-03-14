## Final Assessment Script
## Purpose: Compare final predictions to the test set

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(ggthemes)

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

conf_mat(predictions, truth = play_type, estimate = .pred_class)

# new xpass
prob_metrics <- metric_set(rmse, mae, rsq)

prob_results <- prob_predictions %>%
  select(.pred_pass, pass) %>%
  prob_metrics(truth = pass, estimate = .pred_pass)

final_model_results <- rbind(class_results, prob_results) %>%
  select(-.estimator)

save(final_model_results, file = here("results/final_model_results.rda"))

xpass_results <- predictions %>%
  mutate(prediction = factor(if_else(xpass >= 0.50, 1, 0)),
         pass = factor(pass)) %>%
  select(prediction, pass) %>%
  class_metrics(truth = pass, estimate = prediction)

xpass_prob_results <- prob_predictions %>%
  select(xpass, pass) %>%
  prob_metrics(truth = pass, estimate = xpass)

xpass_final_results <- rbind(xpass_results, xpass_prob_results) %>%
  select(-.estimator)

save(xpass_final_results, file = here("results/xpass_final_results.rda"))

# graphing results ----
prob_predictions %>%
  ggplot(aes(x = xpass, y = .pred_pass, color = play_type)) +
  geom_point(alpha = 0.3) +
  geom_abline(lty = 2) +
  coord_obs_pred() +
  scale_color_brewer(palette = "Set1")

density <- prob_predictions %>%
  ggplot() +
  geom_density(aes(x = xpass), color = "red") +
  geom_density(aes(x = .pred_pass), color = "navy") +
  labs(x = "Predicted Pass Probability", y = "Density") +
  theme_minimal()

ggsave("density_plot.jpg", path = here("plots/"))

# faceted graphs
predictions <- predictions %>%
  mutate(distance = case_when(
            between(ydstogo, 0, 2) ~ "Short",
            between(ydstogo, 3, 7) ~ "Medium",
            .default = "Long"),
    match = if_else(.pred_class == play_type, 1, 0),
    prediction = if_else(xpass >= 0.5, 1, 0),
    xpass_match = if_else(prediction == pass, 1, 0))

faceted_plot <- predictions %>%
  ggplot(aes(x = .pred_class, fill = factor(match))) +
  geom_bar(show.legend = FALSE) +
  facet_grid(vars(distance), vars(down), scales = "free") +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  labs(title = "Prediction Accuracy by Down and Distance")

ggsave(filename = "faceted_plot.jpg", path = here("plots/"))

xpass_table <- predictions %>%
  summarize(accuracy = mean(xpass_match),
            count = n(),
            .by = c(down, distance))

faceted_table <- predictions %>%
  summarize(accuracy = mean(match),
            count = n(),
            .by = c(down, distance)) %>%
  left_join(xpass_table, by = join_by(down, distance, count)) %>%
  mutate(accuracy_diff = accuracy.x - accuracy.y) %>%
  rename(accuracy = accuracy.x) %>%
  relocate(accuracy_diff, .before = count) %>%
  select(-accuracy.y) %>%
  arrange(down, distance)


save(faceted_table, file = here("results/faceted_table.rda"))

