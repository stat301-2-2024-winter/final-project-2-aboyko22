## Final Assessment Script
## Purpose: Compare final predictions to the test set

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(ggthemes)
library(nflverse)

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

# new xpass ----
prob_metrics <- metric_set(rmse, mae, rsq)

prob_results <- prob_predictions %>%
  select(.pred_pass, pass) %>%
  prob_metrics(truth = pass, estimate = .pred_pass)

final_model_results <- rbind(class_results, prob_results) %>%
  select(-.estimator)

save(final_model_results, file = here("results/final_model_results.rda"))
load(here("results/final_model_results.rda"))

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
# Scatterplot - not used
prob_predictions %>%
  ggplot(aes(x = xpass, y = .pred_pass, color = play_type)) +
  geom_point(alpha = 0.3) +
  geom_abline(lty = 2) +
  coord_obs_pred() +
  scale_color_brewer(palette = "Set1")

## Density plot ----
density <- prob_predictions %>%
  ggplot() +
  geom_density(aes(x = xpass), color = "red") +
  geom_density(aes(x = .pred_pass), color = "navy") +
  labs(x = "Predicted Pass Probability", y = "Density") +
  theme_minimal()

ggsave("density_plot.jpg", path = here("plots/"))

## Faceted Plots ----

### Distance ----
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

### Quarter ----
predictions %>%
  summarize(mean_accuracy = mean(match),
            mean_xpass_accuracy = mean(xpass_match),
            n = n(),
            .by = c(qtr, down, play_type)) %>%
  mutate(diff_percent = (mean_accuracy - mean_xpass_accuracy) * 100) %>%
  arrange(-diff_percent) %>% view()

predictions %>%
  mutate(qtr = if_else(qtr == 5, "OT", qtr)) %>%
  ggplot(aes(x = .pred_class, fill = factor(match))) +
  geom_bar(show.legend = FALSE) +
  facet_grid(vars(qtr), vars(down), scales = "free") +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  labs(title = "Prediction Accuracy by Down and Quarter")




predictions %>%
  summarize(accuracy = mean(match), .by = posteam) %>%
  ggplot(aes(x = reorder(posteam, accuracy), y = accuracy)) +
  geom_col(aes(color = posteam, fill = posteam), position = "stack") +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl() +
  theme(axis.text.x = element_nfl_logo()) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "")


## Diff Density plot ----
total_predictions <- predictions %>%
    left_join(prob_predictions) %>%
    mutate(model_error = if_else(match == 1, 0, 
                                 if_else(pass == 1, .pred_run, .pred_pass)), 
          xpass_error = if_else(xpass_match == 1, 0,
                                 if_else(pass == 1, 1 - xpass, xpass)))
    
error_plot <- total_predictions %>%           
  ggplot() +
    geom_density(aes(model_error), color = "navy") +
    geom_density(aes(xpass_error), color = "red") +
  
    geom_vline(xintercept = mean(total_predictions$model_error), color = "navy", lty = 2) +
    geom_vline(xintercept = mean(total_predictions$xpass_error), color = "red", lty = 2) +

  geom_vline(xintercept = 0.585, color = "navy", lty = 3) +
  geom_vline(xintercept = 0.618, color = "red", lty = 3) +
  theme_fivethirtyeight() +
  labs(title = "Density of Prediction Error by Model Type",
       subtitle = "Lines represent mean error with and without correct guesses")

ggsave("error_plot.jpg", path = here("plots/"))

# #Unpredictability Plot ----
unpredictability <- total_predictions %>%
  mutate(epa = if_else(pass == 1, pass_epa, rush_epa)) %>%
  summarize(avg_success = mean(epa),
            model_unpredictability = mean(model_error),
            xpass_unpredictability = mean(xpass_error),
            .by = posteam) %>%
  mutate(model_difference = model_unpredictability - xpass_unpredictability)

unpredictability_plot <- unpredictability %>%  
  ggplot(aes(x = avg_success, y = model_difference)) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lty = 3) +
  geom_label(aes(label = posteam)) +
  theme_fivethirtyeight() + theme(axis.title = element_text()) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Average Success (EPA/Play)", y = "Difference in Model Accuracies",
       title = "Relationship Between Model Differences and Success")

ggsave("unpredictability_plot.jpg", path = here("plots/"))

# Faceted Table ----
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

# Pass Percents ----
predictions %>% count(.pred_class) # 0.588
mean(predictions$pass) # 0.618
mean(predictions$xpass) # 0.622
