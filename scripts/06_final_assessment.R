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

qtr_plot <- predictions %>%
  mutate(qtr = if_else(qtr == 5, "OT", qtr)) %>%
  ggplot(aes(x = .pred_class, fill = factor(match))) +
  geom_bar(show.legend = FALSE) +
  facet_grid(vars(qtr), vars(down), scales = "free") +
  theme_fivethirtyeight() +
  theme(plot.title.position = "plot") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  labs(title = "Prediction Accuracy by Down and Quarter")

ggsave("qtr_prediction.jpg", path = here("plots/"))

predictions %>%
  summarize(accuracy = mean(match), .by = posteam) %>%
  ggplot(aes(x = reorder(posteam, accuracy), y = accuracy)) +
  geom_col(aes(color = posteam, fill = posteam), position = "stack") +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl() +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_nfl_logo(), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", title = "Accuracy Rates by Team")

ggsave("accuracy_rates.jpg", path = here("plots/"))

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

## Unpredictability Plot ----
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

## EPA and Unpredictability
# Literally no relationship
cor_data <- total_predictions %>%
  mutate(qtr = as.numeric(qtr),
         down = as.numeric(down),
         posteam_type = if_else(posteam_type == "home", TRUE, FALSE),
         month = as.numeric(month),
         ) %>%
  select(-c(posteam, defteam, last_play, play_type, distance, .pred_class, goal_to_go)) 

cor_matrix <- cor(cor_data) %>%
  as_tibble()

cor_matrix$var1 <- colnames(cor_matrix)
cor_matrix <- gather(cor_matrix, key = "var2", value = "value", -var1)

cor_matrix %>%
  ggplot(aes(x = var1, y = var2, fill = value)) +
  geom_tile(color = 'black') +
  scale_fill_gradient2(low = "navy", mid = "white", high = "darkred", midpoint = 0, limits = c(-1, 1)) +
  labs(x = "", y = "", fill = "Correlation") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 20, hjust = 0),
        legend.position = "right", legend.direction = "vertical")

cor_matrix %>%
  filter(var1 == ".pred_pass") %>%
  mutate(value = abs(value)) %>%
  view()

summary(lm(.pred_pass ~ xpass, data = total_predictions))

total_predictions %>%
  ggplot(aes(x = xpass, y = .pred_pass, color = match)) +
  geom_point(alpha = 0.3, size = 2) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", axis.title = element_text()) +
  annotate(geom = "rect", xmin = 0.40, xmax = 0.55, ymax = 1, ymin = 0,
           color = "red", alpha = 0.2) +
  labs(x = "Expected Pass Probability",
       y = "Model Pass Probability")

total_predictions %>%
  filter(between(xpass, 0.40, 0.55),
         .pred_pass > 0.75 | .pred_pass < 0.15) %>%
  summarize(n = n(),
            .by = c(qtr, down, score_differential, distance, play_type)) %>% view()

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
