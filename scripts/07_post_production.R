## Post-Production Script
## Purpose: Create files for report & full data EDA

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(janitor)

tidymodels_prefer()
registerDoMC(cores = 6)

# load data ----
load(here("data/cleaned_data/cleaned_data.rda"))
load(here("results/final_fit.rda"))
load(here("recipes/full_recipe.rda"))
load(here("results/best_models.rda"))
load(here("data/data_split/training_data.rda"))

# info about data ----
modeling_data %>%
  ggplot(aes(x = play_type, fill = play_type)) +
  geom_bar() +
  labs(x = "Play Type", y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("navy", "darkred"))

predictions %>%
  mutate(prediction = if_else(xpass >= 0.50, 1, 0),
         correct = if_else(prediction == pass, 1, 0)) %>%
  summarize(accuracy = mean(correct))


# graphing model accuracies ----
accuracy_plot <- best_models %>%
  ggplot(aes(x = wflow_id, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), width = 0.2) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

ggsave(filename = "accuracy_plot.jpg", path = "plots/")

# Identifying Example Team
texans_graph <- modeling_data %>%
  summarize(pass_percent = mean(pass) * 100,
            success = mean(last_play_success),
            .by = posteam) %>%
  adorn_totals(name = "mean") %>%
  mutate(across(where(is.numeric), 
                ~ replace(., n(), .[n()]/(n()-1)))) %>%
  ggplot(aes(x = pass_percent, y = success, label = posteam)) +
  geom_smooth(method = "lm", color = "red") +
  geom_label() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.title.position = "plot") +
  labs(x = "Pass Percentage", y = "Success (EPA/play)",
       title = "Scatterplot of NFL Teams by Playcalling and Success")

ggsave(filename = "texans_graph.jpg", path = here("plots/"))


# Choosing 2023 Houston Texans
texans_data <- modeling_data %>%
  filter(posteam == "HOU")

set.seed(12125)
texans_split <- initial_split(texans_data, prop = 0.75, strata = play_type)

texans_training <- training(texans_split)
texans_testing <- testing(texans_split)

get_metrics <- metric_set(accuracy)

# texans rf forest
rf_spec <- rand_forest(mtry = 8, min_n = 20, trees = 1500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

full_rf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(full_recipe)

# fit to testing
set.seed(31046)
texans_fit <- full_rf %>% fit(texans_training)

# compare
texans_predictions <- bind_cols(predict(texans_fit, texans_testing), texans_testing)
save(texans_predictions, file = here("results/texans_results.rda"))


get_metrics(texans_predictions, truth = play_type, estimate = .pred_class)










