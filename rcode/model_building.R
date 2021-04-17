library(tidyverse)
library(GGally)
library(tidymodels)
load(file = "Data/updated_diamonds.Rda")

# Since 

# Splitting the data set.
set.seed(555)

data_split <- initial_split(diamonds, prop = 0.8)

train_data <- training(data_split)
test_data <- testing(data_split)



# Looking at a few important graphs again with the training data this time
# instead of the complete data.

train_data %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  ggcorr(label = TRUE) +
  ggtitle("Pearson Correlation of Continuous Variables in Training Data")

train_data %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  pairs(main = "Scatterplots of Continuous Variables in Training Data")

# log scale all of the numerical variables
train_data %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  mutate_all(log) %>%
  pairs(main = "Scatterplots of the log of Continuous Variables in Training Data")

train_data %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  mutate_at(c("price", "carat", "x", "y", "z"),log) %>%
  pairs(main = "Scatterplots of the log of Continuous Variables* in Training Data")
title(sub = "*Except table and depth which are kept at their regular values")

# Depth and table percentage both do not appear to be linearly correlated with
# our response variable: price. In fact both variables appear to be evenly
# scattered along all possible values of price.

# Let us make some interaction graphs to explore if any interactions may be
# worthwhile in our models. Carat (and log(carat)) is highly collinear 
# with x, y, and z, and it is an important attribute of a diamond (its weight)
# so let's use that to inspect possible interactions.

train_data %>%
  mutate(log_price = log(price)) %>%
  mutate(log_carat = log(carat)) %>%
  ggplot(aes(x = log_carat, y = log_price, color = cut)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("log(Carat)")+
  ylab("log(Price) (log US Dollars)") +
  ggtitle("Simple log(Carat)*Cut Interaction Plot") +
  theme_minimal()

train_data %>%
  mutate(log_price = log(price)) %>%
  mutate(log_carat = log(carat)) %>%
  ggplot(aes(x = log_carat, y = log_price, color = color)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("log(Carat)")+
  ylab("log(Price) (log US Dollars)") +
  ggtitle("Simple log(Carat)*Color Interaction Plot") +
  theme_minimal()


train_data %>%
  mutate(log_price = log(price)) %>%
  mutate(log_carat = log(carat)) %>%
  ggplot(aes(x = log_carat, y = log_price, color = clarity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("log(Carat)")+
  ylab("log(Price) (log US Dollars)") +
  ggtitle("Simple log(Carat)*Clarity Interaction Plot") +
  theme_minimal()



# All three suggest that there may be some interactions, 
# however the log(Carat)*Color interaction looks to be the weakest

## See if it is feasible to construct confidence intervals for the next 
## interaction plots
train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(cut, color) %>%
  summarise(mean_log_price = mean(log_price),
            total_n = n()) %>%
  arrange(total_n) # minimum n is 89

train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(cut, clarity) %>%
  summarise(mean_log_price = mean(log_price),
            total_n = n()) %>%
  arrange(total_n) # minimum n is 7
## Because this sample size is so low, we may opt to not construct t-intervals
## for some of these categorical combinations

train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(clarity, color) %>%
  summarise(mean_log_price = mean(log_price),
            total_n = n()) %>%
  arrange(total_n) # min n is 33


train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(cut, color) %>%
  summarise(mean_log_price = mean(log_price),
            sd_log_price = sd(log_price),
            total_n = n()) %>%
  ggplot(aes(x = cut, y = mean_log_price, group = color, color = color)) +
  geom_point(position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = mean_log_price-qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)),
                    ymax = mean_log_price+qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n))),
                 position=position_dodge(width=0.5)) +
  geom_line(position=position_dodge(width=0.5)) +
  theme_minimal() +
  scale_colour_brewer(type = "qual", palette = 3)

## let us inspect just J and H
train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(cut, color) %>%
  summarise(mean_log_price = mean(log_price),
            sd_log_price = sd(log_price),
            total_n = n()) %>%
  filter(color %in% c("J", "H")) %>%
  ggplot(aes(x = cut, y = mean_log_price, group = color, color = color)) +
  geom_point() +
  geom_linerange(aes(ymin = mean_log_price-qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)),
                     ymax = mean_log_price+qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)))) +
  geom_line() +
  theme_minimal() +
  scale_colour_brewer(type = "qual", palette = 3)

## let us also inspect H and G
train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(cut, color) %>%
  summarise(mean_log_price = mean(log_price),
            sd_log_price = sd(log_price),
            total_n = n()) %>%
  filter(color %in% c("G", "H")) %>%
  ggplot(aes(x = cut, y = mean_log_price, group = color, color = color)) +
  geom_point() +
  geom_linerange(aes(ymin = mean_log_price-qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)),
                     ymax = mean_log_price+qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)))) +
  geom_line() +
  theme_minimal() +
  scale_colour_brewer(type = "qual", palette = 3)

## and E and D
train_data %>%
  mutate(log_price = log(price)) %>%
  group_by(cut, color) %>%
  summarise(mean_log_price = mean(log_price),
            sd_log_price = sd(log_price),
            total_n = n()) %>%
  filter(color %in% c("E", "D")) %>%
  ggplot(aes(x = cut, y = mean_log_price, group = color, color = color)) +
  geom_point() +
  geom_linerange(aes(ymin = mean_log_price-qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)),
                     ymax = mean_log_price+qt(1-(0.05/(2*35)), total_n - 1)*(sd_log_price/sqrt(total_n)))) +
  geom_line() +
  theme_minimal() +
  scale_colour_brewer(type = "qual", palette = 3)

# Building a recipe
## Going to use a bagged trees imputation method to deal with missing values
## in x, y, and z.
### By using a recipe, we can impute missing values in the test set the
### the exact same way we did for the training set.

diamonds_ridge_rec1 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  # Also depth is a calculation based on x, y, and z.
  step_bagimpute(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(price, carat, x, y, z) %>%
  step_normalize(all_predictors())

diamonds_ridge_rec2 <- recipe(price ~ ., data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  # Also depth is a calculation based on x, y, and z.
  step_bagimpute(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(price, carat, x, y, z) %>%
  step_normalize(all_predictors())

diamonds_ridge_rec3 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  # Also depth is a calculation based on x, y, and z.
  step_bagimpute(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(price, carat, x, y, z) %>%
  step_interact(terms = ~ cut:carat) %>%
  step_interact(terms = ~ clarity:carat) %>%
  step_interact(terms = ~ color:carat) %>%
  step_normalize(all_predictors())

diamonds_rf_rec <- recipe(price ~ ., data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  # Also depth is a calculation based on x, y, and z.
  step_bagimpute(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(price, carat, x, y, z) %>%
  step_normalize(all_predictors())

# Model setups

## This is a "pure" ridge regression model so it will not do feature selection  
ridge_mod <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

  
# Random forest model.
rf_mod <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Create folds for cross-validation parameter tuning
set.seed(444)
folds <- vfold_cv(train_data, v = 10)

# Workflows for the models
ridge_wf1 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec1) 

ridge_wf2 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec2)

ridge_wf3 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec3)


rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(diamonds_rf_rec)

# Tuning model parameters
set.seed(333)

ridge_fit1_rs <- ridge_wf1 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))

ridge_fit1_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line()

ridge_fit2_rs <- ridge_wf2 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))

ridge_fit2_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line()

ridge_fit3_rs <- ridge_wf3 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))

ridge_fit3_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line()


registerDoParallel()
set.seed(222)
rf_fit_tune <- rf_wf %>%
  tune_grid(resamples = folds, grid = 20)
stopImplicitCluster()
