library(tidyverse)
library(GGally)
library(tidymodels)
library(doParallel)
library(vip)
library(rpart.plot)
load(file = "Data/updated_diamonds.Rda")

# We are already aware from EDA that Price is heavily right-skewed.
# Due to this, we will log transform it before splitting the data.
# There is no data leakage as it is a 1-to-1 transformation.

diamonds <- diamonds %>%
  mutate(price = log(price))

# Splitting the data set.
# Due to the long tails in price, we will stratify it using price.
# This should separate price into quartiles and sample from there.
## This strategy follows the same one used in Chapter 5 of Tidy Modeling with R
set.seed(555)

data_split <- initial_split(diamonds, prop = 0.8, strata = price)

train_data <- training(data_split)
test_data <- testing(data_split)

# Save for future use.
save(data_split, file = "Data/data_split.Rda")
save(train_data, file = "Data/train_data.Rda")
save(test_data, file = "Data/test_data.Rda")

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

# log scale carat
train_data %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  mutate(carat = log(carat)) %>%
  pairs(main = "Scatterplots of the Continuous Variables in Training Data (carat log transformed)")

train_data %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  mutate(carat = log(carat),
         x = log(x),
         y = log(y),
         z = log(z)) %>%
  pairs(main = "Scatterplots of the Continuous Variables in Training Data\n (carat, x, y, z log transformed)")


# Depth and table percentage both do not appear to be linearly correlated with
# our response variable: price. In fact both variables appear to be evenly
# scattered along all possible values of price.



# Building a recipe
## Going to use a bagged trees imputation method to deal with missing values
## in x, y, and z.
### By using a recipe, we can impute missing values in the test set the
### the exact same way we did for the training set.

diamonds_ridge_rec1 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_normalize(all_predictors())

diamonds_ridge_rec2 <- recipe(price ~ ., data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_normalize(all_predictors())

diamonds_ridge_rec3 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_interact(terms = ~ cut:carat) %>%
  step_interact(terms = ~ clarity:carat) %>%
  step_interact(terms = ~ color:carat) %>%
  step_normalize(all_predictors())

diamonds_ridge_rec4 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_interact(terms = ~ (cut+clarity+color)^2)  %>%
  step_normalize(all_predictors())

diamonds_ridge_rec5 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_interact(terms = ~ (cut+clarity+color)^3)  %>%
  step_normalize(all_predictors())

diamonds_ridge_rec6 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_interact(terms = ~ (carat+cut+clarity+color)^2)  %>%
  step_normalize(all_predictors())

diamonds_ridge_rec7 <- recipe(price ~ id+cut+clarity+color+carat+x+y+z, data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(carat, x, y, z) %>%
  step_interact(terms = ~ (carat+cut+clarity+color)^3)  %>%
  step_normalize(all_predictors())

# Tree based models generally do not need dummy variables or normalization
diamonds_tree_rec <- recipe(price ~ ., data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z))

# Random forest generally does not need dummy variables or normalization
diamonds_rf_rec <- recipe(price ~ ., data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  step_impute_bag(x, y, z, impute_with = imp_vars(carat, x, y, z))

# Model setups

## This is a "pure" ridge regression model so it will not do feature selection  
ridge_mod <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

# glmnet model where the mixture is now tuned as well.
glmnet_mod <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Regression tree
## Mainly for reference to the random forest as decision trees usually
## do not have good predictive power.
tree_mod <- decision_tree(cost_complexity = tune(), tree_depth = tune(),
                          min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

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

ridge_wf4 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec4)

ridge_wf5 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec5)

ridge_wf6 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec6)

ridge_wf7 <- workflow() %>%
  add_model(ridge_mod) %>%
  add_recipe(diamonds_ridge_rec7)

glmnet_wf1 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec1)

glmnet_wf2 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec2)

glmnet_wf3 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec3)

glmnet_wf4 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec4)

glmnet_wf5 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec5)

glmnet_wf6 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec6)

glmnet_wf7 <- workflow() %>%
  add_model(glmnet_mod) %>%
  add_recipe(diamonds_ridge_rec7)

tree_wf <- workflow() %>%
  add_model(tree_mod) %>%
  add_recipe(diamonds_tree_rec)

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

glmnet_fit1_rs <- glmnet_wf1 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))
glmnet_fit1_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line()
glmnet_fit1_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = mixture, y = mean)) +
  geom_point() +
  geom_line()

glmnet_fit2_rs <- glmnet_wf2 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))
glmnet_fit2_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line()
glmnet_fit2_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = mixture, y = mean)) +
  geom_point() +
  geom_line()

glmnet_fit3_rs <- glmnet_wf3 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))
glmnet_fit3_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line()
glmnet_fit3_rs %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  filter(mean < 0.2) %>%
  ggplot(aes(x = mixture, y = mean)) +
  geom_point() +
  geom_line()

# Saving tuning ridge and glmnet fits for easy access later
save(ridge_fit1_rs, file = "Data/ridge_fit1_rs.Rda")
save(ridge_fit2_rs, file = "Data/ridge_fit2_rs.Rda")
save(ridge_fit3_rs, file = "Data/ridge_fit3_rs.Rda")

save(glmnet_fit1_rs, file = "Data/glmnet_fit1_rs.Rda")
save(glmnet_fit2_rs, file = "Data/glmnet_fit2_rs.Rda")
save(glmnet_fit3_rs, file = "Data/glmnet_fit3_rs.Rda")

set.seed(675)

ridge_fit4_rs <- ridge_wf4 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))
ridge_fit5_rs <- ridge_wf5 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))
ridge_fit6_rs <- ridge_wf6 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))
ridge_fit7_rs <- ridge_wf7 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), levels = 50))

glmnet_fit4_rs <- glmnet_wf4 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))
glmnet_fit5_rs <- glmnet_wf5 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))
glmnet_fit6_rs <- glmnet_wf6 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))
glmnet_fit7_rs <- glmnet_wf7 %>%
  tune_grid(resamples = folds, grid = grid_regular(penalty(), mixture(),
                                                   levels = 50))


# Saving for easy access later
save(ridge_fit4_rs, file = "Data/ridge_fit4_rs.Rda")
save(ridge_fit5_rs, file = "Data/ridge_fit5_rs.Rda")
save(ridge_fit6_rs, file = "Data/ridge_fit6_rs.Rda")
save(ridge_fit7_rs, file = "Data/ridge_fit7_rs.Rda")

save(glmnet_fit4_rs, file = "Data/glmnet_fit4_rs.Rda")
save(glmnet_fit5_rs, file = "Data/glmnet_fit5_rs.Rda")
save(glmnet_fit6_rs, file = "Data/glmnet_fit6_rs.Rda")
save(glmnet_fit7_rs, file = "Data/glmnet_fit7_rs.Rda")

tree_fit_tune <- tree_wf %>%
  tune_grid(resamples = folds, grid = 20)

# Save tree fit for later
save(tree_fit_tune, file = "Data/tree_fit_tune.Rda")

# best tree based on 1 SD rule for cost_complexity
best_tree <- tree_fit_tune %>%
  select_by_one_std_err(metric = "rmse", desc(cost_complexity))

updated_tree_mod <- decision_tree(cost_complexity = best_tree$cost_complexity,
                                  tree_depth = best_tree$tree_depth,
                                  min_n = best_tree$min_n) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Fit the best tree on the sample portion of training data.
## Creating a reference tree for the report.
updated_tree_wf <- tree_wf %>%
  update_model(updated_tree_mod)

set.seed(354)
small_amount_data <- train_data[sample(1:nrow(train_data), size = 100),]

small_tree_fit <- updated_tree_wf %>%
  fit(data = small_amount_data)

small_rpart <- small_tree_fit %>%
  pull_workflow_fit()

pdf(file = "Figures/sample_tree_plot.pdf")
rpart.plot(small_rpart$fit, tweak = 1.2, leaf.round = 3,
           box.palette = c("deepskyblue3", "chartreuse3", "gold2"),
           type = 1, extra = 1, shadow.col = "darkgray",  branch.lty = 2,
           round = 0, prefix = "log price\n", fallen.leaves = FALSE)
dev.off()



# The random forest models were tuned using cross-validation instead of the 
# OOB error due to the fact that the preprocessing includes imputation.

all_cores_minus_one <- parallel::detectCores(logical = FALSE) - 1
cl <- makePSOCKcluster(all_cores_minus_one)
registerDoParallel(cl)
set.seed(222)
rf_fit_tune <- rf_wf %>%
  tune_grid(resamples = folds, grid = 20)
stopCluster(cl = cl)

# Saving tuning random forest fit for easy access later
save(rf_fit_tune, file = "Data/rf_fit_tune.Rda")




# Comparing all of the models so far

the_best <- tibble("Model" = c("Ridge Fit1", "Ridge Fit2", "Ridge Fit3",
                               "Ridge Fit4", "Ridge Fit5", "Ridge Fit6",
                               "Ridge Fit7", "Glmnet Fit1", "Glmnet Fit2",
                               "Glmnet Fit3", "Glmnet Fit4", "Glmnet Fit5",
                               "Glmnet Fit6", "Glmnet Fit7"),
                   "Mean" = c(ridge_fit1_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              ridge_fit2_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              ridge_fit3_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              ridge_fit4_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              ridge_fit5_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              ridge_fit6_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              ridge_fit7_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit1_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit2_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit3_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit4_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit5_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit6_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean),
                              glmnet_fit7_rs %>% show_best(metric = "rmse", n = 1) %>% pull(mean)),
                   "std_err" = c(ridge_fit1_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 ridge_fit2_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 ridge_fit3_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 ridge_fit4_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 ridge_fit5_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 ridge_fit6_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 ridge_fit7_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit1_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit2_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit3_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit4_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit5_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit6_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err),
                                 glmnet_fit7_rs %>% show_best(metric = "rmse", n = 1) %>% pull(std_err)))
rank_ridge <- the_best[1:7,] %>% mutate(rank = dense_rank(Mean)) %>% pull(rank)
rank_glmnet <- the_best[8:14,] %>% mutate(rank = dense_rank(Mean)) %>% pull(rank)
the_best$rank <- c(rank_ridge, rank_glmnet)
best_trees <- tibble("Model" = c(rep("Tree fit", 7), rep("Random Forest Fit", 7)),
                     "Mean" = c(tree_fit_tune %>% show_best(metric = "rmse", n = 7) %>% pull(mean),
                                rf_fit_tune %>% show_best(metric = "rmse", n = 7) %>% pull(mean)),
                     "std_err" = c(tree_fit_tune %>% show_best(metric = "rmse", n = 7) %>% pull(std_err),
                                   rf_fit_tune %>% show_best(metric = "rmse", n = 7) %>% pull(std_err)),
                     "rank" = rep(1:7, 2))
the_best <- bind_rows(the_best, best_trees)
the_best$rank <- factor(the_best$rank)
the_best$model_type <- c(rep("Linear Models", 14), rep("Tree Models", 14))

the_best %>%
  ggplot(aes(x = Model, y = Mean, group = rank, fill = rank)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Mean - std_err, ymax = Mean + std_err),
                position = "dodge", color = "black") +
  ylab("Mean RMSE") +
  labs(caption = "Error bars are +/- 1 standard error.") +
  theme_classic()  +
  scale_fill_brewer(type = "seq", palette = "Paired") +
  facet_grid(~ model_type, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) 





# Examining the random forest models


rf_fit_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = mtry, y = mean, color = min_n)) +
  geom_point(size = 2, position = position_dodge2(width = 0.4)) +
  geom_linerange(aes(ymin = mean - std_err, ymax = mean + std_err),
                 show.legend = TRUE, position = position_dodge2(width = 0.4)) +
  theme_minimal() +
  scale_color_viridis_c() +
  scale_x_continuous(breaks = 1:9) +
  ylab("Mean RMSE") +
  ggtitle("10-fold Cross Validation Mean RMSE Estimates for RF Tuning Parameters mtry and min_n",
          subtitle = "Error bars represent +/- 1 standard error. Points are dodged to avoid overlapping.") 
# It appears higher mtry values and lower min_n values result in a smaller RMSE



rf_fit_tune %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = mtry, y = min_n, size = mean)) +
  geom_point(color = "firebrick2") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:9)


# Now to try a more fine tuned grid using the information from above
## This takes a significant amount of time to run
all_cores_minus_one <- parallel::detectCores(logical = FALSE) - 1
cl <- makePSOCKcluster(all_cores_minus_one)
registerDoParallel(cl)
set.seed(123)
rf_fit_tune_fine <- rf_wf %>%
  tune_grid(resamples = folds, grid = expand.grid("mtry" = 5:9, "min_n" = c(5, 7, 9, 11)))
stopCluster(cl = cl)

save(rf_fit_tune_fine, file = "Data/rf_fit_tune_fine.Rda")

rf_fit_tune_fine %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(x = mtry, y = mean, color = min_n)) +
  geom_point(size = 2, position = position_dodge2(width = 0.4)) +
  geom_linerange(aes(ymin = mean - std_err, ymax = mean + std_err),
                show.legend = TRUE, position = position_dodge2(width = 0.4)) +
  theme_minimal() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = 5:9) +
  ylab("Mean RMSE") +
  ggtitle("10-fold Cross Validation Mean RMSE Estimates for RF Tuning Parameters mtry and min_n",
          subtitle = "Error bars represent +/- 1 standard error") 


rf_fit_tune_fine %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = mtry, y = min_n, size = mean)) +
  geom_point(color = "firebrick2") +
  theme_minimal() +
  scale_x_continuous(breaks = 5:9)


# We can fit the model on the whole training data set and get an estimate
# of the test error using the OOB error.
rf_fit_tune_fine %>% select_best(metric = "rmse") #mtry = 6, min_n = 5

last_rf_mod <- rand_forest(mtry = 6, min_n = 5, trees = 1000) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

last_rf_wf <- rf_wf %>%
  update_model(last_rf_mod)

# Now use it on the training data
set.seed(999)
train_rf_fit <- last_rf_wf %>%
  fit(data = train_data)

train_rf_fit


# Now use it on the training data and also make predictions on test data using
# the training fit.
set.seed(999)
last_rf_fit <- last_rf_wf %>%
  last_fit(split = data_split)



last_rf_fit %>% collect_metrics %>% pull(.estimate)

last_rf_fit %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip() +
  theme_bw() +
  ggtitle("Permutation Importance for Each Variable")
ggsave(filename = "Figures/permimportance.jpg")

# Back in terms of dollars
preds_price <- last_rf_fit %>% collect_predictions %>%
  select(.row, .pred, price) %>%
  mutate(.pred = exp(.pred)) %>%
  mutate(price = exp(price))

save(preds_price, file = "Data/preds_price.Rda")

preds_price %>%
  ggplot(aes(x = .pred, y = price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

preds_price %>% rmse(truth  = price, estimate = .pred)

preds_price <- preds_price %>% mutate(absdiff = abs(.pred - price))

preds_price %>%
  ggplot(aes(absdiff)) +
  geom_density()

preds_price <- preds_price %>%
  mutate("Price_range" = case_when(price < 5000 ~ "Price < $5000",
                                   5000 <= price & price < 10000 ~ "$5000 <= Price < 10000",
                                   price >= 10000 ~ "Price >= 10000"))

preds_price %>%
  group_by(Price_range) %>%
  summarise(rmse = sqrt((1/n())*sum((.pred - price)^2)))


preds_price %>%
  filter(absdiff <= 100) %>%
  count()/10784
preds_price %>%
  filter(absdiff <= 200) %>%
  count()/10784
preds_price %>%
  filter(absdiff <= 300) %>%
  count()/10784
preds_price %>%
  filter(absdiff <= 400) %>%
  count()/10784
preds_price %>%
  filter(absdiff <= 500) %>%
  count()/10784

diamonds %>% filter(price < 5000) %>% count()/53940
diamonds %>% filter(price >= 5000 & price < 10000) %>% count()/53940
diamonds %>% filter(price >= 10000) %>% count()/53940

test_data %>% filter(exp(price) < 5000) %>% count()/10784
test_data %>% filter(exp(price) >= 5000 & price < 10000) %>% count()/10784
test_data %>% filter(exp(price) >= 10000) %>% count()/10784
