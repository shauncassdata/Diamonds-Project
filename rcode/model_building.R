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
  ggplot(aes(log_carat, log_price, color = cut)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("log(Carat)")+
  ylab("log(Price) (log US Dollars)") +
  ggtitle("Simple log(Carat)*Cut Interaction Plot") +
  theme_minimal()

train_data %>%
  mutate(log_price = log(price)) %>%
  mutate(log_carat = log(carat)) %>%
  ggplot(aes(log_carat, log_price, color = color)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("log(Carat)")+
  ylab("log(Price) (log US Dollars)") +
  ggtitle("Simple log(Carat)*Color Interaction Plot") +
  theme_minimal()


train_data %>%
  mutate(log_price = log(price)) %>%
  mutate(log_carat = log(carat)) %>%
  ggplot(aes(log_carat, log_price, color = clarity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("log(Carat)")+
  ylab("log(Price) (log US Dollars)") +
  ggtitle("Simple log(Carat)*Clarity Interaction Plot") +
  theme_minimal()

# All three suggest that there may be some interactions, 
# however the log(Carat)*Color interaction looks to be the weakest

# Building a recipe
## Going to use a bagged trees imputation method to deal with missing values
## in x, y, and z.
### By using a recipe, we can impute missing values in the test set the
### the exact same way we did for the training set.

diamonds_rec <- recipe(price ~ ., data = train_data) %>%
  update_role(id, new_role = "ID") %>% # make sure id is not used in predicting
  # We know that x, y, z, and carat are highly collinear. 
  # Also depth is a calculation based on x, y, and z.
  step_bagimpute(x, y, z, impute_with = imp_vars(carat, depth, x, y, z)) %>%
  step_ordinalscore(cut, color, clarity) %>% 
  step_log(price, carat, x, y, z) %>%
  step_normalize(price, carat, x, y, z)


  

  
  

