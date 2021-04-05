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




  
  
  
  

