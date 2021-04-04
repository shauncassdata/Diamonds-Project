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


