library(tidyverse)
library(GGally)

load(file = "Data/updated_diamonds.Rda")

# Univariate plots

diamonds %>%
  ggplot(aes(x = carat)) +
  geom_histogram(aes(y=..density..), fill = "steelblue", alpha = 0.5) +
  geom_density(size = 1.05, color = "red3") +
  xlab("Weight (Carat)") +
  ylab("Density") +
  ggtitle("Distribution of the Diamonds' Weight (Carat)") +
  theme_minimal() 
# Pretty right skewed.
# Let's look at a log transformation.

diamonds %>%
  ggplot(aes(x = log(carat))) +
  geom_histogram(aes(y=..density..), fill = "steelblue", alpha = 0.5) +
  geom_density(size = 1.05, color = "red3") +
  xlab("Weight (log(Carat))") +
  ylab("Density") +
  ggtitle("Distribution of the Diamonds' Weight (Carat)") +
  theme_minimal() # More symmetric than it was before at least

diamonds %>%
  ggplot(aes(x = cut)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  xlab("Cut") +
  ylab("Count") +
  ggtitle("Proportion of Diamonds Per Cut Type", subtitle = "Sorted from worst (Fair) to best (Ideal)")
## There are very few diamonds that are a "Fair" cut type


diamonds %>%
  ggplot(aes(x = color)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  xlab("Color") +
  ylab("Count") +
  ggtitle("Proportion of Diamonds Per Color Type", subtitle = "Sorted from worst (J) to best (D)")
## Distribution of Color Type is pretty close to uniform for G, F, and E. 
## A lot less of J color type compared to the rest.

diamonds %>%
  ggplot(aes(x = clarity)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  xlab("Clarity") +
  ylab("Count") +
  ggtitle("Proportion of Diamonds Per Clarity Type", subtitle = "Sorted from worst (I1) to best (IF)")
## There are an extremely small amont of I1 compared to all the other clarity types

diamonds %>%
  ggplot(aes(x = depth)) +
  geom_histogram(aes(y=..density..), binwidth = 0.1,
                 fill = "steelblue", alpha = 0.5) +
  geom_density(size = 1.05, color = "violet") +
  ggtitle("Distribution of the Diamonds' Depth Percentage") +
  xlab("Depth (%)") +
  ylab("Density") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = table))  +
  geom_histogram(aes(y=..density..), bins = 100,
                 fill = "steelblue", alpha = 0.5) +
  geom_density(size = 1.05,color = "green4") +
  ggtitle("Distribution of the Diamonds' Table Percentage") +
  xlab("Table (%)") +
  ylab("Density") +
  theme_minimal()


diamonds %>%
  ggplot(aes(x = price)) +
  geom_histogram(aes(y=..density..), bins = 5000,
                 fill = "steelblue", alpha = 0.5) +
  geom_density(size = 1.05,color = "violetred2") +
  ggtitle("Distribution of the Diamonds' Price") +
  xlab("Price (US Dollars)") +
  ylab("Density") +
  theme_minimal()
## Price is really right skewed. 

# Let us look at a log transformation of the price
diamonds %>%
  ggplot(aes(x = log(price))) +
  geom_histogram(aes(y=..density..), bins = 5000,
                 fill = "steelblue", alpha = 0.5) +
  geom_density(size = 1.05,color = "violetred2") +
  ggtitle("Distribution of the Diamonds' Price") +
  xlab("Price (log US Dollars)") +
  ylab("Density") +
  theme_minimal()


## Dropping the 8 rows that have NA values for either x, y, or z
diamonds %>%
  select(x, y, z) %>%
  drop_na() %>%
  ggplot(aes(x = x)) +
  geom_density(fill = "turquoise3", alpha = 0.5) +
  ggtitle("Distribution of the Diamonds' Length") +
  xlab("x (mm)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0, 11)) +
  theme_minimal()
  
diamonds %>%
  select(x, y, z) %>%
  drop_na() %>%
  ggplot(aes(x = y)) +
  geom_density(fill = "deeppink3", alpha = 0.5) +
  ggtitle("Distribution of the Diamonds' Width") +
  xlab("y (mm)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0, 11)) +
  theme_minimal()


diamonds %>%
  select(x, y, z) %>%
  drop_na() %>%
  ggplot(aes(x = z)) +
  geom_density(fill = "palegreen3", alpha = 0.5) +
  ggtitle("Distribution of the Diamonds' Depth") +
  xlab("z (mm)") +
  ylab("Density") +
  scale_x_continuous(limits = c(0, 11)) +
  theme_minimal()  


# Bivariate graphs

diamonds %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  pairs(main = "Scatterplots of Continuous Variables in Diamonds")
# With price being really right skewed it would probably be best to
# log transform it

diamonds %>%
  ggplot(aes(carat, price)) +
  geom_point() +
  xlab("Weight (Carat)") +
  ylab("Price (US Dollars)") +
  ggtitle("Price vs. Weight") +
  theme_minimal()

diamonds %>%
  ggplot(aes(log(carat), log(price))) +
  geom_point() +
  xlab("Weight (log(Carat))") +
  ylab("Price (log US Dollars)") +
  ggtitle("Price vs. Weight") +
  theme_minimal()





## correlation plot
diamonds %>%
  select(price, carat, depth, table, x, y, z) %>%
  drop_na() %>%
  ggcorr(label = TRUE) +
  ggtitle("Pearson Correlation of Continuous Variables in Diamonds")


## boxplots
diamonds %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot() +
  xlab("Cut") +
  ylab("Price (US Dollars)") +
  ggtitle("Distribution of  Price for each Cut of Diamond") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = cut, y = log(price))) +
  geom_boxplot() +
  xlab("Cut") +
  ylab("Price (log US Dollars)") +
  ggtitle("Distribution of log Price for each Cut of Diamond") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = color, y = price)) +
  geom_boxplot() +
  xlab("Color") +
  ylab("Price (US Dollars)") +
  ggtitle("Distribution of Price for each Color of Diamond") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = color, y = log(price))) +
  geom_boxplot() +
  xlab("Color") +
  ylab("Price (log US Dollars)") +
  ggtitle("Distribution of log Price for each Color of Diamond") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = clarity, y = price)) +
  geom_boxplot() +
  xlab("Clarity") +
  ylab("Price (US Dollars)") +
  ggtitle("Distribution of Price for each Clarity of Diamond") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = clarity, y = log(price))) +
  geom_boxplot() +
  xlab("Clarity") +
  ylab("Price (log US Dollars)") +
  ggtitle("Distribution of Price for each Clarity of Diamond") +
  theme_minimal()

## Heatmaps
diamonds %>%
  group_by(cut, color) %>%
  summarise(n_part = n()) %>%
  group_by(cut) %>%
  mutate(Density = n_part/53940) %>%
  ggplot(aes(x = cut, y = color, fill = Density)) +
  geom_tile()  +
  scale_fill_viridis_c(name = "Proportion of \nTotal Data") +
  theme_minimal() +
  xlab("Cut") +
  ylab("Color") +
  ggtitle("Proportion of Total Data for Each Color and Cut") +
  scale_size(name = "Proportion of \nTotal Data")

diamonds %>%
  group_by(cut, clarity) %>%
  summarise(n_part = n()) %>%
  group_by(cut) %>%
  mutate(Density = n_part/53940) %>%
  ggplot(aes(x = cut, y = clarity, fill = Density)) +
  geom_tile()  +
  scale_fill_viridis_c(name = "Proportion of \nTotal Data") +
  theme_minimal() +
  xlab("Cut") +
  ylab("Clarity") +
  ggtitle("Proportion of Total Data for Each Clarity and Cut") +
  scale_size(name = "Proportion of \nTotal Data")

diamonds %>%
  group_by(color, clarity) %>%
  summarise(n_part = n()) %>%
  group_by(color) %>%
  mutate(Density = n_part/53940) %>%
  ggplot(aes(x = color, y = clarity, fill = Density)) +
  geom_tile()  +
  scale_fill_viridis_c(name = "Proportion of \nTotal Data") +
  theme_minimal() +
  xlab("Color") +
  ylab("Clarity") +
  ggtitle("Proportion of Total Data for Each Clarity and Color") +
  scale_size(name = "Proportion of \nTotal Data")


# Trivariate graphs

diamonds %>%
  ggplot(aes(x = clarity, fill = color)) +
  geom_bar(position = "dodge", color = "black") +
  facet_grid(~cut) +
  ggtitle("Number of Diamonds For Each Cut, Clarity, and Color",
          subtitle = "From Worst to Best: Cut (Fair - Ideal), Clarity (I1 - IF), Color (J - D)") +
  ylab("Number of Diamonds") +
  xlab("Clarity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  





