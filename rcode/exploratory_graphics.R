library(tidyverse)
library(GGally)

load(file = "Data/updated_diamonds.Rda")

# Univariate plots

diamonds %>%
  ggplot(aes(x = carat)) +
  geom_density(size = 1.2, alpha = 0.5, fill = "red3") +
  ggtitle("Distribution of the Diamonds' Weight (Carat)") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = cut)) +
  geom_bar() +
  ggtitle("Number of Diamonds Per Cut Type", subtitle = "Sorted from worst (Fair) to best (Ideal)")
## There are very few diamonds that are a "Fair" cut type


diamonds %>%
  ggplot(aes(x = color)) +
  geom_bar() +
  ggtitle("Number of Diamonds Per Color Type", subtitle = "Sorted from worst (J) to best (D)")
## Distribution of Color Type is pretty close to uniform for G, F, and E. 
## A lot less of J color type compared to the rest.

diamonds %>%
  ggplot(aes(x = clarity)) +
  geom_bar() +
  ggtitle("Number of Diamonds Per Clarity Type", subtitle = "Sorted from worst (I1) to best (IF)")
## There are an extremely small amont of I1 compared to all the other clarity types

diamonds %>%
  ggplot(aes(x = depth)) +
  geom_density(size = 1.2, alpha = 0.5, fill = "violet") +
  ggtitle("Distribution of the Diamonds' Depth Percentage") +
  xlab("Depth (%)") +
  theme_minimal()

diamonds %>%
  ggplot(aes(x = table))  +
  geom_density(size = 1.2, alpha = 0.5, fill = "royalblue3") +
  ggtitle("Distribution of the Diamonds' Table Percentage") +
  xlab("Table (%)") +
  theme_minimal()


## Dropping the 8 rows that have NA values for either x, y, or z
diamonds %>%
  select(x, y, z) %>%
  drop_na() %>%
  ggplot(aes(x = x)) +
  geom_density(fill = "turquoise3", alpha = 0.5) +
  ggtitle("Distribution of the Diamonds' Length") +
  xlab("x (mm)") +
  theme_minimal()
  
diamonds %>%
  select(x, y, z) %>%
  drop_na() %>%
  ggplot(aes(x = y)) +
  geom_density(fill = "deeppink3", alpha = 0.5) +
  ggtitle("Distribution of the Diamonds' Width") +
  xlab("y (mm)") +
  theme_minimal()


diamonds %>%
  select(x, y, z) %>%
  drop_na() %>%
  ggplot(aes(x = z)) +
  geom_density(fill = "palegreen3", alpha = 0.5) +
  ggtitle("Distribution of the Diamonds' Depth") +
  xlab("z (mm)") +
  theme_minimal()  


# Bivariate graphs

diamonds %>%
  select(-id) %>%
  ggpairs()


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
  
ggsave(filename = "Figures/cutclaritycolor.png", height = 8, width = 8, dpi = 320)



