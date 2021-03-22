library(tidyverse)
library(GGally)

load(file = "Data/updated_diamonds.Rda")

# Univariate plots

diamonds %>%
  ggplot(aes(x = carat)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  geom_density(col = "red") +
  ggtitle("Distribution of the Diamonds' Weight (Carat)")

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
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  geom_density(col = "green") +
  ggtitle("Distribution of the Diamonds' Depth Percentage") +
  xlab("Depth (%)")

diamonds %>%
  ggplot(aes(x = table)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density(col = "blue") +
  ggtitle("Distribution of the Diamonds' Table Percentage") +
  xlab("Table (%)")






# Bivariate graphs




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



