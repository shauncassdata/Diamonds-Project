library(tidyverse)
library(GGally)

diamonds <- read_csv(file = "Data/diamonds.csv")

# First column is just an id column so should just rename it

diamonds <- diamonds %>%
  rename(id = X1)

# Make sure no duplicate ids
sum(duplicated(diamonds$id)) # 0

# Check the quality of the data and make sure variables fit their definition

glimpse(diamonds)
summary(diamonds)

# No NA values so far that I can see

# Let us make sure the character columns are the correct data type.

unique(diamonds$cut)
# This should be an ordered factor
# Quality of the cut from worst to best goes:
# Fair, Good, Very Good, Premium, Ideal



unique(diamonds$color)
# This too should be an ordered factor
# Quality of color from worst to best goes:
# J, I, H , G, F, E, D

unique(diamonds$clarity)
# This should also be an ordered factor.
# Quality of the clarity from worst to best goes:
# I1, SI2, SI1, VS2, VS1, VVS2, VVS1, IF



diamonds <- diamonds %>% 
  mutate(cut = factor(cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
                      ordered = TRUE)) %>%
  mutate(color = factor(color, levels = c("J", "I", "H", "G", "F", "E", "D"), 
                        ordered = TRUE)) %>%
  mutate(clarity = factor(clarity, levels = c("I1", "SI2", "SI1","VS2", "VS1", "VVS2", "VVS1","IF"),
                          ordered = TRUE))

summary(diamonds)

# carat is a weight (1 carat = 200 mg). It is positive like it should be.
# x, y, and z are all >= 0 which makes sense as these are measurements in mm
# x and y are essentially both measurements of the diameter
# since these are round cut diamonds
# Likewise, table, depth, and price are positive as well which makes sense
# However, thinking realistically, neither x, y, or z should be 0

diamonds %>%
  filter(x == 0 | y == 0 | z == 0) %>%
  count() # 20 in total

diamonds %>%
  filter(x == 0 & y == 0 & z == 0) %>%
  count() # 7

diamonds %>%
  filter(x == 0 | y == 0 | z == 0) %>%
  print(n = Inf)


diamonds %>%
  filter(x == 0 & y == 0 & z == 0) %>%
  print(n = Inf)

diamonds %>%
  filter(x != 0 & y != 0 & z == 0) %>%
  print(n = Inf)

# For the rows where x, y, and depth percentage are nonzero, but z = 0
# we can use the definition of depth percentage to replace z = 0

## Confirm how many digits for x, y, z, and depth percentage first

all(round(diamonds$x, 2) == diamonds$x)
all(round(diamonds$y, 2) == diamonds$y)
all(round(diamonds$z, 2) == diamonds$z)
all(round(diamonds$depth, 1) == diamonds$depth)
### All true.

diamonds$z[diamonds$x != 0 & diamonds$y != 0 & diamonds$z == 0] <- diamonds %>%
  filter(x != 0 & y != 0 & z == 0) %>%
  mutate(temp_z = round((depth * (x + y)) / (2*100), 2)) %>%
  pull(temp_z)

diamonds %>%
  filter(x == 0 | y == 0 | z == 0) %>%
  print(n = Inf)

# For the remaining 0 measurement values since a 0 measurement does not make sense,
# we will replace the 0 values with NA
diamonds <- diamonds %>%
  mutate(x = replace(x, x == 0, NA)) %>%
  mutate(y = replace(y, y == 0, NA)) %>%
  mutate(z = replace(z, z == 0, NA))

summary(diamonds)

# The maximum carat is 5.01. 
# For a round diamond the length and width should be around 11 mm.
# For this data set however we have a y = 58.9 mm which is highly unrealistic.

diamonds %>%
  select(id, carat, depth, x, y, z) %>%
  filter(y >= 11)

# A 2 carat round diamond should be around 8.19 mm
# A 0.5 carat diamond should be around 5.16 mm
# We also know that x and y are both estimates of the diamter of the circle on
# the top of the round diamond. Of course there is some slight variation
# between the two. 
# Let us double check the maximum and minimum x first.
diamonds %>%
  filter(x == 10.740) # x and y are similar here
diamonds %>%
  filter(x == 3.730) # x and y are similar here

# Let us see what the standard deviation of the difference of x and y is:
## excluding the unusual y values

diamonds %>%
  filter(y < 11) %>%
  summarise(sd_xy_diff = sd((x - y), na.rm = TRUE)) # So around 0.06 of a mm

# Going back to extreme y values and knowing the variation
# of the difference of x and y:
diamonds %>%
  select(id, carat, depth, x, y, z) %>%
  filter(y >= 11)
# It appears here that the z value may be the actual y value.
# Also z in all likelihood should not be greater than x or y
diamonds %>%
  select(id, carat, depth, x, y, z) %>%
  filter(z >= y | z >= x)
# Let us fix the extreme y values by replacing with the z value and
# replace the z value using the depth percentage calculation
y11_ids <- diamonds %>%
  filter(y >= 11) %>%
  pull(id)

y11_idsdiamonds <- diamonds[y11_ids,]

save(y11_idsdiamonds, file = "Data/y11_idsdiamonds.Rda")

diamonds$y[diamonds$id %in% y11_ids] <- diamonds$z[diamonds$id %in% y11_ids]
diamonds$z[diamonds$id %in% y11_ids] <- diamonds %>%
  filter(id %in% y11_ids) %>%
  mutate(temp_z = round((depth * (x + y)) / (2*100), 2)) %>%
  pull(temp_z)

diamonds %>%
  filter(id %in% y11_ids)

# Let us look at possible extreme values of z 
# Like we mentioned before z should not be larger than x or y:
diamonds %>%
  select(id, carat, depth, x, y, z) %>%
  filter(z >= y | z >= x)

# We can again fix this using x, y, and depth percentage
zhigh_ids <- diamonds %>%
  select(id, carat, depth, x, y, z) %>%
  filter(z >= y | z >= x) %>%
  pull(id)

zhigh_idsdiamonds <- diamonds[zhigh_ids,]
save(zhigh_idsdiamonds, file = "Data/zhigh_idsdiamonds.Rda")
  
diamonds$z[diamonds$id %in% zhigh_ids] <- diamonds %>%
  filter(id %in% zhigh_ids) %>%
  mutate(temp_z = round((depth * (x + y)) / (2*100), 2)) %>%
  pull(temp_z)
  
  
summary(diamonds)

# Examine scatter plots of x, y, and z to see if there are
# anymore obvious incorrect values 

diamonds %>%
  select(x, y, z) %>%
  ggpairs()

# Looks like there may be a few x and ys that are far apart
# however the difference is not so extreme.
# It may just be due to natural variation

# Looking at x and y values that are more than 2 sd of the difference apart
diamonds %>%
  filter(abs(x - y) > 0.12) # There are a fair amount

# How about 3 sds apart?

diamonds %>%
  filter(abs(x - y) > 0.18)

# 4?

diamonds %>%
  filter(abs(x - y) > 0.24)
  
diamonds %>%
  ggplot(aes(x = abs(x - y))) + geom_histogram(bins = 100)
diamonds %>%
  ggplot(aes(x = x/y)) + geom_histogram(bins = 100)

range(abs(diamonds$x - diamonds$y), na.rm = TRUE)
range(diamonds$x/diamonds$y, na.rm = TRUE)
# These all seem reasonable 
  
  
# Depth should be depth = 2 * z/(x+y)
# depth is in percentage so it would also be * 100
diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(depth != temp_depth) %>%
  select(depth, temp_depth, x, y, z) #3,768 rows

# Looks to be just slight rounding errors of some kind
# can plot it out to make sure
rec_calc_total_depth <- diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(depth != temp_depth) %>%
  ggally_points(aes(x = temp_depth, y = depth)) +
  geom_abline(aes(slope = 1, intercept = 0, color = "red"), linetype = "dashed") +
  xlab("Calculated Total Depth Percentage") +
  ylab("Recorded Total Depth Percentage") +
  ggtitle("Recorded vs. Calculated Total Depth Percentage") +
  scale_color_manual(name = "Ratio", values = c("red" = "red"), labels = "1:1") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1))+
  theme_bw() 

save(rec_calc_total_depth, file = "Data/rec_calc_total_depth.Rda")

# Looks to be some extreme outliers

diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(temp_depth < 30)
# Realistically it does not make sense for the z value to be this low.
# Ideal depth percentage is somewhere in the 50% - 70% range.

diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(temp_depth > 75)



diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs((temp_depth - depth)/depth) >= 0.01) %>%
  count() # 120 in total with a 1% difference between the two

diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs((temp_depth - depth)/depth) >= 0.1) %>%
  count() # 18 in total with a 10% difference between the two.

largediff_depth <- diamonds %>%
  mutate(calc_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs((calc_depth - depth)/depth) >= 0.1) %>%
  select(id, carat, depth, calc_depth, x, y, z)

largediff_depth

save(largediff_depth, file = "Data/largediff_depth.Rda")
# x and y are relatively close like expected except for one row.

# Depth percentage is inherently a transformation of the x, y, and z values.
# It would make sense to adjust for depth percentage values by using the
# calculated depth percentage instead of the recorded value.
# It would also make sense to adjust the erroneous z values by using the same
# formula. In order to reduce biasing the data too much, I will only adjust
# the 18 z values where the difference between the recorded depth percentage
# and the calculated depth percentage is >= 10%. I adjusted the z value as 
# that appeared to be the cause of the issue. x and y appear to be relatively
# close like expected except for one row which is still not that big of a 
# difference between x and y.
error_z_ids <- diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs((temp_depth - depth)/depth) >= 0.1) %>%
  pull(id)

diamonds$z[diamonds$id %in% error_z_ids] <- diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs((temp_depth - depth)/depth) >= 0.1) %>%
  mutate(temp_z = round((depth * (x + y)) / (2*100), 2)) %>%
  pull(temp_z)


range(diamonds$depth)
# Table is actually a percentage: The table width/the total width
range(diamonds$table)

# This range seems acceptable. We don't have the actual table width in mm
# so we cannot confirm this percentage like we could for the depth percentage


# save updated dataset

save(diamonds, file = "Data/updated_diamonds.Rda")

rm(list=ls())












