library(tidyverse)
library(GGally)

diamonds <- read_csv(file = "Data/diamonds.csv")

# First column is just an id column so should just rename it

diamonds <- diamonds %>%
  rename(id = X1)


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
  

  
# Depth should be depth = 2 * z/(x+y)
# depth is in percentage so it would also be * 100
diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(depth != temp_depth) %>%
  select(depth, temp_depth, x, y, z) #3,768 rows

# Looks to be just slight rounding errors of some kind
# can plot it out to make sure
diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(depth != temp_depth) %>%
  ggally_points(aes(x = temp_depth, y = depth))
# Looks to be some extreme outliers

diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs(temp_depth - depth) >= 1) %>%
  count() # 78

diamonds %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  filter(abs(temp_depth - depth) >= 10) %>%
  count() # 15

# Since the number of extreme differences between the formula for depth percentage
# and the recorded value of depth percentage is relatively low we will just
# use the formula for the depth column to maintain consistency.
## depth percentage is inherently a transformation of x, y, and z so we are not
## changing the values of a recorded value, rather we are ensuring the
## transformation is consistent across the entire column.
### using complete.cases() here to not affect the rows with x, y, z = NA

diamonds$depth[complete.cases(diamonds)] <- diamonds %>%
  filter(complete.cases(diamonds)) %>%
  mutate(temp_depth = round(2*(z/(x+y)) * 100, 1)) %>%
  pull(temp_depth)


# Table is actually a percentage: The table width/the total width
range(diamonds$table)

# This range seems acceptable. We don't have the actual table width in mm
# so we cannot confirm this percentage like we could for the depth percentage


# save updated dataset

save(diamonds, file = "Data/updated_diamonds.Rda")













