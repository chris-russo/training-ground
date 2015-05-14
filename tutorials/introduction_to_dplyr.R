# Introduction to dplyr
# http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

# Creator: CR
# Created: 2015-05-13
# Updated: 2015-05-13

#===========
# Libraries
#===========

# Define list of libraries
libs <- c("dplyr",
          "nycflights13",
          "ggplot2")

# Load libraries
sapply(libs, require, character.only = T)

#======
# Data
#======

# Use the 'flights' data.frame from the package nycflights13
dim(flights)
flights)

#====================
# Single table verbs
#====================

# Filter rows with filter()

filter(flights, 
       dest == "SFO", 
       origin == "JFK")

filter(flights, 
       month == 1, 
       day == 1)

# slice()

slice(flights, 1:10)

# Arrange rows with arrange()
arrange(flights, 
        year, 
        month, 
        day)

arrange(flights, 
        desc(arr_delay))

# Select columns with select()

select(flights, 
       year, 
       month, 
       day)

select(flights, 
       year:day)

select(flights, 
       -(year:day))

select(flights, 
       tail_num = tailnum)  # Only selects one column

rename(flights, 
       tail_num = tailnum)  # Updates entire data.frame

# Extract distinct (unique) rows

distinct(
  select(flights, 
         origin))

distinct(
  select(flights, 
         origin, 
         dest))

# Add new columns with mutate()

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60))

# Summarise values with summarise()

summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

# Randomly sample rows with sample_n() and sample_frac()

# Note: Use replace = TRUE to perform a bootstrap sample, 
# and optionally weight the sample with the weight argument.

sample_n(flights, 10)

sample_frac(flights, 0.01)

#====================
# Grouped operations
#====================

by_tailnum <- group_by(flights, tailnum)

delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dist < 2000)

# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()