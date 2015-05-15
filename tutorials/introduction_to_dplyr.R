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

destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n())

daily <- group_by(flights, year, month, day)
per_day <- summarise(daily, flights = n())
per_month <- summarise(per_day, flights = sum(flights))
per_year  <- summarise(per_month, flights = sum(flights))

#==========
# Chaining
#==========

# Step-by-step
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

# Long code
filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

# dplyr operator: x %>% f(y) turns into f(x, y) 
# so you can use it to rewrite multiple operations so 
# you can read from left-to-right, top-to-bottom
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  mutate(dep_delay_lag = lag(dep_delay))