# R-bloggers
# TidyR Challenge: Help Me Do My Job

# Author:   CR
# Created:  2015-05-07
# Updated:  2015-05-07

#===========
# Libraries 
#===========

# Define list of libraries
libs <- c("wakefield",
          "tidyr",
          "dplyr")

# Load libraries
sapply(libs, 
       require, 
       character.only = T)

#======
# Data
#======

d <- r_data_frame(
  n = 100,
  id,
  r_series(date_stamp, 15, name = 'foo_date'),
  r_series(level, 15, name = 'foo_supply'),
  r_series(date_stamp, 10, name = 'bar_date'),
  r_series(level, 10, name = 'bar_supply'),
  r_series(date_stamp, 3, name = 'baz_date'),
  r_series(level, 3, name = 'baz_supply')
)

# foo
med_dates <- d %>% 
  select(ID,foo_date_1:foo_date_15) %>% 
  gather(med_seq, med_date, foo_date_1:foo_date_15)
med_dates$med_seq <- as.integer(sub('^foo_date_','',med_dates$med_seq))
med_supply <- d %>% 
  select(ID,foo_supply_1:foo_supply_15) %>% 
  gather(med_seq, med_supply, foo_supply_1:foo_supply_15)
med_supply$med_seq <- as.integer(sub('^foo_supply_','',med_supply$med_seq))
foo <- left_join(med_dates,med_supply, by=c('ID','med_seq')) %>% 
  select(ID,med_date,med_supply)
foo$med_name <- 'foo'