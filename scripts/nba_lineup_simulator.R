# NBA lineup simulator

# Generate a data frame of all unique player-position combinations
# for a given 14-player NBA roster.

# Author: CR
# Created: 2015-11-11
# Updated: 2015-11-11

# Define list of libraries
libs <-
  c("dplyr",
    "combinat")

# Load libraries
sapply(libs,
       require,
       character.only = TRUE)

# Define list of Knicks players for 2015-16 season
knicks <-
  c("Afflalo",
    "Amundson",
    "Anthony",
    "Calderon",
    "Early",
    "Galloway",
    "Grant",
    "Lopez",
    "O'Quinn",
    "Porzingis",
    "Seraphin",
    "Thomas",
    "Vujacic",
    "Williams")

# Define number of players on the roster
nRoster <- length(knicks)

# Define number of positions
nPos <- 5

# Simulate all player-position permutations
lineup <- 
  expand.grid(
    p1 = knicks,
    p2 = knicks,
    p3 = knicks,
    p4 = knicks,
    p5 = knicks,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

# Generate flag for multiple occurances of values per row
lineup$dups <- 
  ifelse(
    apply(lineup, 1, function(x) length(unlist(table(x)))) < nPos, 
    1,  # if duplicates exist
    0)  # if no duplicates exist

# Concatenate positions
lineup$list <- 
  with(lineup, 
       paste(
         p1, 
         p2, 
         p3, 
         p4, 
         p5, 
         sep = ", ")
       )

# Sort concatenated positions
lineup$list_sorted <- 
  as.character(
    lapply(lineup$list, 
           function(x) sort(unlist(strsplit(x, split = ", ")))
           )
    )

# Remove rows where duplicates exist and drop dups column
lineup <- 
  lineup %>% 
  filter(
    dups == 0,
    !duplicated(list_sorted)) %>%
  select(
    -dups, 
    -list, 
    -list_sorted)

# Check if data sorted properly
dim(t(combn(14, 5))) == dim(lineup)

