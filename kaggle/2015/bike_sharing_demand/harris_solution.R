# Brandon Harris' solution to Bike Sharing Demand, Kaggle
# http://brandonharris.io/kaggle-bike-sharing/

# Creator: CR
# Date created: 05-Feb-2015
# Last updated: 05-Feb-2015

#===========
# Libraries
#===========
library(plyr)
library(party)

#read in train/test
train <- read.csv('./kaggle/bike_sharing_demand/train.csv')
test <- read.csv('./kaggle/bike_sharing_demand/test.csv')

str(train)

# Factorize training set
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)

# Factorize test set
test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)

# Create time column by stripping out timestamp
train_factor$time <- substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)

# Factorize new timestamp column
train_factor$time <- factor(train_factor$time)
test_factor$time <- factor(test_factor$time)

# Create day of week column
train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

# Check to see if certain days are more or less popular for bike rentals
aggregate(train_factor[,"count"],list(train_factor$day),mean)

# Create Sunday variable
train_factor$sunday[train_factor$day == "Sunday"] <- "1"
train_factor$sunday[train_factor$day != "1"] <- "0"

test_factor$sunday[test_factor$day == "Sunday"] <- "1"
test_factor$sunday[test_factor$day != "1"] <- "0"

# Convert to factor
train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

# Convert time and create $hour as integer to evaluate for daypart
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))
test_factor$hour<- as.numeric(substr(test_factor$time,1,2))

# Create daypart column, default to 4 to make things easier for ourselves
train_factor$daypart <- "4"
test_factor$daypart <- "4"


# 4AM - 10AM = 1
train_factor$daypart[(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1
test_factor$daypart[(test_factor$hour < 10) & (test_factor$hour > 3)] <- 1


# 11AM - 3PM = 2
train_factor$daypart[(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2
test_factor$daypart[(test_factor$hour < 16) & (test_factor$hour > 9)] <- 2


# 4PM - 9PM = 3
train_factor$daypart[(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3
test_factor$daypart[(test_factor$hour < 22) & (test_factor$hour > 15)] <- 3

# Convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)
test_factor$daypart <- as.factor(test_factor$daypart)

# Convert hour back to factor
train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)

# Create our formula
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday

# Build our model
fit.ctree <- ctree(formula, data=train_factor)

# Examine model for variable importance
fit.ctree

# Run model against test data set
predict.ctree <- predict(fit.ctree, test_factor)

#============
# Evaluation
#============

# Submissions are evaluated one the 
# Root Mean Squared Logarithmic Error (RMSLE). 
# The RMSLE is calculated as:

# p := predicted count
# a := actual count

rmsle <- ((log(p+1)-log(a+1))^2)^0.5

# Build a dataframe with our results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file='./kaggle/bike_sharing_demand/submit_ctree_v1.csv',row.names=FALSE)