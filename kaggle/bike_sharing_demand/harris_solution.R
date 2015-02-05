# Brandon Harris' solution to Bike Sharing Demand, Kaggle
# http://brandonharris.io/kaggle-bike-sharing/

# Creator: CR
# Date created: 05-Feb-2015
# Last updated: 05-Feb-2015

#===========
# Libraries
#===========
library(plyr)


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
