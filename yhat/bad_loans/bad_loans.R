# Predicting Bad Loans
# http://blog.yhathq.com/posts/machine-learning-for-predicting-bad-loans.html

# Created: 09-Feb-2015
# Updated: 23-Feb-2015

#===========
# Libraries
#===========

library(plyr)
library(randomForest)

#=============
# Import data
#=============

# Lending Club loan performance, 2007-2011
loan_dat <- read.csv(file = './yhat/bad_loans/LoanStats3a.csv', 
                     skip = 1, 
                     header = TRUE)

loan_dat$status <- as.character(loan_dat$loan_status)
loan_dat$status <- ifelse(loan_dat$status == '', NA, loan_dat$status)
status_levels <- count(loan_dat$status)[,1]
loan_dat$status <- factor(loan_dat$status, levels = status_levels, exclude = NA)

