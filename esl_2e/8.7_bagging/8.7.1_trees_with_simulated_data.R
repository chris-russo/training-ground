# The Elements of Statistical Learning (Second Edition)
# 8.7.1 
# Example: Trees with Simulated Data

# Created: 12-Mar-2015
# Updated: 12-Mar-2015

#===========
# Libraries
#===========
library(MASS)


# 1. Generate a sample of size N = 30, with two classes and 
# p = 5 features, each having a standard Gaussian distribution 
# with pairwise correlation 0.95. 

# The response Y should be generated according to:
# Pr(Y = 1|x1 ≤ 0.5) = 0.2 
# Pr(Y = 1|x1 > 0.5) = 0.8 
# with Bayes error of 0.2. 

# Define parameters
N <- 30
p <- 5
cc <- 0.95
mu <- rep(0.5, 5)
Sigma <- matrix(cc, nrow = p, ncol = p) + diag(p)*(1 - cc)

# Generate features
X <- round(mvrnorm(N, mu, Sigma), 2)

# Check pairwise correlation (should be ~0.95)
cor(X)

# Define response
Y <- ifelse(X[, 1] <= 0.5, 
            sample(c(0, 1), prob = c(0.8, 0.2)), 
            sample(c(0, 1), prob = c(0.2, 0.8))
)

# Define dataframe with response (Y) and feature (X) variables
dat <- data.frame(cbind(Y,X))
colnames(dat) <- c('y', 'x1', 'x2', 'x3', 'x4', 'x5')


# 2. Generate a test sample of size 2000 from the same population.

# Define parameters
N_test <- 2000

# Generate test features
X_test <- round(mvrnorm(N_test, mu, Sigma), 2)

# Convert test features into dataframe
test_dat <- data.frame(X_test)
colnames(test_dat) <- c('x1', 'x2', 'x3', 'x4', 'x5')

# Check pairwise correlation (should be ~0.95)
cor(X_test)

# 3. Generate 200 bootstrap samples
# Intro to bootstrapping: http://www.ats.ucla.edu/stat/r/library/bootstrap.htm


# 4. Fit classification trees to the training sample and to each 
# of 200 bootstrap samples.

