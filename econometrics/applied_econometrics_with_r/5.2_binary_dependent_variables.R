# 5.2 Binary Dependent Variables
# Applied Econometrics with R

# Created: 17-Mar-2015
# Updated: 17-Mar-2015

#===========
# Libraries
#===========
library(AER)
library(stats)
library(plyr)
library(ROCR)

#=============
# Import data
#=============
data('SwissLabor') # Generates df 'SwissLabor'

#========
# Probit
#========

# Female labour force participation in Switzerland
yxb_swiss <- participation ~ . + I(age^2)
swiss_probit <- glm(yxb_swiss, data = SwissLabor, family = binomial(link = 'probit'))
summary(swiss_probit)

#===============
# Visualization
#===============

# Spinograms
plot(participation ~ age, data = SwissLabor, ylevels = 2:1)
plot(participation ~ education, data = SwissLabor, ylevels = 2:1)
plot(participation ~ youngkids, data = SwissLabor, ylevels = 2:1)
plot(participation ~ oldkids, data = SwissLabor, ylevels = 2:1)

#=========
# Effects
#=========

# Average of sample marginal effects
fav <- mean(dnorm(predict(swiss_probit, type = 'link')))
fav*coef(swiss_probit)

# Average marginal effects
# Note: In this case, it is preferable to report average 
#       effects for all levels of the factors, averaging only 
#       over continuous regressors.
av <- colMeans(SwissLabor[, -c(1, 7)])
av <- data.frame(rbind(swiss = av, foreign = av), foreign = factor(c('no', 'yes')))
av <- predict(swiss_probit, newdata = av, type = 'link')
av <- dnorm(av)
av['swiss']*coef(swiss_probit)[-7]
av['foreign']*coef(swiss_probit)[-7]

#================================
# Goodness of fit and prediction
#================================

# McFadden's pseudo-R^2
swiss_probit0 <- update(swiss_probit, formula = . ~ 1)
mcf <- 1 - as.vector(logLik(swiss_probit)/logLik(swiss_probit0))

# Predicted values
prop.table(table(true = SwissLabor$participation, pred = round(fitted(swiss_probit))), 2)

# Receiver oprating characteristic (ROC)
pred <- prediction(fitted(swiss_probit), SwissLabor$participation)
plot(performance(pred, 'acc'))
plot(performance(pred, 'tpr', 'fpr'))
abline(0, 1, lty = 2)

#===========================
# Residuals and diagnostics
#===========================

# Sum of squares
deviance(swiss_probit) # or;
sum(residuals(swiss_probit, type = 'deviance')^2)
sum(residuals(swiss_probit, type = 'pearson')^2)

# Analysis of deviance
coeftest(swiss_probit, vcov = sandwich)

# Other standard tests for nested models:
# waldtest()
# linear.hypothesis()
# coeftest()

# Note: sandwich gives the usual regression output with 
#       robustified standard errors and t statistics. However, 
#       sandwich estimates are much more useful and less 
#       controversial in Poisson regressions.

# Quasi-complete separation
