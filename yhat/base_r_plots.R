# yhat
# Base R Plots 
# http://blog.yhathq.com/posts/base-r-plots.html

# Created: 16-Mar-2015
# Updated: 16-Mar-2015

#===========
# Libraries
#===========
library(base)

#============
# Iris plots
#============

plot(iris)
plot(iris, col = iris$Species)

attach(iris)
plot(x = Sepal.Width, 
     y = Sepal.Length)
plot(x = Sepal.Width, 
     y = Sepal.Length, 
     col = Species)
detach(iris)

plot(x = iris$Petal.Width,
     y = iris$Petal.Length, 
     pch = as.numeric(iris$Species),
     col = as.numeric(iris$Species))

hist(iris$Sepal.Width)

virginica <- subset(iris, Species == "virginica")
versicolor <- subset(iris, Species == "versicolor")
setosa <- subset(iris, Species == "setosa")

# Plot distributions for each species
plot(density(virginica$Sepal.Width), 
     col = "blue",
     main = 'Density by Species')
lines(density(versicolor$Sepal.Width), 
      col = "red")
lines(density(setosa$Sepal.Width), 
      col = "green")
legend(x = 2, 
       y = 1.2, 
       c("virginica", "versicolor", "setosa"), 
       c("blue", "red", "green"))

#=========================
# Accidental Deaths plots
#=========================

plot(USAccDeaths, 
     xlab = "Year", 
     ylab = "Accident Deaths in U.S.")

plot(USAccDeaths, 
     xlab = "Year", 
     ylab = "Accident Deaths in U.S.", 
     main = "Traffic Accident Deaths")
points(USAccDeaths, 
       pch = 10)

