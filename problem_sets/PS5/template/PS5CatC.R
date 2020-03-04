#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
#answered the first two questions together with explanation of the 4 plot
# run regression on gamble with specified predictors
#Constant variance assumption for the errors use plot residuals vs. the fitted values
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)
#The residuals vs. fitted values is a simple scatterplot between residuals and predicted values, which looks random with outliers in the up right corner.
#Check the normality assumption use Q-Q plot of the studentized residuals
#The QQ plot has a straight line with points 24, 36 and 39 deviate from the straight line.
plot(model1)
par(mfrow=c(2,2), plot(model1))
summary(model1)
#The Scale-Location plot, like the the first, looks random. 
#The Cooks distance plot tells us which points have the greatest influence on the regression. And we see that points 24, 5 and 39 have great influence on the model.

#Check for large leverage points and plot h values
hatvalues(model1)
plot(hatvalues(model1) , pch=16)
#Plot the hat values with thresholds 
abline (h=2*3/47 , lty =2)
abline (h=3*3/47 , lty =2)
identify(1:47, hatvalues(model1))

#check for outliers and run outlierTest
library(car)
sort(rstudent(model1))
outlierTest(model1, row.names(gamble))
#   rstudent unadjusted p-value Bonferroni p
#24 6.016116         4.1041e-07   1.9289e-05
#39 -2.5060898       1.6269e-02   7.6464e-01
#36  2.1448259       3.7942e-02           NA
#three outliers listed from most to least

#Check for influential points by creating "Bubble plot" with the hat-values and studentized residuals
plot(hatvalues(model1), rstudent(model1) , type = "n" )
cook<-sqrt(cooks.distance(model1))
#bring together leverage, studentized residuals, and cooks distance
points(hatvalues(model1), rstudent(model1), cex=10*cook/max(cook))
abline(h=c(-2,0,2), lty=2)
abline(v=c(2,3)*3/47, lty=2)
identify(hatvalues(model1), rstudent(model1), row.names(gamble))