#Q1
#import dataset
i <- read.csv("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS3/incumbents_subset.csv")
summary(i)
#Build linear regression model 
lm1 <- lm(i$difflog~i$voteshare, data=i)
summary(lm1)
#Make a scatterplot and add the regression line
plot(i$difflog~i$voteshare, main="voteshare vs difflog",xlab = "difflog", ylab = "voteshare") 
abline(lm1)
#Save the residuals of the model in a separate object
re1 <- residuals(lm1)
#Write the prediction equation
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.579031   0.002251  257.19   <2e-16 ***
#difflog     0.041666   0.000968   43.04   <2e-16 ***
#Residual standard error: 0.07867 on 3191 degrees of freedom
#Multiple R-squared:  0.3673,	Adjusted R-squared:  0.3671 
#F-statistic:  1853 on 1 and 3191 DF,  p-value: < 2.2e-16
#y = 0.579 + 0.042x1 + sigma1 (Where epsilon is 0.079^2)

#Q2
#Build linear regression model 
lm2 <- lm(i$presvote~i$difflog)
#Make a scatterplot and add the regression line
plot(i$presvote~i$difflog, main = "presvote vs. difflog", xlab = "difflog", ylab = "prevote")
abline(lm2)
#Save the residuals of the model in a separate object
re2 <- residuals(lm2)
#Write the prediction equation
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.507583   0.003161  160.60   <2e-16 ***
#difflog     0.023837   0.001359   17.54   <2e-16 ***
#Residual standard error: 0.1104 on 3191 degrees of freedom
#Multiple R-squared:  0.08795,	Adjusted R-squared:  0.08767 
#F-statistic: 307.7 on 1 and 3191 DF,  p-value: < 2.2e-16
#y = 0.508 + 0.024x1 + sigma1 (Where epsilon is 0.110^2)

#Q3
#Build linear regression model 
lm3 <- lm(i$voteshare~i$presvote)
#Make a scatterplot and add the regression line
plot(i$voteshare~i$presvote, main = "voteshare vs presvote ", xlab = "presvote", ylab = "voteshare")
abline(lm3)
#Write the prediction equation
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.441330   0.007599   58.08   <2e-16 ***
#prevote     0.388018   0.013493   28.76   <2e-16 ***
#Residual standard error: 0.08815 on 3191 degrees of freedom
#Multiple R-squared:  0.2058,	Adjusted R-squared:  0.2056 
#F-statistic:   827 on 1 and 3191 DF,  p-value: < 2.2e-16
#y = 0.441 + 0.388x1 + sigma1 (Where epsilon is 0.088^2)

#Q4
#Build linear regression model 
lm4 <- lm(re1~re2)
summary(lm4)
#Make a scatterplot and add the regression line
plot(re1~re2, main = "re1 vs re2", xlab = "re1", ylab = "re2")
abline(lm4)
#Write the prediction equation
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -4.860e-18  1.299e-03    0.00        1    
#re2         2.569e-01  1.176e-02   21.84   <2e-16 ***
#Residual standard error: 0.07338 on 3191 degrees of freedom
#Multiple R-squared:   0.13,	Adjusted R-squared:  0.1298 
#F-statistic:   477 on 1 and 3191 DF,  p-value: < 2.2e-16
#y = -4.860e-18 + 2.569e-01x1 + sigma1 (Where epsilon is 0.073^2)

#Q5
#Build linear regression model 
Xa <- i$difflog
Xb <- i$presvote
Y <- i$voteshare
lm5 <- lm(Y~Xa+Xb)
#Write the prediction equation
summary(lm5)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.4486442  0.0063297   70.88   <2e-16 ***
#Xa          0.0355431  0.0009455   37.59   <2e-16 ***
#Xb          0.2568770  0.0117637   21.84   <2e-16 ***
#Residual standard error: 0.07339 on 3190 degrees of freedom
#Multiple R-squared:  0.4496,	Adjusted R-squared:  0.4493 
#F-statistic:  1303 on 2 and 3190 DF,  p-value: < 2.2e-16
#y = 0.449 + 0.0355x1 + 0.257x2 + sigma1 (Where epsilon is 0.073^2)

#Residual standard error is the same, because it's the square root of the residual sum of squares divided by the residual degrees of freedom.

