#Q1
#import dataset
i <- read.csv("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS6/cholesterol.csv")
summary(i)
#part1 
#Fit an additive model
library(mgcv)
mod_lm <- gam(sex ~ fat, data=i)
summary(mod_lm)
#Parametric coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.0184390  0.0469240  -0.393 0.694620    
#fat          0.0019702  0.0005579   3.532 0.000475 ***
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#R-sq.(adj) =  0.0352   Deviance explained = 3.83%
#GCV = 0.11255  Scale est. = 0.11184   n = 315
#deviance explained % is low

#part2
#For women, increasing their fat intake by 1 gram per day would increase 0.00197 of their odds on being in the high cholesterol group
#For men, increasing their fat intake by 1 gram per day would increase 0.00055 of their odds on being in the high cholesterol group
#A woman with a fat intake of 100 grams per day would have 0.215 of being in the high cholesterol group
mod_gam1 <- gam(sex ~ s(fat, bs="cr"), data=i)
summary(mod_gam1)
#Parametric coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.13333    0.01865   7.149 6.27e-12 ***
#Approximate significance of smooth terms:
#  edf Ref.df     F  p-value    
#s(fat) 3.09  3.788 5.154 0.000805 ***
#R-sq.(adj) =  0.0548   Deviance explained = 6.41%
#GCV = 0.11102  Scale est. = 0.10957   n = 315
anova(mod_lm, mod_gam1, test="Chisq")
#Resid. Df Resid. Dev     Df Deviance Pr(>Chi)  
#1    313.00     35.005                           
#2    310.21     34.068 2.7877  0.93758  0.02997 *

#Q2
#import dataset
g <- read.csv("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS6/gdpChange.csv")
summary(g)
g$GDPWdiff <- relevel(g$GDPWdiff, ref = "no change")
summary(g$GDPWdiff)
#no change  negative  positive 
#16      1105      2600
multi1 <- multinom(GDPWdiff ~ REG + OIL, data = g)
summary(multi1)
stargazer(multi1, type="html", out="multi1.htm")










