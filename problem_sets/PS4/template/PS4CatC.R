#load dataset
install.packages(car) 
library(car) 
data(Prestige) 
help(Prestige)
#Q1
P<-Prestige#rename dataset
#Create a new variable
professional <- ifelse(P$type == "prof","1","0")
#Run a linear model 
lm1 <- lm(prestige ~ income + professional + income:professional, data=P)
summary(lm1)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          21.1422589  2.8044261   7.539 2.93e-11 ***
#  income                0.0031709  0.0004993   6.351 7.55e-09 ***
#  professional1        37.7812800  4.2482744   8.893 4.14e-14 ***
#  income:professional1 -0.0023257  0.0005675  -4.098 8.83e-05 ***
#Residual standard error: 8.012 on 94 degrees of freedom
#(4 observations deleted due to missingness)
#Multiple R-squared:  0.7872,	Adjusted R-squared:  0.7804 
#F-statistic: 115.9 on 3 and 94 DF,  p-value: < 2.2e-16
#y = 21.14 + 0.003Xi + 37.78Di - 0.002XiDi + epsiloni (with sigma square as 0,8.012^2)

#Under the condition that other variables remain constant, for 1 unit of increase in income would result in 0.003 unit increase in prestige 
#For professional, the regression line would result in a 37.78 unit upward shift, regardless of income. No effect for white and blue collar workers

#marginal difference for $1,000 increase in income
#y = 21.14 + 0.003Xi + 37.78Di - 0.002XiDi + epsiloni
#when professional=Di=1
#y1 = 21.14 + 0.003(Xi+1000) + 37.78 - 0.002(Xi+1000) + epsiloni
#y^=0.003*1000-0.002*1000=1

#marginal difference for income of $6,000
#y = 21.14 + 0.003Xi + 37.78Di - 0.002XiDi + epsiloni
#when income=Xi=6000
#y2 = 21.14 + 0.003*6000 + 37.78Di - 0.002*6000Di + epsiloni
#y^=37.781-0.002*6000=25.781

#Q2
#determine having yard signs  
#for a two-tailed hypothesis with df=n-k
#n=131 and k=3
(0.042-0)/0.016=2.625
#pvalue for 2.625 is 0.00972 < 0.05, so it affects the vote share

#determine having yard signs next to percincts  
#for a two-tailed hypothesis with df=n-k
#n=131 and k=3
(0.042-0)/0.013=3.231
#pvalue for 3.231 is 0.001568 < 0.05, so it affects the vote share

#Since Precinct assigned lawn signs and Precinct adjacent to lawn signs are both 0 in this case, the proportion of the vote that went to Ken Cuccinelli, the constant is 0.302

#Evaluate the model fit for regression
#use F-test for the model
n=131
k=3
F.test<-((0.094/k)/((1-0.094)/(n-k-1)))
df1<-k
df2<-n-k-1
pvalue<-df(F.test, df1, df2)
#pvalue=0.007141957 < 0.05
#Thus we reject the null hypothesis and conclude that at least one coinficient is significant

