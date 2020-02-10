#Q1
#make the data given into a tble by using matrix
data<-c(14,6,7,7,7,1)
P=matrix(c(14,6,7,7,7,1), nrow=2, ncol=3,byrow=TRUE)
dimnames(P) = list(c("upper", "lower"), c("not-stop", "bribe", "stop"))
addmargins(P)#show the sum of each row and column
exp<-c(27*21/42, 27*13/42, 27*8/42, 15*21/42, 15*13/42, 15*8/42)#calculate the expected value
chi_square<-sum((data-exp)^2/exp)
chi_square#calculate chi-square
#we get the result of chi-square [1] 3.791168
#calculate p-value
p_value=pchisq(3.791168, df=2, lower.tail = F)
p_value
#we get the result of p_value [1] 0.1502306
#A p-value higher than the significance level means that we fail to reject the null hypothesis and conclude that whether officers were more likely to solicit a bribe from drivers are not independent from their classes.
#calculate standardized residuals
SR=c((data-exp)/(exp^.5))
SR
rowp<-c(27/42, 27/42, 27/42, 15/42, 15/42, 15/42)
colp<-c(21/42, 13/42, 8/42, 21/42, 13/42, 8/42)
#calculte adjusted residuals
z=c((data-exp)/(exp*(1-rowp)*(1-colp))^.5)
z 
#view standardized residuals
plot(SR)
mean(SR)
#we get the result [1] -0.007950638
#the residuals are very small(almost 0) and evenly distributed which does not indicate a significant difference between the observed value and expected value

#Q2
#the null hypothesis is that there's no association between whether a village have female leaders position reserved or not and the number of new or repaired drinking water facilities in that village
#the alternative hypothesis is that there exist an association between whether a village have female leaders position reserved or not and the number of new or repaired drinking water facilities in that village
#import data
library(readr)
women <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
lm<-lm(formula = water ~ reserved, data = women)
summary(lm)
#we get R_square as 0.01688
anova(lm)
R_square=c(1.211/(1.211+70.565))
R_square#calculate R_square
#we get R_square [1] 0.01687193 same as above
#run correlation test
cor.test(women$water, women$reserved)
#since p-value(= 0.0197) <0.05 we reject the null hypothesis and conclude that there is an significant correlation beween the two variable
#the coefficient of determination is low (0.01688), indicating only 1% of points fall within the regression line. 
#the coefficient estimate is 9.252, meaning for each unit increase in reserved female positions there will be 9.252 more new or repaired drinking water facilities

#Q3
#import data
library(readr)
ff <- read_csv("fruitfly.csv")
summary(ff)#there's 25 flies, meaninf the lifespan is 57.44
#the distribution of lifespan
hist(ff$lifespan) 
#we can see that it's approximately normal
#plot lifespan vs thorax graph
plot(lifespan~ thorax, main="lifespan vs thorax", xlab="thorax ", ylab="lifespan") 
#the graph is a positive linear association
#calculate the correlation coefficient
cor(ff$lifespan, ff$thorax)
#we get the correlation coefficient [1] 0.6364835, which is close to 1, meaning a relatively highly positive correlation
#do a linear regression
lmff<-lm(lifespan~ thorax)
summary(lmff)
abline(lmff)
#the slope is -61.05, which is negetive meaning that when the one variable increases, the other decreases 
#do cor.test for the significance of correlation
cor.test(ff$lifespan, ff$thorax)
#we have p_value = 1.497e-15, which can be considered highly significant at the 95% confidence level so we reject the null hypothesis and conclude that the correlation between lifespan and thorax is significant

#90% confidence interval for the slope
#by using the formula for typical confidence intervals
error<-confint<-c(144.33-15.77*qt(0.9, df = 123), 144.33+15.77*qt(0.9, df = 123))
confint#we get the result [1] 124.0108 164.6492
#by using the function confint  
confint(lmff, parm = "thorax", level = 0.9)
#            5 %   95 %
#thorax 118.1962 170.47
#we get the result 118.1962 170.47, which is not consistent since it's a 95%
confint(lmff, parm = "thorax", level = 0.8)
#           10 %     90 %
#thorax 124.0133 164.6529
#we get the result 124.0133 164.6529, which is about the same as above

#predict a lifespan of a individual fruitfly
pl<-0.8*144.33-61.05
pl #we get result [1] 54.414
#predict the average lifespan for thorax = 0.8 and confidence intervals
plmff <- predict.lm(lmff, thorax = 0.8, df = 123, interval = "confidence")
#calculate the average
mean(plmff) #we get the average[1] 57.44
#Find the confidence interval at 95%
summary(plmff)  #we get the mean of lwr as 54.16 and the mean of upr as 60.72
#so the confident interval of the lwr and upr is (54.16, 60.72)

#plot the fitted lifespan for a sequence of thorax values with prediction intervals and confidence intervals
#add predictions
pred.int<-predict(lmff, interval = "prediction")
ffp<-cbind(ff, pred.int)
#plot regression line and confidence intervals
install.packages("ggplot2")
library(ggplot2)
p <- ggplot(ffp, aes(x = thorax, y = lifespan)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Fitted Linear Model of lifespan vs thorax")
#add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")
p#plot with prediction intervals in red dash and confidence intervals in gray