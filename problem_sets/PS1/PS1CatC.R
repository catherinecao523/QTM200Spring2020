#Q1
#calculate the mean, sd and upper lower boundary 
z90 <- qt((1-.90)/2, df=24, lower.tail = FALSE)
n<-length(na.omit(y))
sample_mean<-mean(y , na.rm = TRUE) 
sample_sd<-sd(y , na.rm = TRUE)
lower_90<-sample_mean-(z90 * (sample_sd/sqrt(n))) 
upper_90<-sample_mean+(z90 * (sample_sd/sqrt(n))) 
confint90<-c(lower_90, upper_90)
print(confint90)
#we get the result of a 90% confidence interval[1]  93.95993 102.92007

#Q2
#conduct one-sample t-test to see if true mean is greater than 100
t.test(y, mu = 100, alternative = "greater")
#One Sample t-test
#data:  y
#t = -0.59574, df = 24, p-value = 0.7215
#alternative hypothesis: true mean is greater than 100
#95 percent confidence interval:
#  93.95993      Inf
#sample estimates:
#  mean of x 
#     98.44 
#since p-value > 0.05 we fail to reject the null hypothesis thus we cannot conclude that her school mean IQ is higher than the country's. 

#Q3
#import data
expenditure<-read.table("expenditure.txt" , header=T)
#1)
plot(expenditure$X1,expenditure$Y, main="Personal income vs expenditure on public education",xlab="per capita personal income",ylab="per capita expenditure on public education")
#Figure 1 shows a positive linear relationship
plot(expenditure$X2,expenditure$Y, main="Residents under 18 vs expenditure on public education",xlab="Number of residents per thousand under 18 years of age",ylab="per capita expenditure on public education")
#Figure 2 shows a negative relationship
plot(expenditure$X3,expenditure$Y, main="People in urban areas vs expenditure on public education",xlab="Number of people per thousand residing in urban areas",ylab="per capita expenditure on public education")
#Figure 3 shows a almost flat positive linear relationship

#2) for relationship between Y and Region
expenditure$R<-expenditure$Y
expenditure$R<-factor(NA, level=c("Northeast", "North Central", "South", "West"))
#rename region number to actual type
expenditure$R[expenditure$Region==1]<-"Northeast"
expenditure$R[expenditure$Region==2]<-"North Central"
expenditure$R[expenditure$Region==3]<-"South"
expenditure$R[expenditure$Region==4]<-"West"
#do a boxplot
boxplot(expenditure$Y~expenditure$R, main="Expenditure on public education vs Region",xlab="Regions",ylab="per capita expenditure on public education")
#we see on Figure 4 on average West has the highest per capita expenditure on public education

#3)for relationship between Y and X1
plot(expenditure$X1,expenditure$Y, main="personal income vs expenditure on public education",xlab="per capita personal income",ylab="per capita expenditure on public education", col=expenditure$Region, pch=expenditure$Region)
legend(x="topleft", legend = levels(expenditure$R), col = c(1,2,3,4), pch = c(1,2,3,4))
#from Figure 5 we can tell that the 4 regions have a positive linear relationship with West and Central in the middle, Northest with the most income and South the least

