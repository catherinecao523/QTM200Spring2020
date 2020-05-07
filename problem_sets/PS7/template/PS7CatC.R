#1
#import dataset
m <- read.csv("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS7/MexicoMuniData.csv")
summary(m)
m1 <- glm(as.factor(PAN.visits.06) ~ competitive.district, family = binomial, data=m)
summary(ml)



















