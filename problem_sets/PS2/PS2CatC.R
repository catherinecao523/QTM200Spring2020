#make the data given into matrix
data<-c(14,6,7,7,7,1)
P=matrix(c(14,6,7,7,7,1), nrow=2, ncol=3,byrow=TRUE)
dimnames(P) = list(c("upper", "lower"), c("not-stop", "bribe", "stop"))
addmargins(P)
exp<-c(27*21/42, 27*13/42, 27*8/42, 15*21/42, 15*13/42, 15*8/42)
chi_square<-sum((data-exp)^2/exp)
chi_square
#3.791168
p_value=pchisq(3.791168, df=2, lower.tail = F)
p_value
#0.1502306
# A p-value higher than the significance level is not statistically significant and indicates weak evidence against the null hypothesis. 
# This means we fail to reject the null hypothesis and cannot accept the alternative hypothesis.