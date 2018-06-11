#H2O Data Analysis 3 (n=4)


#Original H2O data population
water4<-c(98.1, 100.7, 101.3, 98.79, 97.56, 97.69, 97.95, 100.01, 98.56, 101.05, 101.98,
98.06, 98.91, 100.16, 100.64, 100.3, 101.4, 100.9, 
101.2, 99.2, 102.21, 100.3, 101.43, 101.29, 98.34, 97.78, 97.87, 97.74, 99.49, 
97.92, 99.35, 99.27, 98.1, 98.38, 98.27, 97.63, 99.86, 101.16, 101.42, 101.19,
98.47, 99.78, 99.52, 100.56, 101.02, 102.58, 101.77, 101.7, 102.91, 103.65, 
98.39, 96.24, 99.33, 99.95, 99.49, 99.89, 100.02, 93.2, 94.6, 93.7, 99.7, 
100.3, 99.6, 100.3, 99.9, 99.6, 101.6, 100.6, 92.4, 96.26, 96.3, 99.08, 98.8, 
98.58, 99.23, 96.8, 95.8, 98.28, 98.22, 107.35, 98.21, 94.6, 97.13, 95.79, 97.71)


#Reserved 4 observations (originally sampled from original H20 data population above)
R4.obs<-c(100.6, 99.5, 101.8, 99.8)
mean(R4.obs)
[1] 100.425
var(R4.obs)
[1] 1.055833
sd(R4.obs)
[1] 1.027538
length(R4.obs)
[1] 4


#Basic summary statistics of original population
length(water4)
[1] 85
mean(water4)
[1] 99.20435
median(water4)
[1] 99.35
mean(water4,trim=(0.10))
[1] 99.30275
var(water4)
[1] 5.404699
sd(water4)
[1] 2.324801
range(water4)
[1]  92.40 107.35
IQR(water4)
[1] 2.58
summary(water4)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  92.40   98.06   99.35   99.20  100.60  107.30 
hist(water4,xlab="(percent water)",main="")
title("Histogram of H2O Population",cex.main=1.5)
boxplot(as.data.frame(water4),ylab="(percent water)")
title("Boxplot of H2O Population",cex.main=1.5)
qqnorm(water4,main="")
qqline(water4)
title("Normal Q-Q Plot of H2O Population",cex.main=1.5)


#Shapiro-Wilk normality test of original H2O population
shapiro.test(water4)
        Shapiro-Wilk normality test
data:  water4 
W = 0.9637, p-value = 0.01714
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#Nonparametric bootstrap of H2O sample distribution of n=4
set.seed(18)
B<-25000
pop<-mean(water4)
n4<-4
smpl.dist4<-array()
for(i in 1:B){
  sample4<-(sample(water4,4,replace=TRUE))
  smpl.dist4[i]<-(mean(sample4)-pop)/(sd(sample4)/sqrt(n4))}


#Remove +/- infinities from sample distribution 
pos<-which(smpl.dist4==Inf)
pos
integer(0)
neg<-which(smpl.dist4==-Inf)
neg
integer(0)
smpl.dist4<-smpl.dist4[-c(pos,neg)]
min(smpl.dist4)
[1] -28.40717
max(smpl.dist4)
[1] 38.70846


#Basic summary statistics of H2O sample distribution of n=4
length(smpl.dist4)
[1] 25000 
mean(smpl.dist4)
[1] 0.1384927 
median(smpl.dist4)
[1] 0.04135478 
mean(smpl.dist4,trim=(0.10))
[1] 0.0820431 
var(smpl.dist4)
[1] 2.874454
sd(smpl.dist4)
[1] 1.695421
range(smpl.dist4)
[1] -28.40717  38.70846
IQR(smpl.dist4)
[1] 1.598858
summary(smpl.dist4)
      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-28.41000  -0.71020   0.04135   0.13850   0.88870  38.71000
hist(smpl.dist4,xlab="(percent water)",main="",breaks=30,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist4,xlab="(percent water)",main="",breaks=50,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist4,xlab="(percent water)",main="",breaks=100,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist4,xlab="(percent water)",main="",breaks=400,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
boxplot(as.data.frame(smpl.dist4),ylab="(percent water)")
title("Boxplot of H2O Sample Distribution of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
qqnorm(smpl.dist4,main="")
qqline(smpl.dist4)
title("Normal Q-Q of H2O Sample Distribution of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")



#Shapiro-Wilk normality test of H2O sampling distribution of n=4
shapiro.test(smpl.dist4[1:5000])
        Shapiro-Wilk normality test
data:  smpl.dist4[1:5000] 
W = 0.7845, p-value < 2.2e-16
shapiro.test(smpl.dist4[10001:15000])
        Shapiro-Wilk normality test
data:  smpl.dist4[10001:15000] 
W = 0.8819, p-value < 2.2e-16
shapiro.test(smpl.dist4[20001:25000])
        Shapiro-Wilk normality test
data:  smpl.dist4[20001:25000] 
W = 0.8566, p-value < 2.2e-16
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#MST.FIT of H2O sampling distribution of n=4
mst.smpl.dist4<-mst.fit(y=smpl.dist4,plot.it=TRUE)
mst.smpl.dist4$dp
$beta
     smpl.dist4
[1,] -0.3640967

$Omega
           smpl.dist4
smpl.dist4   1.172179

$alpha
smpl.dist4 
 0.4963351 

$df
[1] 3.262531
#Since the skew is not 0, actual alpha= 0.4963, and the degrees of freedom are not
#large, actual df= 3.263, therefore we can not say that the data is normal (from the
#special conditions of a skew-t distribution)
#Has heavy tails and slightly skewed



#Quantiles of H2O sampling distribution of n=4
q.smpl.dist4<-quantile(smpl.dist4,c(.025,.975))
q.smpl.dist4
     2.5%     97.5% 
-2.631392  3.547868  
plot(density(smpl.dist4),main="",lwd=2,xlim=c(-10,10))
title("Density estimate of H2O sampling distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist4[1],col="blue")
abline(v=q.smpl.dist4[2],col="blue")




#Confidence Interval of H2O sampling distribution of n=4
#Assuming we have a SRS of data (iid)
#xbar is not normally distributed. We modified the formula for the confidence 
#interval so that the "normally distributed" assumption would not need to be met.
tCI.smpl.dist4<-function(data,alpha,dig=3){
n<-length(data)
xbar<-mean(data)
s<-sd(data)
qlower<-quantile(smpl.dist4,c(.025))
qupper<-quantile(smpl.dist4,c(.975))
lower<-xbar-abs(qlower)*s/sqrt(n)
upper<-xbar+abs(qupper)*s/sqrt(n)
print(paste("CI is","(",round(lower,dig),",",round(upper,dig),")",sep=""))}
tCI.smpl.dist4(R4.obs,.05)
[1] "CI is(99.073,102.248)"
#We are 95% confident that the true mean water standard value is between 
#99.073 and 102.248 percent water.




#Hypothesis Test for H2O T.S. sampling distribution of n=4
#H0: (mu)mean = 100
#Ha: (mu)mean not= 100
#Assuming we have a SRS of data (iid)
#xbar is not normally distributed. We modified the formula for the confidence 
#interval so that the "normally distributed" assumption would not need to be met.

#Hypothesis test - test statistic sampling distribution of n=4
set.seed(18)
B<-25000
H0<-100
n4<-4
smpl.dist4HT<-array()
for(i in 1:B){
  sample4<-(sample(water4,4,replace=TRUE))
  smpl.dist4HT[i]<-(mean(sample4)-H0)/(sd(sample4)/sqrt(n4))}


#HT Remove +/- infinities from T.S. sample distribution
posHT<-which(smpl.dist4HT==Inf)
posHT
integer(0)
negHT<-which(smpl.dist4HT==-Inf)
negHT
integer(0)
smpl.dist4HT<-smpl.dist4HT[-c(posHT,negHT)]
min(smpl.dist4HT)
[1] -50.85542
max(smpl.dist4HT)
[1] 24.47074

#Test Statistic (T.S.)
TS.R4.obs<-(mean(R4.obs)-100)/(sd(R4.obs)/sqrt(length(R4.obs)))
TS.R4.obs
[1] 0.8272204

#Critical value
qlower4HT<-quantile(smpl.dist4HT,c(.025))
qlower4HT
     2.5% 
-4.458183  
qupper4HT<-quantile(smpl.dist4HT,c(.975))
qupper4HT
    97.5% 
1.738852 
#The TS of 0.8272, does not exceed the critical value of 1.74, therefore
#we fail to reject the null hypothesis.



#HT Quantiles of H2O T.S. sampling distribution of n=4
q.smpl.dist4HT<-quantile(smpl.dist4HT,c(.025,.975))
q.smpl.dist4HT
     2.5%     97.5% 
-4.458183  1.738852
plot(density(smpl.dist4HT),main="",lwd=2,xlim=c(-10,10))
title("Density est. of H2O T.S. sampling distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist4HT[1],col="blue")
abline(v=q.smpl.dist4HT[2],col="blue")
abline(v=TS.R4.obs,col="red")


#P-value
length(which(smpl.dist4HT<TS.R4.obs))
[1] 22885 
length(which(smpl.dist4HT>TS.R4.obs))
[1] 2115 
length(which(smpl.dist4HT<TS.R4.obs))/25000
[1] 0.9154 
length(which(smpl.dist4HT>TS.R4.obs))/25000
[1] 0.0846 
2*(length(which(smpl.dist4HT>TS.R4.obs))/25000)
[1] 0.1692 
#The p-value, 0.1692, is greater than alpha of 0.05, therefore we fail
#to reject the null hypothesis.

#At alpha=0.05 we have insufficant evidence that the mean water 
#standard value is significantly different from the true mean of 100%.  



#Basic summary statistics of H2O sample distribution of n=4
length(smpl.dist4HT)
[1] 25000
mean(smpl.dist4HT)
[1] -0.9196227
median(smpl.dist4HT)
[1] -0.8060731 
mean(smpl.dist4HT,trim=(0.10))
[1] -0.8144462
var(smpl.dist4HT)
[1] 3.251859
sd(smpl.dist4HT)
[1] 1.803291 
range(smpl.dist4HT)
[1] -50.85542  24.47074 
IQR(smpl.dist4HT)
[1] 1.511216
summary(smpl.dist4HT)
      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-50.86000  -1.55700  -0.80610  -0.91960  -0.04621  24.47000
hist(smpl.dist4HT,xlab="(percent water)",main="",breaks=30,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist4HT,xlab="(percent water)",main="",breaks=50,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist4HT,xlab="(percent water)",main="",breaks=100,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist4HT,xlab="(percent water)",main="",breaks=400,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
boxplot(as.data.frame(smpl.dist4HT),ylab="(percent water)")
title("Boxplot of H2O T.S. Sample Distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
qqnorm(smpl.dist4HT,main="")
qqline(smpl.dist4HT)
title("Normal Q-Q of H2O T.S. Sample Distr. of n=4",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")



#HT Shapiro-Wilk normality test of H2O T.S. sampling distribution of n=4
shapiro.test(smpl.dist4HT[1:5000])
        Shapiro-Wilk normality test
data:  smpl.dist4HT[1:5000] 
W = 0.6895, p-value < 2.2e-16
shapiro.test(smpl.dist4HT[10001:15000])
        Shapiro-Wilk normality test
data:  smpl.dist4HT[10001:15000] 
W = 0.7739, p-value < 2.2e-16
shapiro.test(smpl.dist4HT[20001:25000])
        Shapiro-Wilk normality test
data:  smpl.dist4HT[20001:25000] 
W = 0.7645, p-value < 2.2e-16
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#HT MST.FIT of H2O T.S. sampling distribution of n=4
mst.smpl.dist4HT<-mst.fit(y=smpl.dist4HT,plot.it=TRUE)
mst.smpl.dist4HT$dp
$beta
     smpl.dist4HT
[1,]   -0.3497641

$Omega
             smpl.dist4HT
smpl.dist4HT     1.134700

$alpha
smpl.dist4HT 
  -0.5355831 

$df
[1] 3.121161
#Since the skew is not 0, actual alpha= -0.5356, and the degrees of freedom are not
#large, actual df= 3.1212, therefore we can not say that the data is normal (from the
#special conditions of a skew-t distribution)
#Has heavy tails and slightly skewed



#Hypothesis Test function
tHT.smpl.dist4HT<-function(data,mu0,alpha,type.test,dig=4){
n<-length(data)
TS<-(mean(data)-mu0)/(sd(data)/sqrt(n))
if(type.test==1){p.value<-length(which(smpl.dist4HT<TS.R4.obs))/25000}
if(type.test==2){p.value<-length(which(smpl.dist4HT>TS.R4.obs))/25000}
if(type.test==3){p.value<-2*length(which(smpl.dist4HT>TS.R4.obs))/25000}
p.value<-round(p.value,dig)
print(paste("The p-value is ",p.value,sep=""))
if(p.value<alpha){print("Reject Ho")}
if(p.value>=alpha){print("Fail to reject Ho")}}
tHT.smpl.dist4HT(R4.obs,100,0.05,3)
[1] "The p-value is 0.1692"
[1] "Fail to reject Ho"









