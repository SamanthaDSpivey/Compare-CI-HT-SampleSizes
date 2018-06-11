#H2O Data Analysis 3 (n=3)


#Original H2O data population
water<-c(98.1, 100.7, 101.3, 98.79, 97.56, 97.69, 97.95, 100.01, 98.56, 101.05, 101.98,
98.06, 98.91, 100.16, 100.64, 100.3, 101.4, 100.6, 100.9, 
101.2, 99.2, 102.21, 100.3, 101.43, 101.29, 98.34, 97.78, 97.87, 97.74, 99.49, 
97.92, 99.35, 99.27, 98.1, 98.38, 98.27, 97.63, 99.86, 101.16, 101.42, 101.19,
98.47, 99.78, 99.52, 100.56, 101.02, 102.58, 101.77, 101.7, 102.91, 103.65, 
98.39, 96.24, 99.33, 99.95, 99.49, 99.89, 100.02, 93.2, 94.6, 93.7, 99.7, 
100.3, 99.6, 100.3, 99.9, 99.6, 101.6, 100.6, 92.4, 96.26, 96.3, 99.08, 98.8, 
98.58, 99.23, 96.8, 95.8, 98.28, 98.22, 107.35, 98.21, 94.6, 97.13, 95.79, 97.71)


#Reserved 3 observations (originally sampled from original H20 data population above)
R3.obs<-c(99.5, 101.8, 99.8)
mean(R3.obs)
[1] 100.3667
var(R3.obs)
[1] 1.563333
sd(R3.obs)
[1] 1.250333
length(R3.obs)
[1] 3


#Basic summary statistics of original population
length(water)
[1] 86
mean(water)
[1] 99.22058
median(water)
[1] 99.42
mean(water,trim=(0.10))
[1] 99.32129
var(water)
[1] 5.363763
sd(water)
[1] 2.31598
range(water)
[1]  92.40 107.35
IQR(water)
[1] 2.56
summary(water)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  92.40   98.07   99.42   99.22  100.60  107.30 
hist(water,xlab="(percent water)",main="")
title("Histogram of H2O Population",cex.main=1.5)
boxplot(as.data.frame(water),ylab="(percent water)")
title("Boxplot of H2O Population",cex.main=1.5)
qqnorm(water,main="")
qqline(water)
title("Normal Q-Q Plot of H2O Population",cex.main=1.5)


#Shapiro-Wilk normality test of original H2O population
shapiro.test(water)
        Shapiro-Wilk normality test
data:  water 
W = 0.9628, p-value = 0.01412
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#Nonparametric bootstrap of H2O sample distribution of n=3
set.seed(18)
B<-25000
pop<-mean(water)
n3<-3
smpl.dist3<-array()
for(i in 1:B){
  sample3<-(sample(water,3,replace=TRUE))
  smpl.dist3[i]<-(mean(sample3)-pop)/(sd(sample3)/sqrt(n3))}


#Remove +/- infinities from sample distribution 
pos<-which(smpl.dist3==Inf)
pos
[1]   347  7639  9359 15860 16341 21581
neg<-which(smpl.dist3==-Inf)
neg
[1]   950 24541
smpl.dist3<-smpl.dist3[-c(pos,neg)]
min(smpl.dist3)
[1] -1027.174
max(smpl.dist3)
[1] 660.8256


#Basic summary statistics of H2O sample distribution of n=3
length(smpl.dist3)
[1] 24992
mean(smpl.dist3)
[1] 0.1900372
median(smpl.dist3)
[1] 0.0661501
mean(smpl.dist3,trim=(0.10))
[1] 0.1173999
var(smpl.dist3)
[1] 72.91691
sd(smpl.dist3)
[1] 8.53914
range(smpl.dist3)
[1] -1027.1744   660.8256
IQR(smpl.dist3)
[1] 1.659256
summary(smpl.dist3)
      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-1.027e+03 -7.274e-01  6.615e-02  1.900e-01  9.319e-01  6.608e+02
hist(smpl.dist3,xlab="(percent water)",main="",breaks=700,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist3,xlab="(percent water)",main="",breaks=2000,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist3,xlab="(percent water)",main="",breaks=5000,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist3,xlab="(percent water)",main="",breaks=10000,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
boxplot(as.data.frame(smpl.dist3),ylab="(percent water)")
title("Boxplot of H2O Sample Distribution of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
qqnorm(smpl.dist3,main="")
qqline(smpl.dist3)
title("Normal Q-Q of H2O Sample Distribution of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")



#Shapiro-Wilk normality test of H2O sampling distribution of n=3
shapiro.test(smpl.dist3[1:5000])
        Shapiro-Wilk normality test
data:  smpl.dist3[1:5000] 
W = 0.0824, p-value < 2.2e-16
shapiro.test(smpl.dist3[10001:15000])
        Shapiro-Wilk normality test
data:  smpl.dist3[10001:15000] 
W = 0.048, p-value < 2.2e-16
shapiro.test(smpl.dist3[20001:25000])
        Shapiro-Wilk normality test
data:  smpl.dist3[20001:25000] 
W = 0.5831, p-value < 2.2e-16
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#MST.FIT of H2O sampling distribution of n=3
mst.smpl.dist3<-mst.fit(y=smpl.dist3,plot.it=TRUE)
mst.smpl.dist3$dp
$beta
     smpl.dist3
[1,] -0.2407696

$Omega
           smpl.dist3
smpl.dist3   1.067109

$alpha
smpl.dist3 
 0.3477716 

$df
[1] 1.951288
#Since the skew is not 0, actual alpha= 0.3478, and the degrees of freedom are not
#large, actual df= 1.951, therefore we can not say that the data is normal (from the
#special conditions of a skew-t distribution)
#Has heavy tails and slightly skewed



#Quantiles of H2O sampling distribution of n=3
q.smpl.dist3<-quantile(smpl.dist3,c(.025,.975))
q.smpl.dist3
     2.5%     97.5% 
-3.842692  4.893111 
plot(density(smpl.dist3),main="",lwd=2,xlim=c(-10,10))
title("Density estimate of H2O sampling distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist3[1],col="blue")
abline(v=q.smpl.dist3[2],col="blue")




#Confidence Interval of H2O sampling distribution of n=3
#Assuming we have a SRS of data (iid)
#xbar is not normally distributed. We modified the formula for the confidence 
#interval so that the "normally distributed" assumption would not need to be met.
tCI.smpl.dist3<-function(data,alpha,dig=3){
n<-length(data)
xbar<-mean(data)
s<-sd(data)
qlower<-quantile(smpl.dist3,c(.025))
qupper<-quantile(smpl.dist3,c(.975))
lower<-xbar-abs(qlower)*s/sqrt(n)
upper<-xbar+abs(qupper)*s/sqrt(n)
print(paste("CI is","(",round(lower,dig),",",round(upper,dig),")",sep=""))}
tCI.smpl.dist3(R3.obs,.05)
[1] "CI is(97.593,103.899)"
#We are 95% confident that the true mean water standard value is between 
#97.593 and 103.899 percent water.




#Hypothesis Test for H2O T.S. sampling distribution of n=3
#H0: (mu)mean = 100
#Ha: (mu)mean not= 100
#Assuming we have a SRS of data (iid)
#xbar is not normally distributed. We modified the formula for the confidence 
#interval so that the "normally distributed" assumption would not need to be met.

#Hypothesis test - test statistic sampling distribution of n=3
set.seed(18)
B<-25000
H0<-100
n3<-3
smpl.dist3HT<-array()
for(i in 1:B){
  sample3<-(sample(water,3,replace=TRUE))
  smpl.dist3HT[i]<-(mean(sample3)-H0)/(sd(sample3)/sqrt(n3))}


#HT Remove +/- infinities from T.S. sample distribution
posHT<-which(smpl.dist3HT==Inf)
posHT
[1]   347  9359 15860 16341 21581
negHT<-which(smpl.dist3HT==-Inf)
negHT
[1]   950  7639 24541
smpl.dist3HT<-smpl.dist3HT[-c(posHT,negHT)]
min(smpl.dist3HT)
[1] -1261
max(smpl.dist3HT)
[1] 427

#Test Statistic (T.S.)
TS.R3.obs<-(mean(R3.obs)-100)/(sd(R3.obs)/sqrt(length(R3.obs)))
TS.R3.obs
[1] 0.5079328

#Critical value
qlower3HT<-quantile(smpl.dist3HT,c(.025))
qlower3HT
-6.500924  
qupper3HT<-quantile(smpl.dist3HT,c(.975))
qupper3HT
  2.5 
#The TS, 0.5079, does not exceed the critical value of 2.5, therefore
#we fail to reject the null hypothesis.



#HT Quantiles of H2O T.S. sampling distribution of n=3
q.smpl.dist3HT<-quantile(smpl.dist3HT,c(.025,.975))
q.smpl.dist3HT
      2.5%     97.5% 
-6.500924  2.500000
plot(density(smpl.dist3HT),main="",lwd=2,xlim=c(-10,10))
title("Density est. of H2O T.S. sampling distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist3HT[1],col="blue")
abline(v=q.smpl.dist3HT[2],col="blue")
abline(v=TS.R3.obs,col="red")


#P-value
length(which(smpl.dist3HT<TS.R3.obs))
[1] 20928
length(which(smpl.dist3HT>TS.R3.obs))
[1] 4064
length(which(smpl.dist3HT<TS.R3.obs))/25000
[1] 0.83712
length(which(smpl.dist3HT>TS.R3.obs))/25000
[1] 0.16256
2*(length(which(smpl.dist3HT>TS.R3.obs))/25000)
[1] 0.32512
#The p-value, 0.32512, is greater than alpha of 0.05, therefore we fail
#to reject the null hypothesis.

#At alpha=0.05 we have insufficant evidence that the mean water 
#standard value is significantly different from the true mean of 100%.  



#Basic summary statistics of H2O sample distribution of n=3
length(smpl.dist3HT)
[1] 24992
mean(smpl.dist3HT)
[1] -1.012625
median(smpl.dist3HT)
[1] -0.6981847
mean(smpl.dist3HT,trim=(0.10))
[1] -0.7318597
var(smpl.dist3HT)
[1] 85.41578
sd(smpl.dist3HT)
[1] 9.242066
range(smpl.dist3HT)
[1] -1261   427
IQR(smpl.dist3HT)
[1] 1.635135
summary(smpl.dist3HT)
       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-1.261e+03 -1.539e+00 -6.982e-01 -1.013e+00  9.577e-02  4.270e+02
hist(smpl.dist3HT,xlab="(percent water)",main="",breaks=700,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist3HT,xlab="(percent water)",main="",breaks=2000,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist3HT,xlab="(percent water)",main="",breaks=5000,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist3HT,xlab="(percent water)",main="",breaks=10000,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
boxplot(as.data.frame(smpl.dist3HT),ylab="(percent water)")
title("Boxplot of H2O T.S. Sample Distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
qqnorm(smpl.dist3HT,main="")
qqline(smpl.dist3HT)
title("Normal Q-Q of H2O T.S. Sample Distr. of n=3",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")



#HT Shapiro-Wilk normality test of H2O T.S. sampling distribution of n=3
shapiro.test(smpl.dist3HT[1:5000])
        Shapiro-Wilk normality test
data:  smpl.dist3HT[1:5000] 
W = 0.1514, p-value < 2.2e-16
shapiro.test(smpl.dist3HT[10001:15000])
        Shapiro-Wilk normality test
data:  smpl.dist3HT[10001:15000] 
W = 0.0369, p-value < 2.2e-16
shapiro.test(smpl.dist3HT[20001:25000])
        Shapiro-Wilk normality test
data:  smpl.dist3HT[20001:25000] 
W = 0.4488, p-value < 2.2e-16
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#HT MST.FIT of H2O T.S. sampling distribution of n=3
mst.smpl.dist3HT<-mst.fit(y=smpl.dist3HT,plot.it=TRUE)
mst.smpl.dist3HT$dp
$beta
     smpl.dist3HT
[1,]   -0.2876689

$Omega
             smpl.dist3HT
smpl.dist3HT     1.050366

$alpha
smpl.dist3HT 
  -0.4607676 

$df
[1] 1.928831
#Since the skew is not 0, actual alpha= -0.4608, and the degrees of freedom are not
#large, actual df= 1.929, therefore we can not say that the data is normal (from the
#special conditions of a skew-t distribution)
#Has heavy tails and slightly skewed



#Hypothesis Test function
tHT.smpl.dist3HT<-function(data,mu0,alpha,type.test,dig=4){
n<-length(data)
TS<-(mean(data)-mu0)/(sd(data)/sqrt(n))
if(type.test==1){p.value<-length(which(smpl.dist3HT<TS.R3.obs))/25000}
if(type.test==2){p.value<-length(which(smpl.dist3HT>TS.R3.obs))/25000}
if(type.test==3){p.value<-2*length(which(smpl.dist3HT>TS.R3.obs))/25000}
p.value<-round(p.value,dig)
print(paste("The p-value is ",p.value,sep=""))
if(p.value<alpha){print("Reject Ho")}
if(p.value>=alpha){print("Fail to reject Ho")}}
tHT.smpl.dist3HT(R3.obs,100,0.05,3)
[1] "The p-value is 0.3251"
[1] "Fail to reject Ho"









