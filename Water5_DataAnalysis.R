#H2O Data Analysis 3 (n=5)


#Original H2O data population
water5<-c(98.1, 100.7, 101.3, 98.79, 97.56, 97.69, 97.95, 100.01, 98.56, 101.05, 101.98,
98.06, 98.91, 100.16, 100.64, 100.3, 100.9, 
101.2, 99.2, 102.21, 100.3, 101.43, 101.29, 98.34, 97.78, 97.87, 97.74, 99.49, 
97.92, 99.35, 99.27, 98.1, 98.38, 98.27, 97.63, 99.86, 101.16, 101.42, 101.19,
98.47, 99.78, 99.52, 100.56, 101.02, 102.58, 101.77, 101.7, 102.91, 103.65, 
98.39, 96.24, 99.33, 99.95, 99.49, 99.89, 100.02, 93.2, 94.6, 93.7, 99.7, 
100.3, 99.6, 100.3, 99.9, 99.6, 101.6, 100.6, 92.4, 96.26, 96.3, 99.08, 98.8, 
98.58, 99.23, 96.8, 95.8, 98.28, 98.22, 107.35, 98.21, 94.6, 97.13, 95.79, 97.71)


#Reserved 5 observations (originally sampled from original H20 data population above)
R5.obs<-c(101.4, 100.6, 99.5, 101.8, 99.8)
mean(R5.obs)
[1] 100.62
var(R5.obs)
[1] 0.982 
sd(R5.obs)
[1] 0.9909591
length(R5.obs)
[1] 5


#Basic summary statistics of original population
length(water5)
[1] 84
mean(water5)
[1] 99.17821
median(water5)
[1] 99.34
mean(water5,trim=(0.10))
[1] 99.27191
var(water5)
[1] 5.411041
sd(water5)
[1] 2.326165
range(water5)
[1]  92.40 107.35
IQR(water5)
[1] 2.5775
summary(water5)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  92.40   98.03   99.34   99.18  100.60  107.30  
hist(water5,xlab="(percent water)",main="")
title("Histogram of H2O Population",cex.main=1.5)
boxplot(as.data.frame(water5),ylab="(percent water)")
title("Boxplot of H2O Population",cex.main=1.5)
qqnorm(water5,main="")
qqline(water5)
title("Normal Q-Q Plot of H2O Population",cex.main=1.5)


#Shapiro-Wilk normality test of original H2O population
shapiro.test(water5)
        Shapiro-Wilk normality test
data:  water5 
W = 0.9639, p-value = 0.01877 
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#Nonparametric bootstrap of H2O sample distribution of n=5
set.seed(18)
B<-25000
pop<-mean(water5)
n5<-5
smpl.dist5<-array()
for(i in 1:B){
  sample5<-(sample(water5,5,replace=TRUE))
  smpl.dist5[i]<-(mean(sample5)-pop)/(sd(sample5)/sqrt(n5))}


#Remove +/- infinities from sample distribution 
pos<-which(smpl.dist5==Inf)
pos
integer(0)
neg<-which(smpl.dist5==-Inf)
neg
integer(0)
smpl.dist5<-smpl.dist5[-c(pos,neg)]
min(smpl.dist5)
[1] -24.05548 
max(smpl.dist5)
[1] 31.17620


#Basic summary statistics of H2O sample distribution of n=5
length(smpl.dist5)
[1] 25000 
mean(smpl.dist5)
[1] 0.1108454 
median(smpl.dist5)
[1] 0.03369433 
mean(smpl.dist5,trim=(0.10))
[1] 0.07013827 
var(smpl.dist5)
[1] 2.03427
sd(smpl.dist5)
[1] 1.426278
range(smpl.dist5)
[1] -24.05548  31.17620
IQR(smpl.dist5)
[1] 1.549346
summary(smpl.dist5)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-24.06000  -0.70290   0.03369   0.11080   0.84650  31.18000
hist(smpl.dist5,xlab="(percent water)",main="",breaks=30,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist5,xlab="(percent water)",main="",breaks=50,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist5,xlab="(percent water)",main="",breaks=100,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist5,xlab="(percent water)",main="",breaks=300,xlim=c(-10,10))
title("Histogram of H2O Sample Distribution of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
boxplot(as.data.frame(smpl.dist5),ylab="(percent water)")
title("Boxplot of H2O Sample Distribution of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
qqnorm(smpl.dist5,main="")
qqline(smpl.dist5)
title("Normal Q-Q of H2O Sample Distribution of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")



#Shapiro-Wilk normality test of H2O sampling distribution of n=5
shapiro.test(smpl.dist5[1:5000])
        Shapiro-Wilk normality test
data:  smpl.dist5[1:5000] 
W = 0.9091, p-value < 2.2e-16
shapiro.test(smpl.dist5[10001:15000])
        Shapiro-Wilk normality test
data:  smpl.dist5[10001:15000] 
W = 0.9525, p-value < 2.2e-16
shapiro.test(smpl.dist5[20001:25000])
        Shapiro-Wilk normality test
data:  smpl.dist5[20001:25000] 
W = 0.8992, p-value < 2.2e-16 
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#MST.FIT of H2O sampling distribution of n=5
mst.smpl.dist5<-mst.fit(y=smpl.dist5,plot.it=TRUE)
mst.smpl.dist5$dp
$beta
     smpl.dist5
[1,] -0.3991766

$Omega
           smpl.dist5
smpl.dist5   1.192258

$alpha
smpl.dist5 
 0.5504424 

$df
[1] 4.346725
#Since the skew is not 0, actual alpha= 0.5504, and the degrees of freedom are not
#large, actual df= 4.347, therefore we can not say that the data is normal (from the
#special conditions of a skew-t distribution)
#Has heavy tails and slightly skewed



#Quantiles of H2O sampling distribution of n=5
q.smpl.dist5<-quantile(smpl.dist5,c(.025,.975))
q.smpl.dist5
     2.5%     97.5% 
-2.337370  3.102573   
plot(density(smpl.dist5),main="",lwd=2,xlim=c(-10,10))
title("Density estimate of H2O sampling distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist5[1],col="blue")
abline(v=q.smpl.dist5[2],col="blue")




#Confidence Interval of H2O sampling distribution of n=5
#Assuming we have a SRS of data (iid)
#xbar is not normally distributed. We modified the formula for the confidence 
#interval so that the "normally distributed" assumption would not need to be met.
tCI.smpl.dist5<-function(data,alpha,dig=3){
n<-length(data)
xbar<-mean(data)
s<-sd(data)
qlower<-quantile(smpl.dist5,c(.025))
qupper<-quantile(smpl.dist5,c(.975))
lower<-xbar-abs(qlower)*s/sqrt(n)
upper<-xbar+abs(qupper)*s/sqrt(n)
print(paste("CI is","(",round(lower,dig),",",round(upper,dig),")",sep=""))}
tCI.smpl.dist5(R5.obs,.05)
[1] "CI is(99.584,101.995)"
#We are 95% confident that the true mean water standard value is between 
#99.584 and 101.995 percent water.




#Hypothesis Test for H2O T.S. sampling distribution of n=5
#H0: (mu)mean = 100
#Ha: (mu)mean not= 100
#Assuming we have a SRS of data (iid)
#xbar is not normally distributed. We modified the formula for the confidence 
#interval so that the "normally distributed" assumption would not need to be met.

#Hypothesis test - test statistic sampling distribution of n=5
set.seed(18)
B<-25000
H0<-100
n5<-5
smpl.dist5HT<-array()
for(i in 1:B){
  sample5<-(sample(water5,5,replace=TRUE))
  smpl.dist5HT[i]<-(mean(sample5)-H0)/(sd(sample5)/sqrt(n5))}


#HT Remove +/- infinities from T.S. sample distribution
posHT<-which(smpl.dist5HT==Inf)
posHT
integer(0)
negHT<-which(smpl.dist5HT==-Inf)
negHT
integer(0)
smpl.dist4HT<-smpl.dist5HT[-c(posHT,negHT)]
min(smpl.dist5HT)
[1] -44.47291 
max(smpl.dist5HT)
[1] 18.65282

#Test Statistic (T.S.)
TS.R5.obs<-(mean(R5.obs)-100)/(sd(R5.obs)/sqrt(length(R5.obs)))
TS.R5.obs
[1] 1.399010

#Critical value
qlower5HT<-quantile(smpl.dist5HT,c(.025))
qlower5HT
     2.5% 
-3.992047
qupper5HT<-quantile(smpl.dist5HT,c(.975))
qupper5HT
  97.5% 
1.42179
#The TS of 1.3990, does not exceed the critical value of 1.422, therefore
#we fail to reject the null hypothesis.



#HT Quantiles of H2O T.S. sampling distribution of n=5
q.smpl.dist5HT<-quantile(smpl.dist5HT,c(.025,.975))
q.smpl.dist5HT
   2.5%     97.5% 
-3.992047  1.421790 
plot(density(smpl.dist5HT),main="",lwd=2,xlim=c(-10,10))
title("Density est. of H2O T.S. sampling distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist5HT[1],col="blue")
abline(v=q.smpl.dist5HT[2],col="blue")
abline(v=TS.R5.obs,col="red")
plot(density(smpl.dist5HT),main="",lwd=2,xlim=c(0.5,2.5))
title("Density est. of H2O T.S. sampling distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
abline(v=q.smpl.dist5HT[2],col="blue")
abline(v=TS.R5.obs,col="red")



#P-value
length(which(smpl.dist5HT<TS.R5.obs))
[1] 24345 
length(which(smpl.dist5HT>TS.R5.obs))
[1] 655 
length(which(smpl.dist5HT<TS.R5.obs))/25000
[1] 0.9738 
length(which(smpl.dist5HT>TS.R5.obs))/25000
[1] 0.0262 
2*(length(which(smpl.dist5HT>TS.R5.obs))/25000)
[1] 0.0524 
#The p-value, 0.0524, is slightly greater than alpha of 0.05, therefore we fail
#to reject the null hypothesis.

#At alpha=0.05 we have insufficant evidence that the mean water 
#standard value is significantly different from the true mean of 100%.  



#Basic summary statistics of H2O sample distribution of n=5
length(smpl.dist5HT)
[1] 25000
mean(smpl.dist5HT)
[1] -0.9981335
median(smpl.dist5HT)
[1] -0.91746 
mean(smpl.dist5HT,trim=(0.10))
[1] -0.9249947
var(smpl.dist5HT)
[1] 2.237757
sd(smpl.dist5HT)
[1] 1.495913 
range(smpl.dist5HT)
[1] -44.47291  18.65282 
IQR(smpl.dist5HT)
[1] 1.490134 
summary(smpl.dist5HT)
     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-44.4700  -1.6630  -0.9175  -0.9981  -0.1725  18.6500
hist(smpl.dist5HT,xlab="(percent water)",main="",breaks=30,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist5HT,xlab="(percent water)",main="",breaks=50,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist5HT,xlab="(percent water)",main="",breaks=100,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
hist(smpl.dist5HT,xlab="(percent water)",main="",breaks=300,xlim=c(-10,10))
title("Histogram of H2O T.S. Sample Distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
boxplot(as.data.frame(smpl.dist5HT),ylab="(percent water)")
title("Boxplot of H2O T.S. Sample Distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")
qqnorm(smpl.dist5HT,main="")
qqline(smpl.dist5HT)
title("Normal Q-Q of H2O T.S. Sample Distr. of n=5",cex.main=1.5,sub="Nonparametric Bootstrap from Original H2O Population")



#HT Shapiro-Wilk normality test of H2O T.S. sampling distribution of n=5
shapiro.test(smpl.dist5HT[1:5000])
        Shapiro-Wilk normality test
data:  smpl.dist5HT[1:5000] 
W = 0.9057, p-value < 2.2e-16 
shapiro.test(smpl.dist5HT[10001:15000])
        Shapiro-Wilk normality test
data:  smpl.dist5HT[10001:15000] 
W = 0.9187, p-value < 2.2e-16 
shapiro.test(smpl.dist5HT[20001:25000])
        Shapiro-Wilk normality test
data:  smpl.dist5HT[20001:25000] 
W = 0.7836, p-value < 2.2e-16
#The Shapiro-Wilk's p-value is less than 0.05, so we can say that the data 
#did not come from a normal distribution.



#HT MST.FIT of H2O T.S. sampling distribution of n=5
mst.smpl.dist5HT<-mst.fit(y=smpl.dist5HT,plot.it=TRUE)
mst.smpl.dist5HT$dp
$beta
     smpl.dist5HT
[1,]   -0.4397885

$Omega
             smpl.dist5HT
smpl.dist5HT     1.165315

$alpha
smpl.dist5HT 
  -0.5854894 

$df
[1] 4.126054
#Since the skew is not 0, actual alpha= -0.5855, and the degrees of freedom are not
#large, actual df= 4.1261, therefore we can not say that the data is normal (from the
#special conditions of a skew-t distribution)
#Has heavy tails and slightly skewed



#Hypothesis Test function
tHT.smpl.dist5HT<-function(data,mu0,alpha,type.test,dig=4){
n<-length(data)
TS<-(mean(data)-mu0)/(sd(data)/sqrt(n))
if(type.test==1){p.value<-length(which(smpl.dist5HT<TS.R5.obs))/25000}
if(type.test==2){p.value<-length(which(smpl.dist5HT>TS.R5.obs))/25000}
if(type.test==3){p.value<-2*length(which(smpl.dist5HT>TS.R5.obs))/25000}
p.value<-round(p.value,dig)
print(paste("The p-value is ",p.value,sep=""))
if(p.value<alpha){print("Reject Ho")}
if(p.value>=alpha){print("Fail to reject Ho")}}
tHT.smpl.dist5HT(R5.obs,100,0.05,3)
[1] "The p-value is 0.0524"
[1] "Fail to reject Ho"









