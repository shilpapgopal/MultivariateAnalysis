mu = c(5,5) # Mean vector for all bivariate distributions plotted.
# Four different covariance matrices.
sigma1 = matrix(c(2,1.6,1.6,2),2)
sigma2 = sigma1/2
sigma3 = matrix(c(2,0.6,0.6,2),2)
sigma4 = matrix(c(2,0,0,2),2)
# (x1,x2) locations where bivariate normal density will be evaluated.
x1 = seq(0,10,length=101) # 101 values between 0 and 10.
x2 = seq(0,10,length=101)
# Setup matrices to hold the bivariate normal density function.
pd1 = matrix(NA, nrow=101, ncol=101)
pd2 = matrix(NA, nrow=101, ncol=101)
pd3 = matrix(NA, nrow=101, ncol=101)
pd4 = matrix(NA, nrow=101, ncol=101)

# Install R package mvtnorm if not already done. Use Bristol site.
#install.packages("mvtnorm")
# Link to the mvtnorm package.
library(mvtnorm) # require(mvtnorm) is used if inside a function.
# Calculate the density of the bivariate normal distribution.
for(i in 1:101){
  for(j in 1:101){
    pd1[i,j]=dmvnorm(c(x1[i],x2[j]), mu, sigma1)
    pd2[i,j]=dmvnorm(c(x1[i],x2[j]), mu, sigma2)
    pd3[i,j]=dmvnorm(c(x1[i],x2[j]), mu, sigma3)
    pd4[i,j]=dmvnorm(c(x1[i],x2[j]), mu, sigma4)
  }}

# Plotting the contours of the density function.
# drawlabels=FALSE so contour levels are not labelled.
par(mfrow=c(2,2))
contour(x1,x2,pd1, drawlabels=FALSE, main="Corr 0.8")
contour(x1,x2,pd2, drawlabels=FALSE, main="Corr 0.8")
contour(x1,x2,pd3, drawlabels=FALSE, main="Corr 0.3")
contour(x1,x2,pd4, drawlabels=FALSE, main="Corr 0.0")
# You may need to manually adjust the contours of the R plotting
# window to get circular contours for the last plot!
# Rows of pd.. correspond to the x-direction whilst
# columns of pd.. correspond to the y-direction.

#-----------------------------


# Q 1-----------------------------------------
# a check whether the covar matrices (sigma1, etc) are of full rank?
# b What pattern do you see in the contour plots?
# c Suppose the covar matrix for the bivariate normal dist becomes sigma5

#Tighter ellipses as correlation increases.
#Size of ellipses a measure of det(sigma).
det(sigma1)   # 1.44 Not singular.
det(sigma2)    #  0.36
det(sigma3)    #  3.64
det(sigma4)    #  4

sigma5 = matrix(c(2,-1,-1,2),2)
sigma5
apply(sigma5,2,mean)
det(sigma5)     # 3 so of full rank.

pd5= matrix(NA, nrow=101, ncol=101)
for(i in 1:101){  for(j in 1:101){ 
  pd5[i,j]=dmvnorm(c(x1[i],x2[j]), mu, sigma5) }}
par(mfrow=c(1,1))
contour(x1,x2,pd5, drawlabels=FALSE, main="Corr -0.5")


# Q 2-----------------------------------------
mu0=c(70,170)
sigma=matrix(c(15,50,50,1400),2)
sigma           # Correlation=0.345.  Covariance=50.
cor(sigma)
cov(sigma)

dd = read.table("lecture04-weightdata.txt", header=TRUE) 
s=cov(dd)   # Or s=var(dd).  Both give variance-covariance matrix.
dd=as.matrix(dd)         # Don't forget this line!
s=cov(dd) 
n=nrow(dd)
mean.dd=t(rep(1,n)) %*% dd/n
mean.dd

# Though the above gives the mean vector it is a row vector.
# Thus the command:
# n * t(mean.dd-mu0) %*% sigma.inv %*% (mean.dd-mu0) 
# does NOT give a scalar, but rather has non-comformable arrays!
# Instead consider:
mean.dd=apply(dd,2,mean)
mean.dd

sigma.inv=solve(sigma)
n * t(mean.dd-mu0) %*% sigma.inv %*% (mean.dd-mu0)  #4.46 Test statistic.
1-pchisq(4.468486,2)  # 0.107 P-value of test > 0.05.  Accept null hyp.
qchisq(0.95,2)   # 5.99 5% critical value for chisquare(2) pdf.

x1 = seq(60,90,length=101)              # 101 values between 60 and 90.
x2 = seq(145,195,length=101)
z = matrix(NA, nrow=101, ncol=101)
for(i in 1:101){  
  for(j in 1:101){ 
    z[i,j]=n * t(c(x1[i],x2[j])-mu0) %*% sigma.inv %*% (c(x1[i],x2[j])-mu0)
  }}
par(mfrow=c(1,1))
contour(x1,x2,z, levels=c(5.991465),xlab="x1",ylab="x2")

points(70,170)               # mu0.  
points(71.45,164.7,pch=20)   # Observed mean.

legend(mu0[1],mu0[2],expression(mu[0]),yjust=0.5,bty="n")
legend(mean.dd[1],mean.dd[2],expression(bar(y)),yjust=0.5,bty="n")
legend(70,150,"5% critical contour",xjust=0.1,bty="n")

# Q 3-----------------------------------------

dd = read.table("lecture06-soil-calcium.txt", header=TRUE) 

mu0=c(20,10,4)
mean.dd=apply(dd,2,mean)
n=nrow(dd)
s=var(dd)   # Or use  s=cov(dd).
s.inv=solve(s)
n * t(mean.dd-mu0) %*% s.inv %*% (mean.dd-mu0)

# T^2(3,9)=95.32525.
# Convert T^2 to a F-test.
f.test=((9-3+1)/(9*3))*95.32525  # F(3,7)
f.test #24.7

1-pf(f.test,3,7)   #0.00042 Very small probability so reject H0.

# Q 4-----------------------------------------

dd=read.csv("Lab6.csv",row.names=1)
dd
apply(dd,2,mean)
cov(dd)
cor(dd)
pairs(dd)


# GNIC and PPP.GNIC correlated in plot.  
# Little evidence that other variables show clear pattern.
# Set up some log-transformations.
ddt=dd
ddt[,1]=log(ddt[,1])
ddt[,2]=log(ddt[,2])

pairs(ddt)    # Popn. and Area correlated in plots 
cor(ddt)   # cor(ddt) fails as some values in dd equal zero.

#---
ddt=dd
ddt[,1]=log(ddt[,1])
ddt[,2]=log(ddt[,2])
ddt[ddt==-Inf, ] # chk all rows with -Inf value
ddt[ddt==-Inf]=NA   # Assign all cases where ddt equals -Inf as NA.
#
#Test of hypothesis

mu0=c(15000,20000)

dd2=dd[,3:4]
mean.dd2=apply(dd2,2,mean)

n=nrow(dd2)    # n=188.
s=var(dd2)     # Or use  s=cov(dd2).
s.inv=solve(s)
n * t(mean.dd2-mu0) %*% s.inv %*% (mean.dd2-mu0)# Test statistic.

# Convert T^2 to F-test.
f.test=((187-2+1)/(187*2))*12.92563  # F(2,186)
f.test #6.42

1-pf(f.test,2,186)# 0.00199-Very small probability so reject H0.

#apply(dd,2,mean)
