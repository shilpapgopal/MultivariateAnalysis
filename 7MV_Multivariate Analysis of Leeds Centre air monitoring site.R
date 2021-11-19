dd=read.csv('LEED_2020.csv',row.names = 1)
y1=dd[,1:3]   # Times 01.00, 02.00, 03.00  
length(y1)
ncol(dd)
length(dd)
y1

y2=dd[,8:10]
y2

y3=dd[,17:19]   # A data set with 3 columns as variables and 46 rows.

#-----------1--------------------
#1. Produce plots to understand the distribution of the data in these three samples and
#how there are some dependencies within them.
hist(y1[,1])
# Histogram of first variable in y1.
hist(y1[,2],main="")   # And so on....

# Not all variables are "normally" distributed
pairs(y1)     #mid-night   # Plot scatter plots of variables making up y1.   
pairs(y2)     #morning
pairs(y3)     #night
#-----------2--------------------
#Is transformation needed?
#Possible transformation log(y1) and so on.
pairs(log(y1))    # Shows constant more variance with increasing x.

#-----------3--------------------
# Estimate the mean vectors and covariance matrices in these three samples. What
#can you infer?
head(y1,2)
m1=sapply(y1, mean, na.rm = T)
m1
apply(y1, 2,mean ,na.rm = T)

m2=sapply(y2,mean,na.rm=T)
m2

m3=sapply(y3,mean,na.rm=T)
m3

var(y1)
var(y2)
var(y3)

#-----------4--------------------

#Test the equivalence of mean vectors between the late night and morning rush hour
#samples at the 5% significance level. What is your conclusion?
ydiff=m1-m2       # Difference of means.
spool= 0.5*(var(y1)+var(y2))  # Pooled sample variance.  n1=n2=28
# Hotellings T**2 statistic:
tstat12=(1/28 + 1/28)^(-1)*t(ydiff)%*% solve(spool)%*%ydiff
tstat12   # ~ T**2(3, 28+28-2=54) if H0 true.

#Convert Hotelling's T**2 statistics to F-statstic.
n1=28; n2=28; p=3
ff12=(n1+n2-p-1)*tstat12/((n1+n2-2)*p)
# 29.81373           # ~ F(3,52) if H0 true.  n1+n2-p-1=52
qf(0.95,3,52)        # 5% value for F(3,52) is 2.7826.

1-pf(22.36792,3,52)  # P-value of test is 1.94835e-09.  Small!


#  Strongly reject equality.
#-----------5--------------------

#Do the same test on the mean vectors between the morning and 
#evening rush hours, and between the evening rush hours and 
#late night samples. What are your conclusions?

# -------Morning vs evening

ydiff=m2-m3   
spool= 0.5*(var(y2)+var(y3))  # n1=n2=28
tstat23=(1/28 + 1/28)^(-1)*t(ydiff)%*%solve(spool)%*%ydiff
tstat23          # ~ T**2(3, 28+28-2=54) if H0 true.

#0.9130136
#
n1=28; n2=28; p=3
ff23=(n1+n2-p-1)*tstat23/((n1+n2-2)*p)
ff23

# 0.2930661           # ~ F(3,52) if H0 true.   n1+n2-p-1=28+28-3-1=52
qf(0.95,3,52)        # 2.7826

#  Accept equality at 5% level.


# ---------Night vs evening
ydiff=m1-m3   
spool= 0.5*(var(y1)+var(y3))  # n1=n2=28
tstat13=(1/28 + 1/28)^(-1)*t(ydiff)%*%solve(spool)%*%ydiff
tstat13
  
# 21.90254           # ~ T**2(3, 54) if H0 true.
n1=28; n2=28; p=3
ff13=(n1+n2-p-1)*tstat13/((n1+n2-2)*p)
ff13

# 7.030445           # ~ F(3,52) if H0 true.  n1+n2-p-1=52
qf(0.95,3,52)   # 2.7826

#   reject equality.

#--
# Form pooled estimate of V matrices.
v1=(27/28)*var(y1)
v2=(27/28)*var(y2)

#---------6--------------------
# Test the equivalence of covariance matrices between the three 
#samples. What are your conclusions?

v3=(27/28)*var(y3)
v=(v1+v2+v3)/3         # n1=n2=n3=28.


n=28*3   # k=3 groups, n1=n2=n3=46, p=3 variables.
u=n*log(det(v))-28*log(det(v1))-28*log(det(v2))-28*log(det(v3))
u

# 53.90835   # ~ chisquare(df=0.5*(k-1)*p*(p+1)=12) if H0 true.  
qchisq(0.95,12)  # 5% value for chisquare(12) is 21.02607.

# 21.02607
1-pchisq(53.90835,12)

# 2.837593e-07  # P-value of the test.
# Reject H0 at 5% level.

#------------------------------------
# Box's M statistic
s1=var(y1);  s2=var(y2);  s3=var(y3)
spool=(s1+s2+s3)/3
n=3*28
c=1-(4*(2*3^2+9-1)/(6*(3*28-3)*4))  # 0.9465021
c

m=c*((n-3)*log(det(spool))-27*log(det(s1))-27*log(det(s2))-27*log(det(s3)))
m

# 49.20207    # ~  chisquare(df=0.5*2*3*4=12) if H0 true.
qchisq(0.95,12)

# 21.02607
1-pchisq(49.20207,12)

#  1.928168e-06   # P-value of the test.
# Reject H0 at 5% level.
# Covariance matrices NOT equal.

#-----------7--------------------

#Overall, what can you infer from the dataset?

-Reject idea that variance matrices are equal.
-Morning vs evening means accepted as equal.
-Morning vs night and evening vs night unequal means.
-If covariance matrices not equal, arises the question about 
assuming equality of variances in test statistic for the means!
-A test more like Welch univariate test might be more in order,
Where we do not have to assume equal variances