dd= read.table("lecture04-weightdata.txt", header=TRUE)
dd # Lists the data

#-------Plots of bivariate data------------------------------
par(mfrow=c(2,1), mar=c(4,4,0.2,1)) # mar adjusts margins around plots.
# Default margin is: bottom/left/top/right=c(5,4,4,2)+0.1
# We have removed some of the white space around the plot.
plot(dd$x, ylab="Height (in)", xlab="")
plot(dd$y, ylab="Weight (lb)", xlab="Person")

# Scatterplot.
par(mfrow=c(1,1)) # mfrow now gives one plot in the R plot window.
plot(dd$x,dd$y, xlab="Height (in)", ylab="Weight (lbs)", pch=19,col="red")
abline(h=mean(dd$y), v=mean(dd$x), lty=3, lwd=2.5, col="blue")

#-------Mean vectors------------------------------
n = nrow(dd) # Number of rows of dd.
mean.dd = t(rep(1,n)) %*% dd / n # rep(1,n) gives n ones.
# Doesn't work!
# Make dd a matrix rather than a data-frame.
ddm = as.matrix(dd) # Store as ddm.
mean.ddm = t(rep(1,n)) %*% ddm / n # This works!
mean.ddm # Print means on screen.
apply(dd,2,mean) # This also calculate column means.
# "apply" the function "mean" to columns (=2) of dd. (Rows = 1.)
apply(dd,1,mean) # Row means. Not what we want!

#------Covariance and correlation--------------------
cov.dd = cov(dd) # Gives same as var(dd) command.
cov.dd
var(dd)
diag(cov.dd) # Diagonal elements.
apply(dd,2,var) # This also calculates variance.

#cor using mat operation
cor.dd = cor(dd)
cor.dd

#------------Exercise-----------------------

dd= read.table("lecture04-weightdata.txt", header=TRUE)
dd

is.data.frame(dd)
is.matrix(dd)
ddm=as.matrix(dd) # Make ddm a matrix.
is.data.frame(ddm)

is.matrix(ddm) 
uu=matrix(c(0.0254,0,0,0.453592),nrow=2, byrow=TRUE) # Scaling matrix.
uu
uu=diag(c(0.0254,0.453592)) # Does same as above.
uu

#convert to a different scale of x and y
ddnew = ddm %*% uu
ddnew

# ddnew gives re-scaled values in metres and kg.
# We'd perhaps like to write something like 
#ddnew = A %*% ddm but A is 2x2 matrix
# and dd is nxp (=20x2) data matrix.
uu %*% t(ddm) # This works in R but result is 2x20 matrix.

t(uu %*% t(ddm)) # Gives what we want but looks messy to type!

cov(dd)
cov(ddnew) # Not same as cov(dd) or cov(ddm).

cor(dd)
cor(ddnew) # Same as cor(dd) or cor(ddm).

#Covar varies with scale/unit of the data, corel remains same

# Given cov matrix, find corel matrix: 
#R = Inv(D)*S*Inv(D) where D = diag mat of covar
D=1/diag(sqrt(cov(ddnew))) # D contains inverse of st.dev.
D
D=diag(D) # Makes D a diagonal matrix (contains inverse of st.dev.).
D

D %*% cov(ddnew) %*% D # Gives correlation matrix.


#------Exc 2--------------------------------------------

dd = read.table("practical5-data.txt", header=TRUE)
dd

par(mfrow=c(3,3), mar=c(4,4,0.2,1)) # mar adjusts margins around plots.
plot(dd$M1, dd$M2); plot(dd$M1, dd$M3); plot(dd$M1, dd$M4)
plot(dd$M2, dd$M1); plot(dd$M2, dd$M3); plot(dd$M2, dd$M4)
plot(dd$M3, dd$M1); plot(dd$M3, dd$M2); plot(dd$M3, dd$M4)

par(mfrow=c(1,1))
pairs(dd) # M1 marks seem to be higher than others. M2 lower.

# Correlations between variables all appear to be positive.
ddm = as.matrix(dd) # Make ddm a matrix.
n = nrow(ddm)
ddm

mean.ddm = t(rep(1,n)) %*% ddm/n # t(rep(1,n) %*% dd does NOT work!
mean.ddm

apply(ddm,2,mean) # Does the same as the above. As does apply(dd,2,mean).

#-------1------------------------
# 1.  Calculate the mean vector and covariance matrix of the 
#raw module marks.What can you infer from the mean vector and 
#covariance matrix? What are the max and min means in the mean vector?

#covar matrix
cov(ddm)# By inspection, var of M1&M3 higher than for M2 and M4.
diag(cov(ddm)) # Gives the variances of M1, M2, M3, M4.

uu=diag(c(0.9,1.1,1,1))
uu # I'd check uu is what I want it to be.

dim(ddm)
dim(uu)
ddnew = ddm %*% uu
ddnew

#mean vector
apply(ddnew,2,mean)

#-------2------------------------
#Calculate the adjusted sum of the marks for each student.
#Note: The result of the operation should be a vector of length 20 
#(equal to the number of students).
vv=rep(1,4)
vv
marks = ddnew %*% vv # Sum of re-weighted marks for each student.
marks

#--------------3-------------
# Calculate the mean and variance of individual sums (the mean and variance of
#the vector of sums in part (b)).

apply(ddnew,1,sum) # Also gives sum of re-weighted marks for each student
apply(ddnew,1,mean)

apply(ddnew,1,var) # Variance of marks for each student
apply(ddnew,1,sd) 

mean(marks) # Mean of vector in (b). 

var(apply(ddnew,1,sum)) # Gives variance as a single number!var(marks) # Becomes a variance-covariance matrix.

#----4------------------

#Calculate the same mean and variance, this time using matrix/vector operation
#of linear combination of the mean vector and covariance matrix in part (a).

# Each student mark = 0.9 M1 + 1.1 M2 + M3 + M4
# Mean is 0.9 mean(M1) + 1.1 mean(M2) + mean (M3) + mean(M4)
ddmean=apply(ddm,2,mean) # Mean of ddm raw marks.
ddmean
uu## Transformation matrix as setup above.
ddmean %*% uu # As a 1x4 matrix.

ddmean=apply(ddm,2,mean) # Mean of ddm raw marks.
ddmean
apply(ddnew,2,mean) # As a vector.

sum(ddmean %*%uu) # Same as mean(marks).

ddvar=cov(ddm) 
ddvar

vv = uu %*% ddvar %*% uu # Variance-covariance matrix of transformed marks.
vv

# Sum of transformed marks is
# mark = v1 + v2 + v3 + v4
# Var(mark) = var(v1)+...+var(v4)+cov(v1,v2)+..+cov(v2,v1)+...+cov(v3,v4)
sum(vv) # Same as var(marks).
