dd=read.csv("coursework2018.csv",row.names=1)

# ECA vs NOA

d1=dd[dd[,15]=="ECA",1:13]  # All rows of dd where column 15 is ECA
d2=dd[dd[,15]=="NOA",1:13]  # Only columns 1:13 selected

m1=apply(d1,2,mean)
m1
m2=apply(d2,2,mean)
m2
v1=var(d1) 
v1
v2=var(d2)
v2
n1=length(d1)
n1
n2=length(d2)
n2
options("scipen"=100, "digits"=4)
vpool=((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2)
avector=solve(vpool)%*%(m1-m2)
# Largest values are Healtha, Econc
astar1=diag(vpool)^(-0.5)*avector # Not %*% as diag(..) is a vector.
astar1
#-----3-----------------------------------------
# Taking out UK (country 103)
ddnew=dd[-103,]
d1=ddnew[ddnew[,15]=="ECA",1:13]  # All rows of dd where column 15 is ECA
d2=ddnew[ddnew[,15]=="NOA",1:13]  # Only columns 1:13 selected
m1=apply(d1,2,mean)
m2=apply(d2,2,mean)
v1=var(d1)
v2=var(d2)
n1=nrow(d1)
n2=nrow(d2)
vpool=((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2)
avector=solve(vpool)%*%(m1-m2)
# Largest values are Healtha, Econc
astar2=diag(vpool)^(-0.5)*avector # Not %*% as diag(..) is a vector.
astar2

# Largest values are Healtha 0.266  Healthb -1.1724 Econc -1.811 Econd 0.221 
#
# Classify UK
sum(astar2*dd[103,1:13])   # -5.8936

d1m=as.matrix(d1)
d2m=as.matrix(d2)
z1=d1m%*%astar2
z1
z2=d2m%*%astar2
z2
hist(z1,20)  # And z2= -8.686 (Canada), -5.6888 (USA)

hist(z2,20)

# Hard to discriminate - the countries are similar...

#-------------------Repeat for ECA and NEMA countries
d1=dd[dd[,15]=="ECA",1:13]  # All rows of dd where column 15 is ECA
d2=dd[dd[,15]=="MENA",1:13]  # Only columns 1:13 selected
m1=apply(d1,2,mean)
m2=apply(d2,2,mean)
v1=var(d1)
v2=var(d2)
n1=length(d1)
n2=length(d2)
vpool=((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2)
avector=solve(vpool)%*%(m1-m2)
# Largest values are Healtha, Econc
astar3=diag(vpool)^(-0.5)*avector # Not %*% as diag(..) is a vector.
# Largest values are Healthb -0.134  Econc -0.243 Econd -0.124 
d1m=as.matrix(d1)
d2m=as.matrix(d2)
z1=d1m%*%astar3  # Only outlier Turkmenistan (-45) Range rest -28 to -13.
z2=d2m%*%astar3  # Range -40 to -15
hist(z1)
hist(z2)
# Discrimination difficult as geographic countries too diverse?

#-------------------Repeat for HIGH vs LOW
d1=dd[dd[,16]=="HIGH",1:13]  # All rows of dd where column 15 is ECA
d2=dd[dd[,16]=="LOW",1:13]  # Only columns 1:13 selected
m1=apply(d1,2,mean)
m2=apply(d2,2,mean)
v1=var(d1)
v2=var(d2)
n1=length(d1)
n2=length(d2)
vpool=((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2)
avector=solve(vpool)%*%(m1-m2)
# Largest values are Healtha, Econc
astar=diag(vpool)^(-0.5)*avector # Not %*% as diag(..) is a vector.
# Largest values are Healthb -1.016  Econc 2.706 Econd 0.649 
d1m=as.matrix(d1)
d2m=as.matrix(d2)
z1=d1m%*%astar  # Range 5.9 to 22.7
z1
z2=d2m%*%astar  # Range -8.5 to -1.3
z2
hist(z1)
hist(z2)

#HIGH vs REST
d2=dd[dd[,16]!="HIGH",1:13]  # Only columns 1:13 selected


#------------PCA-----------------------
dd=read.csv("coursework2018.csv",row.names=1)

d1=dd[,1:14]  # All rows of dd 
d1=as.matrix(d1)
mean.d1=apply(d1, 2, mean)   # Take mean of columns
sd.d1=apply(d1, 2, sd) 

v1=var(d1)  

eigval  <- eigen(v1)$values
eigval

eigvec <- eigen(v1)$vectors
eigvec

100*eigval/sum(eigval)

# First eigenvector accounts for 94.5% of the variation!  Essentially a 
#mean...
# 5.3% is second eigenvector
cumsum(100*eigval/sum(eigval))

pp=princomp(d1)
pp
plot(eigval)           # 1st eigenvalue very large!

plot(princomp(d1))     # 1st eigenvalue very large!

pp$loadings

#Loadings above only picks out the large values below.
eigvec

summary(princomp(d1))

#Importance of components:

pp$scores

plot(pp$scores[,1],pp$scores[,2])  # One large -ve outlier on y-axis.
pp$scores[,2]    # Iceland! 

(d1%*%eigvec[,1])[1:10]

pp$scores[1:10,1]

# Slightly different coz princomp command cuts small values in eigen vectors.
# Using eigenvectors use all tiny insignificant values.
pp=princomp(d1,cor=TRUE)
summary(pp)

# Now need seven components

plot(pp) # Plots eigenvalues...

plot(pp$scores)

plot(pp$scores[,1],pp$scores[,2])  # Nice U shape.
pp$loadings

p1=pp$scores[,1]
p2=pp$scores[,2]
plot(p1,p2)  # Plot of scores for first two principal components
points(p1[dd[,16]=="UPM"],p2[dd[,16]=="UPM"],pch=20,col="red")
points(p1[dd[,16]=="HIGH"],p2[dd[,16]=="HIGH"],pch=20,col="blue")
points(p1[dd[,16]=="LOW"],p2[dd[,16]=="LOW"],pch=20,col="green")
points(p1[dd[,16]=="LOM"],p2[dd[,16]=="LOM"],pch=20,col="yellow")

#---------Trying a NODDY example..-------------
dd=matrix(c(2,3,2,4,5,8,9,8,6,7),ncol=2)
dd
mean.d=apply(dd, 2, mean)
mean(dd[,1])   #   [1] 3.2
mean(dd[,2])   #   [1] 7.6
mean.d         #   [1] 3.2 7.6
d2=dd-mean.d     # Standardising rows of dd.
d2
# THIS treats dd as a column downwards!
# Thus subtracting 2-3.2, 3-7.6, 2-3.2, 4-7.6, 5-3.2, .... to give d2!
# NOT centering d2.....
apply(d2,2,mean)    #  [1] -1.76  1.76
var(d2)
x1=dd[,1]-mean.d[1]
x2=dd[,2]-mean.d[2]
x1
x2
var(x1)     #  1.7
var(x2)     #  1.3
cov(x1,x2)  # -0.9   #  See var(dd)!
as.matrix(mean.d)   # Notice as a matrix i2
#What we want is...
matrix(mean.d,ncol=2,byrow=TRUE,nrow=5)
d2=dd-matrix(mean.d,ncol=2,byrow=TRUE,nrow=5)
var(d2)   # Same as var(dd)!
#----End of NODDY Example

#----------Canonical correlations-----------------------------

dd=read.csv("coursework2018.csv",row.names=1)
d1=dd[,1:11]  # All rows of dd but only health and economic variables
d1=as.matrix(d1)
d1=dd[,1:11]  # All rows of dd but only health and economic variables
cor(d1)

ss=(var(d1))
syy=ss[1:4,1:4]
sxx=ss[5:11,5:11]
sxy=ss[5:11,1:4]
syx=ss[1:4,5:11]
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy

eigen(smmy)

smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx
eigen(smmx)

# Largest correlation is sqrt(0.8399)=0.916489
sqrt(diag(ss))

# Largest y eigenvector is
a1=eigen(smmy)$vectors[,1]   #[1] -0.0016822317  0.9999970947 -0.0017253072 -0.0000638643
a1
# Largest x eigenvector is
b1=eigen(smmx)$vectors[,1]
b1

# Standardise
dy=diag(sqrt(diag(ss))[1:4])
dx=diag(sqrt(diag(ss))[5:11])
dy%*%a1
dx%*%b1
#dy%*%eigen(smmy)$vectors[,1]
#dx%*%eigen(smmx)$vectors[,1] 

# Now in terms of correlations
ss=as.matrix(cor(d1))   
syy=ss[1:4,1:4]
sxx=ss[5:11,5:11]
sxy=ss[5:11,1:4]
syx=ss[1:4,5:11]
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy
smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx

dy=diag(sqrt(diag(ss))[1:4])  # Now identities so not needed!
dx=diag(sqrt(diag(ss))[5:11])


dy%*%eigen(smmy)$vectors[,1]

# Most weight is Healthb

dx%*%eigen(smmx)$vectors[,1]

# Most weight is Econb, Econc, Econa, Econe..