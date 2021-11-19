library(Matrix)
options(scipen=999)

per = read.table("Practical10 perception-data.txt",
                 header=TRUE, row.names=1) # Make column 1 the row name.
plot(per)
cor.per=cor(per)
cor.per

cov.per=cov(per)
cov.per

#(a) What pattern do you see in the data?
#
# Two people have some 1 scores.
# Kind/Likeable has two 1, Intelligent zero 1, Happy/Just one 1,
# Small data set
# Likeable = 1 or 9.
# Kind = 1 or 8/9 Kind is almost same as Likeable!
#(b) Consider the correlation matrix. Which variables tend to group?
#####
# Kind, happy, likeable have high correlations.
# Intelligent/just have high correlation.
#(c) What is the rank of the correlation matrix?
#####

det(cor.per)#-4.044539e-18
rankMatrix(cor.per)

ecor = eigen(cor.per)
ecov = eigen(cov.per)

ecov$values #39.16 8.7 0.66 0.3 0.0001
ecor$values #3.26 1.53 0.16 0.03 0.0001
# Rank=4. Correlation matrix not of full rank.

cumsum(ecov$values)/sum(ecov$values) #0.8 0.98 0.99 1 1
cumsum(ecor$values)/sum(ecor$values) #0.6 0.96 0.99 1 1

#-------------------------3------------------------------------
#Two components account for 96%-98% of variation.
#3.Suppose you consider two components and use the correlation matrix only. 
# The factor loadings are the eigenvectors times the square root of the eigenvalues.
ecor$values[1]
ecor$vector[,1]

loadings1 = sqrt(ecor$value[1])*ecor$vector[,1]
loadings2 = sqrt(ecor$value[2])*ecor$vector[,2]
#What can you say about the loadings? Plot the above loadings. 
#Are those points close to the axes? Interpret the pattern that you see.
loadings1
#0.9694553 0.5194021 0.7845174 0.9708704 0.7039644
loadings2
#-0.2311480  0.8069453 -0.5872412 -0.2099491  0.6669269

# Loadings1 gives a weighted average of the variables.
# Loadings2 is contrast of Intelligent/Just (variables 2/5) with the rest.
plot(loadings1,loadings2)
index=c("1","2","3","4","5")
plot(loadings1,loadings2,pch=index)
# Variables 1/4 close together. (Kind/Likeable)

#-------------------------4------------------------------------
# 4 Rotation matrix
#rotate the loadings to make it more interpretable. What can you interpret?

# rotate the loadings to make it more interpretable. What can you interpret?
# 35 degree rotation
rot= 35*pi/180
rott = function(x) return(matrix(c(cos(x), sin(x), -sin(x), cos(x)),2))
rot.matrix = rott(rot)
temp = c(1,0)%*%rot.matrix
temp2 = c(0,1)%*%rot.matrix
plot(loadings1, loadings2, pch=19,
     xlim=c(-1.1, 1.1), ylim=c(-1.1, 1.1))
abline(v=0, h=0, lty=3, lwd=2)
text(loadings1[-1], loadings2[-1]+0.08, colnames(per)[-1]) # Not variable 1.
text(loadings1[1], loadings2[1]-0.08, colnames(per)[1]) # Variable 1.
abline(0, temp[2]/temp[1], lty=2, col=2, lwd=2) # Red dashed lines.
abline(0, temp2[2]/temp2[1], lty=2, col=2, lwd=2)
# 40 degree rotation
rot= 40*pi/180
rot.matrix = rott(rot)
temp = c(1,0)%*%rot.matrix
temp2 = c(0,1)%*%rot.matrix
abline(0, temp[2]/temp[1], lty=2, col=4, lwd=2) # Blue dashed lines.
abline(0, temp2[2]/temp2[1], lty=2, col=4, lwd=2)
# Likeable/Happy/Kind along one axis. Intelligent/Just along another axis.
# Two factors at right angles - independent.

#-------------------------5------------------------------------
#Consider the gapminder data that you are working on in the coursework. Run the
#factor analysis on all of the variables. Can you identify some reasonable factors?

dd=read.csv("coursework2018.csv",row.names=1)
cor.dd = cor(dd[,1:14])
ecor = eigen(cor.dd)
ecor

cumsum(ecor$values)/sum(ecor$values)
#0.44 0.57 0.66 0.73 0.79 0.849 0.89 0.92 0.95 0.973 0.982 0.9906 0.996 1

loadings1 = sqrt(ecor$value[1])*ecor$vector[,1]
loadings1
loadings2 = sqrt(ecor$value[2])*ecor$vector[,2]
loadings2
loadings3 = sqrt(ecor$value[3])*ecor$vector[,3]
loadings3
loadings4 = sqrt(ecor$value[4])*ecor$vector[,4]
loadings4
loadings5 = sqrt(ecor$value[5])*ecor$vector[,5]

plot(loadings1,loadings2,pch=LETTERS[1:14])
# C,H similar = 1c, 2d
# A, B, F similar = 1a, 1b, 2b = children based variable

plot(loadings1,loadings3,pch=LETTERS[1:14])
# A,B,F similar = 1a, 1b, 2b
# I,J similar = 2e, 2f = exports and foreign investment

plot(loadings2,loadings3,pch=LETTERS[1:14])
# C,H similar = 1c, 2d
# I,J similar = 2e, 2f

plot(loadings3,loadings4,pch=LETTERS[1:14])

# R command # Rotates factors by default!
factanal(dd[,1:14],factors=5)

#Call:
factanal(x = dd[, 1:14], factors = 5)

# Health factors
factanal(dd[,1:4],factors=1)

# Economic factors
factanal(dd[,5:11],factors=3)

# Environmental factors
factanal(dd[,12:14],factors=1)
