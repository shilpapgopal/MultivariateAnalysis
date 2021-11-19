# 1 Determinant of a matrix
set.seed(10)
A = matrix(sample(1:10,25,replace=TRUE), 5) # nrow=5
A
det(A)
B = matrix(1:25, 5)
B
det(B) # Why

# 2 Inverse of a matrix
set.seed(10)
A = matrix(sample(1:10,25,replace=TRUE), 5)
A
solve(A)
A %*% solve(A) # Gives the identity...
solve(A) %*% A # ...as does this.
round(A %*% solve(A),5) # Round A%*%solve(A) to 5 decimal places.
B = matrix(1:25, 5)
solve(B) # Why?

# 3 Matrix trace
set.seed(10)
A = matrix(sample(1:10,25,replace=TRUE), 5)
A
sum(diag(A))
C = matrix(1:15, 3)
C
sum(diag(C)) # No warning by R!

# 4 Rotation matrix
# Rotation matrix R.
R = matrix(c(cos(pi/4),sin(pi/4),-sin(pi/4),cos(pi/4)),2)
x <- c(3,1) # Or use x=c(3,1). This is the pre-rotated vector.
xtilde <- R%*%x # The rotated vector.
plot(x, type="n", xlim=c(-2,5), ylim=c(-2,5), xlab="", ylab="", las=1)
# The las=1 sub-command makes axis scales all print horizontally.
abline(h=c(-2:5), v=c(-2:5), col="grey70") # Add a background grid.
abline(h=0, v=0, lwd=2) # Add x-y axes. lwd controls line width.
lines(c(0,x[1]),c(0,x[2]), lty=2, lwd=2)
# lines(c(x1,x2),c(y1,y2)) draws a line between (x1,y1) and (x2,y2).
points(x[1],x[2], pch=19) # Add bullet point to original vector.
lines(c(0,xtilde[1]), c(0,xtilde[2]), lty=2, lwd=2, col="lightblue4")
points(xtilde[1], xtilde[2], pch=19, col=4) # col=4 is blue.
abline(0,1,lty=3, lwd=3.5, col=2) # Rotated x axis. col=2 is red.
abline(0,-1,lty=3, lwd=3.5, col=2) # Line with intercept 0, slope -1.

# 5 Eigenvalues and eigenvectors

set.seed(10)
A=matrix(sample(1:10,40,replace=TRUE),8)
B = t(A) %*% A
B
result = eigen(B)
result
result$values # Gives eigen values.
result$vectors # Gives eigen vectors as a matrix.
result # Both eigen values and eigen vectors.
sum(diag(B)) # trace(B).
sum(result$values) # Sum of eigen values.
det(B) # Determinant det(B).
prod(result$values) # Product of eigen values.
eigen(A) # Why?
C = matrix(c(2,8,5,1,7,4,3,9,6),3)
eigen(C) # Note the values.
det(C) # Why?
solve(C)
#This matrix C has columns xi satisfying x2 + x3 ??? 2x1 = 0. What does this tell you?
C = matrix(c(6,8,2,3,5,4,9,7,1),3) # Now repeat with this matrix.
C
eigen(C)
det(C)
solve(C)

#Exercise

xx=read.csv("Practical3-data.csv")
xx=as.matrix(xx)
xx
yy=matrix(c(0.0254,0,0,0.45359237),nrow=2,byrow=TRUE) # 1lb=0.4536 kg, 1 inch=0.0254 m.
yy

xx%*%yy

# 2. Consider the vector a = t(1, 2, 3, 4) and b = t(4, ???2, ???4, 3)
#Are both vectors orthogonal? Create orthonormal vectors from both vectors. 
#Show that they are orthonormal after the operation.

a=c(1,2,3,4)
a
b=c(4,-2,-4,3)
b
a%*%b # Answer 0, so they are orthogonal.
anorm=a/sqrt(sum(a^2))
anorm

bnorm=b/sqrt(sum(b^2))
bnorm 

anorm%*%bnorm

sum(anorm*bnorm) # Gives 0

sum(anorm^2) # Gives 1
sum(bnorm^2)

anorm=a/sqrt(a%*%a) 
anorm=a/sqrt(t(a)%*%a) 
anorm # Gives 0.1825742 0.3651484 0.5477226 0.7302967
a%*%a
t(a)%*%a
# 3 
# (a) What is the determinant of A?
# (b) What are the eigen values of A?
# (c) What is the rank of A?
a=matrix(c(5,6,7,6,4,5,6,6,7,5,3,6,9,2,0,8,7,1,9,14,8,4,3,4,9),nrow=5,byrow=TRUE)
det(a)
eigen(a)
eigen(a)$value 
# Rank(a)=4 Matrix a has only 4 non-zero eigenvalues
 
# 4
#Remove the last column and the last row of A above, and put this into an object
#called A1. Answer the following questions:
#  (a) What is the determinant of A1?
#  (b) What are the eigen values of A1?
#  (c) Is A1 of full rank?

a1=a[1:4,1:4]
a1
det(a1)
eigen(a1)$value 
#Yes A1 is of full rank. Matrix has all eigenvalues non-zero.

# Some useful R commands.
xx=read.csv('Practical3-data.csv')
# xx is a not a matrix but is treated by R as like a matrix in many cases.
is.data.frame(xx)
is.matrix(xx)
# For some matrix manipulations xx is better as a matrix.
xxnew=as.matrix(xx)
is.data.frame(xxnew)
is.matrix(xxnew)