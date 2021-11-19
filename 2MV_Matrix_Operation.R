
#Diagonal matrix
#1
a = 1:5
diag(a) # Gives a square matrix with diagonal entries a.
diag(11:20)
diag(1,5) # Command format is diag(x,nrow,ncol). Here nrow=5.
diag(1,5,2)
diag(1:2,4,5) # No longer a square matrix.

#2
b = seq(2,12,2) # A sequence from 2 to 12 by steps of size 2.
b
b.diag = diag(b) # diag here makes a diagonal matrix.
b.diag
d = diag(b.diag) # diag here extracts the diagonal of a square matrix.
d
e = matrix(1:25,5) # Matrix of 1:25 with nrow=5, byrow=FALSE.
e
diag(e)

#3
set.seed(10)
a = matrix(sample(1:100,25),5)
a
diag(a) = 1:5
a
diag(a)[3] = 0
a

#Other matrices
#1
a = matrix(1:36,6)
a
lower.tri(a, diag = FALSE) # TRUEs NOT lower triangular as in notes.
lower.tri(a, diag = TRUE) # TRUEs now lower triangular as in notes.
upper.tri(a, diag = TRUE)
# Assign cases where lower.tri(...) gives TRUE as 0.
a[lower.tri(a, diag = FALSE)] = 0 # Gives upper triangular matrix!
a
a1 = matrix(1:36,6)
a1=upper.tri(a1)
a1

#2
j.vec = rep(1,5) # Replicate 1 five times.
j.vec
j.mat = matrix(1,5,5) # Matrix of 1 with nrow=5 and ncol=5.
j.mat
t(j.vec) # Transpose of j.vec.
j.vec %*% t(j.vec) # Vector multiplication. #shilpa wrote is this broadcasting??
t(j.vec) %*% j.vec # And more vector multiplication.

#Matrix multiplication
#1
a = matrix(1:12,4)
a
dim(a)

#2
A = matrix(1:10,5)
A
B = matrix(1:6,2)
B
D = A %*% B # Matrix multiplication.
D # Note the dimension of D.

#3
A = matrix(1:10,5)
B = matrix(1:4,2)
D = matrix(11:14,2)
E = matrix(1:6,2)
A %*% (B+D)
A%*%B + A%*%D
B%*%D # With matrix multiplication...
D%*%B # ...BD is not necessarily the same as DB.
A%*%B%*%E + A%*%D%*%E
A %*% (B+D) %*% E

#Cross prod
a = 1:5
a
sum(a^2) # What is different about these two answers?
t(a) %*% a # There is a difference in how they are regarded by R!
#
# Compare the following:
is.matrix(sum(a^2)) # FALSE
is.matrix(t(a) %*% a) # TRUE
set.seed(10)
j.vec = rep(1,5)
j.vec
A = matrix(sample(1:10,25, replace=TRUE),5)
A
# What is the difference between the following two answers?
t(j.vec) %*% A
colSums(A) # Column sums of A

#Exercises
xx=read.table("practical2-data.csv",header=TRUE)
xx