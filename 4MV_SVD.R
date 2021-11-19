#Eigenvalues and eigenvectors
#The (non-zero) eigen values of AB and AB are the same (assuming conformability).
A= matrix(c(2,1,1,2,5,1,1,2,3),3) # nrow=3, byrow=FALSE is the default.
B= matrix(c(2,1,1,2,5,1,1,2,6),3)
A
B
A %*% B 
B %*% A
eigen(A)
eigen(A%*%B)$values
eigen(B%*%A)$values

E=matrix(c(2,1,1,2,5,1),3)
F=matrix(c(2,1,2,5,1,2),2)
E
F
E%*%F
F%*%E
eigen(E%*%F)$values
eigen(F%*%E)$values # Non-zero eigen values are the same.
round(eigen(E%*%F)$values,5)

#----------------------------------------------------------------
#Diagonalisation

#For a symmetric matrix A, the eigenvectors are both column and row orthogonal.
A=matrix(c(2,1,1,1,4,1,1,1,2),3)
A
C=eigen(A)$vectors # Gives eigen-vectors of A.
C
D=diag(eigen(A)$values) # Diagonal matrix of eigen-values.
D
eigen(A)$values

t(C)%*%C # Is matrix C an orthogonal matrix?
round(t(C)%*%C,3) # Easier to see what is going on!
round(C%*%t(C)) # Try round(C%*%t(C),3)
determinant(C) #if det=+or- 1, then orthogonal
C%*%D%*%t(C) # Matrix A.
A
t(C)%*%A%*%C # Matrix D.
round(t(C)%*%A%*%C,3) # Rounds to 3 decimal places.
D

#----------------------------------------------------------
#Square root or inverse of symmetric matrix

A = matrix(c(2,1,1,1,4,1,1,1,2),3)
C = eigen(A)$vectors
C
D.sqrt = diag(sqrt(eigen(A)$values))
D.sqrt
A.sqrt = C %*% D.sqrt %*% t(C) # Square root of matrix A.
A.sqrt %*% A.sqrt # Gives A.
A
D.inv = diag(1/eigen(A)$values) # Diagonal matrix of 1/eigen-values.
A.inv = C %*% D.inv %*% t(C) # Inverse matrix of A.
A.inv %*% A 
round(A.inv %*% A,3) # Easier to see what is going on.

#---------------------------------------------------
#Singular value decomposition
set.seed(10)
A = matrix(sample(1:10,40,replace=TRUE),8)
A
Asvd = svd(A)
Asvd # Made up of diagonal values d, matrices u and v.

Asvd$u %*% diag(Asvd$d) %*% t(Asvd$v)
round(Asvd$u %*% t(Asvd$u),5) # Not identity matrix.
round(Asvd$v %*% t(Asvd$v),5) # Identity matrix.
round(t(Asvd$u) %*% Asvd$u,5)
round(t(Asvd$v) %*% Asvd$v,5)
# Combine columns of A and A[,1]+A[,2] as matrix A2.
A2 = cbind(A, A[,1]+A[,2]) # A2 is of reduced rank and not square.
A2
A2svd = svd(A2)
A2svd # Notice the singular values.
A2
A2svd$u %*% diag(A2svd$d) %*% t(A2svd$v)


# Excercise

A=matrix(c(14,7,1,5,7,13,3,6,1,3,12,9,5,6,9,11),nrow=4,byrow=TRUE) 

#1
eigen(A)

#2
#A1 is of full rank. Matrix has all eigenvalues non-zero.

#3 diagonalization - spectral decomposition D=t(C).A.C

C=eigen(A)$vectors
t(C) %*% A %*% C
round(t(C) %*% A %*% C, 5) 

#4 sq root of A
D=diag(eigen(A)$values)
invC=solve(C)

Asqrt=C %*% sqrt(D) %*% t(C)
Asqrt %*% Asqrt - A # Gives zero matrix.
round(Asqrt %*% Asqrt - A,5)

Ainv=C %*% diag(1/eigen(A)$values) %*% t(C) 
Ainv %*% A # Gives identity matrix.
round(Ainv %*% A,5) # ID Matrix


#singular value
Asvd1 = svd(A)
Asvd1
eigen(A)$values
