#1
a = 3
a
b = 4
b
a = b = 7 # I'd never write this myself
a
b

#2
w = 10
a = 7
b = 3
w/a
w/a/b # I'd write this as (w/a)/b myself!
w-a*2
a**2
a^2

abs(w-(a+b)*2) # absolute value
w %/% b # integer part
w %% b # remainder

#3

a = c(1,2,3,4,5) # c is a built-in function in R
a
b = 1:5
b
g = seq(0,10,by=2)
g
summary(g) # Summary statistics of g.
h = seq(0,100, length=21)
h
length(h) # Number of elements of h.
# Compare the above use of the seq command with below:
h = seq(0,100,by=8)
h

d = scan()

a <- c(1,2,3,4,5) # Some people use <- as an assignment statement.
g = seq(0,10,by=2) # = works as well.
a + 1
b*2
d^2
a^2/2

#4
a = c(1,2,3,2,2,3,3,2,1)
a
a[a==3]=0 # Assign all cases a=3 as zero.
a
a[a<2] # Show cases with a<2.
length(a[a==1]) # Number of cases with a=1.
length(a[a<2])
a+g 

a = c(1,2,3,2,2,3,3,2,1)
a[a<2]=a[a<2]+10
a
a = c(1,2,3,2,2,3,3,2,1)
a=a[a<2]+10
a

mean(a)
sum(a)
max(a)
min(a)

#basic arithmetic in R: matrices

#1
a = 51:65
b = matrix(a,5)
br = matrix(a,5,byrow=TRUE)
b
br # What is the difference between b and br?
br = matrix(a,5,byrow=T)
br

#2
uk.fit = read.csv("uk-fit.csv", header=TRUE)

uk.fit # List the file in R.
uk.fit[1,] # First row of uk.fit
uk.fit[,2] # List of second column of uk.fit
uk.fit.mat = as.matrix(uk.fit) # Turns the data frame into a matrix.
uk.fit.mat
uk.fit.mat[1,] # First row of the matrix.
# The columns are here labelled like uk.fit.
uk.fit.mat[,2] # Second column of the matrix.

#3
a = 51:65
b = matrix(a,5) # R treats the 5 as the number of rows.
b = matrix(a,nrow=5) # Entries read with byrow=FALSE as the default.
c = matrix(a,nrow=5,byrow=TRUE)
# Compare a, b, and c.
a
b
c
b[3,2] # Element in 3rd row and 2nd column.
b[1:3,1:2] # First three row, first two columns.
b[1:3,] # Mind the position of comma! First three rows.
b[,1:2] # Mind the position of comma! First two columns.
b
b[,b[3,]>57] # Columns of b where row 3 has elements more than 57.
b[b[,1]>53,] # Rows of b where column 1 has elements more than 53.
b[b[,1]>53] # What happened here?

#4 'vectorise' a matrix as follows.
a = 51:65
b = matrix(a,5)
c(b)
as.vector(b)

#5
h = 1:12
h
mh = matrix(h, 5) # Why the warning message?
mh

#6
a = matrix(11:20,5)
# basic arithmetic
a
a+5
a-10

a*2
a/2
a^2 # NOT the same as matrix multiplication!
# Simply the square of the elements of matrix a.

#7
a = matrix(11:20,5)
a # Compare a here with it's transpose.
t(a) 

#8
a = 1:5
a # By definition, this is a vector in R.
b = matrix(11:40,10)
b
b[1,] # a vector in R
b[,2] # a vector in R
matrix(b[1,], nrow=1) # A matrix with one row.
matrix(b[,2], nrow=1) # Not a column vector!
# By default, when you convert a vector in R into a matrix, it is
# translated into a matrix with one column.
matrix(a)
matrix(b[1,])
matrix(b[,2])
matrix(b) # A matrix with one column made up of elements of matrix b!

#9
a = 1:5
a
t(a) # A matrix with one row.
t(t(a)) # A matrix with one column!
# Compare:
is.matrix(a) # FALSE, a is a vector.
is.matrix(t(a)) # TRUE, t(a) is a matrix.
as.matrix(a) # Turn a into a matrix with 5 rows and one column.
as.matrix(t(a)) # Turns t(a) into a matrix with one row and 5 columns.

b = matrix(11:40,10)
b[1,] # a vector in R
b[,2] # a vector in R
t(b[1,])
t(b[,2])
t(matrix(b[1,], nrow=1))
t(matrix(b[,2], nrow=1))
t(matrix(a))
t(matrix(b[1,]))
t(matrix(b[,2]))

#Exe 1

# This ensures you all get the same values that I use!
set.seed(10) 
# sample(1:40) gives a random permutation of numbers 1 to 40.
# Put into matrix "a" with 10 rows and uses byrow=FALSE as default.
a = matrix(sample(1:40),10)
a
#(a) show the first column of a?
a[,1]
#(b) show the third row of a?
a[3,]
#(c) show the rows of a that correspond to the element in the first column greater
#than 10?
a[a[,1]>10,]
#(d) show the columns of a that correspond to the element in the third row less than
#25?
a[,a[3,]<25]
#(e) show the elements of a that are less than 6?
a[a<6]
#(f) replace the elements of a that are less than 6 with 10?
a[a<6]=10
#(g) add 7 to the elements of a that are now less than 10.
a[a<10]=a[a<10]+7
a

set.seed(10) # So that we all get the same values for a and b.
a = matrix(sample(1:40),10)
b = a
#What R commands can you use to show the equality of matrices a and b?
b==a
a-b
sum(a-b)
sum((a-b)^2)
