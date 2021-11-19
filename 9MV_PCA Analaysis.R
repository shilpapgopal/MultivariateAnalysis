dd=read.csv("coursework2018.csv", row.names=1)
#1. Perform the principal components analysis on each group of countries for each group of variables. Are there any patterns that you can see?
d1=dd[dd[,15]=="ECA",1:14]  # All rows (41) of dd where column 15 is ECA
d1 # 41 rows of 14 cols

pp=princomp(d1)
pp$loadings

#---


d1=dd[dd[,15]=="LAC",1:14]  # All rows (19) of dd where column 15 is LAC
pp=princomp(d1)
pp$loadings

#----------
d1=dd[dd[,15]=="EAPAC",1:14]  # Too FEW rows (13) for number of variables
d1=dd[dd[,15]=="MENA",1:14]  # Too FEW rows (12) for number of variables
d1=dd[dd[,15]=="NOA",1:14]  # Too FEW rows (2) for number of variables
d1=dd[dd[,15]=="SA",1:14]  # Too FEW rows (5) for number of variables
d4=dd[dd[,16]=="LOW",1:14]  # Too FEW rows (6) for number of variables
d5=dd[dd[,15]=="NOA",1:4]  # Too FEW rows (2) for number of variables
d6=dd[dd[,15]=="SA",1:4]  # Too FEW rows (5) for number of variables
d5=dd[dd[,15]=="NOA",5:11]  # Too FEW rows (2) for number of variables
d6=dd[dd[,15]=="SA",5:11]  # Too FEW rows (5) for number of variables
d5=dd[dd[,15]=="NOA",12:14]  # Too FEW rows (2) for number of variables
d6=dd[dd[,15]=="SA",12:14]  # Too FEW rows (5) for number of variables
#----------

d1=dd[dd[,15]=="SSA",1:14]  # All rows (17) of dd where column 15 is ECA
pp=princomp(d1)
pp$loadings
#----------

d1=dd[dd[,16]=="HIGH",1:14]  # All rows (41) of dd where column 15 is HIGH
pp1=princomp(d1)
pp1$loadings
# Comp 1 = 0.977 Econa + 0.208 Envb
# Comp 2 = 0.210 Econa - 0.977 Envb
#----------

d2=dd[dd[,16]=="LOM",1:14]  # All rows (32) of dd where column 15 is LOM
pp2=princomp(d2)
pp2$loadings
# Comp 1 = 0.991 Econa + 0.137 Envb
# Comp 2 = 0.137 Econa - 0.990 Envb
d22=as.matrix(d2)
v1=var(d22)  
eigen(v1)

#----------


#----------
d3=dd[dd[,16]=="UPM",1:14]  # All rows (30) of dd where column 15 is UPM
pp3=princomp(d3)
pp3$loadings
summary(pp3)
# Comp 1 = 0.984 Econa + 0.176 Envb
# Comp 2 = 0.176 Econa - 0.984 Envb


#Testing covariance matrix, all 14 variables---------------------------

v1=(40/41)*var(d1[,1:14])
v2=(31/32)*var(d2[,1:14])
v3=(29/30)*var(d3[,1:14])
v=(41*v1+32*v2+30*v3)/(41+32+30)         # n1=41, n2=32, n3=30.
#
k=3
p=14
n=41+32+30   # k=3 groups, n1=42, n2=32, n3=30, p=14 variables.
u=n*log(det(v))-41*log(det(v1))-32*log(det(v2))-30*log(det(v3))
# u=1573.578   # ~ chisquare(df=0.5*(k-1)*p*(p+1)=12) if H0 true.
df=0.5*(k-1)*p*(p+1)    # 210
qchisq(0.95,df)  # 5% value for chisquare(210) is 244.8076.

# 244.8076
1-pchisq(u,df)   # Reject H0! P=0!

#   4.041241e-06   # P-value of the test.
# Reject H0 at 5% level.

# Testing covariance matrix, variables 1-4
#
v1=(40/41)*var(d1[,1:4])
v2=(31/32)*var(d2[,1:4])
v3=(29/30)*var(d3[,1:4])
v=(41*v1+32*v2+30*v3)/(41+32+30)         # n1=41, n2=32, n3=30.
#
k=3;  p=4;  n=41+32+30   # k=3 groups, n1=42, n2=32, n3=30, p=14 variables.
u=n*log(det(v))-41*log(det(v1))-32*log(det(v2))-30*log(det(v3))
# ~ chisquare(df=0.5*(k-1)*p*(p+1)=12) if H0 true.
df=0.5*(k-1)*p*(p+1)    
# 
1-pchisq(u,df)   # Reject H0! P=0!

#  P=0    # Reject H0 at 5% level.




# Testing covariance matrix, variables 5-11
#
v1=(40/41)*var(d1[,5:11])
v2=(31/32)*var(d2[,5:11])
v3=(29/30)*var(d3[,5:11])
v=(41*v1+32*v2+30*v3)/(41+32+30)         # n1=41, n2=32, n3=30.
#
k=3
p=7
n=41+32+30   # k=3 groups, n1=42, n2=32, n3=30, p=14 variables.
u=n*log(det(v))-41*log(det(v1))-32*log(det(v2))-30*log(det(v3))
# ~ chisquare(df=0.5*(k-1)*p*(p+1)=12) if H0 true.
df=0.5*(k-1)*p*(p+1)    
# 
1-pchisq(u,df)   # Reject H0! P=0!

#  P=0    # Reject H0 at 5% level.




# Testing covariance matrix, variables 12-14
#
v1=(40/41)*var(d1[,12:14])
v2=(31/32)*var(d2[,12:14])
v3=(29/30)*var(d3[,12:14])
v=(41*v1+32*v2+30*v3)/(41+32+30)         # n1=41, n2=32, n3=30.
#
k=3
p=3
n=41+32+30   # k=3 groups, n1=42, n2=32, n3=30, p=14 variables.
u=n*log(det(v))-41*log(det(v1))-32*log(det(v2))-30*log(det(v3))
# ~ chisquare(df=0.5*(k-1)*p*(p+1)=12) if H0 true.
df=0.5*(k-1)*p*(p+1)    
# 
1-pchisq(u,df)   # Reject H0! P=0!

#  P=0    # Reject H0 at 5% level.


#---Look at subsets of countries and variables------------------

d1=dd[dd[,15]=="ECA",1:4]  # All rows (41) of dd where column 15 is ECA
pp1=princomp(d1)
pp1$loadings
# Comp 1 = 1.000 Healthd
# Comp 2 = -1.000 Healthc


d2=dd[dd[,15]=="EAPAC",1:4]  # All rows (13) where EAPAC
pp2=princomp(d2)
pp2$loadings
# Comp 1 = 0.982 Econa + 0.188 Envb
# Comp 2 = -0.188 Econa + 0.982 Envb

d3=dd[dd[,15]=="LAC",1:4]  # All rows (19) of dd where column 15 is ECA
pp3=princomp(d3)
pp3$loadings
# Comp 1 = 1.000 Healthd
# Comp 2 = 0.456 Healtha - 0.889 Healthc


d4=dd[dd[,15]=="MENA",1:4]  # All rows (12) where MENA
pp4=princomp(d4)
pp4$loadings
# Comp 1 = 1.000 Healthd
# Comp 2 = -0.442 Healtha - 0.897 Healthc



d7=dd[dd[,15]=="SSA",1:4]  # All rows (17) of dd where column 15 is ECA
pp7=princomp(d7)
pp7$loadings
# Comp 1 = 0.997 Healthd
# Comp 2 = 0.141 Healtha - 0.989 Healthc

d1=dd[dd[,15]=="ECA",5:11]  # All rows (41) of dd where column 15 is ECA
pp1=princomp(d1)
pp1$loadings

# Comp 1 = 1.000 Econa
# Comp 2 = -0.289 Econe - 0.956 Econf
#
d2=dd[dd[,15]=="EAPAC",5:11]  # All rows (13) where EAPAC
pp2=princomp(d2)
pp2$loadings

# Comp 1 = 1.000 Econa
# Comp 2 = -0.106 Econb + 0.992 Econe
#
d3=dd[dd[,15]=="LAC",5:11]  # All rows (19) of dd where column 15 is ECA
pp3=princomp(d3)
pp3$loadings
# Comp 1 = 1.000 Econa
# Comp 2 = 0.993 Econe

d4=dd[dd[,15]=="MENA",5:11]  # All rows (12) where MENA
pp4=princomp(d4)
pp4$loadings
# Comp 1 = 1.000 Econa
# Comp 2 = -0.149 Econb +0.983 Econe



d7=dd[dd[,15]=="SSA",5:11]  # All rows (17) of dd where column 15 is ECA
pp7=princomp(d7)
pp7$loadings
# Comp 1 = 1.000 Econa
# Comp 2 = 0.145 Econb +0.966 Econe +0.175 Econf

d1=dd[dd[,15]=="ECA",12:14]  # All rows (41) of dd where column 15 is ECA
pp1=princomp(d1)
pp1$loadings
# Comp 1 = -1.000 Envb
# Comp 2 =  1.000 Enva

d2=dd[dd[,15]=="EAPAC",12:14]  # All rows (13) where EAPAC
pp2=princomp(d2)
pp2$loadings
# Comp 1 = -1.000 Envb
# Comp 2 =  0.982 Enva - 0.190 Envc

d3=dd[dd[,15]=="LAC",12:14]  # All rows (19) of dd where column 15 is ECA
pp3=princomp(d3)
pp3$loadings

# Comp 1 = -1.000 Envb
# Comp 2 =  1.000 Enva
d4=dd[dd[,15]=="MENA",12:14]  # All rows (12) where MENA
pp4=princomp(d4)
pp4$loadings
# Comp 1 = -1.000 Envb
# Comp 2 =  0.997 Enva



d7=dd[dd[,15]=="SSA",12:14]  # All rows (17) of dd where column 15 is ECA
pp7=princomp(d7)
pp7$loadings
# Comp 1 = -1.000 Envb
# Comp 2 =  1.000 Enva

n=length(d1[,1])
plot(pp1$scores[,1],pp1$scores[,2],pch=labels(d1)[[1]][1:n])

biplot(pp1)
summary(pp1)
