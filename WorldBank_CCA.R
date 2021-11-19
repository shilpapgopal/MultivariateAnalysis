#----------Canonical correlations LAB-----------------------------
options(scipen=999)
options(scipen=0)
dd=read.csv("coursework2018.csv",row.names=1)
d1=dd[,1:11]  # All rows of dd but only health and economic variables
d1=as.matrix(d1)
cor(d1)

ss=(var(d1))
syy=ss[1:4,1:4] # y = health
sxx=ss[5:11,5:11] # x = econ
sxy=ss[5:11,1:4] # xy = econ, health
syx=ss[1:4,5:11] # yx = hea, eco

#inv(Syy) Syx inv(Sxx) Sxy 
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy
eigen(smmy)

#inv(Sxx) Sxy inv(Syy) Syx 
smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx
eigen(smmx)

# Largest correlation is sqrt(0.8399)=0.916489
sqrt(diag(ss))

# Largest y eigenvector is
a1=eigen(smmy)$vectors[,1]   #[1] -0.0016822317  0.9999970947 -0.0017253072 -0.0000638643
eigen(smmy)$values
a1
# Largest x eigenvector is
b1=eigen(smmx)$vectors[,1]
eigen(smmx)$values
b1

# Standardise
dy=diag(sqrt(diag(ss))[1:4])
dx=diag(sqrt(diag(ss))[5:11])
dy%*%a1 # stand. coeff a1 of first largest Evec smmy
dx%*%b1 # stand. coeff b1 of first largest Evec smmx

#------------------- CCA with cor---------------------------
# Now in terms of correlations
ss=as.matrix(cor(d1))   
syy=ss[1:4,1:4]
sxx=ss[5:11,5:11]
sxy=ss[5:11,1:4]
syx=ss[1:4,5:11]
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy
smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx

# Largest correlation is sqrt(0.8399)=0.916489
sqrt(diag(ss))

# Largest y eigenvector is
a1=eigen(smmy)$vectors[,1]   #health
eigen(smmy)$values
a1
# Largest x eigenvector is
b1=eigen(smmx)$vectors[,1]  #econ
eigen(smmx)$values
b1

dy=diag(sqrt(diag(ss))[1:4])  # Now identities so not needed!
dx=diag(sqrt(diag(ss))[5:11])

dy%*%a1

# Most weight is Healthb
dx%*%b1

# Most weight is Econb, Econc, Econa, Econe..
#----------------------------------CW------------------------------

load("mv_cw_cleandata_29mar.Rdata")
finaldf<-finaldf %>% 
  rename(
    enva = cell_phones,
    envb = electric_use,
    envc = co2_emission,
    
    ecoa = gnipercapita_internat,
    ecob = childrenelders_per100,
    ecoc = corruption_perceptionindex,
    ecod = gdp_growthovr10yrs,
    ecoe = percentgdp_exports,
    ecof = percentgdp_foreigninvest,
    ecog = females15_labourforce,
    
    healtha = newborn_mortality_rate,
    healthb = total_fertility,
    healthc = dtp_immunized,
    healthd = govt_health_spend_pp
  )
head(finaldf)

finaldf <- finaldf[, c("enva", "envb", "envc", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog","healtha","healthb","healthc","healthd")]
nrow(finaldf)

#----------1 Canonical correlations CW between economy & health-----------------------------
d1 <- finaldf[, c( "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog","healtha","healthb","healthc","healthd")]
d1=as.matrix(d1)

ss=cov(d1) 
syy=ss[8:11,8:11] # y  = health
sxx=ss[1:7,1:7]   # x  = economy
sxy=ss[1:7,8:11]  # xy = economy, health
syx=ss[8:11,1:7]  # yx = health, economy

#inv(Syy) Syx inv(Sxx) Sxy 
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy
eigen(smmy)
#inv(Sxx) Sxy inv(Syy) Syx 
smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx
eigen(smmx)

# Largest y(health) eigenvector is
a1=eigen(smmy)$vectors[,1]   
#a1
# Largest x(economy) eigenvector is
b1=eigen(smmx)$vectors[,1]
#b1
# Standardise
dy=diag(sqrt(diag(ss))[8:11]) # y = health
dx=diag(sqrt(diag(ss))[1:7])  # x = economy
dy%*%a1 # stand. coeff a1 of first largest Evec health-smmy
dx%*%b1 # stand. coeff b1 of first largest Evec economy-smmx


#----------2 Canonical correlations CW between env & health-----------------------------
d1 <- finaldf[, c("enva", "envb", "envc", "healtha","healthb","healthc","healthd")]
d1=as.matrix(d1)

ss=(cov(d1))
syy=ss[4:7,4:7] # y = health
sxx=ss[1:3,1:3] # x = env
sxy=ss[1:3,4:7] # xy = env, health
syx=ss[4:7,1:3] # yx = health, env

#inv(Syy) Syx inv(Sxx) Sxy 
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy
eigen(smmy)

#inv(Sxx) Sxy inv(Syy) Syx 
smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx
eigen(smmx)

# Largest y eigenvector is
a1=eigen(smmy)$vectors[,1]   
#a1

# Largest x eigenvector is
b1=eigen(smmx)$vectors[,1]
#b1
# Standardise
dy=diag(sqrt(diag(ss))[4:7]) # y = health
dx=diag(sqrt(diag(ss))[1:3]) # x = env
dy%*%a1 # stand. coeff a1 of first largest Evec health - smmy
dx%*%b1 # stand. coeff b1 of first largest Evec env - smmx

#----------3 Canonical correlations CW between env & economy -----------------------------
d1 <- finaldf[, c("enva", "envb", "envc", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog")]
d1=as.matrix(d1)

ss=(cov(d1))
syy=ss[4:10,4:10] # y = economy
sxx=ss[1:3,1:3] # x = env
sxy=ss[1:3,4:10] # xy = env, economy
syx=ss[4:10,1:3] # yx = economy, env

#inv(Syy) Syx inv(Sxx) Sxy 
smmy=solve(syy)%*%syx%*%solve(sxx)%*%sxy
eigen(smmy)

#inv(Sxx) Sxy inv(Syy) Syx 
smmx=solve(sxx)%*%sxy%*%solve(syy)%*%syx
eigen(smmx)

# Largest y eigenvector is
a1=eigen(smmy)$vectors[,1]   
#a1
# Largest x eigenvector is
b1=eigen(smmx)$vectors[,1]
#b1
# Standardise
dy=diag(sqrt(diag(ss))[4:10]) # y = economy
dx=diag(sqrt(diag(ss))[1:3])  # x = env
dy%*%a1 # stand. coeff a1 of first largest Evec economy-smmy
dx%*%b1 # stand. coeff b1 of first largest Evec env-smmx

#------------------------------------------------------------------------
# -------Corr Plot for testing-------------
#cor(finaldf[,4:17])
my_data <- finaldf[, c("enva", "envb", "envc","healtha","healthb","healthc","healthd", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog")]
M<-cor(my_data)
corrplot(M,method="number",type = "upper")
# env & eco
my_data <- finaldf[, c("enva", "envb", "envc", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog")]
M<-cor(my_data)
corrplot(M, method="number",type="upper")

# env & health
my_data <- finaldf[, c("enva", "envb", "envc", "healtha","healthb","healthc","healthd")]
M<-cor(my_data)
corrplot(M, method="number",type="upper")

# health & eco
my_data <- finaldf[, c("healtha","healthb","healthc","healthd", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog")]
M<-cor(my_data)
corrplot(M, method="number",type="upper")

#-----------------------------------------------

# ------- 1 CCA - Hyp Testing - env, eco

# We want to test whether all of the corr between the x  & y variables are zero
# H0 : Σyx = 0 under H0, there is no linear relationship bw the x(env) &  y(economy) variables OR  all the ri equal zero

d1 <- finaldf[, c("enva", "envb", "envc", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog")]
d1=as.matrix(d1)

ss=(cov(d1))
rr=(cor(d1))
syy=ss[4:10,4:10] # y = economy
sxx=ss[1:3,1:3] # x = env

det(ss)/(det(syy) *det(sxx) ) #0.14

ryy=rr[4:10,4:10] # y = economy
rxx=rr[1:3,1:3] # x = env
det(rr)/(det(ryy) *det(rxx) ) #0.14

n=nrow(d1)
vv = (n-1)/n * ss
vyy=vv[4:10,4:10] # y = economy
vxx=vv[1:3,1:3] # x = env
lambda = det(vv)/(det(vyy) *det(vxx) ) #0.14

p=7 #eco
q=3 #env
U = -( n-0.5 *(p+q+3) ) * log (lambda) #230
U
qchisq(0.95,p*q) # #chi sq at 21 = 32.67
# conclusion: U > chisq(5%) ie 230 > 32.67
# Reject H0 at Chisq(21) 5% SL
1-pchisq(U,p*q) # 0
#(pq).

# Conclusion - Reject H0, There is linear relationship bw env & economy

# ------2 CCA - Hyp Testing - env & health----------------------

# We want to test whether all of the corr between the x  & y variables are zero
# H0 : Σyx = 0 under H0, there is no linear relationship bw the x(health) &  y(env) variables OR  all the ri equal zero

d1 <- finaldf[, c("enva", "envb", "envc", "healtha","healthb","healthc","healthd")]
d1=as.matrix(d1)

ss=(cov(d1))
rr=(cor(d1))
syy=ss[1:3,1:3] # y = env
sxx=ss[4:7,4:7] # x = health

det(ss)/(det(syy) *det(sxx) ) #0.376

ryy=rr[1:3,1:3] # y = health
rxx=rr[4:7,4:7] # x = env
det(rr)/(det(ryy) *det(rxx) ) #0.376

n=nrow(d1)
vv = (n-1)/n * ss
vyy=vv[1:3,1:3] # y = health
vxx=vv[4:7,4:7] # x = env
lambda = det(vv)/(det(vyy) *det(vxx) ) # 0.376

p=3
q=4
U = -( n-0.5 *(p+q+3) ) * log (lambda) #119
qchisq(0.95,p*q) # chi sq at 12 = 21.026
# conclusion: U > chisq(5%) ie 119 > 21.026
# Reject H0 at Chisq(12) 5% SL
1-pchisq(U,p*q)
# Conclusion - Reject H0, there exists linear relationship bw health & env

# ------ 3 CCA - Hyp Testing - health & eco------------------------

# We want to test whether all of the corr between the x  & y variables are zero
# H0 : Σyx = 0 under H0, there is no linear relationship bw the x(economy) &  y(health) variables OR  all the ri equal zero

d1 <- finaldf[, c( "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog","healtha","healthb","healthc","healthd")]
d1=as.matrix(d1)

ss=(cov(d1))
rr=(cor(d1))
syy=ss[8:11,8:11] # y  = health
sxx=ss[1:7,1:7]   # x  = economy

det(ss)/(det(syy) *det(sxx) ) #0.04

ryy=rr[8:11,8:11] # y = health
rxx=rr[1:7,1:7] # x = env
det(rr)/(det(ryy) *det(rxx) ) #0.04

n=nrow(d1)
vv = (n-1)/n * ss
vyy=vv[8:11,8:11] # y = health
vxx=vv[1:7,1:7] # x = env
lambda = det(vv)/(det(vyy) *det(vxx) ) #0.04

p=4
q=7
U = -( n-0.5 *(p+q+3) ) * log (lambda) #374
#chi sq at 18 = 21.026
qchisq(0.95,p*q) # 41
# conclusion: U > chisq(5%) ie 374 > 41
# Reject H0 at Chisq(28) 5% SL
1-pchisq(U,p*q)

# Conclusion - Reject H0, There is linear relationship bw health & economy
#-------------------------------------------


