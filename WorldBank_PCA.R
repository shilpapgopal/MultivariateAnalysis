library(tidyverse)
library(caret)
library(devtools)
library(ggbiplot)
#install.packages("devtools")
#install.packages(c("FactoMineR", "factoextra"))
library("devtools")
#install_github("kassambara/factoextra")
library("factoextra")
library("FactoMineR")
library("factoextra")

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

#-----------------health--------------------------------------------------------------------------
newdf <- data.frame(finaldf$region,finaldf$healtha, finaldf$healthb, finaldf$healthc, finaldf$healthd)
names(newdf) <- c("region", "healtha", "healthb", "healthc","healthd")
d1=newdf[newdf[,1]=="ECA",2:5]  
pp7=princomp(d1)
pp7$loadings
summary(pp7)

d1=finaldf[finaldf[,2]=="ECA",14:17]  
pp7=princomp(d1)
pp7$loadings
summary(pp7)

d2=finaldf[finaldf[,2]=="EAPAC",14:17]
pp7=princomp(d2)
pp7$loadings
summary(pp7)

d3=finaldf[finaldf[,2]=="LAC",14:17] 
pp7=princomp(d3)
pp7$loadings
summary(pp7)

d4=finaldf[finaldf[,2]=="MENA",14:17] 
pp7=princomp(d4)
pp7$loadings
summary(pp7)

d5=finaldf[finaldf[,2]=="SSA",14:17]
pp7=princomp(d5)
pp7$loadings
summary(pp7)

#********
d1=finaldf[finaldf[,3]=="HIGH",14:17]  
pp7=princomp(d1)
#pp7$loadings
summary(pp7)

d2=finaldf[finaldf[,3]=="LOM",14:17]
pp7=princomp(d2)
#pp7$loadings
summary(pp7)

d3=finaldf[finaldf[,3]=="UPM",14:17] 
pp7=princomp(d3)
#pp7$loadings
summary(pp7)

d4=finaldf[finaldf[,3]=="LOW",14:17] 
pp7=princomp(d4)
#pp7$loadings
summary(pp7)


#-------------#economy-----------------------------------------------------------------------------
d1=finaldf[finaldf[,2]=="ECA",7:13]  
pp7=princomp(d1)
#pp7$loadings
summary(pp7)

d2=finaldf[finaldf[,2]=="EAPAC",7:13]
pp7=princomp(d2)
#pp7$loadings
summary(pp7)

d3=finaldf[finaldf[,2]=="LAC",7:13]
pp7=princomp(d3)
#pp7$loadings
summary(pp7)

d4=finaldf[finaldf[,2]=="MENA",7:13]
pp7=princomp(d4)
#pp7$loadings
summary(pp7)

d5=finaldf[finaldf[,2]=="SSA",7:13]
pp7=princomp(d5)
#pp7$loadings
summary(pp7)

#***********
d1=finaldf[finaldf[,3]=="HIGH",7:13]  
pp7=princomp(d1)
#pp7$loadings
summary(pp7)

d2=finaldf[finaldf[,3]=="LOW",7:13]
pp7=princomp(d2)
#pp7$loadings
summary(pp7)

d3=finaldf[finaldf[,3]=="LOM",7:13]
pp7=princomp(d3)
#pp7$loadings
summary(pp7)

d4=finaldf[finaldf[,3]=="UPM",7:13]
pp7=princomp(d4)
pp7$loadings
summary(pp7)



#-------------#env----------------------------------------------------------------
d1=finaldf[finaldf[,2]=="ECA",4:6]  
pp7=princomp(d1)
#pp7$loadings
summary(pp7)

d2=finaldf[finaldf[,2]=="EAPAC",4:6] 
pp7=princomp(d2)
pp7$loadings

d3=finaldf[finaldf[,2]=="LAC",4:6]
pp7=princomp(d3)
pp7$loadings

d4=finaldf[finaldf[,2]=="MENA",4:6]
pp7=princomp(d4)
pp7$loadings

d5=finaldf[finaldf[,2]=="SSA",4:6] 
pp7=princomp(d5)
pp7$loadings

#-------------#all-------------------------


d1=finaldf[finaldf[,2]=="ECA",4:17]  
pp7=princomp(d1)
pp7$loadings
summary(pp7)

d2=finaldf[finaldf[,2]=="LAC",4:17] 
pp7=princomp(d2)
pp7$loadings
summary(pp7)

d3=finaldf[finaldf[,2]=="SSA",4:17]
pp7=princomp(d3)
pp7$loadings
summary(pp7)

d4=finaldf[finaldf[,3]=="HIGH",4:17]
pp7=princomp(d4)
pp7$loadings
summary(pp7)

d5=finaldf[finaldf[,3]=="LOM",4:17] 
pp7=princomp(d5)
pp7$loadings
summary(pp7)
pp7$scores
plot(pp7$scores[,1],pp7$scores[,2],pch=labels(d5)[[1]][1:n])
biplot(pp7)

d6=finaldf[finaldf[,3]=="UPM",4:17]
pp7=princomp(d6)
pp7$loadings
summary(pp7)
pp7
#--------------------------------------


dd_all=finaldf[,4:17]
pp=princomp(dd_all, cor=TRUE)
(pp$sdev)^2  # squaring the sdev gives eigen values

summary(pp)
pp$scores
plot(pp)
pp

p1=pp$scores[,1]
p2=pp$scores[,2]
plot(p1,p2, main = "Plot Income using P1 & P2")  # Plot of scores for first two principal components
points(p1[dd[,3]=="UPM"],p2[dd[,3]=="UPM"],pch=20,col="red")
points(p1[dd[,3]=="HIGH"],p2[dd[,3]=="HIGH"],pch=20,col="blue")
points(p1[dd[,3]=="LOW"],p2[dd[,3]=="LOW"],pch=20,col="green")
points(p1[dd[,3]=="LOM"],p2[dd[,3]=="LOM"],pch=20,col="yellow")


p1=pp$scores[,1]
p2=pp$scores[,2]
plot(p1,p2,main="Plot Geography using P1 & P2")  # Plot of scores for first two principal components
points(p1[dd[,2]=="ECA"],p2[dd[,2]=="ECA"],pch=20,col="red")
points(p1[dd[,2]=="EAPAC"],p2[dd[,2]=="EAPAC"],pch=20,col="blue")
points(p1[dd[,2]=="LAC"],p2[dd[,2]=="LAC"],pch=20,col="green")
points(p1[dd[,2]=="MENA"],p2[dd[,2]=="MENA"],pch=20,col="yellow")
points(p1[dd[,2]=="SSA"],p2[dd[,2]=="SSA"],pch=20,col="black")

ggplot(dd, aes(p1, p2, col = Species, fill = income)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

biplot(pp)

corrplot(pp$loadings, is.corr=TRUE)
#--rough
v1=var(d1)  

eigval  <- eigen(v1)$values
eigvec <- eigen(v1)$vectors
100*eigval/sum(eigval)
cumsum(100*eigval/sum(eigval))
plot(eigval)

#---
#---------manually scale & recentre---------- no correlation
my_data <- finaldf[, c(4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
ppf <- preProcess(my_data, method=c("center", "scale"))
norm1 <- predict(ppf, my_data)
summary(norm1)

pp=princomp(norm1)
pp$loadings
summary(pp)
pp$scores
plot(pp)
pp
#eigval  <- eigen(v1)$values
#eigval

eigval  <- eigen(cov(norm1))$values
eigval
100*eigval/sum(eigval)

pcadf <- pp$scores[,1:10]
nrow(pcadf)

#------------------

#env
d6=finaldf[,4:6]
pp7=princomp(d6,cor=TRUE)
#pp7$loadings
summary(pp7)
pp7

#eco
d4=finaldf[,7:13]
pp7=princomp(d4,cor=TRUE)
#pp7$loadings
summary(pp7)

#health
d1=finaldf[,14:17]  
pp7=princomp(d1,cor=TRUE)
#pp7$loadings
summary(pp7)

plot(pp7)

res.pca <- PCA(norm1, graph = FALSE)
print(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val
plot(pp)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Coordinates of variables
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")

library("corrplot")
corrplot(var$cos2, is.corr=TRUE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Use only lineplot
fviz_eig(res.pca, geom="line")

head(var$contrib, 4)

#----------------------------------------------------------

# Purpose: We are interested in whether the covariances come from the same population. 
# Test if Higher class/Lower middle class/Upper middle class features have covariances from same populn using Likelihood ratio test:
# As n is moderately large, we use Likelihood ratio test
#H0 : Σ1 = Σ2 = Σ3 
#H1: Σ1!=Σ2!= Σ3 

d1=finaldf[finaldf[,3]=="HIGH",1:17]  
d2=finaldf[finaldf[,3]=="LOM",1:17]
d3=finaldf[finaldf[,3]=="UPM",1:17]

sum(finaldf[,3]=="LOW") #8
n1 = sum(finaldf[,3]=="HIGH") #49
n2 = sum(finaldf[,3]=="LOM") #33
n3 = sum(finaldf[,3]=="UPM") #37

v1=((n1-1)/n1)*var(d1[,4:17])
v2=((n2-1)/n2)*var(d2[,4:17])
v3=((n3-1)/n3)*var(d3[,4:17])
v=(n1*v1+n2*v2+n3*v3)/(n1+n2+n3)  # n1=49, n2=33, n3=37

k=3          # number of categories      
p=14         # p=14 variables/features
n=n1+n2+n3   # k=3 groups 
u=n*log(det(v))-n1*log(det(v1))-n2*log(det(v2))-n3*log(det(v3)) # test statitic = 1511

df=0.5*(k-1)*p*(p+1)    # 210
qchisq(0.95,df)  # 5% value for chisquare(210) is = 244.8076

# 244.8076
1-pchisq(u,df)   # P=0, Reject H0, bcos u > chisq(k=210) at 5% SL
#Conclusion: covariances come from different population


# Testing cov matrix, variables 1-4------------------
#
v1=((n1-1)/n1)*var(d1[,4:7])
v2=((n2-1)/n2)*var(d2[,4:7])
v3=((n3-1)/n3)*var(d3[,4:7])
v=(n1*v1+n2*v2+n3*v3)/(n1+n2+n3)           # n1=41, n2=32, n3=30.
#
k=3;  p=4;  
n=n1+n2+n3  # k=3 groups, n1=42, n2=32, n3=30, p=14 variables.
u=n*log(det(v))-41*log(det(v1))-32*log(det(v2))-30*log(det(v3))
# ~ chisquare(df=0.5*(k-1)*p*(p+1)=12) if H0 true.
df=0.5*(k-1)*p*(p+1)    
# 
1-pchisq(u,df)   # Reject H0! P=0!