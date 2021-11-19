library(tidyverse)
library(caret)
library(MASS)
options(scipen=999)
options(scipen=0)
#install.packages("vctrs", repos = "https://packagemanager.rstudio.com/cran/latest")
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
my_data <- finaldf[, c("country", "region" ,"income" ,"enva", "envb", "envc","healtha","healthb","healthc","healthd", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog")]

#--------------------------------------------------------------

df_data_pk <-finaldf[, c(1,2,3)]

# creating output of PCA
my_data <- finaldf[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
ppf <- preProcess(my_data, method=c("center", "scale"))
norm1 <- predict(ppf, my_data)
summary(norm1)

pca_input_df <- norm1[, c(4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
pp=princomp(pca_input_df)
pp$loadings
summary(pp)
head(pp$scores)
(pp$sdev)^2 #eigen values

pcadf <- pp$scores[,1:4]
nrow(pcadf)
var(pca_input_df)


final_pca_output_df <- cbind(df_data_pk, pcadf)
#--------------------------------------------------------------
#scenario 1 - in CW report
# LAC vs MENA
dd=my_data
d1=dd[dd[,2]=="LAC",4:17]  # All rows of dd where region is LAC
d2=dd[dd[,2]=="MENA",4:17]   # All rows of dd where region is MENA
m1=apply(d1,2,mean)
m2=apply(d2,2,mean)

v1=cov(d1) 
v2=cov(d2)

n1=nrow(d1)
n2=nrow(d2)

spool=((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2)
dim(v1) # 14 14
dim(spool) # 14 14
avector=solve(spool)%*%(m1-m2)
dim(avector)#14 1
# Largest values are ecob,healthb,envc
astar=diag(spool)^(0.5)*avector
astar #14 1

d1m=as.matrix(d1)
d2m=as.matrix(d2)
z1=d1m%*%astar # LAC
z2=d2m%*%astar # MENA
hist(z1,30)  
hist(z2,30)

dim(astar)
dim(dd[52,4:17])
z=sum(astar*dd[52,4:17]) #66,211
z=sum(astar*dd[2,4:17])  #1,14,653
z=sum(astar*dd[9,4:17])  #4,01,479
z=sum(astar*dd[36,4:17]) #1,12,493 # Egypt - MENA 
z=sum(astar*dd[23,4:17]) #2,31,889 # Chile - LAC

# Group discriminant function 
z1_bar = mean(z1)#139150
z2_bar = mean(z2)#313026
group = 0.5 %*% (z1_bar+z2_bar) #2,26,088
#group1 = LCA
#group2 = MENA
{
if(z>group){
  cat("Group1 - LAC"); 
}
else{ 
  cat("Group2 - MENA");
}
}

dd[9,1:17] # Bahrain-MENA
dd[2,1:17] #Algeria-MENA
dd[36,1:17] #Egypt -MENA
dd[23,1:17] # Chile - LAC
#----------------------------------------------------

# scenario 2 - pass output of PCA to LDA - library MASS - 
#with transformation
#Accuracy = 73.91%
# predict income
set.seed(8)
nrow(final_pca_output_df)
d1=final_pca_output_df

df <- d1[, c(3,4,5)]
nrow(df)

training.samples1 <- df$income %>% createDataPartition(p = 0.8, list = FALSE)
train.data1 <- df[training.samples1, ]
nrow(train.data1)
test.data1 <- df[-training.samples1, ]
nrow(test.data1)

# Estimate preprocessing parameters
preproc.param1 <- train.data1 %>% preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed1 <- preproc.param1 %>% predict(train.data1)
test.transformed1 <- preproc.param1 %>% predict(test.data1)

# Fit the model
model <- lda(income~., data = train.transformed1)
# Make predictions
predictions <- model %>% predict(test.transformed1)
# Model accuracy
mean(predictions$class==test.transformed1$income)


model <- lda(income~., data = train.transformed1)
model

plot(model)

predictions <- model %>% predict(test.transformed1)
#predictions <- model %>% predict(train.transformed1)
names(predictions)
predictions
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.transformed1, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = income))

table(predictions$class, test.transformed1$income)
mean(predictions$class==test.transformed1$income)

#------------------------------------------


# scenario 2 - pass output of PCA to LDA - library MASS - 
#without transformation
set.seed(9)
# predict income
nrow(final_pca_output_df)
d1=final_pca_output_df

df <- d1[, c(3,4,5)]
nrow(df)

training.samples1 <- df$income %>% createDataPartition(p = 0.8, list = FALSE)
train.data1 <- df[training.samples1, ]
nrow(train.data1)
test.data1 <- df[-training.samples1, ]
nrow(test.data1)

# Estimate preprocessing parameters
#preproc.param1 <- train.data1 %>% preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
#train.transformed1 <- preproc.param1 %>% predict(train.data1)
#test.transformed1 <- preproc.param1 %>% predict(test.data1)

# Fit the model
model <- lda(income~., data = train.data1)
# Make predictions
predictions <- model %>% predict(test.data1)
# Model accuracy
mean(predictions$class==test.data1$income)


model <- lda(income~., data = train.data1)
model

plot(model)

predictions <- model %>% predict(test.data1)
#predictions <- model %>% predict(train.transformed1)
names(predictions)
predictions
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.data1, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = income))

table(predictions$class, test.data1$income)
mean(predictions$class==test.data1$income)

#-----------------------------------------------------------
# ---- PUT IN CW-----------
#scenario 2 - pass the original data to LDA
# predict income , consider whole dataset, Accuracy = 78.26%
df <- finaldf[, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

training.samples1 <- df$income %>% createDataPartition(p = 0.8, list = FALSE)
train.data1 <- df[training.samples1, ]
nrow(train.data1)
test.data1 <- df[-training.samples1, ]
nrow(test.data1)

# Estimate preprocessing parameters
preproc.param1 <- train.data1 %>% preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed1 <- preproc.param1 %>% predict(train.data1)
test.transformed1 <- preproc.param1 %>% predict(test.data1)

# Fit the model
model <- lda(income~., data = train.transformed1)
# Make predictions
predictions <- model %>% predict(test.transformed1)
# Model accuracy
mean(predictions$class==test.transformed1$income)

model <- lda(income~., data = train.transformed1)
model

plot(model)

predictions <- model %>% predict(test.transformed1)
names(predictions)

# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 

lda.data <- cbind(train.transformed1, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = income))

table(predictions$class, test.transformed1$income)


mean(predictions$class==test.transformed1$income)
