load("mv_cw_cleandata_29mar.Rdata")

#install.packages("caret")
#install.packages("writexl") 
library(corrplot)
library(psych)
library(ggcorrplot)
library(ggplot2)
library("PerformanceAnalytics")
library(MASS)
library(tidyverse)
library(caret)
library("writexl")
library(corrplot)
#write_xlsx(finaldf,"cw_data.xlsx")

#----------------
finaldf %>% count(country) #127
finaldf %>% count(region)
finaldf %>% count(income)
#------------------

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
#-------------------


finaldf <- finaldf[, c("enva", "envb", "envc", "ecoa","ecob","ecoc","ecod","ecoe","ecof","ecog","healtha","healthb","healthc","healthd")]
M<-cor(finaldf)
corrplot(M, method = "circle")
corrplot(M,method="number",type = "upper")


library(corrplot)
test <- matrix(data = rnorm(400), nrow=14, ncol=14)
png(height=700, width=700, pointsize=15, file="overlap.png")
corrplot(M, method = "color", addCoef.col="grey", order = "AOE")



my_data <- finaldf[, c(4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
M<-cor(my_data)

#chart.Correlation(my_data, histogram=TRUE,chart.Correlation(my_data, histogram=TRUE,chart.Correlation(my_data, histogram=TRUE, pch=19, col="blue")

pairs(my_data, col="darkblue")


corrplot(M, method = "circle")
#----------------------

df1 <- finaldf[, c(5,6,7)]
pairs(df1, gap=0, c("red", "green", "blue") [df1$co2_emission], pch=21)

#------FROM IRIS DATASET------------
# predict region for whole dataset, Accuracy = 78%
df <- finaldf[, c(2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

training.samples1 <- df$region %>% createDataPartition(p = 0.8, list = FALSE)
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
model <- lda(region~., data = train.transformed1)
# Make predictions
predictions <- model %>% predict(test.transformed1)
# Model accuracy
mean(predictions$class==test.transformed1$region)


model <- lda(region~., data = train.transformed1)
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
  geom_point(aes(color = region))

table(predictions$class, test.transformed1$region)
mean(predictions$class==test.transformed1$region)
#--------------------
# predict income consider whole dataset, Accuracy = 78%
df <- finaldf[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]


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

#----------------------

# predict income for only region=ECA , Accuracy = 85%
d1=finaldf[finaldf[,2]=="ECA",]

df <- d1[, c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
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


#-----------------------------------------------------------------------
# 4 April ---- PCA---


d1=finaldf[finaldf[,15]=="ECA",4:17]  # All rows (17) of dd where column 15 is ECA
pp7=princomp(d7)
pp7$loadings

d7=finaldf[finaldf[,2]=="SSA",4:17]  # All rows (17) of dd where column 15 is ECA
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

