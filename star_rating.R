#business analysis star rating 

setwd("C:/Users/sthan/OneDrive/Desktop/Universität Luzern/MASTER UNILU/FS2021/Big_Data")
library(tidyverse)
library(corrplot)
library(glmnet)
library(magrittr)
data <- read.csv(file = 'busiclean.csv')
head(data)

#selecting the attributes for the stars

data_prep<-data %>% select(9,13:74)

data_clean<-na.omit(data_prep)
#there is just one observation with an entry for every attribute. we have to use an other identify strategy. we wil focus on the normal attributes which are normal in our opinions. this
#might be food, ambient, service and so one. on yelp there are several other paramteres which are kind of unnecessary for the normal restaurant visitiors.

summary(data_prep)
# on the first step i would select covariates with less than 20000NAs into the final dataset for the analysis. we need to have a dataset, which is big eneough to make anaylsis.

data_prep2<-data_prep %>% select(1:11, 13:15, 18:35, 37:39, 41)
data_prep2omit<-na.omit(data_prep2)
#removing blank spots 

df<-data_prep2omit

df[df==""]<-NA
df<-df[complete.cases(df),]

#transform data to numercs(dummy variable), TRUE= 1, FALSE=0
df %<>% mutate_if(is.logical,as.numeric) 
#repalcing char values with logical 
df2<-df%>%select(1,2,4:25,27:33,36)

#creating dummy variables df3
df3<-df
df3$attributes.WiFi[df3$attributes.WiFi== "no"]<- 0
df3$attributes.WiFi[df3$attributes.WiFi== "paid"]<- 1
df3$attributes.WiFi[df3$attributes.WiFi== "free"]<- 2

df3$attributes.Alcohol[df3$attributes.Alcohol== "none"]<- 0
df3$attributes.Alcohol[df3$attributes.Alcohol== "beer_and_wine"]<- 1
df3$attributes.Alcohol[df3$attributes.Alcohol== "full_bar"]<- 2


df3$attributes.NoiseLevel[df3$attributes.NoiseLevel== "very_loud"]<-0
df3$attributes.NoiseLevel[df3$attributes.NoiseLevel== "loud"]<- 1
df3$attributes.NoiseLevel[df3$attributes.NoiseLevel== "average"]<- 2
df3$attributes.NoiseLevel[df3$attributes.NoiseLevel== "quiet"]<- 3

df3$attributes.RestaurantsAttire[df3$attributes.RestaurantsAttire== "dressy"]<- 0
df3$attributes.RestaurantsAttire[df3$attributes.RestaurantsAttire== "casual"]<- 1

cor <- round(cor(df3[,c(2:36)]),2) 
corrplot(cor)

ols <- lm(stars ~ ., data = df3)
summary(ols)

#lasso 
lasso <- glmnet(as.matrix(df3[,c(2:36)]), df3$stars, alpha = 1) # We save the model under the name "lasso"
plot(lasso, xvar = "lambda", label = TRUE)

#cross validation for finding the right lamda value
lasso.cv <- cv.glmnet(as.matrix(df3[,c(2:36)]), df3$stars, type.measure = "mse", nfolds = 5, alpha = 1)

plot(lasso.cv)

print(paste0("Optimal lambda that minimizes cross-validated MSE: ", lasso.cv$lambda.min))
print(paste0("Optimal lambda using one-standard-error-rule: ", lasso.cv$lambda.1se))

# Print Lasso coefficients
print(coef(lasso.cv, s = "lambda.min"))



# alpha = 0 specifies a Ridge model

# Estimate the Ridge
ridge <- glmnet(as.matrix(df3[,c(2:36)]), df3$stars, alpha = 0)

# Plot the path of the Ridge coefficients
plot(ridge, xvar = "lambda", label = TRUE)


# Cross-validate the Ridge model 
ridge.cv <- cv.glmnet(as.matrix(df3[,c(2:36)]), df2$stars, type.measure = "mse", nfolds = 5, alpha = 0)

# Plot the MSE in the cross-validation samples
plot(ridge.cv)

# Print the optimal lambda value
print(paste0("Optimal lambda that minimizes cross-validated MSE: ", ridge.cv$lambda.min))
print(paste0("Optimal lambda using one-standard-error-rule: ", ridge.cv$lambda.1se))


####################  Ridge Coefficients  ########################

# Print Ridge coefficients
print(coef(ridge.cv, s = "lambda.min"))

# Save for later comparison
coef_ridge <- coef(ridge.cv, s = "lambda.min") 

