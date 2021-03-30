#business analysis star rating 

setwd("C:/Users/sthan/OneDrive/Desktop/Universität Luzern/MASTER UNILU/FS2021/Big_Data")
library(tidyverse)
library(glmnet)
library(magrittr)
library(stargazer)
data <- read.csv(file = 'busiclean.csv')
head(data)

#selecting the attributes, which define the rating 
data_prep<-data %>% select(9,13:74)

data_clean<-na.omit(data_prep)
#there is just one observation with an entry for every attribute. we have to use an other identification strategy.
#we will focus on variables that apply to a large number of restaurants. Yelp also has variables that apply to niche restaurants. 
#An example of this would be a karaoke restaurant..

summary(data_prep)
# on the first step we select covariates with less than 20000NAs into the final dataset for the analysis.
#We need to have a dataset, which is big eneough to make an anaylsis.

data_prep2<-data_prep %>% select(1:11, 13:15, 18:35, 37:39, 41)
data_prep2omit<-na.omit(data_prep2)
#removing blank spots 

df<-data_prep2omit

df[df==""]<-NA
df<-df[complete.cases(df),]

#transform data to numercs(dummy variable), TRUE= 1, FALSE=0
df %<>% mutate_if(is.logical,as.numeric) 


#creating dummy variables

df[,"attributes.WiFi"]<-as.factor(df[,"attributes.WiFi"])
df[,"attributes.Alcohol"]<-as.factor(df[,"attributes.Alcohol"])
df[,"attributes.NoiseLevel"]<-as.factor(df[,"attributes.NoiseLevel"])
df[,"attributes.RestaurantsAttire"]<-as.factor(df[,"attributes.RestaurantsAttire"])
df[,"attributes.RestaurantsPriceRange2"]<-as.factor(df[,"attributes.RestaurantsPriceRange2"])


summary(df)

ols <- lm(stars ~ ., data = df)
summary(ols)
#the most important attribute for the star rating seems to be the noise level. This is negatively correlated with the rating. 
#If you want to see the results separately or save them, then uncomment the code below.
#stargazer::stargazer(ols, type = "text", out =   "ols Yelp")


#Lasso and ridge were only applied to see which attributes were seen as important.
#lasso 
df_ml<-df
df_ml[,"attributes.WiFi"]<-as.numeric(df_ml[,"attributes.WiFi"])
df_ml[,"attributes.Alcohol"]<-as.numeric(df_ml[,"attributes.Alcohol"])
df_ml[,"attributes.NoiseLevel"]<-as.numeric(df_ml[,"attributes.NoiseLevel"])
df_ml[,"attributes.RestaurantsAttire"]<-as.numeric(df_ml[,"attributes.RestaurantsAttire"])
df_ml[,"attributes.RestaurantsPriceRange2"]<-as.numeric(df_ml[,"attributes.RestaurantsPriceRange2"])
str(df_ml)
summary(df_ml)

lasso <- glmnet(as.matrix(df_ml[,c(2:36)]), df_ml$stars, alpha = 1) 
plot(lasso, xvar = "lambda", label = TRUE)
# on the very left the lasso model is similar to the ols. On the very right we have a sparse model.
# The sparse model consists of two covariates.
#The further to the right the variables are, the more decisive these variables are for the rating.



#cross validation for finding the right lamda value
lasso.cv <- cv.glmnet(as.matrix(df_ml[,c(2:36)]), df_ml$stars, type.measure = "mse", nfolds = 5, alpha = 1)

plot(lasso.cv)

print(paste0("Optimal lambda that minimizes cross-validated MSE: ", lasso.cv$lambda.min))
print(paste0("Optimal lambda using one-standard-error-rule: ", lasso.cv$lambda.1se))

# Print Lasso coefficients
print(coef(lasso.cv, s = "lambda.min"))
#According to lasso is the attribute Ambience touristy is not so important for the ratings.
#As these are machine learning approaches, the coefficients cannot be interpreted. 


# alpha = 0 specifies a Ridge model

# Estimate the Ridge
ridge <- glmnet(as.matrix(df_ml[,c(2:36)]), df_ml$stars, alpha = 0)

# Plot the path of the Ridge coefficients
plot(ridge, xvar = "lambda", label = TRUE)


# Cross-validate the Ridge model 
ridge.cv <- cv.glmnet(as.matrix(df_ml[,c(2:36)]), df_ml$stars, type.measure = "mse", nfolds = 5, alpha = 0)

# Plot the MSE in the cross-validation samples
plot(ridge.cv)

# Print the optimal lambda value
print(paste0("Optimal lambda that minimizes cross-validated MSE: ", ridge.cv$lambda.min))
print(paste0("Optimal lambda using one-standard-error-rule: ", ridge.cv$lambda.1se))


#  Ridge Coefficients  #

# Print Ridge coefficients
print(coef(ridge.cv, s = "lambda.min"))



