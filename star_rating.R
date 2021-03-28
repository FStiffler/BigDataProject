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





