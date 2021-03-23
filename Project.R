library(jsonlite)
library(tidyverse)

business <- fromJSON(sprintf('[%s]', paste(readLines('yelp_academic_dataset_business.json'), collapse=',')), simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)
review<-fromJSON(sprintf('[%s]', paste(readLines('yelp_academic_dataset_review.json'), collapse=',')), simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)
user<-fromJSON(sprintf('[%s]', paste(readLines('yelp_academic_dataset_user.json'), collapse=',')), simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)

#Analyzes:
#The effect of stars assigned on average in reviews on the probability of being a elite user (Uluru OLS)
#-indepenent variable: average stars
#-covariates: quality of reviews
#-dependent variable: binary elite or nor

#What are the most important factors for restaurants to have good reviews (forward selection)
#-dependent: stars of restaurant





