library(jsonlite)
library(tidyverse)

business<-stream_in(file("yelp_academic_dataset_business.json"))
business <- fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_business.json"), collapse=",")), simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)
