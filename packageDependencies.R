##### Set dependencies
dependecies <- c("jsonlite","tidyverse","data.table", "glmnet", "vroom", "skimr", "microbenchmark", "leaps", "stargazer", "scatterplot3d", "fastDummies")

##### Install and load packages
missing <- dependecies[!(dependecies %in% installed.packages()[,"Package"])]
if(length(missing)) install.packages(missing)
lapply(dependecies, library, character.only = T)
