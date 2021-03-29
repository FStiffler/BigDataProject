# Load dependencies
source("packageDependencies.R")

# Load business data ####

## List with all json object strings as single list elements
jsonStrings<-as.list(readLines('yelp_academic_dataset_business.json'))# Read each line seperately and combine resulting vector to a list

## Create function to modify json file in a way that subobjects are recognized as well (in the raw data, some subobjects of a json object are defined as a single string value)
modifyObject<-function(x){
  
  string<-x
  subObj<-as.vector(str_extract_all(string, "(?<=\\\"\\{)(\\'.+?)(?=\\}\\\")", simplify = T)) #Extract information contained in subobjects currently defined as single string value (example object after business parking)
  subObj<-str_replace_all(subObj, "'", "\"") #replace keys with encapsulated with ' with "
  subObj<-str_replace_all(subObj, "False", "\"False\"") #encapsulate values with "
  subObj<-str_replace_all(subObj, "True", "\"True\"") #encapsulate values with "
  subObj<-str_replace_all(subObj, "None", "\"None\"") #encapsulate values with "
  
  modOriginal<-str_replace_all(string, "(?<=\\\"\\{)(\\'.+?)(?=\\}\\\")", "!!!!!") #replace the information in subobjects from original string
  modOriginal<-str_replace_all(modOriginal, '\\"\\{!!!!!', '\\{!!!!!') #Remove quotation marks of preventing subobjects to be recognized as such
  modOriginal<-str_replace_all(modOriginal, '!!!!!\\}\"', '!!!!!\\}') #Remove quotation marks of preventing subobjects to be recognized as such
  modOriginal<-as.vector(str_split(modOriginal, "!!!!!", simplify = T)) #splot original string into parts before and after subobject
  
  subObj[length(modOriginal)]<-"" #add an empty value to have two equally long string vectors for pasting
  
  newString<-paste0(modOriginal,subObj, collapse = "") #paste values of modOriginal to first value of subObj so that first values go together, second go together, usw. Collapse to one string
  
  #Output
  newString
  
}

## Vectorize function so that is applicable to all elements in list
modifyObjects<-Vectorize(modifyObject, SIMPLIFY = F)

## Create list with modified json strings 
outputList<-modifyObjects(jsonStrings)

## Read strings to dataframe
business1 <- tibble(fromJSON(sprintf('[%s]', paste(outputList, collapse=',')), simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE))

## Unnest columns with lists in a way that each element in the list has its own column
business<-business1%>% 
  mutate(attributes.BusinessParking=ifelse(attributes.BusinessParking %in% c("None","{}"),list(NULL),attributes.BusinessParking))%>%
  unnest_wider(attributes.BusinessParking, names_sep = ".")%>%
  mutate(attributes.Ambience=ifelse(attributes.Ambience%in% c("None","{}"),list(NULL),attributes.Ambience))%>%
  unnest_wider(attributes.Ambience, names_sep = ".")%>%
  mutate(attributes.GoodForMeal=ifelse(attributes.GoodForMeal%in% c("None","{}"),list(NULL),attributes.GoodForMeal))%>%
  unnest_wider(attributes.GoodForMeal, names_sep = ".")%>%
  mutate(attributes.HairSpecializesIn=ifelse(attributes.HairSpecializesIn%in% c("None","{}"),list(NULL),attributes.HairSpecializesIn))%>%
  unnest_wider(attributes.HairSpecializesIn, names_sep = ".")%>%
  mutate(attributes.BestNights=ifelse(attributes.BestNights%in% c("None","{}"),list(NULL),attributes.BestNights))%>%
  unnest_wider(attributes.BestNights, names_sep = ".")%>%
  mutate(attributes.Music=ifelse(attributes.Music%in% c("None","{}"),list(NULL),attributes.Music))%>%
  unnest_wider(attributes.Music, names_sep = ".")%>%
  mutate(attributes.DietaryRestrictions=ifelse(attributes.DietaryRestrictions%in% c("None","{}"),list(NULL),attributes.DietaryRestrictions))%>%
  unnest_wider(attributes.DietaryRestrictions, names_sep = ".")

## Save as csv
fwrite(business, file="businessLarge.csv")

# Load user data ####
user<-stream_in(file('yelp_academic_dataset_user.json'), verbose = T)
fwrite(user, file="userLarge.csv")

# Remove objects and collect garbage ####
rm(list=ls())
gc()
