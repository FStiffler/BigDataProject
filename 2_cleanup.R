library(tidyverse)
library(vroom)
library(data.table)

# import user file with particular variables
user <- vroom("user.csv", 
              col_select = c(user_id, review_count, useful, average_stars, elite),#columns to select
              col_types = c(elite = "c")) #read in column elite as character


# look at data

##Structure of data
str(user)
head(user)

##How are the number of reviews distributed on the lower end?
ggplot(user, aes(x=review_count))+
  geom_histogram(binwidth = 1, color="red")+
  scale_x_continuous(breaks = seq(0, 100, 1), limits = c(0,100)) #a lot of users with 1 reviews

##Are there users with 0 reviews?
any(user$review_count==0) #yes


## Are there even non elite users?
any(is.na(user$elite)) #yes


#prepare user data
userNew<-user%>%
  filter(review_count>=1)%>% #only take users with at least one comment into consideration
  mutate(eliteDummy=ifelse(!is.na(elite),1,0))%>% #create dummy if at some point, user is an elite user
  select(-elite)%>% #drop original elite column
  mutate(usefulPerReview=useful/review_count)%>% #create variable for average useful mentions per review 
  select(-useful)

#write smaller csv file
vroom_write(userNew, "userSmall.csv", delim = ",")
  


