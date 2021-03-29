# Load dependencies (if not done already)
source("packageDependencies.R")

# Clean up of businessLarge.csv to convert it to a smaller csv file ####

# Make sure R is in 64 bits, for better performance
Sys.info()[["machine"]] # check output

# Load data and have a look at it 
busi <- fread("businessLarge.csv") # fastest read method according to benchmark test
glimpse(busi)
summary(busi)
skim(busi)

# First Problem: Multiple different categories per business. 22 categories with ~1200 subcategories (https://blog.yelp.com/2018/01/yelp_category_list#section21)
# We can limit our analysis to the restaurants parent-category.
busi <- 
  busi %>%
  filter(grepl("Restaurant", categories))
# 1/3 of the observations remaining (50k)

# Some businesses are closed
qplot(busi$is_open)
# To further reduce our data load, we can restrict our analysis on restaurants that were still in business at the time of data collection.
busi <- 
  busi %>%
  filter(is_open==1)
# 32k observations remaining

# Now, lets check for columns that need to be cleaned up or removed.
summary(busi)

# We remove all columns that contain +95% NAs.
busi <- 
  busi %>%
  select(-attributes.AcceptsInsurance,
         -attributes.HairSpecializesIn.straightperms,
         -attributes.HairSpecializesIn.coloring,
         -attributes.HairSpecializesIn.extensions,
         -attributes.HairSpecializesIn.africanamerican,
         -attributes.HairSpecializesIn.curly,
         -attributes.HairSpecializesIn.kids,
         -attributes.HairSpecializesIn.perms,
         -attributes.HairSpecializesIn.asian,
         -attributes.RestaurantsCounterService,
         -attributes.AgesAllowed,
         -attributes.DietaryRestrictions.vegan,
         -attributes.DietaryRestrictions.kosher,
         -attributes.DietaryRestrictions.halal,
         -attributes.DietaryRestrictions.vegetarian,
         -attributes.Open24Hours,
         -"attributes.DietaryRestrictions.dairy-free", # We need to put quotes around the names to capture the hyphen
         -"attributes.DietaryRestrictions.gluten-free",
         -"attributes.DietaryRestrictions.soy-free")

# Remove blank values
busi[busi==""]<-NA

# After the removal of these 19 columns (81 remaining), we check the character-type columns for errors. 
# One way to do this is with qplots, as most columns allow only a selection of values from the Yelp system.
# Correct columns if necessary

qplot(busi$attributes.RestaurantsTableService)
busi$attributes.RestaurantsTableService <- as.logical(busi$attributes.RestaurantsTableService) #convert to logical

qplot(busi$attributes.WiFi) # error: contains disruptive characters, None = NA?
busi$attributes.WiFi <- str_remove_all(busi$attributes.WiFi, "u'") #correct spelling
busi$attributes.WiFi <- str_remove_all(busi$attributes.WiFi, "'") #correct spelling
busi$attributes.WiFi <- recode(busi$attributes.WiFi, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.WiFi <- as.factor(busi$attributes.WiFi) #convert to factor

qplot(busi$attributes.BikeParking)
busi$attributes.BikeParking <- as.logical(busi$attributes.BikeParking) #convert to logical

qplot(busi$attributes.BusinessParking.garage) # None = NA?
busi$attributes.BusinessParking.garage <- recode(busi$attributes.BusinessParking.garage, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.BusinessParking.garage <- as.logical(busi$attributes.BusinessParking.garage) #convert to logical

qplot(busi$attributes.BusinessParking.street) # None = NA?
busi$attributes.BusinessParking.street <- recode(busi$attributes.BusinessParking.street, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.BusinessParking.street <- as.logical(busi$attributes.BusinessParking.street) #convert to logical

qplot(busi$attributes.BusinessParking.validated) # None = NA?
busi$attributes.BusinessParking.validated <- recode(busi$attributes.BusinessParking.validated, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.BusinessParking.validated <- as.logical(busi$attributes.BusinessParking.validated) #convert to logical

qplot(busi$attributes.BusinessParking.lot)# None = NA?
busi$attributes.BusinessParking.lot <- recode(busi$attributes.BusinessParking.lot, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.BusinessParking.lot <- as.logical(busi$attributes.BusinessParking.lot) #convert to logical

qplot(busi$attributes.BusinessParking.valet) # ok

qplot(busi$attributes.BusinessAcceptsCreditCards) 
busi$attributes.BusinessAcceptsCreditCards <- as.logical(busi$attributes.BusinessAcceptsCreditCards) #convert to logical

qplot(busi$attributes.RestaurantsReservations) # None = NA?
busi$attributes.RestaurantsReservations <- recode(busi$attributes.RestaurantsReservations, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.RestaurantsReservations <- as.logical(busi$attributes.RestaurantsReservations) #convert to logical

qplot(busi$attributes.WheelchairAccessible) 
busi$attributes.WheelchairAccessible <- as.logical(busi$attributes.WheelchairAccessible) #convert to logical

qplot(busi$attributes.Caters) # None = NA?
busi$attributes.Caters <- recode(busi$attributes.Caters, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Caters <- as.logical(busi$attributes.Caters) #convert to logical

qplot(busi$attributes.OutdoorSeating) # None = NA?
busi$attributes.OutdoorSeating <- recode(busi$attributes.OutdoorSeating, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.OutdoorSeating <- as.logical(busi$attributes.OutdoorSeating) #convert to logical

qplot(busi$attributes.RestaurantsGoodForGroups) 
busi$attributes.RestaurantsGoodForGroups <- as.logical(busi$attributes.RestaurantsGoodForGroups) #convert to logical

qplot(busi$attributes.HappyHour) 
busi$attributes.HappyHour <- as.logical(busi$attributes.HappyHour) #convert to logical

qplot(busi$attributes.BusinessAcceptsBitcoin) 
busi$attributes.BusinessAcceptsBitcoin <- as.logical(busi$attributes.BusinessAcceptsBitcoin) #convert to logical

qplot(busi$attributes.RestaurantsPriceRange2) # ok ( four possible values for the price range)
busi$attributes.RestaurantsPriceRange2<-str_replace_all(busi$attributes.RestaurantsPriceRange2, "None", as.character(NA)) #replace None by NA
busi$attributes.RestaurantsPriceRange2 <- as.factor(busi$attributes.RestaurantsPriceRange2) #convert to factor

qplot(busi$attributes.Ambience.touristy) # None = NA?
busi$attributes.Ambience.touristy <- recode(busi$attributes.Ambience.touristy, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.touristy <- as.logical(busi$attributes.Ambience.touristy) #convert to logical

qplot(busi$attributes.Ambience.hipster) # None = NA?
busi$attributes.Ambience.hipster <- recode(busi$attributes.Ambience.hipster, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.hipster <- as.logical(busi$attributes.Ambience.hipster) #convert to logical

qplot(busi$attributes.Ambience.romantic) # None = NA?
busi$attributes.Ambience.romantic <- recode(busi$attributes.Ambience.romantic, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.romantic <- as.logical(busi$attributes.Ambience.romantic) #convert to logical

qplot(busi$attributes.Ambience.divey) # None = NA?
busi$attributes.Ambience.divey <- recode(busi$attributes.Ambience.divey, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.divey <- as.logical(busi$attributes.Ambience.divey) #convert to logical

qplot(busi$attributes.Ambience.intimate) # None = NA?
busi$attributes.Ambience.intimate <- recode(busi$attributes.Ambience.intimate, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.intimate <- as.logical(busi$attributes.Ambience.intimate) #convert to logical

qplot(busi$attributes.Ambience.trendy) # None = NA?
busi$attributes.Ambience.trendy <- recode(busi$attributes.Ambience.trendy, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.trendy <- as.logical(busi$attributes.Ambience.trendy) #convert to logical

qplot(busi$attributes.Ambience.upscale) # None = NA?
busi$attributes.Ambience.upscale <- recode(busi$attributes.Ambience.upscale, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.upscale <- as.logical(busi$attributes.Ambience.upscale) #convert to logical

qplot(busi$attributes.Ambience.classy) # None = NA?
busi$attributes.Ambience.classy <- recode(busi$attributes.Ambience.classy, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.classy <- as.logical(busi$attributes.Ambience.classy) #convert to logical

qplot(busi$attributes.Ambience.casual) # None = NA?
busi$attributes.Ambience.casual <- recode(busi$attributes.Ambience.casual, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Ambience.casual <- as.logical(busi$attributes.Ambience.casual) #convert to logical

qplot(busi$attributes.HasTV) 
busi$attributes.HasTV <- as.logical(busi$attributes.HasTV) #convert to logical

qplot(busi$attributes.Alcohol) # Many spelling errors, None = NA
busi$attributes.Alcohol <- str_remove_all(busi$attributes.Alcohol, "u'") #correct spelling
busi$attributes.Alcohol <- str_remove_all(busi$attributes.Alcohol, "'") #correct spelling
busi$attributes.Alcohol <- str_replace_all(busi$attributes.Alcohol, "None", as.character(NA)) #Replace None by none
busi$attributes.Alcohol <- as.factor(busi$attributes.Alcohol) #convert to factor

qplot(busi$attributes.GoodForMeal.dessert) # None = NA?
busi$attributes.GoodForMeal.dessert <- recode(busi$attributes.GoodForMeal.dessert, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.GoodForMeal.dessert <- as.logical(busi$attributes.GoodForMeal.dessert) #convert to logical

qplot(busi$attributes.GoodForMeal.latenight) # None = NA?
busi$attributes.GoodForMeal.latenight <- recode(busi$attributes.GoodForMeal.latenight, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.GoodForMeal.latenight <- as.logical(busi$attributes.GoodForMeal.latenight) #convert to logical

qplot(busi$attributes.GoodForMeal.lunch) # None = NA?
busi$attributes.GoodForMeal.lunch <- recode(busi$attributes.GoodForMeal.lunch, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.GoodForMeal.lunch <- as.logical(busi$attributes.GoodForMeal.lunch) #convert to logical

qplot(busi$attributes.GoodForMeal.dinner) # None = NA?
busi$attributes.GoodForMeal.dinner <- recode(busi$attributes.GoodForMeal.dinner, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.GoodForMeal.dinner <- as.logical(busi$attributes.GoodForMeal.dinner) #convert to logical

qplot(busi$attributes.GoodForMeal.brunch) # None = NA?
busi$attributes.GoodForMeal.brunch <- recode(busi$attributes.GoodForMeal.brunch, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.GoodForMeal.brunch <- as.logical(busi$attributes.GoodForMeal.brunch) #convert to logical

qplot(busi$attributes.GoodForMeal.breakfast) # None = NA?
busi$attributes.GoodForMeal.breakfast <- recode(busi$attributes.GoodForMeal.breakfast, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.GoodForMeal.breakfast <- as.logical(busi$attributes.GoodForMeal.breakfast) #convert to logical

qplot(busi$attributes.DogsAllowed)
busi$attributes.DogsAllowed <- as.logical(busi$attributes.DogsAllowed) #convert to logical

qplot(busi$attributes.RestaurantsTakeOut) # None = NA?
busi$attributes.RestaurantsTakeOut <- recode(busi$attributes.RestaurantsTakeOut, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.RestaurantsTakeOut <- as.logical(busi$attributes.RestaurantsTakeOut) #convert to logical

qplot(busi$attributes.NoiseLevel) # Spelling errors, None = NA
busi$attributes.NoiseLevel <- str_remove_all(busi$attributes.NoiseLevel, "u'") #correct spelling
busi$attributes.NoiseLevel <- str_remove_all(busi$attributes.NoiseLevel, "'") #correct spelling
busi$attributes.NoiseLevel <- str_replace_all(busi$attributes.NoiseLevel, "None", as.character(NA)) #replace None by NA
busi$attributes.NoiseLevel <- as.factor(busi$attributes.NoiseLevel) #convert to factor

qplot(busi$attributes.RestaurantsAttire) # Spelling errors
busi$attributes.RestaurantsAttire <- str_remove_all(busi$attributes.RestaurantsAttire, "u'") #correct spelling
busi$attributes.RestaurantsAttire <- str_remove_all(busi$attributes.RestaurantsAttire, "'") #correct spelling
busi$attributes.RestaurantsAttire <- str_replace_all(busi$attributes.RestaurantsAttire, "None", as.character(NA)) #replace None by NA
busi$attributes.RestaurantsAttire <- as.factor(busi$attributes.RestaurantsAttire) #convert to factor


qplot(busi$attributes.RestaurantsDelivery) # None = NA?
busi$attributes.RestaurantsDelivery <- recode(busi$attributes.RestaurantsDelivery, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.RestaurantsDelivery <- as.logical(busi$attributes.RestaurantsDelivery) #convert to logical

qplot(busi$attributes.GoodForKids) 
busi$attributes.GoodForKids <- as.logical(busi$attributes.GoodForKids) #convert to logical

qplot(busi$attributes.ByAppointmentOnly) 
busi$attributes.ByAppointmentOnly <- as.logical(busi$attributes.ByAppointmentOnly) #convert to logical

qplot(busi$attributes.GoodForDancing) 
busi$attributes.GoodForDancing <- as.logical(busi$attributes.GoodForDancing) #convert to logical

qplot(busi$attributes.BestNights.monday) # ok

qplot(busi$attributes.BestNights.tuesday) # ok

qplot(busi$attributes.BestNights.friday) # ok

qplot(busi$attributes.BestNights.wednesday) # ok

qplot(busi$attributes.BestNights.thursday) # ok

qplot(busi$attributes.BestNights.sunday) # ok

qplot(busi$attributes.BestNights.saturday) # ok

qplot(busi$attributes.Music.dj) # None = NA?
busi$attributes.Music.dj <- recode(busi$attributes.Music.dj, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Music.dj <- as.logical(busi$attributes.Music.dj) #convert to logical

qplot(busi$attributes.Music.background_music) # ok

qplot(busi$attributes.Music.no_music) # ok ( only false or NA )

qplot(busi$attributes.Music.background_music) # ok

qplot(busi$attributes.Music.jukebox) # None = NA?
busi$attributes.Music.jukebox <- recode(busi$attributes.Music.jukebox, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Music.jukebox <- as.logical(busi$attributes.Music.jukebox) #convert to logical

qplot(busi$attributes.Music.live) # None = NA?
busi$attributes.Music.live <- recode(busi$attributes.Music.live, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Music.live <- as.logical(busi$attributes.Music.live) #convert to logical

qplot(busi$attributes.Music.video) # ok

qplot(busi$attributes.Music.karaoke) # None = NA?
busi$attributes.Music.karaoke <- recode(busi$attributes.Music.karaoke, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Music.karaoke <- as.logical(busi$attributes.Music.karaoke) #convert to logical

qplot(busi$attributes.BYOB) 
busi$attributes.BYOB <- as.logical(busi$attributes.BYOB) #convert to logical

qplot(busi$attributes.CoatCheck)
busi$attributes.CoatCheck <- as.logical(busi$attributes.CoatCheck) #convert to logical

qplot(busi$attributes.Smoking) # Spelling errors, many NAs, None = NA?
busi$attributes.Smoking <- str_remove_all(busi$attributes.Smoking, "u'") #spelling error
busi$attributes.Smoking <- str_remove_all(busi$attributes.Smoking, "'") #spelling error
busi$attributes.Smoking <- str_replace_all(busi$attributes.Smoking, "None", as.character(NA)) #replace None by NA
busi$attributes.Smoking <- as.factor(busi$attributes.Smoking) #convert to factor

qplot(busi$attributes.DriveThru) # None = NA?
busi$attributes.DriveThru <- recode(busi$attributes.DriveThru, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.DriveThru <- as.logical(busi$attributes.DriveThru) #convert to logical

qplot(busi$attributes.BYOBCorkage) # Spelling errors, many Nas
busi$attributes.BYOBCorkage <- str_remove_all(busi$attributes.BYOBCorkage, "u'") #spelling error
busi$attributes.BYOBCorkage <- str_remove_all(busi$attributes.BYOBCorkage, "'") #spelling error
busi$attributes.BYOBCorkage <- str_replace_all(busi$attributes.BYOBCorkage, "None", as.character(NA)) #replace None by NA
busi$attributes.BYOBCorkage <- as.factor(busi$attributes.BYOBCorkage) #convert to factor

qplot(busi$attributes.Corkage) # None = NA?
busi$attributes.Corkage <- recode(busi$attributes.Corkage, None = as.character(NA)) #clean up the value differences of None, No and False. Mean all the same
busi$attributes.Corkage <- as.logical(busi$attributes.Corkage) #convert to logical

qplot(busi$hours.Monday) # Hours need to be worked on, maybe not so useful for our analysis.
qplot(busi$hours.Tuesday) # 
qplot(busi$hours.Wednesday) # 
qplot(busi$hours.Thursday) # 
qplot(busi$hours.Friday) # 
qplot(busi$hours.Saturday) # 
qplot(busi$hours.Sunday) # 



# Great, our dataset is somewhat ready for the analysis!
skim(busi) #Column type frequency:  character 20  logical 55  numeric 6 

# Write remaining obseravtions into new csv file
fwrite(busi, file="businessSmall.csv")

# Clean up of userLarge.csv to convert it to a smaller csv file ####

# Import user file with particular variables
user <- fread("userLarge.csv", select=c(user_id="character", review_count="numeric", useful="numeric", average_stars="numeric", elite = "character")) #fastest read method according to benchmark test

# Look at data

## Structure of data
str(user)
head(user)

## How are the number of reviews distributed on the lower end?
ggplot(user, aes(x=review_count))+
  geom_histogram(binwidth = 1, color="red")+
  scale_x_continuous(breaks = seq(0, 100, 1), limits = c(0,100)) #a lot of users with 1 review

## Are there users with 0 reviews?
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
fwrite(userNew, "userSmall.csv")

# Remove objects and collect garbage ####
rm(list=ls())
gc()
