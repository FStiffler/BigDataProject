# Script to clean up the business dataset, after converting to csv format.
library(tidyverse)
library(vroom)
library(data.table)

# Make sure R is in 64 bits, for better performance
sys.info()[["machine"]] # check output

# import the Yelp business dataset, make sure you have converted the json format to csv format first.
busi <- vroom("CSVFiles/business.csv") # fastest read method, alternatively use fread("CSVFiles/business.csv") (make benchmark for presentation)
glimpse(busi)
summary(busi)

# First Problem: Multiple different categories per business. 22 categories with ~1200 subcategories (https://blog.yelp.com/2018/01/yelp_category_list#section21)
# We can limit our analysis to the restaurants parent-category.
busi <- 
  busi %>%
  filter(grepl("Restaurant", categories))
# 1/3 of the observations remaining (50k)

# Some businesses are closed
qplot(busi$is_open)
# to further reduce our data load, we can restrict our analysis on restaurants that were still in business at the time of data collection.
busi <- 
  busi %>%
  filter(is_open==1)
# 32k observations remaining

# Now, lets check for columns that need to be cleaned up or removed.
summary(busi)

# we remove all columns that contain +95% NAs.
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
         -attributes.Open24Hours)

# How to remove these columns? Cannot use select() because of the Hyphen...
############### Achtung Error #########################################
-attributes.DietaryRestrictions.dairy-free,
-attributes.DietaryRestrictions.gluten-free,
-attributes.DietaryRestrictions.soy-free,

# After the removal of these columns, we check the character-type columns for errors.
# One way to do this is with qplots, as most columns allow only a selection of values from the Yelp system.

# visualize attributes to check for errors
qplot(busi$attributes.RestaurantsTableService) # ok
qplot(busi$attributes.WiFi) # error: contains disruptive characters
qplot(busi$attributes.BikeParking) # ok
qplot(busi$attributes.BusinessParking.garage) # None = False?
qplot(busi$attributes.BusinessParking.street) # None = False?
qplot(busi$attributes.BusinessParking.validated) # None = False?
qplot(busi$attributes.BusinessParking.lot)# None = False?
qplot(busi$attributes.BusinessParking.valet) # ok
qplot(busi$attributes.BusinessAcceptsCreditCards) # ok
qplot(busi$attributes.RestaurantsReservations) # ok
qplot(busi$attributes.WheelchairAccessible) # ok
qplot(busi$attributes.Caters) # ok
qplot(busi$attributes.OutdoorSeating) # None = False?
qplot(busi$attributes.RestaurantsGoodForGroups) # ok
qplot(busi$attributes.HappyHour) # ok
qplot(busi$attributes.BusinessAcceptsBitcoin) # ok
qplot(busi$attributes.RestaurantsPriceRange2) # ok ( four possible values for the price range)
qplot(busi$attributes.Ambience.touristy) # None = False?
qplot(busi$attributes.Ambience.hipster) # ok
qplot(busi$attributes.Ambience.romantic) # None = False?
qplot(busi$attributes.Ambience.divey) # None = False?
qplot(busi$attributes.Ambience.intimate) # None = False?
qplot(busi$attributes.Ambience.trendy) # None = False?
qplot(busi$attributes.Ambience.upscale) # None = False?
qplot(busi$attributes.Ambience.classy) # None = False?
qplot(busi$attributes.Ambience.casual) # None = False?
qplot(busi$attributes.HasTV) # ok
qplot(busi$attributes.Alcohol) # Many spelling errors
qplot(busi$attributes.GoodForMeal.dessert) # None = False?
qplot(busi$attributes.GoodForMeal.latenight) # None = False?
qplot(busi$attributes.GoodForMeal.lunch) # None = False?k
qplot(busi$attributes.GoodForMeal.dinner) # ok
qplot(busi$attributes.GoodForMeal.brunch) # None = False?
qplot(busi$attributes.GoodForMeal.breakfast) # None = False?
qplot(busi$attributes.DogsAllowed) # ok
qplot(busi$attributes.RestaurantsTakeOut) # None = False?
qplot(busi$attributes.NoiseLevel) # Spelling errors
qplot(busi$attributes.RestaurantsAttire) # Spelling errors
qplot(busi$attributes.RestaurantsDelivery) # None = False?
qplot(busi$attributes.GoodForKids) # ok
qplot(busi$attributes.RestaurantsDelivery) # None = False?
qplot(busi$attributes.ByAppointmentOnly) # ok
qplot(busi$attributes.GoodForDancing) # ok
qplot(busi$attributes.BestNights.monday) # ok
qplot(busi$attributes.BestNights.tuesday) # ok
qplot(busi$attributes.BestNights.friday) # ok
qplot(busi$attributes.BestNights.wednesday) # ok
qplot(busi$attributes.BestNights.thursday) # ok
qplot(busi$attributes.BestNights.sunday) # ok
qplot(busi$attributes.BestNights.saturday) # ok
qplot(busi$attributes.Music.dj) # ok
qplot(busi$attributes.Music.background_music) # ok
qplot(busi$attributes.Music.no_music) # ok ( only false or NA )
qplot(busi$attributes.Music.background_music) # ok
qplot(busi$attributes.Music.jukebox) # ok
qplot(busi$attributes.Music.live) # ok
qplot(busi$attributes.Music.video) # ok
qplot(busi$attributes.Music.karaoke) # ok
qplot(busi$attributes.BYOB) # ok
qplot(busi$attributes.CoatCheck) # ok
qplot(busi$attributes.Smoking) # Spelling errors, many NAs
qplot(busi$attributes.DriveThru) # ok
qplot(busi$attributes.BYOBCorkage) # Spelling errors, many Nas
qplot(busi$attributes.Corkage) # ok
qplot(busi$hours.Monday) # Hours need to be worked on, maybe not so useful for our analysis.
qplot(busi$hours.Tuesday) # 
qplot(busi$hours.Wednesday) # 
qplot(busi$hours.Thursday) # 
qplot(busi$hours.Friday) # 
qplot(busi$hours.Saturday) # 
qplot(busi$hours.Sunday) # 

# Now, lets clean up the attributes

# Test the clean up with the WiFi attribute
busi$attributes.WiFi <- str_remove_all(busi$attributes.WiFi, "'")
busi$attributes.WiFi <- str_remove_all(busi$attributes.WiFi, "u")

# doesnt work in the pipe, wtf
busi <- 
  busi %>%
  str_remove_all(attributes.WiFi, "'")


### Clean up the other annoted attributes

### Drop columns that we dont need (e.g. Adress, Name, etc.)

# import the other files, not relevant rn
review <- vroom("CSVFiles/reviews.csv")
user <- vroom("CSVFiles/user.csv")