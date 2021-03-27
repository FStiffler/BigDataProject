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
         -attributes.Open24Hours,
         -"attributes.DietaryRestrictions.dairy-free", # We need to put quotes around the names to capture the hyphen
         -"attributes.DietaryRestrictions.gluten-free",
         -"attributes.DietaryRestrictions.soy-free")


# After the removal of these 19 columns (81 remaining), we check the character-type columns for errors. 
# One way to do this is with qplots, as most columns allow only a selection of values from the Yelp system.

qplot(busi$attributes.RestaurantsTableService) # ok
qplot(busi$attributes.WiFi) # error: contains disruptive characters, None = no?
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
qplot(busi$attributes.Alcohol) # Many spelling errors, None = none
qplot(busi$attributes.GoodForMeal.dessert) # None = False?
qplot(busi$attributes.GoodForMeal.latenight) # None = False?
qplot(busi$attributes.GoodForMeal.lunch) # None = False?
qplot(busi$attributes.GoodForMeal.dinner) # ok
qplot(busi$attributes.GoodForMeal.brunch) # None = False?
qplot(busi$attributes.GoodForMeal.breakfast) # None = False?
qplot(busi$attributes.DogsAllowed) # ok
qplot(busi$attributes.RestaurantsTakeOut) # None = False?
qplot(busi$attributes.NoiseLevel) # Spelling errors, None = NA
qplot(busi$attributes.RestaurantsAttire) # Spelling errors
qplot(busi$attributes.RestaurantsDelivery) # None = False?
qplot(busi$attributes.GoodForKids) # ok
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
qplot(busi$attributes.Smoking) # Spelling errors, many NAs, None = no?
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

# Correct the spelling errors
# somehow I can't manage to put multiple strings that need to be removed in one str_remove command,
# nor does the str_remove work in the pipe %>%

busi$attributes.WiFi <- str_remove_all(busi$attributes.WiFi, "u'")
busi$attributes.WiFi <- str_remove_all(busi$attributes.WiFi, "'")

busi$attributes.Alcohol <- str_remove_all(busi$attributes.Alcohol, "u'")
busi$attributes.Alcohol <- str_remove_all(busi$attributes.Alcohol, "'")

busi$attributes.NoiseLevel <- str_remove_all(busi$attributes.NoiseLevel, "u'")
busi$attributes.NoiseLevel <- str_remove_all(busi$attributes.NoiseLevel, "'")

busi$attributes.RestaurantsAttire <- str_remove_all(busi$attributes.RestaurantsAttire, "u'")
busi$attributes.RestaurantsAttire <- str_remove_all(busi$attributes.RestaurantsAttire, "'")

busi$attributes.Smoking <- str_remove_all(busi$attributes.Smoking, "u'")
busi$attributes.Smoking <- str_remove_all(busi$attributes.Smoking, "'")

busi$attributes.BYOBCorkage <- str_remove_all(busi$attributes.BYOBCorkage, "u'")
busi$attributes.BYOBCorkage <- str_remove_all(busi$attributes.BYOBCorkage, "'")

# All attributes are spelled correctly now. However, we need to clean up the value differences of None, No and False. 
# Presumably, these three specifications mean the same thing.

busi$attributes.WiFi <- recode(busi$attributes.WiFi, None = "no")

busi$attributes.Alcohol <- recode(busi$attributes.Alcohol, None = "none")

busi$attributes.NoiseLevel <- na_if(busi$attributes.NoiseLevel, "None")

busi$attributes.RestaurantsAttire <- na_if(busi$attributes.RestaurantsAttire, "None")

busi$attributes.Smoking <- recode(busi$attributes.Smoking, None = "no")

busi$attributes.BYOBCorkage <- recode(busi$attributes.BYOBCorkage, None = "no")

busi$attributes.BusinessParking.garage <- recode(busi$attributes.BusinessParking.garage, None = "False")
busi$attributes.BusinessParking.garage <- as.logical(busi$attributes.BusinessParking.garage)

busi$attributes.BusinessParking.street <- recode(busi$attributes.BusinessParking.street, None = "False")
busi$attributes.BusinessParking.street <- as.logical(busi$attributes.BusinessParking.street)

busi$attributes.BusinessParking.validated <- recode(busi$attributes.BusinessParking.validated, None = "False")
busi$attributes.BusinessParking.validated <- as.logical(busi$attributes.BusinessParking.validated)

busi$attributes.BusinessParking.lot <- recode(busi$attributes.BusinessParking.lot, None = "False")
busi$attributes.BusinessParking.lot <- as.logical(busi$attributes.BusinessParking.lot)

busi$attributes.OutdoorSeating <- recode(busi$attributes.OutdoorSeating, None = "False")
busi$attributes.OutdoorSeating <- as.logical(busi$attributes.OutdoorSeating)

busi$attributes.Ambience.touristy <- recode(busi$attributes.Ambience.touristy, None = "False")
busi$attributes.Ambience.touristy <- as.logical(busi$attributes.Ambience.touristy)

busi$attributes.Ambience.romantic <- recode(busi$attributes.Ambience.romantic, None = "False")
busi$attributes.Ambience.romantic <- as.logical(busi$attributes.Ambience.romantic)

busi$attributes.Ambience.divey <- recode(busi$attributes.Ambience.divey, None = "False")
busi$attributes.Ambience.divey <- as.logical(busi$attributes.Ambience.divey)

busi$attributes.Ambience.intimate <- recode(busi$attributes.Ambience.intimate, None = "False")
busi$attributes.Ambience.intimate <- as.logical(busi$attributes.Ambience.intimate)

busi$attributes.Ambience.trendy <- recode(busi$attributes.Ambience.trendy, None = "False")
busi$attributes.Ambience.trendy <- as.logical(busi$attributes.Ambience.trendy)

busi$attributes.Ambience.upscale <- recode(busi$attributes.Ambience.upscale, None = "False")
busi$attributes.Ambience.upscale <- as.logical(busi$attributes.Ambience.upscale)

busi$attributes.Ambience.classy <- recode(busi$attributes.Ambience.classy, None = "False")
busi$attributes.Ambience.classy <- as.logical(busi$attributes.Ambience.classy)

busi$attributes.Ambience.casual <- recode(busi$attributes.Ambience.casual, None = "False")
busi$attributes.Ambience.casual <- as.logical(busi$attributes.Ambience.casual)

busi$attributes.GoodForMeal.dessert <- recode(busi$attributes.GoodForMeal.dessert, None = "False")
busi$attributes.GoodForMeal.dessert <- as.logical(busi$attributes.GoodForMeal.dessert)

busi$attributes.GoodForMeal.latenight <- recode(busi$attributes.GoodForMeal.latenight, None = "False")
busi$attributes.GoodForMeal.latenight <- as.logical(busi$attributes.GoodForMeal.latenight)

busi$attributes.GoodForMeal.lunch <- recode(busi$attributes.GoodForMeal.lunch, None = "False")
busi$attributes.GoodForMeal.lunch <- as.logical(busi$attributes.GoodForMeal.lunch)

busi$attributes.GoodForMeal.brunch <- recode(busi$attributes.GoodForMeal.brunch, None = "False")
busi$attributes.GoodForMeal.brunch <- as.logical(busi$attributes.GoodForMeal.brunch)

busi$attributes.GoodForMeal.breakfast <- recode(busi$attributes.GoodForMeal.breakfast, None = "False")
busi$attributes.GoodForMeal.breakfast <- as.logical(busi$attributes.GoodForMeal.breakfast)

busi$attributes.RestaurantsTakeOut <- recode(busi$attributes.RestaurantsTakeOut, None = "False")
busi$attributes.RestaurantsTakeOut <- as.logical(busi$attributes.RestaurantsTakeOut)

busi$attributes.RestaurantsDelivery <- recode(busi$attributes.RestaurantsDelivery, None = "False")
busi$attributes.RestaurantsDelivery <- as.logical(busi$attributes.RestaurantsDelivery)

# Great, our dataset is somewhat ready for the analysis!

unique(busi$attributes.RestaurantsAttire) # maybe remove this column?

# other columns for removal: is_open , maybe the hours.weekday series

# Write the clean file:
vroom_write(busi, "busivroom.csv", delim = ",")
# 15.7 MB (small size increase after the as.logical changes)

fwrite(busi, file="busiclean.csv")
# 13.8 MB (somehow the file got smaller)

########## To get fewer lines, I tried all of these methods, without success...... ###############
busi <- recode(busi, None = "False")

busi <- 
  busi %>% 
  mutate_all(
    function(x) case_when(
      x == "None" ~ "False"
      )
    )

busi <- 
  busi %>% 
  mutate_all(funs(str_replace(., "None", "False")))

busi <- sapply(busi,function(x) {x <- gsub("None","False",x)})

# import the other files, not relevant rn
review <- vroom("CSVFiles/reviews.csv")
user <- vroom("CSVFiles/user.csv")
