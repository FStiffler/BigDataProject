# Test speed of businessLarge.csv and userLarge.csv import using four different methods, measured with microbenchmark
source("packageDependencies.R")


# Microbenchmark business file: ####
results_business <- microbenchmark(
  'base::read.csv' = read.csv("businessLarge.csv"),
  'readr::read_csv' = read_csv("businessLarge.csv"),
  'vroom::vroom' = vroom("businessLarge.csv"),
  'data.table::fread' = fread("businessLarge.csv"),
  times = 25L
)

#Plot results
autoplot(results_business) #<---Output for Presentation

# Microbenchmark business file: ####
results_user <- microbenchmark(
  
  'readr::read_csv' = read_csv("userLarge.csv", 
                               col_types = cols_only(user_id = "c", review_count="d",useful="d",average_stars="d",elite="c")),
    
  'vroom::vroom' = vroom("userLarge.csv", 
                         col_select = c(user_id, review_count, useful, average_stars, elite),
                         col_types = c(elite = "c")),
  
  'data.table::fread' = fread("userLarge.csv",
                              select=c(user_id="character", review_count="numeric", useful="numeric", average_stars="numeric", elite = "character")),
  
  times = 10L
)

#Plot results
autoplot(results_user) #<---Output for Presentation



