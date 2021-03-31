# Load dependencies (if not done already)
source("packageDependencies.R")

#Analysis of businessSmall.csv file ####
#What is the determining factor for a good star rating of a restaurant?

# Load data
business<-fread("businessSmall.csv")

# Replace empty strings
business[business==""]<-NA

# Prepare Data 
business1<-business%>%
  select(stars, attributes.RestaurantsTableService:attributes.Corkage)%>% #Select variables relevant for star rating
  mutate_if(is.character, as.factor)%>% #Convert character variables to factors
  mutate(attributes.RestaurantsPriceRange2=as.factor(attributes.RestaurantsPriceRange2))%>% #Convert restaurant price range to factor
  mutate_if(is.logical, as.numeric) #convert logical to numeric (dummies)

  
businessNoNA<-na.omit(business1)
# No observation remains

# we will focus on variables that apply to a large number of restaurants. Yelp also has variables that apply to niche restaurants. 
# An example of this would be a karaoke restaurant..
summary(business1)

# On the first step we select covariates with less than 20000NAs into the final dataset for the analysis.
# We need to have a dataset, which is big eneough to make an anaylsis.
business2<-business1%>%select(stars:attributes.RestaurantsReservations, attributes.Caters:attributes.RestaurantsGoodForGroups,
                              attributes.RestaurantsPriceRange2:attributes.GoodForMeal.breakfast, attributes.RestaurantsTakeOut:attributes.GoodForKids)

# Removing blank spots 
df<-na.omit(business2)

# All NA's removed
summary(df)

# Lasso and Ridge Regression ####

summary(df)

ols <- lm(stars ~ ., data = df)
summary(ols)

ols$coefficients[-1][which(ols$coefficients[-1]==max(ols$coefficients[-1]))]
#the most important attribute for the star rating seems to be the Business Parking Street. This is positive correlated with the rating. 
#If you want to see the results separately or save them, then uncomment the code below.
#stargazer::stargazer(ols, type = "text", out =   "ols Yelp")


#Lasso and ridge were only applied to see which attributes were seen as important.
#lasso 
df_ml<-df%>%
  mutate(dummy_cols(., select_columns = "attributes.WiFi"))%>% #create dummies for attributes.WiFi levels
  mutate(dummy_cols(., select_columns = "attributes.Alcohol"))%>%
  mutate(dummy_cols(., select_columns = "attributes.NoiseLevel"))%>%
  mutate(dummy_cols(., select_columns = "attributes.RestaurantsAttire"))%>%
  mutate(dummy_cols(., select_columns = "attributes.RestaurantsPriceRange2"))%>%
  select(-attributes.WiFi,  -attributes.Alcohol, -attributes.NoiseLevel,
         -attributes.RestaurantsAttire, -attributes.RestaurantsPriceRange2)%>% #remove original factor variables
select(-attributes.WiFi_free, #remove one dummy per original factor variable to prevent dummy trap
         -attributes.Alcohol_beer_and_wine, 
         -attributes.NoiseLevel_average, 
         -attributes.RestaurantsAttire_casual,
         -attributes.RestaurantsPriceRange2_1,
         )
  

str(df_ml)
summary(df_ml)

lasso <- glmnet(as.matrix(df_ml[,c(2:44)]), df_ml$stars, alpha = 1) 
plot(lasso, xvar = "lambda", label = TRUE)
# on the very left the lasso model is similar to the ols. On the very right we have a sparse model.
# The sparse model consists of two covariates.
# The further to the right the variables are, the more decisive these variables are for the rating.
# The two most important varaibles are the vars 5 and 23
names(df_ml[,5]) #<--Output
names(df_ml[,23]) #<--Output


# alpha = 0 specifies a Ridge model

# Estimate the Ridge
ridge <- glmnet(as.matrix(df_ml[,c(2:44)]), df_ml$stars, alpha = 0)

# Plot the path of the Ridge coefficients
plot(ridge, xvar = "lambda", label = TRUE)

#Again Variable 5 Business Parking street is the most important

# Print Ridge coefficients 


# Forward Selection ####
forward<-regsubsets(stars~.,data=df ,nvmax=50, method ="forward", intercept = FALSE) #forward selection of variables
overview<-summary(forward)$which #logical matrix showing which variable is in what forward model

which(overview[1,]==TRUE) #<-----------------------------------------------------------------------------------------------------------------------------------Output 
# Most important variable "Business accepts credits cards"

# Data frame with forward selection results
variableSelection<-data.frame(x=1:forward$last, y=summary(forward)$adjr2, best=factor(c(1, rep(0,forward$last-1))), label=c(names(which(overview[1,]==TRUE)), rep("",forward$last-1)))

# Plot forward selection results
ggplot(variableSelection, aes(x=x, y=y, label=label))+ #<-----------------------------------------------------------------------------------------------------------Output
  geom_line()+
  geom_point(data = variableSelection, aes(color=best))+
  geom_text(hjust=-0.1, color="red")+
  xlab("Number of Variables")+
  ylab("Adjusted R^2")+
  ggtitle("Change of adjusted R^2 when including more variables")+ 
  theme_bw()+
  scale_y_continuous(breaks=seq(0.94, 0.97, 0.001))+
  scale_color_manual(values=c("black", "red"))+
  theme(legend.position = "none")

## The variable business accepts credits cards already explains around 94.8% of the variance in the average stars rating
## Note this forward selection is a greedy approach. In each iteration the variable reducing the variance the most is chosen. This does not 
## necessarely mean that the selected variables are also the best combination of variables. 

# User Analysis ####
# We estimate the effect of average stars assigned per review on the probability of 
# being an elite user (Uluru OLS)

# For this, we need the user data with the user's average stars, a dummy variable for elite status, 
# and potentially the user's review-count and the rated usefulness of his reviews

user <- fread("userSmall.csv") # file size 84.4 MB
skim(user)
# +2MM observations, 5 rows
# user_id, review_count, average_stars, eliteDummy, usefulPerReview
# 4.36% are elite users

'
The goal is to use the Uluru alogrithm for the ols estimators, but we go there step by step.
Steps:
1)  lm()
2)  manual ols
2.1)    calculations form economictheoryblog.com
2.2)    function from the lecture
3)  uluru ols (for the simple model)
4)  uluru for the other models
5)  computational time comparisons
'


# 1)
# We try three different regressions using the built in lm() function and save the results for later.
simple_ols <- lm(data = user, formula = eliteDummy ~ average_stars)
summary(simple_ols)

extended_ols <- lm(data = user, formula = eliteDummy ~ average_stars + usefulPerReview)
summary(extended_ols)

full_ols <- lm(data = user, formula = eliteDummy ~ average_stars + usefulPerReview + review_count)
summary(full_ols)

stargazer(simple_ols, extended_ols, full_ols, title="Results", align=TRUE,  type="text")
# be aware of the small R2.

# 2) manual ols, to get to the uluru form

#  define X matrix and y vector
X <- as.matrix(cbind(1,user$average_stars)) # start out with the simple ols
y <- as.matrix(user$eliteDummy)

# 2.1) from https://economictheoryblog.com/2016/02/20/rebuild-ols-estimator-manually-in-r/
#  estimate the coeficients beta
#  beta = ((X'X)^(-1))X'y
beta <- solve(t(X)%*%X)%*%t(X)%*%y
## calculate residuals
#  res = y - beta1 - beta2*X2
res <- as.matrix(y-beta[1]-beta[2]*X[,2])
## define the number of observations (n) and the number of
#  parameters (k)
n <- nrow(user)
k <- ncol(X)
## calculate the Variance-Covariance matrix (VCV)
#  VCV = (1/(n-k))res'res(X'X)^(-1)
VCV <- 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X)
## calculate standard errors (se) of coefficients
se <- sqrt(diag(VCV))
## calculate the p-values
p_value <- rbind(2*pt(abs(beta[1]/se[1]), df=n-k,
                      lower.tail= FALSE),
                 2*pt(abs(beta[2]/se[2]), df=n-k,
                      lower.tail= FALSE))
## combine all necessary information
output <- as.data.frame(cbind(c("(Intercept)","height"),
                              beta,se,p_value))
names(output) <- c("Coefficients:","Estimate", 
                   "Std. Error","Pr(>|t|)")
# output is in line with the lm() results

# 2.2) formula from the lecture
beta_ols <- 
  function(X, y) {
    
    # compute cross products and inverse
    XXi <- solve(crossprod(X,X))
    Xy <- crossprod(X, y) 
    
    return( XXi  %*% Xy )
  }
beta_ols(X, y)
# same results as with lm() and the previous manual build.


# 3) Uluru ols, from the slides
beta_uluru <-
  function(X_subs, y_subs, X_rem, y_rem) {
    
    # compute beta_fs (this is simply OLS applied to the subsample)
    XXi_subs <- solve(crossprod(X_subs, X_subs))
    Xy_subs <- crossprod(X_subs, y_subs)
    b_fs <- XXi_subs  %*% Xy_subs
    
    # compute \mathbf{R}_{rem}
    R_rem <- y_rem - X_rem %*% b_fs
    
    # compute \hat{\beta}_{correct}
    b_correct <- (nrow(X_subs)/(nrow(X_rem))) * XXi_subs %*% crossprod(X_rem, R_rem)
    
    # beta uluru       
    return(b_fs + b_correct)
  }

# set size of subsample
n_subs <- 10000
# select subsample and remainder
n_obs <- nrow(X)
#create row indeces
sample_obs<-sample(1L:nrow(X), n_subs, replace=F)
#subset X and y
X_subs <- X[sample_obs,]
y_subs <- y[sample_obs, ]
X_rem <- X[-sample_obs,]
y_rem <- y[-sample_obs,]

# apply the uluru estimator
simple_ols_uluru <- beta_uluru(X_subs, y_subs, X_rem, y_rem)
simple_ols_uluru
simple <- as_tibble(cbind(simple_ols$coefficients, simple_ols_uluru))
simple <- simple %>%
  rename("lm()" = V1,
         "Uluru" = V2)

# pretty much the same results for the simple ols as with the previous methods! 


# 4)
# Uluru for the extended_ols and full_ols.

# extended_ols:
X <- as.matrix(cbind(1,user$average_stars, user$usefulPerReview))
# set size of subsample
n_subs <- 10000
# select subsample and remainder
n_obs <- nrow(X)
#create row indeces
sample_obs<-sample(1L:nrow(X), n_subs, replace=F)
#subset X and y
X_subs <- X[sample_obs,]
y_subs <- y[sample_obs, ]
X_rem <- X[-sample_obs,]
y_rem <- y[-sample_obs,]
# apply the uluru estimator
extended_ols_uluru <- beta_uluru(X_subs, y_subs, X_rem, y_rem)
extended_ols_uluru
extended <- as_tibble(cbind(extended_ols$coefficients, extended_ols_uluru))
extended <- extended %>%
  rename("lm()" = V1,
         "Uluru" = V2)

# full_ols:
X <- as.matrix(cbind(1,user$average_stars, user$usefulPerReview, user$review_count)) 
# set size of subsample
n_subs <- 10000
# select subsample and remainder
n_obs <- nrow(X)
#create row indeces
sample_obs<-sample(1L:nrow(X), n_subs, replace=F)
#subset X and y
X_subs <- X[sample_obs,]
y_subs <- y[sample_obs, ]
X_rem <- X[-sample_obs,]
y_rem <- y[-sample_obs,]
# apply the uluru estimator
full_ols_uluru <- beta_uluru(X_subs, y_subs, X_rem, y_rem)
full_ols_uluru
full <- as_tibble(cbind(full_ols$coefficients, full_ols_uluru))
full <- full %>%
  rename("lm()" = V1,
         "Uluru" = V2)

# 5)

# define subsamples
n_subs_sizes <- seq(from = 1000, to = 500000, by=5000)
n_runs <- length(n_subs_sizes)
# compute uluru result, stop time
mc_results <- rep(NA, n_runs)
mc_times <- rep(NA, n_runs)
for (i in 1:n_runs) {
  # set size of subsample
  n_subs <- n_subs_sizes[i]
  # select subsample and remainder
  n_obs <- nrow(X)
  # create row indeces
  sample_obs<-sample(1L:nrow(X), n_subs, replace=F)
  # subset X and y
  X_subs <- X[sample_obs,]
  y_subs <- y[sample_obs,]
  X_rem <- X[-sample_obs,]
  y_rem <- y[-sample_obs,]
  
  mc_results[i] <- beta_uluru(X_subs, y_subs, X_rem, y_rem)[2] # the first element is the intercept
  mc_times[i] <- system.time(beta_uluru(X_subs, y_subs, X_rem, y_rem))[3]
  
}

# compute ols results and ols time
ols_time <- system.time(beta_ols(X, y))
ols_res <- beta_ols(X, y)[2]
# Let's visualize the comparison with OLS.
# prepare data to plot
plotdata <- data.frame(beta1 = mc_results,
                       time_elapsed = mc_times,
                       subs_size = n_subs_sizes)
fwrite(x = plotdata, file = "uluru_ols_benchmark_data.csv")
# First, let's look at the time used estimate the linear model.
ggplot(plotdata, aes(x = subs_size, y = time_elapsed)) +
  geom_point(color="darkgreen") + 
  geom_hline(yintercept = ols_time[3],
             color = "red", 
             size = 1) +
  theme_minimal() +
  ylab("Time elapsed") +
  xlab("Subsample size")

# Finally, let's have a look at how close the results are to OLS.
ggplot(plotdata, aes(x = subs_size, y = beta1)) +
  geom_hline(yintercept = ols_res,
             color = "red", 
             size = 1) +
  geom_hline(yintercept = 1.5,
             color = "green",
             size = 1) +
  geom_point(color="darkgreen") + 
  
  theme_minimal() +
  ylab("Estimated coefficient") +
  xlab("Subsample size")

pdf("uluru_ols_benchmark.pdf")
dev.off()