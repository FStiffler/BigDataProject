# We estimate the effect of average stars assigned per review on the probability of 
# being an elite user (Uluru OLS)

# For this, we need the user data with the user's average stars, a dummy variable for elite status, 
# and potentially the user's review-count and the rated usefulness of his reviews

library(tidyverse)
library(data.table)
library(skimr)
library(stargazer)

user <- fread("CSVFiles_small/userSmall.csv") # file size 84.4 MB
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
n_subs_sizes <- seq(from = 1000, to = 500000, by=10000)
n_runs <- length(n_subs_sizes)
# compute uluru result, stop time
mc_results <- rep(NA, n_runs)
mc_times <- rep(NA, n_runs)
for (i in 1:n_runs) {
  # set size of subsample
  n_subs <- n_subs_sizes[i]
  # select subsample and remainder
  n_obs <- nrow(X)
  X_subs <- X[1L:n_subs,]
  y_subs <- y[1L:n_subs]
  X_rem <- X[(n_subs+1L):n_obs,]
  y_rem <- y[(n_subs+1L):n_obs]
  
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



### plot ols in a scatterplot3d, from https://mgimond.github.io/Stats-in-R/regression.html
library(scatterplot3d) 
s3d <- scatterplot3d(user$average_stars, user$usefulPerReview, user$eliteDummy, 
                     highlight.3d=TRUE, angle=55, scale.y=0.7, pch=16,
                     xlab = "average stars", ylab = "usefulPerReview", zlab="elite")
# Add the 3-D regression plane defined by our model M2
s3d$plane3d(extended_ols, lty.box="solid")
