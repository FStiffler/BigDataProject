# We estimate the effect of average stars assigned per review on the probability of 
# being an elite user (Uluru OLS)

# For this, we need the user data with the user's average stars, a dummy variable for elite status, 
# and potentially the user's reviewcount and the rated usefullness of his reviews

library(tidyverse)
library(data.table)
library(skimr)

user <- fread("userSmall.csv") # file size 84.4 MB
skim(user)
# +2MM observations, 5 rows
# user_id, review_count, average_stars, eliteDummy, usefulPerReview
# 4.36% are elite users

# different ols with lm() function.
simple_ols <- lm(data = user, formula = eliteDummy ~ average_stars)
summary(simple_ols)

extended_ols <- lm(data = user, formula = eliteDummy ~ average_stars + usefulPerReview)
summary(normal_ols)

full_ols <- lm(data = user, formula = eliteDummy ~ average_stars + usefulPerReview + review_count)
summary(full_ols)



# manual ols -> uluru ols (not yet)

## build OLS estimator manually from https://economictheoryblog.com/2016/02/20/rebuild-ols-estimator-manually-in-r/
#  define X matrix and y vector
X <- as.matrix(cbind(1,user$average_stars))
y <- as.matrix(user$eliteDummy)
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

# formula from the slides
beta_ols <- 
  function(X, y) {
    
    # compute cross products and inverse
    XXi <- solve(crossprod(X,X))
    Xy <- crossprod(X, y) 
    
    return( XXi  %*% Xy )
  }
beta_ols(X, y)
# same results as with lm() and the previous manual build.

# somehow we need to implement this in the uluru ols estimator

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
beta_uluru(X_subs, y_subs, X_rem, y_rem)

# different results....


### plot ols in a scatterplot3d, from https://mgimond.github.io/Stats-in-R/regression.html
library(scatterplot3d) 
s3d <- scatterplot3d(dat$Education, dat$Professional , dat$Income, 
                     highlight.3d=TRUE, angle=55, scale.y=0.7, pch=16,
                     xlab = "Education", ylab = "Professional", zlab="Income")
# Add the 3-D regression plane defined by our model M2
s3d$plane3d(M2, lty.box="solid")