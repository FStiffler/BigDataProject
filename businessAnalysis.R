#business analysis star rating 
library(tidyverse)
library(leaps)

data <- read.csv(file = 'busiclean.csv')
head(data)

#selecting the attributes for the stars

data_prep<-data %>% select(9,13:74)

data_clean<-na.omit(data_prep)
#there is just one observation with an entry for every attribute. we have to use an other identify strategy. we wil focus on the normal attributes which are normal in our opinions. this
#might be food, ambient, service and so one. on yelp there are several other paramteres which are kind of unnecessary for the normal restaurant visitiors.

summary(data_prep)
# on the first step i would select covariates with less than 20000NAs into the final dataset for the analysis. we need to have a dataset, which is big eneough to make anaylsis.

data_prep2<-data_prep %>% select(1:11, 13:15, 18:35, 37:39, 41)
data_prep2omit<-na.omit(data_prep2)
#removing blank spots 

df<-data_prep2omit

df[df==""]<-NA
df<-df[complete.cases(df),]

#transform data to numercs(dummy variable), TRUE= 1, FALSE=0
df %<>% mutate_if(is.logical,as.numeric) 


#creating dummy variables df3
df3<-df


df3[,"attributes.WiFi"]<-as.factor(df3[,"attributes.WiFi"])
df3[,"attributes.Alcohol"]<-as.factor(df3[,"attributes.Alcohol"])
df3[,"attributes.NoiseLevel"]<-as.factor(df3[,"attributes.NoiseLevel"])
df3[,"attributes.RestaurantsAttire"]<-as.factor(df3[,"attributes.RestaurantsAttire"])
df3[,"attributes.RestaurantsPriceRange2"]<-as.factor(df3[,"attributes.RestaurantsPriceRange2"])

# forward selection ####
forward<-regsubsets(stars~.,data=df3 ,nvmax=50, method ="forward", intercept = FALSE) #forward selection of variables
overview<-summary(forward)$which #logical matrix showing which variable is in what forward model
which(overview[1,]==TRUE) #most important variable "Business accepts credits cards"

# data frame with forward selection results
variableSelection<-data.frame(x=1:forward$last, y=summary(forward)$adjr2, best=factor(c(1, rep(0,forward$last-1))))

#plot forward selection results
ggplot(variableSelection, aes(x=x, y=y))+
  geom_line()+
  geom_point(data = variableSelection, aes(color=best))+
  xlab("Number of Variables")+
  ylab("Adjusted R^2")+
  ggtitle("Change of adjusted R^2 when including more variables")+
  theme_bw()+
  scale_y_continuous(breaks=seq(0.95, 0.97, 0.001))+
  scale_color_manual(values=c("black", "red"))+
  theme(legend.position = "none")

## The variable business accepts credits cards already explains around 95.6% of the variance in the average stars rating
## Note this is an greedy approach. In each iteration the variable reducing the variance the most is chosen. This does not 
## necessarely mean that the selected variables are also the best combination of variables. 
