library(dplyr)
library(tidyr)
library(lubridate)
library(rpart)
library(randomForest)
library(MASS)
library(zoo)
library(caret)
library(pander)

# load(file="uber_test_data.rda")

data=tbl_df(uber_unlist)
data$trips_in_first_30_days=as.numeric(as.character(data$trips_in_first_30_days))
data$signup_date=as.Date(data$signup_date)
data$avg_rating_of_driver=as.numeric(as.character(data$avg_rating_of_driver))
data$avg_surge=as.numeric(as.character(data$avg_surge))
data$last_trip_date=as.Date(data$last_trip_date)
data$surge_pct=as.numeric(as.character(data$surge_pct))
data$weekday_pct=as.numeric(as.character(data$weekday_pct))
data$avg_dist=as.numeric(as.character(data$avg_dist))
data$avg_rating_by_driver=as.numeric(as.character(data$avg_rating_by_driver))


numeric_data_inds=sapply(data,is.numeric)
cat_data_inds=!sapply(data,is.numeric)

#tabulations of categorical data
pander(data %>% group_by(city) %>% summarise(Count=n()))
pander(data %>% group_by(phone) %>% summarise(Count=n()))
pander(data %>% group_by(uber_black_user) %>% summarise(Count=n()))

#generating retained variable
data$retained=0
data$retained[which(data$trips_in_first_30_days>0)]=1
mean(data$retained,na.rm=TRUE)

data$retained=as.factor(data$retained)
data %>% group_by(retained) %>% summarise(Count=n())


#numeric exploration

summary(data[,numeric_data_inds])


#covariates
covariates=c("city","phone","uber_black_user","avg_rating_of_driver",
             "avg_surge","surge_pct","weekday_pct","avg_dist","avg_rating_by_driver")






