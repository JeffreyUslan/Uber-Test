
##Loading Packages
library(dplyr)
library(tidyr)
library(randomForest)
library(caret)
library(readr)
library(lubridate)

##Importing Data
data=tbl_df(read_csv("Holland America Revenue Science Interview REtention Dataset.csv"))
##Managing data types
#factors
data$BKNG_ID=as.factor(data$BKNG_ID)
data$HOME_STATE=as.factor(data$HOME_STATE)
data$NATIONALITY=as.factor(data$NATIONALITY)
data$HOME_COUNTRY=as.factor(data$HOME_COUNTRY)
data$GROUP_TYPE=as.factor(data$GROUP_TYPE)
data$BKNG_STATUS=as.factor(data$BKNG_STATUS)
data$CURRENCY_CODE=as.factor(data$CURRENCY_CODE)
data$AIR_FLAG=as.factor(data$AIR_FLAG)
data$BKNG_SOURCE=as.factor(data$BKNG_SOURCE)
data$BKNG_TYPE=as.factor(data$BKNG_TYPE)
data$GENDER=as.factor(data$GENDER)
data$DINING_TIME_CODE=as.factor(data$DINING_TIME_CODE)
data$DINING_TIME_CONF_FLAG=as.factor(data$DINING_TIME_CONF_FLAG)
data$META=as.factor(data$META)

#dates
data$BKNG_OPEN_DATE=as.Date(data$BKNG_OPEN_DATE,"%m/%d/%Y")
data$BKNG_CANCEL_DATE=as.Date(data$BKNG_CANCEL_DATE,"%m/%d/%Y")
data$INSURANCE_PYMT_DATE=as.Date(data$INSURANCE_PYMT_DATE,"%m/%d/%Y")
data$SAIL_DATE=as.Date(data$SAIL_DATE,"%m/%d/%Y")




##Explorations

#indeces for data types
numeric_data_inds=which(sapply(data,is.numeric))
cat_data_inds=which(sapply(data,is.factor))

#basic summaries
summary(data[,numeric_data_inds])
summary(data[,cat_data_inds])

#identify and remove near zero variables
nzv=nzv(data[,c(numeric_data_inds,cat_data_inds)],saveMetrics=TRUE)
data=data[,-which(names(data) %in% rownames(nzv)[which(nzv$nzv)])]
#identify and remove variables with high missing values
missing_inds=apply(data,2,function(x){sum(is.na(x))/length(x)})
data=data[,-which(missing_inds>.2)]

data$BKNG_ID=NULL
data$BKNG_CANCEL_DATE=NULL
data$BKNG_OPEN_DATE=NULL
data$SAIL_DATE=NULL
data=data %>% filter(BKNG_STATUS!="G")
data$BKNG_STATUS=as.factor(as.character(data$BKNG_STATUS))

#create categorical of insurance payment
data=data %>% mutate(insurance= INSURANCE_PYMT_DATE>min(data$INSURANCE_PYMT_DATE))
data$INSURANCE_PYMT_DATE=NULL

#Tabular break down of numeric variables by booking status
data %>% group_by(BKNG_STATUS)  %>%
  summarise(AGE=mean(AGE),NBR_CRUISES=mean(NBR_CRUISES),NET_TKT_REV=mean(NET_TKT_REV))

# some cross tabulations
data %>% group_by(BKNG_STATUS,DINING_TIME_CODE) %>% summarise(count=n())
data %>% group_by(BKNG_STATUS,BKNG_TYPE) %>% summarise(count=n())
data %>% group_by(BKNG_STATUS,META) %>% summarise(count=n())
data %>% group_by(BKNG_STATUS,insurance) %>% summarise(count=n())


#Do people with children cancel? Yes, of course.
ggplot(data)+theme_bw()+geom_density(aes(x=AGE,color=BKNG_STATUS))
ggplot(data)+theme_bw()+geom_density(aes(x=NET_TKT_REV,color=BKNG_STATUS))






## Create a model that predicts whether a person will cancel a booking
#creating a training and test set
data=na.omit(data)
inTrain = createDataPartition(y=data$BKNG_STATUS, p = .6)[[1]]
training = data[ inTrain,]
testing = data[-inTrain,]

#building random forest model
modFit <- randomForest(BKNG_STATUS ~ .,data=training)

#testing on training set as a sanity check
train_pred=predict(modFit,newdata= training)
confusionMatrix(training$BKNG_STATUS,train_pred )

#testing on test set
test_pred=predict(modFit,newdata= testing)
confusionMatrix(testing$BKNG_STATUS,test_pred)

#Relative variable importance according to the Gini Information Index
varImpPlot(modFit,main="Variable Importance")


## What kind of people book suites (meta in ('S','D')) (Active bookings only)
#filtering for active bookings
suite_people=data %>% filter(BKNG_STATUS=="B")
suite_people$Suite="Coach"
suite_people$Suite[which(suite_people$META %in% c('S','D'))]="Suite"
suite_people$Suite=as.factor(suite_people$Suite)
suite_people$META=NULL

#Use random forests as a shortcut to identify variable importance
modFit <- randomForest(Suite ~ .,data=suite_people)
varImpPlot(modFit,main="Variable Importance")


#The top three variables are numeric with 
suite_people %>% group_by(Suite) %>% 
  summarise(AGE=mean(AGE),NBR_CRUISES=mean(NBR_CRUISES),NET_TKT_REV=mean(NET_TKT_REV))

#people who are willing to pay more
ggplot(suite_people)+theme_bw()+geom_density(aes(x=NET_TKT_REV,color=Suite))

#slight age bias
ggplot(suite_people)+theme_bw()+geom_density(aes(x=AGE,color=Suite))

#If you're going to cruise often- do it in style
ggplot(suite_people %>% filter(NBR_CRUISES>0,NBR_CRUISES<50))+theme_bw()+geom_density(aes(x=NBR_CRUISES,color=Suite))

#Agencies book people on suites more ferquently
suite_people %>% group_by(Suite,BKNG_SOURCE) %>%  summarise(count=n())







