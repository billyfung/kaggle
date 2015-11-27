#Given time and location, predict the crime

library(ggplot2)
library(dplyr)
test <- read.csv("D:/Documents/kaggle/SF_Crime/test.csv")
train <- read.csv("D:/Documents/kaggle/SF_Crime/train.csv")
#dplyr magic!
crimes<- train %>% group_by(Category, DayOfWeek) %>% summarise(Count = n())

#plot 
m <- ggplot(crimes, aes(x=reorder(Category,Count), y=Count))

m + geom_bar(stat='identity') + coord_flip()+xlab("") +facet_wrap(~DayOfWeek,ncol=4) +theme(legend.position = "none")

#want to see which pddistrict has the most crimes
p <- ggplot(crimes, aes(x=PdDistrict, y = Count)) + geom_bar(stat="identity") +xlab("District")+ylab("Number of Reported Crimes")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

#hmm maybe we should look at the resolutions
resolved<- train %>% group_by(Category, Resolution) %>% summarise(Count = n())
#probably doesn't actually help in categorizing the type of crime in each location

#use lubridate to make use of dates
library(lubridate)
train$Dates <- ymd_hms(train$Dates)
train$Year <- as.factor(year(train$Dates))
train$Month <- as.factor((month(train$Dates)))
train$Day <- as.factor(day(train$Dates))
train$Hour <- as.factor(hour(train$Dates))

#look at some graphs of months and crime
crime_month <- train %>% group_by(Category, Month) %>% summarise(Count = n())
d <- ggplot(crime_month, aes(x=Category, y=Count))
d + geom_bar(stat='identity') + facet_wrap(~Month, ncol=6) + coord_flip()

e <- ggplot(crime_month, aes(x=Month, y=Count))
e + geom_bar(stat='identity') + ylab("Number of Reported Crimes")

crime_day <- train %>% group_by(Category, Day) %>% summarise(Count = n())
a <- ggplot(crime_day, aes(x=Day, y=Count))
a + geom_bar(stat='identity')+ ylab("Number of Reported Crimes")

j <- ggplot(crimes, aes(x=DayOfWeek, y=Count))
j + geom_bar(stat='identity') + ylab("Number of Reported Crimes")

#hmm let's check out the hour when crimes are reported
h <- ggplot(train, aes(x=train$Hour))
h + geom_histogram() + xlab("Hour") + ylab("Number of Reported Crimes")
