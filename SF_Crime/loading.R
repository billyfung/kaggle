#Given time and location, predict the crime

library(ggplot2)
library(dplyr)
test <- read.csv("D:/Documents/kaggle/SF_Crime/test.csv")
train <- read.csv("D:/Documents/kaggle/SF_Crime/train.csv")
#dplyr magic!
counts <- summarise(group_by(train, Category), Counts=length(Category))
counts <- counts[order(-counts$Counts),]
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

train$DayOfWeek <- factor(train$DayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday", "Saturday", "Sunday"), ordered=T)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#hmm let's check out the hour when crimes are reported
h <- ggplot(train, aes(x=train$Hour, y = ..count..))
h + geom_histogram(aes(fill=DayOfWeek)) + xlab("Hour") + ylab("Number of Reported Crimes")+
  scale_fill_manual(values=cbPalette)+
  facet_wrap(~DayOfWeek, ncol=1)+
  theme(legend.position = "none")

#let's map crime locations
library(ggmap)
map <- get_map(location = 'Mission Dolores Park, San Francisco', zoom = 12, source="osm", color = "bw")
#lets look at the top 3 crimes for now
top3 <- train[train$Category %in% counts$Category[c(1,3:4)],]

sfmap <- ggmap(map)
sfmap +  geom_point(data=top3, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
  scale_fill_manual(values=cbPalette)+
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Type of Crime"))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

library(plotly)