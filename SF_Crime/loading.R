library(ggplot2)
library(dplyr)
test <- read.csv("D:/Documents/kaggle/SF_Crime/test.csv")
train <- read.csv("D:/Documents/kaggle/SF_Crime/train.csv")
#dplyr magic!
crimes<- train %>% group_by(Category, PdDistrict) %>% summarise(Count = n())

#plot 
m <- ggplot(crimes, aes(x=reorder(Category,Count), y=Count, fill = PdDistrict))

m + geom_bar(stat='identity') + coord_flip()+xlab("") +facet_grid(.~PdDistrict) +theme(legend.position = "none")

p <- ggplot(crimes, aes(x=PdDistrict, y = Count)) + geom_bar(stat="identity") +xlab("District")+ylab("Number of Reported Crimes")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
#want to see which district has the most crimes

