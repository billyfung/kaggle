#Organizing all the data inputs
rawtrain <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/train.csv")
rawtest <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/test.csv")

rawfull$Name <- as.character(rawfull$Name)
rawfull <- rbind(rawtrain, rawtest)
rawfull$Title <- sapply(rawfull$Name, function(x){strsplit(x,split='[,.]')[[1]][2]})
rawfull$Title = sub(pattern = " ", replacement = "", rawfull$Title)
#Combining some titles
rawfull$Title[rawfull$Title %in% c("Mlle", "Mme")] = "Mlle"
rawfull$Title[rawfull$Title %in% c("Capt", "DOn", "Major", "Sir")] = "Sir"
rawfull$Title[rawfull$Title %in% c("Dona", "Jonkheer", "Lady", "the Countess")] = "Lady"

rawtrain$Name <- as.character(rawtrain$Name)
rawtrain$Title <- sapply(rawtrain$Name, function(x){strsplit(x,split='[,.]')[[1]][2]})
rawtrain$Title = sub(pattern = " ", replacement = "", rawtrain$Title)
#Combining some titles
rawtrain$Title[rawtrain$Title %in% c("Mlle", "Mme")] = "Mlle"
rawtrain$Title[rawtrain$Title %in% c("Capt", "DOn", "Major", "Sir")] = "Sir"
rawtrain$Title[rawtrain$Title %in% c("Dona", "Jonkheer", "Lady", "the Countess")] = "Lady"

rawtrain$Survived <- as.factor(rawtrain$Survived)
m <- ggplot(rawtrain, aes(x=Title, y=Age))
m + geom_point() +ggtitle("Age and Title of Passengers")
  geom_point(shape=1)  +geom_smooth()

rawfull$Title <- as.factor(rawfull$Title)
#extract surnames as well
rawfull$Surname = sapply(rawfull$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})

