#Organizing all the data inputs
rawtrain <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/train.csv")
rawtest <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/test.csv")
rawtest$Survived <- 'NA'
rawfull <- rbind(rawtest, rawtrain)
rawfull$Name <- as.character(rawfull$Name)
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

rawfull$FamilySize = rawfull$SibSp + rawfull$Parch +1
rawfull$FamilyID = paste(as.character(rawfull$FamilySize), rawfull$Surname, sep = "")
full <- rawfull
library(rpart)
#fill in ages with tree, next time try a rnn
fit.Age <- rpart(Age[!is.na(Age)]~Pclass+Title+Sex+FamilySize , data=full[!is.na(full$Age),],method='anova')
full$Age[is.na(full$Age)] <- predict(fit.Age, full[is.na(full$Age),])
full$Embarked[which(is.na(full$Embarked))] <- 'S'

#look at family size vs survived
table(rawfull$FamilySize, rawfull$Survived)
#we can see that families/couples have higher survival rate
#so far we know that women and children more likely to survive, especially in families
j <- ggplot(train, aes(x=Age, y=..count..))
j + geom_histogram(aes(fill=Survived)) + facet_grid(~Sex)

full$Survived <- as.factor(full$Survived)
full$FamilySize <- as.factor(full$FamilySize)

full$dibs <- "No"
full$dibs[which(full$Sex == "female" | full$Age < 20)] <- "Yes"
full$dibs <- as.factor(full$dibs)
full$FamilyID <- as.factor(full$FamilyID)

#split back into testing and training
test <- full[full$Survived=='NA',]
train <- subset(full, full$Survived!='NA')

