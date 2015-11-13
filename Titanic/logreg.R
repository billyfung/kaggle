#titanic kaggle competition 
#want to predict survival 
#891 observations
library(ggplot2)
rawtrain <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/train.csv")
#418 observations
rawtest <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/test.csv")
rawtrain$Survived = factor(rawtrain$Survived)
m <- ggplot(rawtrain, aes(x=Age))
m + geom_histogram(aes(y = ..count.., fill=Survived), binwidth = 5) + theme_classic()+ scale_x_continuous(breaks=seq(0, 100, 10))+
  ggtitle("Survival of 891 Passengers based on Cabin Class") + scale_y_continuous(breaks=seq(0, 100, 10)) + facet_grid(Pclass~Sex)+
  scale_fill_discrete(name="Survived",
                      labels=c("No", "Yes")) 

missmap(rawtrain, main="Training Data Missing", legend=FALSE)

summary(train)
#we can see that missing values for age, use mice to fill in
library(mice)
imp.train <- mice(train, m=4)
train.complete <- complete(imp.train)
#well this seems to take forever...
library(Amelia)
#noms we care, ords we might care, idvars we dont care
noms <- c('Pclass', 'Sex', 'SibSp', 'Parch')
ords <- c()
idvars <- c('Name', 'Cabin', 'Embarked', 'Ticket', 'Fare')
a.out <- amelia(rawtrain, noms = noms, ords = ords, idvars = idvars)
a2.out <- amelia(rawtest, noms = noms, ords = ords, idvars = idvars)

imputedAge <- a.out$imputations$imp3$Age
imputedAgeTest <- a2.out$imputations$imp3$Age
train <- rawtrain
test <- rawtest
train$Age <- imputedAge
test$Age <- imputedAgeTest
model <- glm(Survived ~ Age + Sex + Pclass, family = binomial, data=train)
glmPredict <- predict(model, newdata=test, type = "response")
summary(glmPredict)
glmPredict <- ifelse(glmPredict >0.5, 1, 0)
#predict 134 survive, 198 death without filling in NA
table(glmPredict)
#after filling in data with imputation, predict 154 survive, 264 death
test$Survived <- glmPredict
submission <- test[,c(1,12)]
write.csv(submission, 'rf_submission.csv', row.names =F)
#0.75598 for first submission

#try random forest
library(party)
train$Fare[train$Fare == 0] <- median(train$Fare, na.rm=TRUE)

rf <- cforest(Survived~Pclass+Sex+Age+Fare+dibs+FamilySize+FamilyID+Fare+Embarked, data=train, controls =cforest_unbiased(ntree=1000, mtry=3))
Survived <- predict(rf, test, OOB = TRUE, type = 'response')
summary(Survived)
rfPredict <- ifelse(test$Survived >0.5, 1, 0)
test$Survived <- Survived
#2nd attempt, not much done to data
#2371 place, 0.77512 correct

submission <- test[,c("PassengerId", "Survived")]
write.csv(submission, 'rf2_submission.csv', row.names =F)
#3rd attempt 0.80383