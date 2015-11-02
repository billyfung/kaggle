#titanic kaggle competition 
#want to predict survival 
#891 observations
rawtrain <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/train.csv")
#418 observations
rawtest <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/test.csv")
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
a.out <- amelia(train, noms = noms, ords = ords, idvars = idvars)
a2.out <- amelia(test, noms = noms, ords = ords, idvars = idvars)

imputedAge <- a.out$imputations$imp4$Age
imputedAgeTest <- a2.out$imputations$imp5$Age

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
write.csv(submission, 'submission.csv', row.names =F)
#0.75598 for first submission
