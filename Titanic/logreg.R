#titanic kaggle competition 
#want to predict survival 
#891 observations
train <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/train.csv")
#418 observations
test <- read.csv("~/Documents/Kaggle-local/kaggle/Titanic/test.csv")
summary(train)
#we can see that missing values for age, but let's see what we get without adjusting
model <- glm(Survived ~ Age + Sex + Pclass, family = binomial, data=train)
glmPredict <- predict(model, newdata=test, type = "response")
summary(glmPredict)
