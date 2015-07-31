library(class) #needed for the islr knn approach
library(caret)
library(ISLR)

setwd("~/Documents/R/fp_targeting/")
data = read.csv("KNN_Exp.csv")
attach(data)
plot(x,y, col=as.factor(age))

#standardise the data

data$x=scale(data$x)
data$y=scale(data$y)



#plot the data again
plot(data$x,data$y, col=as.factor(data$age))

#create testing and training sets
inTrain <- createDataPartition(y=age, p=0.75, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]



#run the knn prediction

#modelFit <- train(age ~., data=training, method= "knn")
modelFit <- train(as.factor(age) ~., data=training , method= "knn")
modelFit$finalModel
predictions <- predict(modelFit, newdata=testing)
predictions
confusionMatrix(predictions, testing$age)

#fitting the model to data with no age data
testing2 <-testing[,-4]
predictions2 <- predict(modelFit, newdata=testing2)
predictions2
