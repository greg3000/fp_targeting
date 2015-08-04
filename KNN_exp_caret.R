
library(caret)


setwd("C:\\Users\\Stephen Oxley\\Documents\\GitHub\\fp_targeting")
data = read.csv("EPC_KNN_TEST2.csv")
attach(data)
plot(X_COORDINATE,Y_COORDINATE, col=as.factor(PROP_AGE))

data <- data[,3:5]

#recode data to pre/post 1930 - don't run this section if want to maintain
#original date categories 
data$PROP_AGE[data$PROP_AGE==101] <- "pre_1930"
data$PROP_AGE[data$PROP_AGE==102] <- "post_1930"
data$PROP_AGE[data$PROP_AGE==103] <- "post_1930"
data$PROP_AGE[data$PROP_AGE==104] <- "post_1930"
data$PROP_AGE[data$PROP_AGE==105] <- "post_1930"
data$PROP_AGE[data$PROP_AGE==106] <- "post_1930"




#standardise the data
data$X_COORDINATE=scale(data$X_COORDINATE)
data$Y_COORDINATE=scale(data$Y_COORDINATE)



#plot the data again
plot(data$X_COORDINATE,data$Y_COORDINATE, col=as.factor(data$PROP_AGE))

#create testing and training sets
inTrain <- createDataPartition(y=PROP_AGE, p=0.75, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

summary(data)
table(data$PROP_AGE)
#run the knn prediction

head(data)

#modelFit <- train(age ~., data=training, method= "knn")
modelFit <- train(as.factor(PROP_AGE) ~., data=training , method= "knn")
modelFit$finalModel
predictions <- predict(modelFit, newdata=testing)
predictions
confusionMatrix(predictions, testing$PROP_AGE)

#fitting the model to data with no age data
testing2 <-testing[,-4]
predictions2 <- predict(modelFit, newdata=testing2)
predictions2
