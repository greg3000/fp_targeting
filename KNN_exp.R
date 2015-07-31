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

#set random seed for reproducable results
set.seed(1)
#create a vector containg the class labels for the training observations
cl <- as.matrix(training[,dim(training)[2]])  

#run the knn prediction
knn.pred=knn(training, testing, cl, k=2)

#create confusion matrix
rtab = table(knn.pred, testing$age)
accuracy = (rtab[1,1]+rtab[2,2])/sum(rtab)
accuracy

