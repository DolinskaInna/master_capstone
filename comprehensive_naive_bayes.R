#Data Visualization

#Visual 1
# ggplot(work.data, aes(Publication.Year, colour = Classifier))+
#   geom_freqpoly(binwidth = 1) + labs(title = "Publication Year Distribution by Classifier")
# 
# #Visual 2
# c <- ggplot(work.data, aes(x=Count.of.Simple.Family.Members, fill=Classifier, color=Classifier)) +
#   geom_histogram(binwidth = 1) + labs(title="Count.of.Simple.Family.Members Distribution by Classifier")
# c + theme_bw()

#visual 3
#ggpairs(work.data)

#Building a model
#split data into training and test data sets
work.data$Classifier <- as.factor(work.data$Classifier)
work.data <- na.omit(work.data)
set.seed(123)
training <- work.data[indxTrain, ]
testing <- work.data[-indxTrain, ]

#Check dimensions of the split
prop.table(table(work.data$Classifier))*100

prop.table(table(training$Classifier))*100
prop.table(table(testing$Classifier))*100

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-10]
y = training$Classifier

library(e1071)
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model

#Model Evaluation
#Predict testing set
Predict <- predict(model, newdata = testing)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict, testing$Classifier)

#Plot Variable performance
X <- varImp(model)
plot(X)


# Reduction the number of variables ---------------------------------------

#Building a model
#split data into training and test data sets
red.indxTrain <- createDataPartition(y = principal.work.data$Classifier, p = 0.7, list = F)
red.training <- na.omit(principal.work.data[indxTrain, ])
red.testing <- na.omit(principal.work.data[-indxTrain, ])

#Check dimensions of the split
prop.table(table(principal.work.data$Classifier))*100

prop.table(table(red.training$Classifier))*100
prop.table(table(red.testing$Classifier))*100

#create objects x which holds the predictor variables and y which holds the response variables
red.x = red.training[,-10]
red.y = red.training$Classifier

library(e1071)
red.model = train(red.x,red.y,'nb',trControl=trainControl(method='cv',number=10))
red.model

#Model Evaluation
#Predict testing set
red.Predict <- predict(red.model, newdata = red.testing)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(red.Predict, red.testing$Classifier)

#Plot Variable performance
X.red <- varImp(red.model)
plot(X.red)


# Important components ----------------------------------------------------

#Building a model
#split data into training and test data sets
#red.indxTrain <- createDataPartition(y = principal.work.data$Classifier, p = 0.7, list = F)
important.training <- na.omit(important.naive.work.data[indxTrain, ])
important.testing <- na.omit(important.naive.work.data[-indxTrain, ])

#Check dimensions of the split
prop.table(table(important.naive.work.data$Classifier))*100

prop.table(table(important.training$Classifier))*100
prop.table(table(important.testing$Classifier))*100

#create objects x which holds the predictor variables and y which holds the response variables
important.x = important.training[,-8]
important.y = important.training$Classifier

library(e1071)
important.model = train(important.x,important.y,'nb',trControl=trainControl(method='cv',number=10))
important.model

#Model Evaluation
#Predict testing set
important.Predict <- predict(important.model, newdata = important.testing)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(important.Predict, important.testing$Classifier)

#Plot Variable performance
X.red <- varImp(red.model)
plot(X.red)


