library(caret)
library(ggplot2)

svm.work.data <- work.data

#Building a model
#split data into training and test data sets
svm.indxTrain <- indxTrain
svm.training <- svm.work.data[svm.indxTrain, ]
svm.testing <- svm.work.data[-svm.indxTrain, ]

svm.training[['Classifier']] = factor(svm.training[["Classifier"]])
#Training a model
svm.traincontrol <- trainControl(method = 'repeatedcv', number = 10, repeats = 4)
svm_linear <- train(Classifier ~., data = svm.training, method = 'svmLinear', trControl = svm.traincontrol,
                    preProcess = c('center', 'scale'), tuneLength = 10)
svm_linear

svm.test.predict <- predict(svm_linear, newdata = svm.testing)
confusionMatrix(table(svm.test.predict, svm.testing$Classifier))

#Selecting C-value in Linear classifier
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(Classifier ~., data = svm.training, method = "svmLinear",
                         trControl=svm.traincontrol,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)
#The plot is showing that our classifier is giving best accuracy on C = 0.05.
test_pred_grid <- predict(svm_Linear_Grid, newdata = svm.testing)
test_pred_grid
confusionMatrix(table(test_pred_grid, svm.testing$Classifier))


# Principal components ----------------------------------------------------

svm.principal.training <- principal.work.data[svm.indxTrain, ]
svm.principal.testing <- principal.work.data[-svm.indxTrain, ]

svm.principal.training[['Classifier']] = factor(svm.principal.training[["Classifier"]])
#Training a model
svm.principal.traincontrol <- trainControl(method = 'repeatedcv', number = 10, repeats = 4)
svm_principal.linear <- train(Classifier ~., data = svm.principal.training, method = 'svmLinear', trControl = svm.traincontrol,
                    preProcess = c('center', 'scale'), tuneLength = 10)
svm_principal.linear

svm.principal.test.predict <- predict(svm_principal.linear, newdata = svm.principal.testing)
confusionMatrix(table(svm.principal.test.predict, svm.principal.testing$Classifier))
