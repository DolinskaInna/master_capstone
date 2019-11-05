library(caret)

svm.work.data <- work.data
svm.work.data$Simple.Legal.Status <- as.character(svm.work.data$Simple.Legal.Status)
svm.work.data$Simple.Legal.Status[svm.work.data$Simple.Legal.Status == "Active"] <- 4
svm.work.data$Simple.Legal.Status[svm.work.data$Simple.Legal.Status == "Inactive"] <- 3
svm.work.data$Simple.Legal.Status[svm.work.data$Simple.Legal.Status == "Pending"] <- 2
svm.work.data$Simple.Legal.Status[svm.work.data$Simple.Legal.Status == "Undetermined"] <- 1
svm.work.data$Simple.Legal.Status <- as.numeric(svm.work.data$Simple.Legal.Status)

svm.work.data$Quality.of.Family <- as.character(svm.work.data$Quality.of.Family)
svm.work.data$Quality.of.Family[svm.work.data$Quality.of.Family == "High"] <- 1
svm.work.data$Quality.of.Family[svm.work.data$Quality.of.Family == "Low"] <- 0
svm.work.data$Quality.of.Family <- as.numeric(svm.work.data$Quality.of.Family)

svm.work.data$Classifier <- as.character(svm.work.data$Classifier)
svm.work.data$Classifier[svm.work.data$Classifier == "Strong"] <- 1
svm.work.data$Classifier[svm.work.data$Classifier == "Weak"] <- 0
svm.work.data$Classifier <- as.numeric(svm.work.data$Classifier)

svm.work.data.n <- as.data.frame(lapply(svm.work.data[,1:11], normalize))
svm.work.data.n$Simple.Legal.Status <- NULL
svm.work.data[is.na(svm.work.data)] <- 0
#Building a model
#split data into training and test data sets
svm.indxTrain <- createDataPartition(y = svm.work.data$Classifier, p = 0.7, list = F)
svm.training <- svm.work.data[svm.indxTrain, ]
svm.testing <- svm.work.data[-svm.indxTrain, ]

svm.training[['Classifier']] = factor(svm.training[["Classifier"]])
#Training a model
svm.traincontrol <- trainControl(method = 'repeatedcv', num ber = 10, repeats = 4)
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
