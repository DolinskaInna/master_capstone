library(randomForest)

#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = work.data$Classifier, p = 0.7, list = F)
training <- work.data[indxTrain, ]
testing <- work.data[-indxTrain, ]

testing.unlabeled <- testing[, -12]
testing.labels <- testing$Classifier

set.seed(123)
# Training using &lsquo;random forest&rsquo; algorithm
model <- train(Classifier ~ Publication.Year + Count.of.Simple.Family.Members + Simple.Family.Cited.by.Count +
                 Count.of.Other.References + Count.of.Cited.by.Patents + Count.of.Cites.Patents + 
                   Simple.Legal.Status + Count.of.claims + Quality.of.Family, # Survived is a function of the variables we decided to include
               data = training, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation

testing.unlabeled$Pred.Classifier <- predict(model, newdata = testing)
confusionMatrix(testing.unlabeled$Pred.Classifier, testing$Classifier)
