library(randomForest)

#Building a model
#split data into training and test data sets
rf.indxTrain <- createDataPartition(y = work.data$Classifier, p = 0.7, list = F)
rf.training <- work.data[rf.indxTrain, ]
rf.testing <- work.data[-rf.indxTrain, ]

rf.testing.unlabeled <- rf.testing[, -12]
rf.testing.labels <- rf.testing$Classifier

set.seed(123)
# Training using &lsquo;random forest&rsquo; algorithm
rf.model <- train(Classifier ~ Publication.Year + Count.of.Simple.Family.Members + Simple.Family.Cited.by.Count +
                 Count.of.Other.References + Count.of.Cited.by.Patents + Count.of.Cites.Patents + 
                   Simple.Legal.Status + Count.of.claims + Quality.of.Family, # Survived is a function of the variables we decided to include
               data = rf.training, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation

rf.testing.unlabeled$Pred.Classifier <- predict(model, newdata = rf.testing)
confusionMatrix(rf.testing.unlabeled$Pred.Classifier, rf.testing$Classifier)


# Principal components ----------------------------------------------------

rf.principal.training <- principal.work.data[rf.indxTrain, ]
rf.principal.testing <- principal.work.data[-rf.indxTrain, ]

rf.principal.testing.unlabeled <- rf.principal.testing[, -12]
rf.principal.testing.labels <- rf.principal.testing$Classifier

set.seed(123)
# Training using &lsquo;random forest&rsquo; algorithm
rf.principal.model <- train(Classifier ~ Count.of.Simple.Family.Members + Simple.Family.Cited.by.Count +
                    Count.of.Other.References + Count.of.Cited.by.Patents + Count.of.Cites.Patents + 
                    Simple.Legal.Status + Quality.of.Family, # Survived is a function of the variables we decided to include
                  data = rf.training, # Use the train data frame as the training data
                  method = 'rf',# Use the 'random forest' algorithm
                  trControl = trainControl(method = 'cv', # Use cross-validation
                                           number = 5)) # Use 5 folds for cross-validation

rf.principal.testing.unlabeled$Pred.Classifier <- predict(model, newdata = rf.principal.testing)
confusionMatrix(rf.principal.testing.unlabeled$Pred.Classifier, rf.principal.testing$Classifier)
