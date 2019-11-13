library(randomForest)

#Building a model
#split data into training and test data sets
rf.indxTrain <- indxTrain
rf.training <- work.data[rf.indxTrain, ]
rf.testing <- work.data[-rf.indxTrain, ]

rf.testing.unlabeled <- rf.testing[, -10]
rf.testing.labels <- rf.testing$Classifier

set.seed(123)
# Training using &lsquo;random forest&rsquo; algorithm
rf.model <- train(Classifier ~ Publication.Year + Count.of.Simple.Family.Members +
                    Simple.Family.Cited.by.Count + Count.of.Other.References + Count.of.Cited.by.Patents +
                    Count.of.Cited.by.Patents.Within.3.years + Count.of.Cited.by.Patents.Within.5.years +
                    Count.of.claims + Quality.of.Family_High + Quality.of.Family_Low + Simple.Legal.Status_Active +
                    Simple.Legal.Status_Inactive + Simple.Legal.Status_Pending + Simple.Legal.Status_Undetermined, # Survived is a function of the variables we decided to include
               data = rf.training, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation

rf.testing.unlabeled$Pred.Classifier <- predict(model, newdata = rf.testing)
confusionMatrix(rf.testing.unlabeled$Pred.Classifier, rf.testing$Classifier)


# Principal components ----------------------------------------------------

rf.principal.training <- principal.work.data[rf.indxTrain, ]
rf.principal.testing <- principal.work.data[-rf.indxTrain, ]

rf.principal.testing.unlabeled <- rf.principal.testing[, -10]
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
