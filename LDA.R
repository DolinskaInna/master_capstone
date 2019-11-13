require(MASS)

lda.work.data <- work.data

lda.training <- lda.work.data[indxTrain, ]
lda.testing <- lda.work.data[-indxTrain, ]
lda.train.labels <- lda.training$Classifier
lda.test.labels <- lda.testing$Classifier
lda.testing$Classifier <- NULL
lda.training$Classifier <- NULL

lda.model <- lda(lda.train.labels ~., data = lda.training, CV = T)

lda.predicted <- predict(lda.model, data = lda.testing)
