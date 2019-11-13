

#data <- read.csv(choose.files(), header = T, sep = ";", stringsAsFactors = T)

ipc.data <- data[c('Publication.Year', 'Main.IPC.Subclass', 'Count.of.Simple.Family.Members', 'Simple.Family.Cited.by.Count', 
                    'Count.of.Other.References', 'Count.of.Cited.by.Patents', 'Count.of.Cites.Patents', 'Count.of.Cited.by.Patents.Within.3.years',
                    'Count.of.Cited.by.Patents.Within.5.years', 'Simple.Legal.Status', 'Count.of.claims', 'Quality.of.Family',
                    'Classifier')]
ipc.data$Publication.Year <- 2020-ipc.data$Publication.Year
ipc.data <- fastDummies::dummy_cols(ipc.data, select_columns = c('Simple.Legal.Status', 'Main.IPC.Subclass','Quality.of.Family'))
ipc.data$Simple.Legal.Status <- NULL
ipc.data$Quality.of.Family <- NULL
ipc.data$Simple.Legal.Status_ <- NULL
ipc.data$Main.IPC.Subclass <- NULL
classifier <- ipc.data$Classifier


# SVM ---------------------------------------------------------------------

svm.ipc.data <- ipc.data
#Building a model
#split data into training and test data sets
svm.indxTrain <- indxTrain
svm.training <- svm.ipc.data[svm.indxTrain, ]
svm.testing <- svm.ipc.data[-svm.indxTrain, ]

svm.training[['Classifier']] = factor(svm.training[["Classifier"]])
#Training a model
svm.ipc.traincontrol <- trainControl(method = 'repeatedcv', number = 10, repeats = 4)
svm_linear <- train(Classifier ~., data = svm.training, method = 'svmLinear', trControl = svm.traincontrol,
                    preProcess = c('center', 'scale'), tuneLength = 10)
svm_linear

svm.test.predict <- predict(svm_linear, newdata = svm.testing)
confusionMatrix(table(svm.test.predict, svm.testing$Classifier))


# KNN ---------------------------------------------------------------------

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

knn.work.data <- na.omit(ipc.data)

#Building a model
#split data into training and test data sets
knn.indxTrain <- createDataPartition(y = ipc.data$Classifier, p = 0.7, list = F)
knn.training <- knn.work.data[knn.indxTrain, -10]
knn.testing <- knn.work.data[-knn.indxTrain, -10]

#Creating seperate dataframe for 'Classifier' feature which is our target.
train.knn.labels <- knn.work.data[knn.indxTrain,10]
test.knn.labels <-knn.work.data[-knn.indxTrain,10]

library(class)
sqrt(NROW(train.knn.labels))
#We have 2593 observations in our training data set.
#The square root of 2593 is around 50.92, therefore we’ll create two models. 
#One with ‘K’ value as 50 and the other model with a ‘K’ value as 51.
knn.50 <- knn(train=knn.training, test=knn.testing, cl=train.knn.labels, k=50)


#Model Evaluation
#Calculate the proportion of correct classification for k = 50
ACC.50 <- 100 * sum(test.knn.labels == knn.50)/NROW(test.knn.labels)

# Check prediction against actual value in tabular form for k=50
table(knn.50 ,test.knn.labels)


confusionMatrix(table(knn.50, test.knn.labels))

