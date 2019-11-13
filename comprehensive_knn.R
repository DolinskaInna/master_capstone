#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

knn.work.data <- na.omit(work.data)

#Building a model
#split data into training and test data sets
#knn.indxTrain <- createDataPartition(y = work.data$Classifier, p = 0.7, list = F)
knn.indTrain <- indxTrain
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
#Calculate the proportion of correct classification for k = 26, 27
ACC.50 <- 100 * sum(test.knn.labels == knn.50)/NROW(test.knn.labels)


# Check prediction against actual value in tabular form for k=50
table(knn.50 ,test.knn.labels)


confusionMatrix(table(knn.50, test.knn.labels))


#Optimization
i=1
k.optm=1
for (i in 1:51){
  knn.mod <- knn(train=knn.training, test=knn.testing, cl=train.knn.labels, k=i)
  k.optm[i] <- 100 * sum(test.knn.labels == knn.mod)/NROW(test.knn.labels)
  k=i
  cat(k,'=',k.optm[i],'
      ')
   }
#Accuracy plot
plot(c(1:i), k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#Correction
knn.47 <- knn(train=knn.training, test=knn.testing, cl=train.knn.labels, k=47)
ACC.47 <- 100 * sum(test.knn.labels == knn.47)/NROW(test.knn.labels)
# Check prediction against actual value in tabular form for k=50
table(knn.47 ,test.knn.labels)
confusionMatrix(table(knn.47, test.knn.labels))



# Using only principal components -----------------------------------------

knn.principal.work.data <- na.omit(principal.work.data)

#knn.principal.indxTrain <- createDataPartition(y = knn.principal.work.data$Classifier, p = 0.7, list = F)
knn.principal.training <- knn.principal.work.data[knn.indxTrain, -10]
knn.principal.testing <- knn.principal.work.data[-knn.indxTrain, -10]

#Creating seperate dataframe for 'Classifier' feature which is our target.
train.knn.principal.labels <- knn.principal.work.data[knn.indxTrain,10]
test.knn.principal.labels <-knn.principal.work.data[-knn.indxTrain,10]

library(class)
sqrt(NROW(train.knn.principal.labels))
#We have 2593 observations in our training data set.
#The square root of 2593 is around 50.92, therefore we’ll create two models. 
#One with ‘K’ value as 50 and the other model with a ‘K’ value as 51.
knn.principal.50 <- knn(train=knn.principal.training, test=knn.principal.testing, cl=train.knn.principal.labels, k=50)
knn.principal.51 <- knn(train=knn.principal.training, test=knn.principal.testing, cl=train.knn.principal.labels, k=51)

#Model Evaluation
#Calculate the proportion of correct classification for k = 26, 27
ACC.principal.50 <- 100 * sum(test.knn.principal.labels == knn.50)/NROW(test.knn.principal.labels)
ACC.principal.51 <- 100 * sum(test.knn.principal.labels == knn.51)/NROW(test.knn.principal.labels)

# Check prediction against actual value in tabular form for k=50
table(knn.principal.50 ,test.knn.principal.labels)
# Check prediction against actual value in tabular form for k=51
table(knn.principal.51 ,test.knn.principal.labels)

confusionMatrix(table(knn.principal.50, test.knn.principal.labels))
confusionMatrix(table(knn.principal.51, test.knn.principal.labels))

#Optimization
i=1
k.optm=1
for (i in 1:51){
  knn.mod <- knn(train=knn.principal.training, test=knn.principal.testing, cl=train.knn.principal.labels, k=i)
  k.optm[i] <- 100 * sum(test.knn.labels == knn.mod)/NROW(test.knn.labels)
  k=i
  cat(k,'=',k.optm[i],'
      ')
}
#Accuracy plot
plot(c(1:i), k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#Correction
knn.principal.24 <- knn(train=knn.principal.training, test=knn.principal.testing, cl=train.knn.principal.labels, k=24)
ACC.24 <- 100 * sum(test.knn.principal.labels == knn.principal.24)/NROW(test.knn.principal.labels)
# Check prediction against actual value in tabular form for k=50
table(knn.principal.24 ,test.knn.principal.labels)
confusionMatrix(table(knn.principal.24, test.knn.principal.labels))
