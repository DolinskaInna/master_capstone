#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

knn.work.data <- work.data
knn.work.data$Simple.Legal.Status <- as.character(knn.work.data$Simple.Legal.Status)
knn.work.data$Simple.Legal.Status[knn.work.data$Simple.Legal.Status == "Active"] <- 4
knn.work.data$Simple.Legal.Status[knn.work.data$Simple.Legal.Status == "Inactive"] <- 3
knn.work.data$Simple.Legal.Status[knn.work.data$Simple.Legal.Status == "Pending"] <- 2
knn.work.data$Simple.Legal.Status[knn.work.data$Simple.Legal.Status == "Undetermined"] <- 1
knn.work.data$Simple.Legal.Status <- as.numeric(knn.work.data$Simple.Legal.Status)

knn.work.data$Quality.of.Family <- as.character(knn.work.data$Quality.of.Family)
knn.work.data$Quality.of.Family[knn.work.data$Quality.of.Family == "High"] <- 1
knn.work.data$Quality.of.Family[knn.work.data$Quality.of.Family == "Low"] <- 0
knn.work.data$Quality.of.Family <- as.numeric(knn.work.data$Quality.of.Family)

knn.work.data$Classifier <- as.character(knn.work.data$Classifier)
knn.work.data$Classifier[knn.work.data$Classifier == "Strong"] <- 1
knn.work.data$Classifier[knn.work.data$Classifier == "Weak"] <- 0
knn.work.data$Classifier <- as.numeric(knn.work.data$Classifier)

knn.work.data.n <- as.data.frame(lapply(knn.work.data[,1:11], normalize))
knn.work.data.n$Simple.Legal.Status <- NULL

#Building a model
#split data into training and test data sets
knn.indxTrain <- createDataPartition(y = work.data$Classifier, p = 0.7, list = F)
knn.training <- knn.work.data.n[knn.indxTrain, ]
knn.testing <- knn.work.data.n[-knn.indxTrain, ]

#Creating seperate dataframe for 'Classifier' feature which is our target.
train.knn.labels <- knn.work.data[knn.indxTrain,12]
test.knn.labels <-knn.work.data[-knn.indxTrain,12]

library(class)
sqrt(NROW(train.knn.labels))
#We have 2593 observations in our training data set.
#The square root of 2593 is around 50.92, therefore we’ll create two models. 
#One with ‘K’ value as 50 and the other model with a ‘K’ value as 51.
knn.50 <- knn(train=knn.training, test=knn.testing, cl=train.knn.labels, k=50)
knn.51 <- knn(train=knn.training, test=knn.testing, cl=train.knn.labels, k=51)

#Model Evaluation
#Calculate the proportion of correct classification for k = 26, 27
ACC.50 <- 100 * sum(test.knn.labels == knn.50)/NROW(test.knn.labels)
ACC.51 <- 100 * sum(test.knn.labels == knn.51)/NROW(test.knn.labels)

# Check prediction against actual value in tabular form for k=50
table(knn.50 ,test.knn.labels)
# Check prediction against actual value in tabular form for k=51
table(knn.51 ,test.knn.labels)

confusionMatrix(table(knn.50, test.knn.labels))
confusionMatrix(table(knn.51, test.knn.labels))

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
plot(1:i, k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
