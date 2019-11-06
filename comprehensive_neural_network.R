library(caret)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

neu.work.data <- work.data
neu.work.data$Simple.Legal.Status <- as.character(neu.work.data$Simple.Legal.Status)
neu.work.data$Simple.Legal.Status[neu.work.data$Simple.Legal.Status == "Active"] <- 4
neu.work.data$Simple.Legal.Status[neu.work.data$Simple.Legal.Status == "Inactive"] <- 3
neu.work.data$Simple.Legal.Status[neu.work.data$Simple.Legal.Status == "Pending"] <- 2
neu.work.data$Simple.Legal.Status[neu.work.data$Simple.Legal.Status == "Undetermined"] <- 1
neu.work.data$Simple.Legal.Status <- as.numeric(neu.work.data$Simple.Legal.Status)

neu.work.data$Quality.of.Family <- as.character(neu.work.data$Quality.of.Family)
neu.work.data$Quality.of.Family[neu.work.data$Quality.of.Family == "High"] <- 1
neu.work.data$Quality.of.Family[neu.work.data$Quality.of.Family == "Low"] <- 0
neu.work.data$Quality.of.Family <- as.numeric(neu.work.data$Quality.of.Family)

neu.work.data$Classifier <- as.character(neu.work.data$Classifier)
neu.work.data$Classifier[neu.work.data$Classifier == "Strong"] <- 1
neu.work.data$Classifier[neu.work.data$Classifier == "Weak"] <- 0
neu.work.data$Classifier <- as.numeric(neu.work.data$Classifier)

neu.work.data.n <- as.data.frame(lapply(neu.work.data[,1:11], normalize))
#neu.work.data.n$Simple.Legal.Status <- NULL
#neu.work.data[is.na(neu.work.data)] <- 0

#Building a model
#split data into training and test data sets
neu.indxTrain <- createDataPartition(y = neu.work.data$Classifier, p = 0.7, list = F)
neu.training <- neu.work.data.n[neu.indxTrain, ]
neu.testing <- neu.work.data.n[-neu.indxTrain, ]

#neu.training.classifier[['Classifier']] = factor(neu.training[["Classifier"]])
neu.training.classifier <- factor(neu.work.data[neu.indxTrain, 12])
neu.testing.classifier <- factor(neu.work.data[-neu.indxTrain, 12])

#Neural Network
library(neuralnet)
nn <- neuralnet(neu.training.classifier ~ Publication.Year + Count.of.Simple.Family.Members +
                  Simple.Family.Cited.by.Count + Count.of.Other.References + Count.of.Cited.by.Patents +
                  Count.of.Cited.by.Patents.Within.3.years + Count.of.Cited.by.Patents.Within.5.years +
                  Simple.Legal.Status + Count.of.claims + Quality.of.Family, data=neu.training, hidden=c(2,2), linear.output=FALSE, threshold=0.5)
nn$result.matrix
plot(nn)

#Test
nn.results <- compute(nn, neu.testing)
neu.results <- data.frame(actual = neu.testing.classifier, prediction = nn.results$net.result)

roundedresults<-sapply(neu.results[,2:3],round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)

confusionMatrix(table(actual,prediction.2))
