library(caret)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

neu.work.data <- work.data
neu.work.data.n <- as.data.frame(lapply(neu.work.data[,1:11], normalize))
#neu.work.data.n$Simple.Legal.Status <- NULL
neu.work.data <- na.omit(neu.work.data)

#Building a model
#split data into training and test data sets
neu.indxTrain <- createDataPartition(y = neu.work.data$Classifier, p = 0.7, list = F)
neu.training <- neu.work.data.n[neu.indxTrain, ]
neu.testing <- neu.work.data.n[-neu.indxTrain, ]

#neu.training.classifier[['Classifier']] = factor(neu.training[["Classifier"]])
neu.training.classifier <- factor(as.character(neu.work.data[neu.indxTrain, 12]))
neu.testing.classifier <- factor(as.character(neu.work.data[-neu.indxTrain, 12]))

#Neural Network
library(neuralnet)
nn <- neuralnet(neu.training.classifier ~ Publication.Year + Count.of.Simple.Family.Members +
                  Simple.Family.Cited.by.Count + Count.of.Other.References + Count.of.Cited.by.Patents +
                  Count.of.Cited.by.Patents.Within.3.years + Count.of.Cited.by.Patents.Within.5.years +
                  Simple.Legal.Status + Count.of.claims + Quality.of.Family, data=neu.training, hidden=c(2,2), linear.output=FALSE, threshold=0.1)
nn$result.matrix
plot(nn)

#Test
nn.results <- compute(nn, neu.testing)
neu.results <- data.frame(actual = neu.testing.classifier, prediction = nn.results$net.result)

roundedresults<-sapply(neu.results[,2:3],round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)

neu.res.u <- union(neu.testing.classifier,roundedresultsdf$prediction.2)
neu.res.t <- table(factor(neu.testing.classifier, neu.res.u), factor(roundedresultsdf$prediction.2, neu.res.u))
confusionMatrix(neu.res.t)
