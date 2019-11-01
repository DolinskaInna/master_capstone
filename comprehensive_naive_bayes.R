#Data Visualization

#Visual 1
ggplot(work.data, aes(Publication.Year, colour = Classifier))+
  geom_freqpoly(binwidth = 1) + labs(title = "Publication Year Distribution by Classifier")

#Visual 2
c <- ggplot(work.data, aes(x=Count.of.Simple.Family.Members, fill=Classifier, color=Classifier)) +
  geom_histogram(binwidth = 1) + labs(title="Count.of.Simple.Family.Members Distribution by Classifier")
c + theme_bw()

#visual 3
ggpairs(work.data)

#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = work.data$Classifier, p = 0.7, list = F)
training <- work.data[indxTrain, ]
testing <- work.data[-indxTrain, ]

#Check dimensions of the split
prop.table(table(work.data$Classifier))*100

prop.table(table(training$Classifier))*100
prop.table(table(testing$Classifier))*100

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-12]
y = training$Classifier

library(e1071)
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model

#Model Evaluation
#Predict testing set
Predict <- predict(model, newdata = testing)

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict, testing$Classifier)

#Plot Variable performance
X <- varImp(model)
plot(X)

#Laplace Correction
patent.classifier <- naiveBayes(training[, -12], training$Classifier)
patent.pred <- predict(patent.classifier, testing)
#confusionMatrix(patent.pred, testing$Classifier)
gmodels::CrossTable(patent.pred, testing$Classifier, prop.chisq = F, chisq = F, prop.t = F, dnn = c("Predicted", "Actual"))
confusionMatrix(patent.pred, testing$Classifier)

up.patent.classifier <- naiveBayes(training[, -12], training$Classifier, laplace = 1)
up.patent.pred <- predict(up.patent.classifier, testing)
gmodels::CrossTable(up.patent.pred, testing$Classifier, prop.chisq = F, chisq = F, prop.t = F, dnn = c("Predicted", "Actual"))
confusionMatrix(up.patent.pred, testing$Classifier)


#Principal component analysis
pca.work.data <- work.data[c('Publication.Year', 'Count.of.Simple.Family.Members', 'Simple.Family.Cited.by.Count',
                           'Count.of.Other.References', 'Count.of.Cited.by.Patents', 'Count.of.Cites.Patents',
                           'Count.of.Cited.by.Patents.Within.3.years', 'Count.of.Cited.by.Patents.Within.5.years', 
                           'Count.of.claims')]
pca.data <- princomp(pca.work.data, scores = T, cor = T)
loadings(pca.data)

#screeplot of eigen values ( Value of standard deviation is considered as eigen values)
screeplot(pca.data, type = 'line', main = 'Screeplot')
#Biplot of score variables
biplot(pca.data)
#Scores of the components
pca.data$scores[1:10,]


#Exploratory Factor Analysis
#Using PCA we've determined 3 factors - Comp 1, Comp 2 and Comp 3
pcaFac <- factanal(pca.work.data, factors = 5, rotation = 'varimax')
pcaFac
