library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)

data <- read.csv(choose.files(), header = T, sep = ";", stringsAsFactors = T)

work.data <- data[c('Publication.Year', 'Count.of.Simple.Family.Members', 'Simple.Family.Cited.by.Count', 
'Count.of.Other.References', 'Count.of.Cited.by.Patents', 'Count.of.Cites.Patents', 'Count.of.Cited.by.Patents.Within.3.years',
'Count.of.Cited.by.Patents.Within.5.years', 'Simple.Legal.Status', 'Count.of.claims', 'Quality.of.Family',
'Classifier')]
indxTrain <- createDataPartition(y = svm.work.data$Classifier, p = 0.7, list = F)

work.data$Publication.Year <- 2020-work.data$Publication.Year
str(work.data)
describe(work.data)

work.data <- fastDummies::dummy_cols(work.data, select_columns = c('Simple.Legal.Status', 'Quality.of.Family'))
work.data$Simple.Legal.Status <- NULL
work.data$Quality.of.Family <- NULL
work.data$Simple.Legal.Status_ <- NULL
work.data$Main.IPC.Subclass <- NULL
work.data$Classifier <- as.character(work.data$Classifier)
work.data$Classifier[work.data$Classifier == "Strong"] <- 1
work.data$Classifier[work.data$Classifier == "Weak"] <- 0
classifier <- work.data$Classifier

#visualize the missing data
missmap(work.data)


# Principal Component Analysis --------------------------------------------

work.data$Simple.Legal.Status <- as.character(work.data$Simple.Legal.Status)
work.data$Simple.Legal.Status[work.data$Simple.Legal.Status == "Active"] <- 4
work.data$Simple.Legal.Status[work.data$Simple.Legal.Status == "Inactive"] <- 3
work.data$Simple.Legal.Status[work.data$Simple.Legal.Status == "Pending"] <- 2
work.data$Simple.Legal.Status[work.data$Simple.Legal.Status == "Undetermined"] <- 1
work.data$Simple.Legal.Status <- as.numeric(work.data$Simple.Legal.Status)

work.data$Quality.of.Family <- as.character(work.data$Quality.of.Family)
work.data$Quality.of.Family[work.data$Quality.of.Family == "High"] <- 1
work.data$Quality.of.Family[work.data$Quality.of.Family == "Low"] <- 0
work.data$Quality.of.Family <- as.numeric(work.data$Quality.of.Family)

work.data$Classifier <- as.character(work.data$Classifier)
work.data$Classifier[work.data$Classifier == "Strong"] <- 1
work.data$Classifier[work.data$Classifier == "Weak"] <- 0
work.data$Classifier <- as.numeric(work.data$Classifier)


t.work.data <- na.omit(as.matrix(work.data[, -12]))
desc.stats.t <- prcomp(t(t.work.data), scale = T)
summary(desc.stats.t)

plot(desc.stats.t$x[,1], desc.stats.t$x[,2])
decs.stats.var <- desc.stats$sdev^2
decs.stats.var.per <- round(decs.stats.var/sum(decs.stats.var)*100, 1)
barplot(decs.stats.var.per, main = "Percentage plot", xlab = "Principal component", ylab = "Percent Variation")

library(ggplot2)
decs.stats.data <- data.frame(Sample = rownames(desc.stats.t$x), X = desc.stats.t$x[,1], Y = desc.stats.t$x[,2])

ggplot(data=decs.stats.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", decs.stats.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", decs.stats.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("Patent Data PCA Graph")


principal.work.data <- work.data[c('Count.of.Simple.Family.Members', 'Simple.Family.Cited.by.Count',
                                   'Count.of.Other.References', 'Count.of.Cited.by.Patents', 'Count.of.Cites.Patents', 'Count.of.Cited.by.Patents.Within.3.years',
                                   'Count.of.Cited.by.Patents.Within.5.years', 'Simple.Legal.Status', 'Quality.of.Family',
                                   'Classifier')]

important.naive.work.data <- work.data[c('Publication.Year', 'Count.of.claims', 'Count.of.Simple.Family.Members',  
                                   'Count.of.Cited.by.Patents', 'Count.of.Cites.Patents', 
                                   'Simple.Legal.Status', 'Quality.of.Family',
                                   'Classifier')]

str(principal.work.data)
principal.work.data$Classifier <- as.factor(principal.work.data$Classifier)
missmap(work.data)
principal.work.data <- na.omit(principal.work.data)

important.naive.work.data$Classifier <- as.factor(important.naive.work.data$Classifier)
important.naive.work.data <- na.omit(important.naive.work.data)
