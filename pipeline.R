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

str(work.data)
describe(work.data)

#visualize the missing data
missmap(work.data)
