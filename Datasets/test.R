library("jsonlite")
library("party")
library("tm")
library("dplyr")
library("ggplot2")
library("wordcloud")
library("rpart")
library("caret")
library("rpart.plot")
library("randomForest")
library("e1071")

setwd("C:\\Users\\acer\\Documents\\Acads\\Semester 6\\DWDM Lab\\Project")

train <- fromJSON("train.json")

#Data cleaning
corpus <- VCorpus(VectorSource(train$ingredients))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- VCorpus(VectorSource(corpus))

#word frequencies
frequencies <-  DocumentTermMatrix(corpus)
nrow(frequencies)

sparse <- removeSparseTerms(frequencies, 0.99)

dim(sparse)

sparseRep <- as.data.frame(as.matrix(sparse))
sparseRep$cuisine <-as.factor(train$cuisine)

#partitioning into test and training set
inTrain <- createDataPartition(y = sparseRep$cuisine, p = 0.8, list = FALSE)
training <- sparseRep[inTrain,]
testing <- sparseRep[-inTrain,]

#Prediction using decision tree
set.seed(9347)
cartModelFit <- rpart(cuisine ~ ., data = training, method = "class")
prp(cartModelFit) #plot tree
cartPredict <- predict(cartModelFit, newdata = testing, type = "class")

cartCM <- confusionMatrix(cartPredict, testing$cuisine)
cartCM
#accuracy = 0.4189

#Prediction using random forest
output.forest <- randomForest(cuisine ~ ., data = training)
print(output.forest) 
RFprediction <- predict(output.forest, testing)
cartCM <- confusionMatrix(RFprediction, testing$cuisine)
cartCM
#accuracy = 0.7368




