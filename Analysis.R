rm(list=ls())
library(caret)
library(kernlab)
library(randomForest)
getwd()
setwd("C:\\Users\\AMANIDD\\Documents\\datasciencecoursera\\PracticalMachineLearning")
trainURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainURL, destfile = "./pml-training.csv")
download.file(testURL, destfile = "./pml-testing.csv")

training<-read.csv("pml-training.csv", header=TRUE, dec=".", sep=",")
testing<-read.csv("pml-testing.csv", header=TRUE, dec=".", sep=",")

# Defining and threshold level of NA and empty values
max_level <- 0.95 * dim(training)[1]

# Let remove columns which are fully 'NA' or full empty or have more than 95 % NA or empty values
usefull_cols <- !apply(training, 2, function(x) sum(is.na(x)) > max_level || sum(x=="") > max_level)
training1 <- training[, usefull_cols]
# Now, let drop 7 first columns which don't seem very usefull
training2 <- subset(training1, select=-c(1:7))
# We can also remove variable with very low variance
nearZvar <- nearZeroVar(training2, saveMetrics = TRUE)
training2 <- training2[ , nearZvar$nzv==FALSE] 
dim(training2)
#Finally, we remove variable strongly correlated with a correlation coefficient more than 95%
corr_matrix <- abs(cor(training2[,-dim(training2)[2]]))
# Making the default diagonal values from '1' to '0', so that these values aren't included later 
diag(corr_matrix) <- 0

# Here we will be removing the columns which are highly correlated
correlated_col <- findCorrelation(corr_matrix, verbose = FALSE , cutoff = .95)
training2 <- training2[, -c(correlated_col)]
dim(training2)

#Splitting training2 dataset into 2 datasets
inTrain<-createDataPartition(training2, p=0.9, list=FALSE)
training2A<-training2[inTrain,]
training2B<-training2[-inTrain,]

#Data Modeling
set.seed(3267)
randMod<-randomForest(classe~., data=training2A, importance=TRUE)
randMod
#Testing previous model on the training2B dataset
predictions<-predict(randMod, newdata=training2B)
#Confusion matrix
confusionMatrix(predictions, training2B$classe)
#Let check the out of sample error basis on the confusion matrix
sum(diag(confusionMatrix(predictions, training2B$classe)$table))/
sum(confusionMatrix(predictions, training2B$classe)$table)
#Apply model on test dataset provided
predictions_test<-predict(randMod, newdata=testing)
predictions_test

# Let run the following function to submit the answer in the testing dataset.
pml_write_files<- function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions_test)








# Testing the above model of the training dataset
predictions <- predict(randMod, newdata=testing)
# Showing the Confusion Matrix here :
confusionMatrix(predictions, training2$classe)




















# the training dataset
inTrain = createDataPartition(training$classe, p = 0.7, list=FALSE)
train1A <- training[inTrain,]
train2A <- training[-inTrain,]

# While creating the model, we have to specify 'importance=TRUE' for further ease in Variable-Importance plot.
# randForMod <- randomForest(classe~., data=train1A)
randomForMod <- randomForest(classe~., data=train1A, importance=TRUE)
randomForMod















# Testing the above model on train2A  dataset of the training dataset
train2A_pred <- predict(randomForMod, newdata=train2A)
# Showing the Confusion Matrix here :
confusionMatrix(train2A_pred, train2A$classe)

confM <- confusionMatrix(train2A_pred, train2A$classe)
sum(diag(confM$table))/sum(confM$table)












