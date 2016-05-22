
# Install required R package
## If any of the following packages are not yet installed , then install by removing the hash and running the line individually

install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("corrplot")
install.packages("rattle"); 
install.packages("repmis")


#Load the R packages
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(rattle); 
library(repmis);

#Data Importing

## Import from the url

# trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
# testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# training <- source_data(trainingURL, na.strings = c("NA", "#DIV/0!", ""), header = TRUE)
# testing <- source_data(testingURL, na.strings = c("NA", "#DIV/0!", ""), header = TRUE)

## alternatively download the data in local machine and import data from local file

training <- read.csv("F:/Data Science/mc/programs/week4/pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("F:/Data Science/mc/programs/week4/pml-testing.csv", na.strings = c("NA", ""))

dim(training)
dim(testing)


# Data cleaning

## Remove columns that contain NA missing values
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

## Remove variable that does not contribute much to predict the the outcome 'classe'.

### Open the data csv file in xls
### You can see that the index column, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window have very littile predicting power for the outcome 'classe'
### These columns are the first seven column in the csv file

trainingData <- training[, -c(1:7)]
testingData <- testing[, -c(1:7)]

dim(trainingData)
dim(testingData)



#Data spliting

## The training data needs to be splitted to a training set and a validaion set in order to take care of out-of-sample errors.
## We are using a 70:30 ratio for training data set and validation data set
set.seed(7826) 
inTrain <- createDataPartition(trainingData$classe, p = 0.7, list = FALSE)
finalTrainingData <- trainingData[inTrain, ]
validationData <- trainingData[-inTrain, ]

dim(finalTrainingData)
dim(validationData)

 

# Prediction 
# Apply prediction algorithm on testing data set



### We should use 5-fold cross validation when applying the algorithm.
### default setting in trainControl function is 10, set the vallue as 5
controlRf <- trainControl(method="cv", 5)

### apply the alogorithm 
modelRf <- train(classe ~., data=finalTrainingData, method="rf", trControl=controlRf, ntree=250)
print(modelRf, digits = 4)



### Estimate the performance of the model on the validation data set.
predictRf <- predict(modelRf, validationData)
confusionMatrix(validationData$classe, predictRf)


accuracy <- postResample(predictRf, validationData$classe)
print(accuracy)


outOfSample <- 1 - as.numeric(confusionMatrix(validationData$classe, predictRf)$overall[1])
print(outOfSample)




# Predicting for Test Data Set
### Use random forests to predict the outcome variable classe for the testing set.
result <- predict(modelRf, testingData)

print (result)




### Correlation Matrix 
plot1 <- cor(finalTrainingData[, -length(names(finalTrainingData))]) 
corrplot(plot1, method="color")

### Decision Tree 
treeModel <- rpart(classe ~ ., data=finalTrainingData, method="class")
prp(treeModel) 

