library(caret)
library(randomForest)
set.seed(12345)
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("pml-training.csv")){
        download.file(trainURL, "pml-training.csv")
}
if(!file.exists("pml-testing.csv")){
        download.file(testURL, "pml-testing.csv")
}

#Read the data sets
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("pml-testing.csv",na.strings = c("NA", "#DIV/0!", ""))

# remove near zero variables 
nsv <- nearZeroVar(training)
training <- training[, -nsv]
testing <- testing[, -nsv]

# remove columns with no variability

remNoVariabilityCols <- sapply(training, function(x) mean(is.na(x))) > 0.95
training <- training[, remNoVariabilityCols == F]
testing <- testing[, remNoVariabilityCols == F]

# remove columns that may not affect prediction
training <- training[, -(1:5)]
testing <- testing[, -(1:5)]

# To save computer resources create data partition with just 25% of data in train model.
inTrain <- createDataPartition(training$classe, p=0.25, list = FALSE)
subTraining <- training[inTrain,]
subTesting <- training[-inTrain,]
# Use CV method to save computer resoures
fitControl <- trainControl(method = "cv", number = 4)
modFit <- train(classe ~ ., data = subTraining, method = "rf", trControl = fitControl, prox = TRUE)
modFit
predTrain <- predict(modFit, subTesting)
confusionMatrix(predTrain, subTesting$classe)

accur <- postResample(subTesting$classe, predTrain)
modAccuracy <- accur[[1]]
modAccuracy
out_of_sample_error <- 1 - modAccuracy
out_of_sample_error
answers <- predict(modFit, testing)
answers <- as.character(answers)
answers
