# Get the data csv files
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))

if(!file.exists("./data")){dir.create("./data")}
trainingDataUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testDataUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainingFileName <- "./data/pml-training.csv"
testFileName <- "./data/pml-test.csv"
if(!file.exists(trainingFileName)) {
    download.file(trainingDataUrl, destfile=trainingFileName)
}
if(!file.exists(testFileName)) {
    download.file(testDataUrl, destfile=testFileName)
}

training <- read.csv(trainingFileName, na.strings=c("NA", ""))
test <- read.csv(testFileName, na.strings=c("NA", ""))

## Cleaning Data
# Remove the predictors with mssing values
training <- training[, colSums(is.na(training))==0]
test <- test[, colSums(is.na(test))==0]
# After checking the names and values of the columns using *head(training)*, I decide to remove the first seven columns, since they are irrelevant and non-numeric and therefore, of little influence on predication
training <- training[, -c(1:7)]
test <- test[, -c(1:7)]

## Data Partitioning
# Now, we split the training set into two partition, training set and validation set
set.seed(1234)
inTrain <- createDataPartition(training$classe, p = 0.7, list = FALSE)
train <- training[inTrain, ]
valid <- training[-inTrain, ]

## Cross Validation
# We use 5-fold cross validation, which is a compromise of accuracy and efficiency on my computer
fitControl <- trainControl(method='cv', number = 5)

## Model Training
# My first attempt is random forest method, since as far as I know, it is a very good classification algorithm. Fortunately, it gives me a prediction result which is good enough, so I don't need to try other algorithms. I expect the out of sample error is low and should be below 3%.
fitModel <- randomForest(classe ~ ., data = train, importance = FALSE, trControl = fitControl)
fitModel
## Assessing Out of Sample Error
pred <- predict(fitModel, valid)
confMat <- confusionMatrix(valid$classe, pred)
confMat
# The accuracy is sufficiently good.
confMat$overall[1]
# The out of sample error rate is very low, which is what I expected.

## Predicting the Test Set
# Use my prediction model to predict 20 different test cases
predict(fitModel, test)

