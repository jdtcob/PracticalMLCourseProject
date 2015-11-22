# Read in data
train_RAW <- read.csv("pml-training.csv", header = TRUE, sep=",")

# Generate brief snapshot of data
head(train_RAW)
str(train_RAW)

# Calculate number of NAs per variable
all_NA <- NULL
for (i in 1:length(train_RAW)){
  all_NA <- cbind(all_NA, sum(is.na(train_RAW[,i])))
}

# Remove variables with any NAs (looking at 'all_NA', either zero or 19,000+ NAs)
trainClean1 <- train_RAW[,-(which(all_NA > 0))]

# Calculate number of blanks per variable
all_sp<-NULL
for (j in 1:length(trainClean1)) {
  all_sp <- cbind(all_sp, as.numeric(sum(trainClean1[,j]=="")))
}

# Remove variables with any NAs (looking at 'all_NA', either zero or 19,000+ NAs)
trainClean2 <- trainClean1[,-(which(all_sp>0))]

# Remove the first 7 variables, non essential to classificiation
trainClean2 <- trainClean2[,-c(1:7)]
finalFeatures <- names(trainClean2) # Class labels are stored in the last variable


# Load Caret package, partition training and test sets based entirely on the training data
library(caret)
trainInd <- createDataPartition(trainClean2$classe, p=0.75 ,list=FALSE)
train_data <- trainClean2[trainInd,]
test_data <- trainClean2[-trainInd,]

# Create a random forest model with cross validation
controlOpts <- trainControl(method = "repeatedcv",number = 5, repeats = 10)
model <- train(classe~., data=train_data, trControl = controlOpts, method="rf")

# Calculate predictions, generate confusion matrix
predictions <- predict(model, test_data[,1:52])
confusionMatrix(predictions, test_data$classe)

# Find variable overlap (blind test data and trained model)
finalFeatures <- names(trainClean2)
all_loc <- NULL
for (z in finalFeatures){
  all_loc <- cbind(all_loc, which(z==names(test_RAW)))
}
all_loc <- as.numeric(all_loc)

# Read in blind test data, Create separate lower dimensional test data
test_RAW <- read.csv("pml-testing.csv", header = TRUE, sep=",")
test_data_blind <- test_RAW[,all_loc]

# Generate class predictions based on Random Forest model with test data (unknown class labels)
predictionsBlindCV <- predict(model, test_data_blind)

# Write predictions to files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(as.character(predictionsBlindCV))

