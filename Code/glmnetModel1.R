library(data.table)
library(caret)
library(ggpubr)
library(Metrics)
library(ggplot2)
library(dplyr)
library(plotmo)
library(yardstick)
library(glmnet)
#Read in dataset

master_table <- fread("./Data/master_table.csv")

#splitting data into training/testing data using the trainIndex object
trainIndex <- createDataPartition(master_table$Attendance,p=0.80,list=FALSE)
train <- master_table[trainIndex,] #training data (75% of data)
test <- master_table[-trainIndex,] #testing data (25% of data)

# Clean up train/test
train <- as.data.table(train)
train$trainFactorAtt <- as.numeric(as.factor(train$Team))
test$testFactorAtt <- as.numeric(as.factor(test$Team))
train$Group.1 <- NULL
test$Group.1 <- NULL
setnames(train, "Stadium Capacity", "Average Stadium Capacity")
setnames(test, "Stadium Capacity", "Average Stadium Capacity")


#Split train/test X/y on the features we want and the label
trainX <- train[, c('trainFactorAtt','W', 'L', 'FPI','Average Stadium Capacity', 'PD')]
trainY <- train[,'Attendance']
testX <- test[, c('testFactorAtt','W', 'L', 'FPI', 'Average Stadium Capacity','PD')]
testY <- test[,'Attendance']



team<- test$Team
year <- test$Year

# Convert to matrix for GLMNet model
trainX <- data.matrix(trainX)
testX <- data.matrix(testX)

#Cross validation to find best lambda
gl_model<-cv.glmnet(trainX, as.double(unlist(trainY)), alpha = 0)
bestLam <- gl_model$lambda.min

#Train glmnet
glM <- glmnet(trainX, as.double(unlist(trainY)), alpha = 0)
plot_glmnet(glM)
coeef <- predict(glM, s=bestLam, newx = testX, type = "coefficients")

#Predict based on model
pred <- predict(glM, s=bestLam, newx = testX)
results <- cbind(team, year,  pred, testY)
results$pred <-results$'1'
results$actual <- results$Attendance
results$Attendance <-  NULL
results$'1' <- NULL
results <- as.data.frame(results)


#Find RMSE from predictions
results$RMSE<- 0
testY <- as.double(unlist(testY))
for (i in 1:nrow(results)) {
  results$pred[i]
  results$actual[i]
  results$RMSE[i] <- rmse_vec(as.vector(pred[i]), as.vector(testY[i]))
}


#Inspect model
mean(results$RMSE)
coeef
sumSquares <- sum((results$actual - mean(results$actual))^2)
sumSqaureError <- sum((results$pred - results$actual)^2)
rsq <- 1 - sumSqaureError / sumSquares
rsq
