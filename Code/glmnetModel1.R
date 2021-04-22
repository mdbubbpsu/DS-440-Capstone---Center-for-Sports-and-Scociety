library(data.table)
library(caret)
library(ggpubr)
library(Metrics)
library(ggplot2)
library(dplyr)
library(plotmo)
library(yardstick)
library(glmnet)
library(corrplot)
library(Hmisc)
library(mlbench)


################################################################################
# TWO MAIN FILES IN THIS REPOSITORY: featureEngineering.R and glmnetModel.R
# all the others were data preprocesessing
################################################################################

#Read in dataset

master_table <- fread("./Data/master_table.csv")
test <- fread("./Data/NAIVETEST.csv")
master_table$Conference <- as.numeric(as.factor(master_table$Conference))
#splitting data into training/testing data using the trainIndex object
#trainIndex <- createDataPartition(master_table$Attendance,p=0.80,list=FALSE)
train <- master_table #training data (75% of data)
#test <- master_table[-trainIndex,] #testing data (25% of data)
# Clean up train/test
train <- as.data.table(train)
train$teamAsFactor <- as.numeric(as.factor(train$Team))
test$teamAsFactor <- as.numeric(as.factor(test$Team))

setnames(train, "capacity", "Stadium Capacity")
setnames(test, "capacity", "Stadium Capacity")

#Split train/test X/y on the features we want and the label
trainX <- train[, c('teamAsFactor','W', 'L', 'FPI','Stadium Capacity','PD')]
trainY <- train[,'Attendance']
testX <- test[, c('teamAsFactor','W', 'L', 'FPI', 'Stadium Capacity', 'PD')]
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

test$FabricatedPredAtt <- results$pred

#Inspect model
mean(results$RMSE)
coeef
sumSquares <- sum((results$actual - mean(results$actual))^2)
sumSqaureError <- sum((results$pred - results$actual)^2)
rsq <- 1 - sumSqaureError / sumSquares


# Compare before and after attendance
statusQuo <- aggregate(test$Attendance, by= list(Team = test$Team), FUN=sum)
Fabricated <-aggregate(test$FabricatedPredAtt, by= list(Team = test$Team), FUN=sum)

teamCount <- as.data.table(count(test,Team))
Fabricated<- merge(Fabricated,teamCount , by = 'Team')
Fabricated$new <- Fabricated$x / Fabricated$n
statusQuo<- merge(statusQuo,teamCount , by = 'Team')
statusQuo$new <- statusQuo$x / statusQuo$n



statusQuo$AverageAttPerYear <- statusQuo$new
statusQuo$x <- NULL
statusQuo$new <- NULL
statusQuo$numberOfYears <- statusQuo$n
statusQuo$n <- NULL

Fabricated$AverageAttPerYear <- Fabricated$new
Fabricated$x <- NULL
Fabricated$new <- NULL
Fabricated$numberOfYears <- statusQuo$n
Fabricated$n <- NULL
Fabricated$percentCapacity <- NULL
Fabricated$percentChange <- NULL

Fabricated$differenceFromStatusQuo <-  Fabricated$AverageAttPerYear - statusQuo$AverageAttPerYear 

for (i in 1 : range(nrow(Fabricated))[1]){
  statusQuo$percentCapacity[i] <- (statusQuo$AverageAttPerYear[i] / mean(test$`Stadium Capacity`[test$Team==statusQuo$Team[i] ])) * 100
  Fabricated$percentCapacity[i] <- (Fabricated$AverageAttPerYear[i] / mean(test$`Stadium Capacity`[test$Team==Fabricated$Team[i] ])) * 100
  Fabricated$percentChange[i] <-Fabricated$percentCapacity[i] -  statusQuo$percentCapacity[i]
}
################################################
statusQuo$numberOfYears <- NULL
Fabricated$numberOfYears <- NULL
################################################

percentChange<- sum(Fabricated$percentChange)
statusQuo
Fabricated
rsq

percentChange



