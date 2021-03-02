library(data.table)
library(caret)
library(ggpubr)
library(Metrics)

#Read in dataset
data<-fread("./Data/model0_data.csv")


#Set seed for randomization
set.seed(123) 

#creating index so data can be split into train and test
trainIndex <- createDataPartition(data$Attendance,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
train <- data[trainIndex,] #training data (75% of data)
test <- data[-trainIndex,] #testing data (25% of data)

#Hold out attendance from test set 
y_test<-test$Attendance
test$Attendance<-NULL

#create regression model using lm for variables mentioned below
model1<- lm(Attendance ~ Year+ PF + PA + PD + AvgPF + AvgPA + W + L + FPI + RK, data = train)
model<-summary(model1)
model$sigma
model$residuals
model$coefficients
#Create predictions on test set
Att<-predict(model1, test)

#Create table to analyze results
results<-data.table()

results$Team <- test$Team
results$Actual<-y_test
results$Predicted<-Att

for (i in 1:nrow(results)) {
  results$RMSE <- rmse(results$Actual[i], results$Predicted[i])
}

#Write out data
#fwrite(results, "./Data/model0results.csv")
