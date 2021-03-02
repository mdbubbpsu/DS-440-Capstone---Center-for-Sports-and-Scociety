#ross type model

library(data.table)
library(caret)
library(ggpubr)
library(Metrics)
library(ggplot2)

#Read in dataset
data_big10<-fread("./Data/model0_data.csv")
data_big10$Conf <- "BIG10"
data_sec <- fread("./Data/model0data_sec.csv")
data_sec$Conf <- "SEC"
data <- rbind(data_big10, data_sec)

str(data)
data$Year <- as.factor(data$Year)

data_new <- data[,c(1,2,8,9,10,13)]
str(data_new)

#Set seed for randomization
set.seed(123) 

#creating index so data can be split into train and test
trainIndex <- createDataPartition(data_new$Attendance,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
train <- data_new[trainIndex,] #training data (75% of data)
test <- data_new[-trainIndex,] #testing data (25% of data)

#Hold out attendance from test set 
y_test<-test$Attendance
test$Attendance<-NULL


#create regression model using lm for variables mentioned below
model1<- lm(Attendance ~ W + L, data = train)
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
  results$RMSE[i] <- rmse(results$Actual[i], results$Predicted[i])
}

#Write out data
#fwrite(results, "./Data/model0results.csv")

results <- data.frame(results)
results$Conference <- NA
results[1:20,]$Conference <- "BIG10"
results[21:32,]$Conference <- "SEC"

ggplot(data = results, aes(x = Actual, y = Predicted, color = Conference)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(group = 1)) +
  labs(title = "Attendance- Actual vs. Predicted",
       x = "Actual Attendance",
       y = "Predicted Attendance")
