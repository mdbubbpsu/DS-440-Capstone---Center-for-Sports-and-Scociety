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
data_big10<-fread("./Data/espn_big10.csv")
data_sec <- fread("./Data/espn_sec.csv")
data_acc <- fread("./Data/espn_acc.csv")
data_pac12 <- fread("./Data/espn_pac12.csv")
data_big12 <- fread("./Data/espn_big12.csv")
master_table <- rbind(data_big10, data_sec, data_acc, data_pac12, data_big12)


str(master_table)

kaggle_stats <- fread("./Data/kaggle_total_stats_clean.csv")

kaggle_stats$Win <- NULL
kaggle_stats$Loss <- NULL


#Change team names in the Big Ten
master_table$Team <- gsub('Indiana Hoosiers', "Indiana", master_table$Team)
master_table$Team <- gsub('Northwestern Wildcats', "Northwestern", master_table$Team)
master_table$Team <- gsub('Penn State Nittany Lions', "Penn State", master_table$Team)
master_table$Team <- gsub('Nebraska Cornhuskers', "Nebraska", master_table$Team)
master_table$Team <- gsub('Rutgers Scarlet Knights', "Rutgers", master_table$Team)
master_table$Team <- gsub('Maryland Terrapins', "Maryland", master_table$Team)
master_table$Team <- gsub('Illinois Fighting Illini', "Illinois", master_table$Team)
master_table$Team <- gsub('Michigan State Spartans', "Michigan State", master_table$Team)
master_table$Team <- gsub('Ohio State Buckeyes', "Ohio State", master_table$Team)
master_table$Team <- gsub('Wisconsin Badgers', "Wisconsin", master_table$Team)
master_table$Team <- gsub('Iowa Hawkeyes', "Iowa", master_table$Team)
master_table$Team <- gsub('Minnesota Golden Gophers', "Minnesota", master_table$Team)
master_table$Team <- gsub('Michigan Wolverines', "Michigan", master_table$Team)
master_table$Team <- gsub('Purdue Boilermakers', "Purdue", master_table$Team)

#Change team names in the ACC
master_table$Team <- gsub('Clemson Tigers', "Clemson", master_table$Team)
master_table$Team <- gsub('Florida State Seminoles', "Fliroda State", master_table$Team)
master_table$Team <- gsub('Georgia Tech Yellow Jackets', "Georgia Tech", master_table$Team)
master_table$Team <- gsub('NC State Wolfpack', "NC State", master_table$Team)
master_table$Team <- gsub('Syracuse Orange', "Syracuse", master_table$Team)
master_table$Team <- gsub('Virginia Tech Hokies', "Virginia", master_table$Team)
master_table$Team <- gsub('Boston College Eagles', "Boston College", master_table$Team)

#Change team names in the PAC 12
master_table$Team <- gsub('Oregon State Beavers', "Oregon State", master_table$Team)
master_table$Team <- gsub('USC Trojans', "USC", master_table$Team)
master_table$Team <- gsub('Colorado Buffaloes', "Colorado", master_table$Team)
master_table$Team <- gsub('UCLA Bruins', "UCLA", master_table$Team)
master_table$Team <- gsub('Washington Huskies', "Washington", master_table$Team)
master_table$Team <- gsub('Arizona Wildcats', "Arizona", master_table$Team)
master_table$Team <- gsub('California Golden Bears', "California", master_table$Team)
master_table$Team <- gsub('Oregon Ducks', "Oregon", master_table$Team)
master_table$Team <- gsub('Arizona State Sun Devils', "Arizona State", master_table$Team)
master_table$Team <- gsub('Utah Utes', "Utah", master_table$Team)
master_table$Team <- gsub('Stanford Cardinal', "Stanford", master_table$Team)
master_table$Team <- gsub('Washington State Cougars', "Washington State", master_table$Team)


#Change team names in the SEC
master_table$Team <- gsub("Alabama Crimson Tide", "Alabama", master_table$Team)
master_table$Team <- gsub("Georgia Bulldogs", "Georgia", master_table$Team)
master_table$Team <- gsub("Texas A&M Aggies", "Texas A&M", master_table$Team)
master_table$Team <- gsub("Arkansas Razorbacks", "Arkansas", master_table$Team)
master_table$Team <- gsub("Ole Miss Rebels", "Ole Miss", master_table$Team)
master_table$Team <- gsub("Missouri Tigers", "Missouri", master_table$Team)
master_table$Team <- gsub("Florida Gators", "Florida", master_table$Team)
master_table$Team <- gsub("Kentucky Wildcats", "Kentucky", master_table$Team)
master_table$Team <- gsub("Mississippi State Bulldogs", "Mississippi State", master_table$Team)
master_table$Team <- gsub("Tennessee Volunteers", "Tennessee", master_table$Team)
master_table$Team <- gsub("South Carolina Gamecocks", "South Carolina", master_table$Team)
master_table$Team <- gsub("Vanderbilt Commodores", "Vanderbilt", master_table$Team)
master_table$Team <- gsub("Auburn Tigers", "Auburn", master_table$Team)
master_table$Team <- gsub("LSU Tigers", "LSU", master_table$Team)

#Change team names in the Big 12
master_table$Team <- gsub('Baylor Bears', "Baylor", master_table$Team)
master_table$Team <- gsub('Iowa State Cyclones', "Iowa State", master_table$Team)
master_table$Team <- gsub('Kansas State Wildcats', "Kansas State", master_table$Team)
master_table$Team <- gsub('Oklahoma Sooners', "Oklahoma", master_table$Team)
master_table$Team <- gsub('West Virginia Mountaineers', "West Virginia", master_table$Team)
master_table$Team <- gsub('Kansas Jayhawks', "Kansas", master_table$Team)
master_table$Team <- gsub('Texas Longhorns', "Texas", master_table$Team)
master_table$Team <- gsub('Oklahoma State Cowboys', "Oklahoma State", master_table$Team)
master_table$Team <- gsub('TCU Horned Frogs', "TCU", master_table$Team)
master_table$Team <- gsub('Texas Tech Red Raiders', "Texas Tech", master_table$Team)

kaggle <- as.data.table(fread("./Data/CFBattendance.csv"))
avgCapacity <- aggregate(kaggle[,12], list(kaggle$Team), mean)
avgCapacity$Team <- avgCapacity$Group.1
master_table <- merge(master_table, avgCapacity, by='Team')

#Merge together kaggle data and master data
ross_stats <- merge(master_table, kaggle_stats, by=c('Team', 'Year'))
ross_stats

#ross_stats$Year <- as.factor(ross_stats$Year)
#ross_stats <- ross_stats %>% separate('W.L', c("W", "L"))

ross_stats$W <-substr(ross_stats$W.L, 1,gregexpr(pattern = "-", train$W.L)[[1]])
ross_stats$W<- gsub("[^0-9.]","",ross_stats$W) 

ross_stats$L <-substr(ross_stats$W.L, gregexpr(pattern = "-", train$W.L)[[1]] +1, nchar(ross_stats$W.L))
ross_stats$L<- gsub("[^0-9.]","",ross_stats$L) 

#ross_stats$L <- as.integer(ross_stats$L)
ross_stats <- na.omit(ross_stats)


trainIndex <- createDataPartition(ross_stats$Attendance,p=0.80,list=FALSE)

#splitting data into training/testing data using the trainIndex object


train <- ross_stats[trainIndex,] #training data (75% of data)
test <- ross_stats[-trainIndex,] #testing data (25% of data)

train <- as.data.table(train)
train$trainFactorAtt <- as.numeric(as.factor(train$Team))
test$testFactorAtt <- as.numeric(as.factor(test$Team))

train$Group.1 <- NULL
test$Group.1 <- NULL
setnames(train, "Stadium Capacity", "Average Stadium Capacity")
setnames(test, "Stadium Capacity", "Average Stadium Capacity")


trainX <- train[, c('trainFactorAtt','W', 'L', 'FPI','Average Stadium Capacity', 'PD')]
trainY <- train[,'Attendance']

testX <- test[, c('testFactorAtt','W', 'L', 'FPI', 'Average Stadium Capacity','PD')]

team<- test$Team
year <- test$Year
testY <- test[,'Attendance']

trainX <- data.matrix(trainX)
testX <- data.matrix(testX)

gl_model<-cv.glmnet(trainX, as.double(unlist(trainY)), alpha = 1)


bestLam <- gl_model$lambda.min

glM <- glmnet(trainX, as.double(unlist(trainY)), alpha = 1)

plot_glmnet(glM)

pred <- predict(glM, s=bestLam, newx = testX)

results <- cbind(team, year,  pred, testY)
results$pred <-results$'1'
results$actual <- results$Attendance
results$Attendance <-  NULL
results$'1' <- NULL
results <- as.data.frame(results)

for (i in 1:nrow(results)) {
  results$pred[i]
  results$actual[i]
  results$RMSE[i] <- RMSE(results$pred[i], results$actual[i])
}

