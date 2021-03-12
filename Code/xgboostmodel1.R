#Load required packages
library(data.table)
library(caret)
library(ggpubr)
library(Metrics)
library(ggplot2)
library(xgboost)
library(tidyr)
library(DiagrammeR)

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


#Merge together kaggle data and master data
ross_stats <- merge(master_table, kaggle_stats, by=c('Team', 'Year'))
ross_stats

ross_stats$Year <- as.factor(ross_stats$Year)
ross_stats <- ross_stats %>% separate('W.L', c("W", "L"))

ross_stats$W <- as.integer(ross_stats$W)
ross_stats$L <- as.integer(ross_stats$L)


#creating index so data can be split into train and test
trainIndex <- createDataPartition(ross_stats$Attendance,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
train <- ross_stats[trainIndex,] #training data (75% of data)
test <- ross_stats[-trainIndex,] #testing data (25% of data)

#Hold out attendance from test set 
y_test<-test$Attendance
test$Attendance<-0

#Create y_train variable
y_train<-train$Attendance

#Extract columns for modeling
model_train<-train[,.(W, L, Attendance)]
model_test<-test[,.(W,L, Attendance)]

#Convert to matrices
model_train<-as.matrix(model_train)
model_test<-as.matrix(model_test)

#Create proper representation of data for modeling
dtrain <- xgb.DMatrix(model_train,label=y_train,missing=NA)
dtest <- xgb.DMatrix(model_test,missing=NA)

param <- list(  objective           = "reg:linear",
                gamma               =.01,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = .01,
                max_depth           = 15,
                min_child_weight    = 10,
                subsample           = .99,
                colsample_bytree    = .99,
                tree_method = 'hist'
)

#Cross Validation
XGB_cv<-xgb.cv(params=param,nrounds=10000,nfold=5,missing=NA,data=dtrain,print_every_n=1,nthread=4, 
               early_stopping_rounds=25)

#removed test = dtest and put after the cross validation (potential bug fix)
watchlist <- list(train = dtrain)

#Train the model
XGB_model<-xgb.train(params=param,nrounds= 3299,missing=NA,data=dtrain,watchlist=watchlist,
                     print_every_n=1,nthread=4)

#Create Predictions
pred<-predict(XGB_model, newdata=dtest)

test$Predicted <- pred
test$Attendance <- y_test

RMSE <- function (x, test) sqrt(mean((x-test$Predicted)^2))
test$Error <- lapply(test$Attendance, FUN = RMSE, test)


cols<-XGB_model[["feature_names"]]
importance <- xgb.importance(feature_names = cols, model = XGB_model)
xgb.plot.importance(importance_matrix = importance, top_n = 50)


xgb.plot.tree(model = XGB_model, trees =0)
