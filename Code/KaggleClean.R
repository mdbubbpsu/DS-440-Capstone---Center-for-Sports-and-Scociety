library(data.table)
library(tidyr)


#Read in dataset
kaggle<-fread("./Data/CFBattendance.csv")


#Subset data to look at just Big Ten games
big10<-kaggle[Conference == "Big-10"]


#Split up result column into multiple columns
big10$Result<-gsub("OT","",big10$Result)
big10$Result<-gsub("–","",big10$Result)
big10<-separate(big10, Result, c("Result","Points"), sep = "[ ]")
big10<-transform(big10, Points = as.numeric(Points))
big10<-separate(big10, Points, c("Points_For","Points_Against"), sep = 2)

#Transform points for and against into numeric types
big10<-transform(big10, Points_For = as.numeric(Points_For))
big10<-transform(big10, Points_Against = as.numeric(Points_Against))


#Create new column, "Win" so that total number of wins can be calculated. Do same for Loss
big10$Win <-ifelse(big10$Result == "W", 1, 0)
big10$Loss <-ifelse(big10$Result == "L", 1, 0)




######################################################
#Sample of how to generate one row of model ready data
######################################################

#Focus on Indiana
IU<-big10[big10$Team=="Indiana"]

#Looking at 2000 season
IU2000<-IU[IU$Year==2000]


#Compute summary stats 
IU2000$Wins<-sum(IU2000$Win)
IU2000$Losses<-sum(IU2000$Loss)
IU2000$TotalPoints_For<-sum(IU2000$Points_For)
IU2000$TotalPoints_Against<-sum(IU2000$Points_Against)
IU2000$PD<-IU2000$TotalPoints_For-IU2000$TotalPoints_Against
IU2000$AvgPPG<-mean(IU2000$Points_For)
IU2000$AvgPA<-mean(IU2000$Points_Against)
IU2000$AvgAttendance<-mean(IU2000$Attendance)


#Subset out columns not needed for modeling
drops<-c("Date","Time","Opponent","Site","Result","Points_For","Points_Against","Current Wins","Current Losses",
         "New Coach", "Tailgating","PRCP","SNOW","SNWD","TMAX","TMIN","Conference","Month","Day","Year","Win",
         "Loss","TV")

IU2000<-IU2000[, !drops, with = FALSE]

#Create one row in our model (one row represents the results of one team over the course of a season)
ModelRowIU2000<-IU2000[1,]

