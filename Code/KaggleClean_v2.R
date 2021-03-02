library(data.table)
library(tidyr)
library(lubridate)
library(dplyr)

setwd('/Users/matthewmcquaite/Documents/GitHub/DS-440-Capstone---Center-for-Sports-and-Scociety')

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
#Sample of how to generate all rows of model ready data by team
######################################################
big10


Stats <- as.data.frame(big10 %>%
  group_by(Year, Team) %>%
  summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against),
  AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))


fwrite(Stats, "./Data/model0data.csv")

#SEC

#Subset data to look at just SEC games
sec<-kaggle[Conference == "SEC"]


#Split up result column into multiple columns
sec$Result<-gsub("OT","",sec$Result)
sec$Result<-gsub("–","",sec$Result)
sec<-separate(sec, Result, c("Result","Points"), sep = "[ ]")
sec<-transform(sec, Points = as.numeric(Points))
sec<-separate(sec, Points, c("Points_For","Points_Against"), sep = 2)

#Transform points for and against into numeric types
sec<-transform(sec, Points_For = as.numeric(Points_For))
sec<-transform(sec, Points_Against = as.numeric(Points_Against))

sec$Win <-ifelse(sec$Result == "W", 1, 0)
sec$Loss <-ifelse(sec$Result == "L", 1, 0)

Stats_sec <- as.data.frame(sec %>%
                         group_by(Year, Team) %>%
                         summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against),
                                   AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))

fwrite(Stats_sec, "./Data/model0data_sec.csv")
