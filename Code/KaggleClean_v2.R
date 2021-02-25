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

#Focus on Indiana
IU<-big10[big10$Team=="Indiana"]

#Wins
Win <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = sum(Win))

#Losses
Loss <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = sum(Loss))

PF <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = sum(Points_For))

PA <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = sum(Points_Against))

PD <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = sum(Points_For - Points_Against))

AvgPPG <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = mean(Points_For))

AvgPA <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = mean(Points_Against))

Attendance <- IU %>%
  mutate(date = Year) %>%
  group_by(date) %>%
  summarize(x = mean(Attendance))

IU_MRD <- data.frame(year <- c(2000:2018), Wins <- Win$x, Losses <- Loss$x, PointsFor <- PF$x, PointsAgainst <- PA$x,
                     PointDifferential <- PD$x, AvgPPG <- AvgPPG$x, AvgPA <- AvgPA$x, AvgAttendance <- Attendance$x)
new_names <- c('Year', 'Wins', 'Losses', 'PointsFor', 'PointsAgainst', 'PointDifferential', 'AvgPPG', 'AvgPA', 'AvgAttendance')
names(IU_MRD) <- new_names
