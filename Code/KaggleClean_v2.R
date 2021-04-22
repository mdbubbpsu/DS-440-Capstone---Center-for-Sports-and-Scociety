library(data.table)
library(tidyr)
library(lubridate)
library(dplyr)


#Read in dataset
kaggle<-fread("./Data/CFBattendance.csv")


#Subset data to look at just Big Ten games
big10<-kaggle[Conference == "Big-10"]

#################################################

big10$newResult <- big10$Result
#Split up result column into multiple columns
big10$newResult<-gsub("W","",big10$newResult)
big10$newResult<-gsub("L","",big10$newResult)
big10$newResult<-gsub("OT","",big10$newResult)
#big10$Result<-gsub("–","",big10$Result)
big10$Points_For <- 0
big10$Points_Against<- 0

for (i in 1: nrow(big10)){
big10$Points_For[i] <-strsplit(big10$newResult, "[–]")[[i]][1]
big10$Points_Against[i] <-strsplit(big10$newResult, "[–]")[[i]][2]
}


#Transform points for and against into numeric types
big10<-transform(big10, Points_For = as.numeric(Points_For))
big10<-transform(big10, Points_Against = as.numeric(Points_Against))

#Create new column, "Win" so that total number of wins can be calculated. Do same for Loss
big10$Win <-ifelse(grepl("W", big10$Result, fixed = TRUE), 1, 0)
big10$Loss <-ifelse(grepl("L", big10$Result, fixed = TRUE), 1, 0)


######################################################
#Sample of how to generate all rows of model ready data by team
######################################################
big10 <- na.omit(big10)


Stats <- as.data.frame(big10 %>%
  group_by(Year, Team) %>%
  summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against) ,
  AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))


#SEC

#Subset data to look at just SEC games
sec<-kaggle[Conference == "SEC"]
sec$newResult <- sec$Result
#Split up result column into multiple columns
sec$newResult<-gsub("W","",sec$newResult)
sec$newResult<-gsub("L","",sec$newResult)
sec$newResult<-gsub("OT","",sec$newResult)
sec$newResult<-gsub("\\(vacated\\)","",sec$newResult)
sec$Points_For <- 0
sec$Points_Against<- 0
for (i in 1: nrow(sec)){
  sec$Points_For[i] <-strsplit(sec$newResult, "[–]")[[i]][1]
  sec$Points_Against[i] <-strsplit(sec$newResult, "[–]")[[i]][2]
}

#Transform points for and against into numeric types
sec<-transform(sec, Points_For = as.numeric(Points_For))
sec<-transform(sec, Points_Against = as.numeric(Points_Against))

#Create new column, "Win" so that total number of wins can be calculated. Do same for Loss
sec$Win <-ifelse(grepl("W", sec$Result, fixed = TRUE), 1, 0)
sec$Loss <-ifelse(grepl("L", sec$Result, fixed = TRUE), 1, 0)

sec <- na.omit(sec)
Stats_sec <- as.data.frame(sec %>%
                         group_by(Year, Team) %>%
                         summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against),
                                   AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))

Stats <- rbind(Stats, Stats_sec)
#Big-12

#Subset data to look at just Big-12 games
Big12<-kaggle[Conference == "Big-12"]
Big12$newResult <- Big12$Result
#Split up result column into multiple columns
Big12$newResult<-gsub("W","",Big12$newResult)
Big12$newResult<-gsub("L","",Big12$newResult)
Big12$newResult<-gsub("OT","",Big12$newResult)
Big12$newResult<-gsub("\\(vacated\\)","",Big12$newResult)

Big12$Points_For <- 0
Big12$Points_Against<- 0
for (i in 1: nrow(Big12)){
  Big12$Points_For[i] <-strsplit(Big12$newResult, "[–]")[[i]][1]
  Big12$Points_Against[i] <-strsplit(Big12$newResult, "[–]")[[i]][2]
}

#Transform points for and against into numeric types
Big12<-transform(Big12, Points_For = as.numeric(Points_For))
Big12<-transform(Big12, Points_Against = as.numeric(Points_Against))

#Create new column, "Win" so that total number of wins can be calculated. Do same for Loss
Big12$Win <-ifelse(grepl("W", Big12$Result, fixed = TRUE), 1, 0)
Big12$Loss <-ifelse(grepl("L", Big12$Result, fixed = TRUE), 1, 0)

Big12 <- na.omit(Big12)
Big12 <- as.data.frame(Big12 %>%
                             group_by(Year, Team) %>%
                             summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against),
                                       AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))
Stats <- rbind(Stats, Big12)





#ACC

#Subset data to look at just Big-12 games
ACC<-kaggle[Conference == "ACC"]
ACC$newResult <- ACC$Result
#Split up result column into multiple columns
ACC$newResult<-gsub("W","",ACC$newResult)
ACC$newResult<-gsub("L","",ACC$newResult)
ACC$newResult<-gsub("OT","",ACC$newResult)
ACC$newResult<-gsub("\\(vacated\\)","",ACC$newResult)

ACC$Points_For <- 0
ACC$Points_Against<- 0
for (i in 1: nrow(ACC)){
  ACC$Points_For[i] <-strsplit(ACC$newResult, "[–]")[[i]][1]
  ACC$Points_Against[i] <-strsplit(ACC$newResult, "[–]")[[i]][2]
}

#Transform points for and against into numeric types
ACC<-transform(ACC, Points_For = as.numeric(Points_For))
ACC<-transform(ACC, Points_Against = as.numeric(Points_Against))

#Create new column, "Win" so that total number of wins can be calculated. Do same for Loss
ACC$Win <-ifelse(grepl("W", ACC$Result, fixed = TRUE), 1, 0)
ACC$Loss <-ifelse(grepl("L", ACC$Result, fixed = TRUE), 1, 0)

ACC <- na.omit(ACC)
ACC <- as.data.frame(ACC %>%
                             group_by(Year, Team) %>%
                             summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against),
                                       AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))
Stats <- rbind(Stats, ACC)


#Pac-12

#Subset data to look at just Big-12 games
Pac12<-kaggle[Conference == "Pac-12"]
Pac12$newResult <- Pac12$Result
#Split up result column into multiple columns
Pac12$newResult<-gsub("W","",Pac12$newResult)
Pac12$newResult<-gsub("L","",Pac12$newResult)
Pac12$newResult<-gsub("OT","",Pac12$newResult)
Pac12$newResult<-gsub("\\(vacated\\)","",Pac12$newResult)

Pac12$Points_For <- 0
Pac12$Points_Against<- 0
for (i in 1: nrow(Pac12)){
  Pac12$Points_For[i] <-strsplit(Pac12$newResult, "[–]")[[i]][1]
  Pac12$Points_Against[i] <-strsplit(Pac12$newResult, "[–]")[[i]][2]
}

#Transform points for and against into numeric types
Pac12<-transform(Pac12, Points_For = as.numeric(Points_For))
Pac12<-transform(Pac12, Points_Against = as.numeric(Points_Against))

#Create new column, "Win" so that total number of wins can be calculated. Do same for Loss
Pac12$Win <-ifelse(grepl("W", Pac12$Result, fixed = TRUE), 1, 0)
Pac12$Loss <-ifelse(grepl("L", Pac12$Result, fixed = TRUE), 1, 0)

Pac12 <- na.omit(Pac12)
Pac12 <- as.data.frame(Pac12 %>%
                       group_by(Year, Team) %>%
                       summarize(Win = sum(Win), Loss = sum(Loss), PF = sum(Points_For), PA = sum(Points_Against), PD = sum(Points_For - Points_Against),
                                 AvgPF = mean(Points_For), AvgPA = mean(Points_Against), Attendance = mean(Attendance)))
Stats <- rbind(Stats, Pac12)
fwrite(Stats, "./Data/CORRECTCSV.csv")