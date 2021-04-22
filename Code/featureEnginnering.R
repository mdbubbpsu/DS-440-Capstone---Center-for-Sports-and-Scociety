
# Packages
library(data.table)
library(caret)
library(ggpubr)
library(Metrics)
library(ggplot2)
library(xgboost)
library(tidyr)
library(DiagrammeR)
library(dplyr)
library(plotmo)
library(yardstick)
library(glmnet)
library(knitr)


################################################################################
# TWO MAIN FILES IN THIS REPOSITORY: featureEngineering.R and glmnetModel.R
# all the others were data preprocesessing
################################################################################

#Read in dataset
data_big10 <-fread("./Data/espn_big10.csv")
data_big10$Conference = 'Big10'
data_sec <- fread("./Data/espn_sec.csv")
data_sec$Conference = 'SEC'
data_acc <- fread("./Data/espn_acc.csv")
data_acc$Conference = 'ACC'
data_pac12 <- fread("./Data/espn_pac12.csv")
data_pac12$Conference = 'PAC12'
data_big12 <- fread("./Data/espn_big12.csv")
data_big12$Conference = 'BIG12'
master_table <- rbind(data_big10, data_sec, data_acc, data_pac12, data_big12)

str(master_table)

# GOOD
kaggle_stats <- fread("./Data/CORRECTCSV.csv")
# BAD
# kaggle_stats <- fread("./Data/CORRECTCSV.csv")

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

#ross_stats[, c('W', 'L', 'FPI', 'RK', 'PF', 'PA', 'PD', 'AvgPF', 'AvgPA')] <- lapply(ross_stats[,c('W', 'L', 'FPI', 'RK', 'PF', 'PA', 'PD', 'AvgPF', 'AvgPA')], as.numeric)
# Standarize numeric columns
#ross_stats$W <- scale(ross_stats$W)
#ross_stats$L <- scale(ross_stats$L)
ross_stats$FPI <- scale(ross_stats$FPI)
ross_stats$RK <- scale(ross_stats$RK)
ross_stats$PF <- scale(ross_stats$PF)
ross_stats$PA <- scale(ross_stats$PA)
ross_stats$PD <- scale(ross_stats$PD)
ross_stats$AvgPF <- scale(ross_stats$AvgPF)
ross_stats$AvgPA <- scale(ross_stats$AvgPA)

att <- fread("./Data/accurateStadiumAtt.csv")
att$Team <- att$team

master_table <- merge(ross_stats, att, by='Team')
master_table <-na.omit(master_table)

#master_table$capacity <- scale(master_table$capacity)


master_table <- master_table[, -c("stadium","PF", "PA", "AvgPF", "AvgPA","city", "RK" ,"state", "team", "conference", "built", "expanded", "div", "latitude", "longitude")]
##################################################
#master_table <- master_table[Year == 2016 | Year == 2017 | Year == 2018 | Year == 2015]

####################################################################################################
####################################################################################################
#fullMaster <- fread("./Data/fullMaster.csv")
#master_table <- fullMaster
#master_table$FPI <- scale(master_table$FPI)
#master_table$PD <- scale(master_table$PD)
####################################################################################################
####################################################################################################


# Find number of unique Wins in Big10
big10UniqueNumberOfWins <- master_table[Conference == 'Big10', .(number_of_distinct_orders = uniqueN(W)), by = Conference]
newTestBig10 <- matrix(ncol=3, nrow=big10UniqueNumberOfWins$number_of_distinct_orders)
colnames(newTestBig10) <- c('Wins', 'AvgFPI', 'AvgPD')
subSet <-  master_table[Conference == 'Big10']
uniqueWins <- sort(unique(subSet$W))

# Loop through each value of a unique win and add the average FPI/PD for each win group
for (i in 0:big10UniqueNumberOfWins$number_of_distinct_orders) {
  
  newTestBig10[i,'Wins']<- uniqueWins[i]
  newTestBig10[i,'AvgFPI'] <- mean(master_table$FPI[master_table$W ==  uniqueWins[i] & master_table$Conference == 'Big10'])
  newTestBig10[i,'AvgPD' ] <- mean(master_table$PD[master_table$W ==  uniqueWins[i] & master_table$Conference == 'Big10'])

  
}
avg_FPI_PD_per_win <- newTestBig10
avg_FPI_PD_per_win
# Find differences in marginal wins for FPI and PD for TOP teams
newTestBig10 <- as.data.table(newTestBig10)
newTestBig10 <- na.omit(newTestBig10)
avgFPI <- newTestBig10[,AvgFPI]
diffFPI = c()
avgPD <- newTestBig10[,AvgPD]
diffPD = c()
avgPD <- newTestBig10[,AvgPD]
for (i in 0:length(avgFPI) + 1){
  print(avgFPI[i + 1])
  diffFPI[i] = avgFPI[i + 1] - avgFPI[i]
  diffPD[i] = avgPD[i + 1] - avgPD[i]
}
totalDiff <- cbind(diffFPI, diffPD)
totalDiff <- na.omit(totalDiff)
totalDiff <- as.data.table(totalDiff)
totalDiff$toXWins <- uniqueWins[-1]

marginal_values <- totalDiff
marginal_values

fabricateBig10 <-master_table[Conference == 'Big10'& (Year == 2017)]
realBefore <- fabricateBig10
realBefore$Real_Attendance <- realBefore$Attendance
realBefore$Attendance <- NULL

#####################################################################
# Baseline to judge attendance values off of. Sum of big10 teams
statusQuoAtt = rowsum(fabricateBig10$Attendance, fabricateBig10$Team)

#####################################################################

# Find range of FPI and PD
rangeFPI <- abs(range(fabricateBig10$FPI)[1] - range(fabricateBig10$FPI)[2])
rangePD <- abs(range(fabricateBig10$PD)[1] - range(fabricateBig10$PD)[2])

# Find top and bottom thirds of teams in Big 10
bottomThirdFPI <- range(fabricateBig10$FPI)[1] + rangeFPI / 3
bottomThirdPD <- range(fabricateBig10$PD)[1] + rangeFPI / 3
topThirdFPI <- range(fabricateBig10$FPI)[2] - rangeFPI / 3
topThirdrPD <- range(fabricateBig10$PD)[2] - rangeFPI / 3


# Split fabcricated DT into thirds based off of range for being in bottom third of FPI or PD
bottomThirdDf <- fabricateBig10[fabricateBig10$FPI < bottomThirdFPI & fabricateBig10$PD < bottomThirdPD]
topThirdDf <- fabricateBig10[fabricateBig10$FPI > topThirdFPI & fabricateBig10$PD >topThirdrPD]
topBefore <- topThirdDf
bottomBefore <- bottomThirdDf
middle <- anti_join(fabricateBig10, rbind(bottomThirdDf, topThirdDf))


# Change FPI values for Fabricated data
for (i in 1:nrow(bottomThirdDf)){
  bottomThirdDf$W[i] = bottomThirdDf$W[i]+1
  bottomThirdDf$L[i] = bottomThirdDf$L[i]-1
  bottomThirdDf$FPI[i] =  bottomThirdDf$FPI[i] + totalDiff$diffFPI[totalDiff$toXWins == bottomThirdDf$W[i]]
  bottomThirdDf$PD[i] =  bottomThirdDf$PD[i] + totalDiff$diffPD[totalDiff$toXWins == bottomThirdDf$W[i]]
  print(bottomThirdDf$FPI[i])
}
for (i in 1:nrow(topThirdDf)){
  topThirdDf$FPI[i] =  topThirdDf$FPI[i] - totalDiff$diffFPI[totalDiff$toXWins == topThirdDf$W[i]]
  topThirdDf$PD[i] =  topThirdDf$PD[i] - totalDiff$diffPD[totalDiff$toXWins == topThirdDf$W[i]]
  topThirdDf$W[i] = topThirdDf$W[i]-1
  topThirdDf$L[i] = topThirdDf$L[i]+1
  

  print(topThirdDf$FPI[i])
}

naiveTest <- rbind(bottomThirdDf, middle , topThirdDf)


fwrite(naiveTest, "./Data/NAIVETEST.csv")
fwrite(master_table, "./Data/master_table.csv")

