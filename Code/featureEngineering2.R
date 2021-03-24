library(rvest)
library(tidyverse)
library(janitor)


#PointDifferential

PointsFor <- data.frame()

years <- c(2010:2020)

groups <- c(1,4,5,8,9)

for(conf in groups){
for(year in years){
url <- read_html(paste0("https://www.espn.com/college-football/stats/team/_/season/", year,"/group/", conf,"/table/passing/sort/totalPoints/dir/desc"))
tables <- html_nodes(url, "table")

table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 

teamNames <- as.data.frame(table1[1])
teamStats <- as.data.frame(table1[2])

table2 <- cbind(teamNames, teamStats)

final <- table2[,c(1,9)]
final <- final[2:nrow(final),]
names(final)[1]<-paste("Team")
names(final)[2]<-paste("PF")
final$Year <- year
final$Conf <- conf

PointsFor <- rbind(PointsFor, final)

}}

############

PointsAgainst <- data.frame()

years <- c(2010:2020)

groups <- c(1,4,5,8,9)

for(conf in groups){
for(year in years){
  url <- read_html(paste0("https://www.espn.com/college-football/stats/team/_/view/defense/season/", year,"/group/", conf,"/table/passing/sort/totalPoints/dir/asc"))
  tables <- html_nodes(url, "table")
  
  table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
  
  teamNames <- as.data.frame(table1[1])
  teamStats <- as.data.frame(table1[2])
  
  table2 <- cbind(teamNames, teamStats)
  
  final <- table2[,c(1,9)]
  final <- final[2:nrow(final),]
  names(final)[1]<-paste("Team")
  names(final)[2]<-paste("PA")
  final$Year <- year
  final$Conf <- conf
  
  PointsAgainst <- rbind(PointsAgainst, final)
  
}}



master_table_PD <- merge(PointsFor, PointsAgainst, by=c('Team', 'Year'))
str(master_table_PD)
master_table_PD$PF <- as.integer(master_table_PD$PF)
master_table_PD$PA <- as.integer(master_table_PD$PA)

for(i in (1:nrow(master_table_PD))){
  master_table_PD$PD[i] <- master_table_PD$PF[i]-master_table_PD$PA[i]
}

master_table_PD$Conference <- master_table_PD$Conf.x
master_table_PD$Conf.x <- NULL
master_table_PD$Conf.y <- NULL

master_table_PD$Conference <- gsub(1, "ACC", master_table_PD$Conference)
master_table_PD$Conference <- gsub(8, "SEC", master_table_PD$Conference)
master_table_PD$Conference <- gsub(5, "Big10", master_table_PD$Conference)
master_table_PD$Conference <- gsub(9, "PAC12", master_table_PD$Conference)
master_table_PD$Conference <- gsub(4, "BIG12", master_table_PD$Conference)

master_table_PD <- master_table_PD[,c(1,2,5,6)]



#Intergating with ESPN FPI

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
espn_data <- rbind(data_big10, data_sec, data_acc, data_pac12, data_big12)

espn_data <- espn_data %>% separate('W.L', c("W", "L"))

espn_data$W <- as.integer(espn_data$W)
espn_data$L <- as.integer(espn_data$L)

master_data <- merge(master_table_PD, espn_data, by=c('Team', 'Year', 'Conference'))


#Capacity


url <- read_html("http://www.collegegridirons.com/comparisons.htm")
tables <- html_nodes(url, "table")

table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 

capacity <- as.data.frame(table1[8])
capacity <- capacity[,c(2,3,4)]
names(capacity)[1]<-paste("Team")
names(capacity)[2]<-paste("Conference")
names(capacity)[3]<-paste("Capacity")
capacity <- capacity[c(13:50, 93:118),]

espn_names <- sort(unique(espn_data$Team))
espn_names <- espn_names[2:66]
capacity_names <- sort(capacity$Team)
capacity_names[65] <- 'test'

names <- data.table()
names$Espn <- espn_names
names$cap <- capacity_names

names$cap[28] = 'Michican'
names$cap[27] = 'Michican State'

names$cap[32:65] <- names$cap[31:64]

names$cap[53] = 'Texas'
names$cap[52] = 'Texas A&M'
names$cap[30] = 'Mississippi State'
names$cap[31] = 'Missouri'
names$cap[32] = 'NC State'
names$cap[33] = 'Nebraska'
names$cap[34] = 'North Carolina'
names$cap[35] = 'Northwestern'
names$cap[36] = 'Notre Dame'
names$cap[37] = 'Ohio State'
names$cap[38] = 'Oklahoma'
names$cap[39] = 'Oklahoma State'
names$cap[40] = "Mississippi"

names <- names[c(1:35, 37:65),]

names$Espn[39] = 'Oklahoma State Cowboys'
names$Espn[38] = 'Oklahoma Sooners'
names$Espn[37] = 'Ohio State Buckeyes'
names$Espn[36] = 'Northwwestern Wildcats'
names$Espn[35] = 'North Carolina Tar Heels'
names$Espn[34] = 'Nebraska Cornhuskers'
names$Espn[33] = 'NC State Wolfpack Tigers'
names$Espn[32] = 'Missouri Tigers'
names$Espn[31] = 'Mississippi State Bulldogs'
names$Espn[30] = 'Ole Miss Rebels'


names$Espn[51] = 'Texas Longhorns'
names$Espn[52] = 'Texas A&M Aggies'

names$Espn[27] = 'Michigan Wolverines'
names$Espn[28] = 'Michigan State Spartans'


capacity <- capacity[order(capacity$Team),]
capacity$Team <- names$Espn


total_data <- merge(master_data, capacity, by = 'Team')
total_data <- total_data[,c(1:8,10)]
names(total_data)[3]<-paste("Conference")

fwrite(total_data, "./Data/master_table_v2.csv")
