install.packages("rvest")
install.packages("janitor")
library(rvest)
library(tidyverse)
library(janitor)

years <- c(2020:2005)

#big 10

master_tableb10 <- data.table('Team' = NA, 'W-L' = NA, 'FPI' = NA, 'RK' = NA, 'TREND' = NA, 'PROJ' = NA, 'WINOUT' = NA, '6WIN' = NA, 'DIV' = NA, 'CONF' = NA, 'PLAYOFF' = NA, 'NC' = NA, 'WINNC' = NA, 'Year' = NA)

for(i in years){
  url <- read_html(paste0("https://www.espn.com/college-football/fpi/_/season/", i, "/group/5"))
  url
  
  tables <- html_nodes(url, "table")
  tables
  
  table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
  str(table1)
  table1 <- as.data.frame(table1)
  table1$Year <- i
  table1 <- table1[2:nrow(table1),]
  
  master_tableb10 <- rbind(master_tableb10, table1, use.names = F)
}

master_tableb10 <- data.frame(master_tableb10)


#run this to get the necessary columns
master_cols <- c(1,2,3,4,14)
master_tableb10 <- master_tableb10[master_cols]
names(master_tableb10)[5]<-paste("Year")
str(master_tableb10)

fwrite(master_tableb10, "./Data/espn_big10.csv")

#SEC
master_tablesec <- data.table('Team' = NA, 'W-L' = NA, 'FPI' = NA, 'RK' = NA, 'TREND' = NA, 'PROJ' = NA, 'WINOUT' = NA, '6WIN' = NA, 'DIV' = NA, 'CONF' = NA, 'PLAYOFF' = NA, 'NC' = NA, 'WINNC' = NA, 'Year' = NA)

for(i in years){
  url <- read_html(paste0("https://www.espn.com/college-football/fpi/_/season/", i, "/group/8"))
  url
  
  tables <- html_nodes(url, "table")
  tables
  
  table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
  str(table1)
  table1 <- as.data.frame(table1)
  table1$Year <- i
  table1 <- table1[2:nrow(table1),]
  
  master_tablesec <- rbind(master_tablesec, table1, use.names = F)
}

master_tablesec <- data.frame(master_tablesec)


#run this to get the necessary columns
master_cols <- c(1,2,3,4,14)
master_tablesec <- master_tablesec[master_cols]
names(master_tablesec)[5]<-paste("Year")
str(master_tablesec)

fwrite(master_tablesec, "./Data/espn_sec.csv")

#big 12
master_tablebig12 <- data.table('Team' = NA, 'W-L' = NA, 'FPI' = NA, 'RK' = NA, 'TREND' = NA, 'PROJ' = NA, 'WINOUT' = NA, '6WIN' = NA, 'DIV' = NA, 'CONF' = NA, 'PLAYOFF' = NA, 'NC' = NA, 'WINNC' = NA, 'Year' = NA)

for(i in years){
  url <- read_html(paste0("https://www.espn.com/college-football/fpi/_/season/", i, "/group/4"))
  url
  
  tables <- html_nodes(url, "table")
  tables
  
  table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
  str(table1)
  table1 <- as.data.frame(table1)
  table1$Year <- i
  table1 <- table1[2:nrow(table1),]
  
  master_tablebig12 <- rbind(master_tablebig12, table1, use.names = F)
}

master_tablebig12 <- data.frame(master_tablebig12)


#run this to get the necessary columns
master_cols <- c(1,2,3,4,14)
master_tablebig12 <- master_tablebig12[master_cols]
names(master_tablebig12)[5]<-paste("Year")
str(master_tablebig12)

fwrite(master_tablebig12, "./Data/espn_big12.csv")

#pac12
master_tablepac12 <- data.table('Team' = NA, 'W-L' = NA, 'FPI' = NA, 'RK' = NA, 'TREND' = NA, 'PROJ' = NA, 'WINOUT' = NA, '6WIN' = NA, 'DIV' = NA, 'CONF' = NA, 'PLAYOFF' = NA, 'NC' = NA, 'WINNC' = NA, 'Year' = NA)

for(i in years){
  url <- read_html(paste0("https://www.espn.com/college-football/fpi/_/season/", i, "/group/9"))
  url
  
  tables <- html_nodes(url, "table")
  tables
  
  table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
  str(table1)
  table1 <- as.data.frame(table1)
  table1$Year <- i
  table1 <- table1[2:nrow(table1),]
  
  master_tablepac12 <- rbind(master_tablepac12, table1, use.names = F)
}

master_tablepac12 <- data.frame(master_tablepac12)


#run this to get the necessary columns
master_cols <- c(1,2,3,4,14)
master_tablepac12 <- master_tablepac12[master_cols]
names(master_tablepac12)[5]<-paste("Year")
str(master_tablepac12)

fwrite(master_tablepac12, "./Data/espn_pac12.csv")


#acc
master_tableacc <- data.table('Team' = NA, 'W-L' = NA, 'FPI' = NA, 'RK' = NA, 'TREND' = NA, 'PROJ' = NA, 'WINOUT' = NA, '6WIN' = NA, 'DIV' = NA, 'CONF' = NA, 'PLAYOFF' = NA, 'NC' = NA, 'WINNC' = NA, 'Year' = NA)

for(i in years){
  url <- read_html(paste0("https://www.espn.com/college-football/fpi/_/season/", i, "/group/1"))
  url
  
  tables <- html_nodes(url, "table")
  tables
  
  table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
  str(table1)
  table1 <- as.data.frame(table1)
  table1$Year <- i
  table1 <- table1[2:nrow(table1),]
  
  master_tableacc <- rbind(master_tableacc, table1, use.names = F)
}

master_tableacc <- data.frame(master_tableacc)


#run this to get the necessary columns
master_cols <- c(1,2,3,4,14)
master_tableacc <- master_tableacc[master_cols]
names(master_tableacc)[5]<-paste("Year")
str(master_tableacc)

fwrite(master_tableacc, "./Data/espn_acc.csv")

