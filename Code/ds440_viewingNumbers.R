install.packages("rvest")
library(rvest)
library(tidyverse)

url <- read_html("https://www.sportsmediawatch.com/college-football-tv-ratings/")
url

tables <- html_nodes(url, "table")
tables

table1 <- url %>% html_nodes("table") %>% .[1] %>% html_table(fill = T)
str(table1)

table1 <- as.data.frame(table1)
table1
table1$X1 <- NULL
table1$X3 <- NULL

table1$X2 <- gsub(".*:", "", table1$X2)

table1$Team1 <- sub("-.*", "", table1$X2)
table1$Team1 <- str_remove_all(table1$Team1, "[*]")

table1$Team2 <- sub(".*-", "", table1$X2)
table1$Team2 <- str_remove_all(table1$Team2, "[*]")

table1$team1_conf <- NA
table1$team2_conf <- NA

table1$Team1 <- trimws(table1$Team1, "left")

BIG10 <- c("ILL", "IND", "IOWA", "MICH", "MSU", "MINN", "NEB", "NW", "NWSTN", "OSU", 
           "PSU", "PUR", "RUTG", "UMD", "WIS", "WISC")
ACC <- c("BC", "CLEM", "DUKE", "FSU", "GT", "LOU", "MIA", "NCST", "NC ST", "PITT", "SYR", 
         "UNC", "UVA", "VT", "WAKE")
BIG12 <- c("BAY", "ISU", "IA ST", "KU", "KSU", "OKLA", "OU", "OKST", "OK ST", "TCU", "TEX", 
           "TTU", "WVU")
PAC12 <- c("ARIZ", "ASU", "CAL", "COLO", "COL", "ORE", "ORST", "OR ST", "STAN", "UCLA", "USC", 
           "UTAH", "WASH", "UW", "WSU")
SEC <- c("ALA", "BAMA", "ARK", "AUB", "FLA", "UF", "UGA", "UK", "LSU", "MISS", "MSST", "MS ST", 
         "MIZ", "MIZZ", "SC", "TENN", "TAMU", "T A&M", "VAN")
AAC <- c("CIN", "CONN", "ECU", "HOU", "MEM", "NAVY", "SMU", "USF", "TEM", "TULN", "TULANE", "TLSA", "TULSA", "UCF")
CUSA <- c("CHAR", "FAU", "FIU", "LT", "MRSH", "MARSH", "MTSU", "UNT", "N TEX", "ODU", "RICE", "USM", "UTEP"
          ,"UTSA", "WKU")

table1

for(i in 2:nrow(table1)){
  if(table1$Team1[i] %in% BIG10){
    table1$team1_conf[i] <- "BIG10"
  }else if(table1$Team1[i] %in% ACC){
      table1$team1_conf[i] <- "ACC"
  }else if(table1$Team1[i] %in% BIG12){
    table1$team1_conf[i] <- "BIG12"
  }else if(table1$Team1[i] %in% PAC12){
    table1$team1_conf[i] <- "PAC12"
  }else if(table1$Team1[i] %in% SEC){
    table1$team1_conf[i] <- "SEC"
  }else if(table1$Team1[i] %in% AAC){
    table1$team1_conf[i] <- "AAC"
  }else if(table1$Team1[i] %in% CUSA){
    table1$team1_conf[i] <- "CUSA"
  }else{
    table1$team1_conf[i] <- "Other"
  }
}

for(i in 2:nrow(table1)){
  if(table1$Team2[i] %in% BIG10){
    table1$team2_conf[i] <- "BIG10"
  }else if(table1$Team2[i] %in% ACC){
    table1$team2_conf[i] <- "ACC"
  }else if(table1$Team2[i] %in% BIG12){
    table1$team2_conf[i] <- "BIG12"
  }else if(table1$Team2[i] %in% PAC12){
    table1$team2_conf[i] <- "PAC12"
  }else if(table1$Team2[i] %in% SEC){
    table1$team2_conf[i] <- "SEC"
  }else if(table1$Team2[i] %in% AAC){
    table1$team2_conf[i] <- "AAC"
  }else if(table1$Team1[i] %in% CUSA){
    table1$team2_conf[i] <- "CUSA"
  }else{
    table1$team2_conf[i] <- "Other"
  }
}

write.csv(table1, file = "teams_with_conf")

table2 <- url %>% html_nodes("table") %>% .[2] %>% html_table(fill = T)
table2
table2 <- as.data.frame(table2)

table2$Team1 <- sub("-.*", "", table2$X2)
table2$Team2 <- sub(".*-", "", table2$X2)

table2$team1_conf <- NA
table2$team2_conf <- NA

for(i in 2:nrow(table2)){
  if(table2$Team1[i] %in% BIG10){
    table2$team1_conf[i] <- "BIG10"
  }else if(table2$Team1[i] %in% ACC){
    table2$team1_conf[i] <- "ACC"
  }else if(table2$Team1[i] %in% BIG12){
    table2$team1_conf[i] <- "BIG12"
  }else if(table2$Team1[i] %in% PAC12){
    table2$team1_conf[i] <- "PAC12"
  }else if(table2$Team1[i] %in% SEC){
    table2$team1_conf[i] <- "SEC"
  }else if(table2$Team1[i] %in% AAC){
    table2$team1_conf[i] <- "AAC"
  }else if(table2$Team1[i] %in% CUSA){
    table2$team1_conf[i] <- "CUSA"
  }else{
    table2$team1_conf[i] <- "Other"
  }
}

for(i in 2:nrow(table2)){
  if(table2$Team2[i] %in% BIG10){
    table2$team2_conf[i] <- "BIG10"
  }else if(table2$Team2[i] %in% ACC){
    table2$team2_conf[i] <- "ACC"
  }else if(table2$Team2[i] %in% BIG12){
    table2$team2_conf[i] <- "BIG12"
  }else if(table2$Team2[i] %in% PAC12){
    table2$team2_conf[i] <- "PAC12"
  }else if(table2$Team2[i] %in% SEC){
    table2$team2_conf[i] <- "SEC"
  }else if(table2$Team2[i] %in% AAC){
    table2$team2_conf[i] <- "AAC"
  }else if(table2$Team1[i] %in% CUSA){
    table2$team2_conf[i] <- "CUSA"
  }else{
    table2$team2_conf[i] <- "Other"
  }
}
