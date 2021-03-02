install.packages("rvest")
install.packages("janitor")
library(rvest)
library(tidyverse)
library(janitor)

#how i am doing this- not very automated and very annoying
#1. run this to create 2020 year

url <- read_html("https://www.espn.com/college-football/fpi/_/season/2020/group/5")
url

tables <- html_nodes(url, "table")
tables

table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
str(table1)
table1 <- as.data.frame(table1)
table1$Year <- 2020
table1 %>% row_to_names(row_number = 1)
#table1 <- table1[-c(1),]

master_table <- table1

#run this chunk changing the year in url and the year column down from 2019-2005

url <- read_html("https://www.espn.com/college-football/fpi/_/season/2005/group/5")
url

tables <- html_nodes(url, "table")
tables

table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
str(table1)
table1 <- as.data.frame(table1)
table1$Year <- 2005
#table1 %>% row_to_names(row_number = 1)
table1 <- table1[-c(1),]
master_table <- rbind(master_table, table1)

master_table <- master_table %>% row_to_names(row_number = 1)
str(master_table)
#down to here


#run this to get the necessary columns
master_cols <- c(1,2,3,4,14)
master_table <- master_table[master_cols]
names(master_table)[5]<-paste("Year")
str(master_table)


#SEC

url <- read_html("https://www.espn.com/college-football/fpi/_/season/2020/group/8")
url

tables <- html_nodes(url, "table")
tables

table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
str(table1)
table1 <- as.data.frame(table1)
table1$Year <- 2020
table1 %>% row_to_names(row_number = 1)
#table1 <- table1[-c(1),]

master_table_sec <- table1

url <- read_html("https://www.espn.com/college-football/fpi/_/season/2005/group/8")
url

tables <- html_nodes(url, "table")
tables

table1 <- url %>% html_nodes("table") %>% html_table(fill = T) 
str(table1)
table1 <- as.data.frame(table1)
table1$Year <- 2005
#table1 %>% row_to_names(row_number = 1)
table1 <- table1[-c(1),]
master_table_sec <- rbind(master_table_sec, table1)


master_table_sec <- master_table_sec %>% row_to_names(row_number = 1)


master_cols <- c(1,2,3,4,14)
master_table_sec <- master_table_sec[master_cols]
names(master_table_sec)[5]<-paste("Year")
str(master_table_sec)

master_table_sec['Year'] <- c(NA, head(master_table_sec['Year'], dim(master_table_sec)[1] - 1)[[1]])
