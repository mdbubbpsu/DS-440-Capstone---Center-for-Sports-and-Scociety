install.packages("rvest")
install.packages("janitor")
library(rvest)
library(tidyverse)
library(janitor)
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




master_cols <- c(1,2,3,4,14)
master_table <- master_table[master_cols]
names(master_table)[5]<-paste("Year")
str(master_table)


psu_ranks <- master_table[master_table$Team == "Penn State Nittany Lions",]
psu_ranks
