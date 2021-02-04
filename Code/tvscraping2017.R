library(purrr) 
library(rvest)
library(data.table)
library(stringr)
library(tidyr)

#2018 Data Link
path1<-"https://www.sportsmediawatch.com/2017-college-football-tv-ratings/"

#Grab tables on webpage using read_html and html_table
tables<-html_table(read_html(path1),fill = TRUE)
temp<-NULL
result<-data.table()

#For loop to iterate through all tables on the page, convert them to data tables, and rbind them together
for (i in 1:length(tables)) {
  temp<-data.table(tables[[i]])
  result<-rbind(result,temp, fill=T)
}

result$Year<-2017

if (file.exists("2017ratings.csv")) {
  file.remove("2017ratings.csv")
}

#Write out result as 2017ratings csv
fwrite(result, "./Data/2017ratings.csv")




