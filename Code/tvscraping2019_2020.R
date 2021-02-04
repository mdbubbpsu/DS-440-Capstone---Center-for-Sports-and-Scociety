library(purrr) 
library(rvest)
library(data.table)
library(stringr)
library(tidyr)

#2020 Data Link
path1<-"https://www.sportsmediawatch.com/college-football-tv-ratings/"

#Grab tables on webpage using read_html and html_table
tables<-html_table(read_html(path1),fill = TRUE)
temp<-NULL
result<-data.table()

#For loop to iterate through all tables on the page, convert them to data tables, and rbind them together
for (i in 1:length(tables)) {
  temp<-data.table(tables[[i]])
  result<-rbind(result,temp, fill=T)
}

#Rename columns
setnames(result, c("X1","X2","X3","X4","X5","X6","X7"), 
         c("Date_Time","Game","Network","Rating","Plus_minus",
           "Viewership","Plus_minus_two"))

#Create year column
result$Year<-2020


#2019 Data link
path2<-"https://www.sportsmediawatch.com/college-football-tv-ratings/2/"

#Grab tables on webpage using read_html and html_table
tables2<-html_table(read_html(path2),fill = TRUE)
temp2<-NULL
result2<-data.table()


for (i in 1:length(tables2)) {
  temp2<-data.table(tables2[[i]])
  result2<-rbind(result2,temp2, fill=T)
}

#For loop to iterate through all tables on the page, convert them to data tables, and rbind them together
setnames(result2, c("X1","X2","X3","X4","X5","X6","X7"), 
         c("Date_Time","Game","Network","Rating","Plus_minus",
           "Viewership","Plus_minus_two"))

#Create year column
result2$Year<-2019


#Rbind together 2019 and 2020 data into final csv
final<-rbind(result,result2)

#Check if the file we want to generate exists, remove it if it does
if (file.exists("2019and2020ratings.csv")) {
  file.remove("2019and2020ratings.csv")
}


#Write out final table

fwrite(final, "./Data/2019and2020ratings.csv")


