library(purrr) 
library(rvest)
library(data.table)
library(stringr)
library(tidyr)

#Archive Data Link
path1<-"https://www.sportsmediawatch.com/college-football-tv-ratings-archived/"

#Different pages to access
pages<-c("1","2","3","4")


#Create temporary 
temp<-NULL
result<-data.table()


for (i in 1:length(pages)) {
  webpage<-paste0(path1,pages[i])
  tables<-html_table(read_html(webpage),fill = TRUE)
  for (j in 1:length(tables)) {
    temp<-data.table(tables[[j]])
    result<-rbind(result,temp, fill=T)
  }
  
}

if (file.exists("archiveratings.csv")) {
  file.remove("archiveratings.csv")
}

#Write out result as archiveratings csv
fwrite(result, "./Data/archiveratings.csv")




