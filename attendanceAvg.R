library(reshape2)
library(data.table)
set.seed(555)

kaggle <- fread('./Data/CFBeattendanceCSV.csv')
kaggle

teams <- c('Northwestern','Wisconsin', 'Illinois', 'Penn State', 'Indiana', 'Michigan State', 'Nebraska', 'Rutgers')
teams2 <- c('Wisconsin')
for(i in teams){
sub<- kaggle[team == i]
year <- sub$Year
team <- kaggle$Team

# Northwestern, Wisconsin, Illinois, PSU, Indiana, MSU, Nebraska, Rutgers

#result <- sub$Result

#pointsFor <- substring(result,3,gregexpr(pattern = "–", result)[[1]]-1)
#pointsAgainst <- ifelse (nchar(substring(result,gregexpr(pattern = "–", result)[[1]]+1)) > 2,substring(result,gregexpr(pattern = "–", result)[[1]]+1,gregexpr(pattern = " ", result)[[26]]-1),substring(result,gregexpr(pattern = "–", result)[[1]]+1))
#sum(as.numeric(pointsFor))
#sum(as.numeric(pointsAgainst))


  



# Idea taken from https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame
print(i)

print(aggregate(sub[, 'Attendance'], list(year), mean))
}


