#model0

Stats
master_table

master_table$Team <- gsub("Ohio State Buckeyes", "Ohio State", master_table$Team)
master_table$Team <- gsub('Wisconsin Badgers', "Wisconsin", master_table$Team)
master_table$Team <- gsub('Iowa Hawkeyes', "Iowa", master_table$Team)
master_table$Team <- gsub('Indiana Hoosiers', "Indiana", master_table$Team)
master_table$Team <- gsub('Northwestern Wildcats', "Northwestern", master_table$Team)
master_table$Team <- gsub('Penn State Nittany Lions', "Penn State", master_table$Team)
master_table$Team <- gsub('Minnesota Golden Gophers', "Minnesota", master_table$Team)
master_table$Team <- gsub('Nebraska Cornhuskers', "Nebraska", master_table$Team)
master_table$Team <- gsub('Michigan Wolverines', "Michigan", master_table$Team)
master_table$Team <- gsub('Purdue Boilermakers', "Purdue", master_table$Team)
master_table$Team <- gsub('Rutgers Scarlet Knights', "Rutgers", master_table$Team)
master_table$Team <- gsub('Maryland Terrapins', "Maryland", master_table$Team)
master_table$Team <- gsub('Illinois Fighting Illini', "Illinois", master_table$Team)
master_table$Team <- gsub('Michigan State Spartans', "Michigan State", master_table$Team)

Stats$Win <- NULL
Stats$Loss <- NULL
master_table

total_stats <- merge(Stats, master_table, by=c('Team', 'Year'))

total_stats <- total_stats %>% separate('W-L', c("W", "L"))

fwrite(total_stats, file = 'model0_data.csv')
