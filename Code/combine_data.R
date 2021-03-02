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


#SEC

Stats_sec
master_table_sec

master_table_sec$Team <- gsub("Alabama Crimson Tide", "Alabama", master_table_sec$Team)
master_table_sec$Team <- gsub("Georgia Bulldogs", "Georgia", master_table_sec$Team)
master_table_sec$Team <- gsub("Texas A&M Aggies", "Texas A&M", master_table_sec$Team)
master_table_sec$Team <- gsub("Arkansas Razorbacks", "Arkansas", master_table_sec$Team)
master_table_sec$Team <- gsub("Ole Miss Rebels", "Ole Miss", master_table_sec$Team)
master_table_sec$Team <- gsub("Missouri Tigers", "Missouri", master_table_sec$Team)

Stats_sec$Win <- NULL
Stats_sec$Loss <- NULL

total_stats_sec <- merge(Stats_sec, master_table_sec, by=c('Team', 'Year'))

total_stats_sec <- total_stats_sec %>% separate('W-L', c("W", "L"))

fwrite(total_stats_sec, "./Data/model0data_sec.csv")
