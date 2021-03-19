library(data.table)



DT<-fread()



teams<-unique(DT$team)
wins<-unique(DT$wins)
losses<-unique(DT$losses)
PD<-unique(DT$PD)
FPI<-unique(DT$FPI)
Attendance<-unique(DT$Attendance)
Capacity<-unique(DT$Capacity)



result<-data.table()
for(i in 1:1000) {
  
  temp$team<-rand(teams,1)
  temp$wins<-rand(wins,1)
  temp$losses<-rand(losses,1)
  temp$PD<-rand(PD,1)
  temp$FPI<-rand(FPI,1)
  temp$Attendance<-rand(Attendance,1)
  temp$Capacity<-rand(Capacity,1)
  
  if(temp[i,] not in result) {
    result<-rbind(result,temp)
  }
  
}