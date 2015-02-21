DOSatReedy<-data.frame(read.csv("DOSatReedy.csv"))
DOSatReedy$Date<-as.Date(DOSatReedy$Date, format="%m/%d/%Y")
DOSatReedy$DOSatReedy<-as.numeric(levels(DOSatReedy$DOSatReedy))[DOSatReedy$DOSatReedy]
DOSatReedy$Month <- format(DOSatReedy$Date, format="%b")
DOSatReedy$Month <- factor(DOSatReedy$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May",
"Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
DOSatReedy$Year <- format(DOSatReedy$Date, format="%Y")

boxplot(DOSatReedy$DOSatReedy ~ DOSatReedy$Month)
boxplot(DOSatReedy$DOSatReedy ~ DOSatReedy$Year)
DOSatReedy$YearMonth<-format(DOSatReedy$Date, format="%Y-%m")
boxplot(DOSatReedy$DOSatReedy ~ DOSatReedy$YearMonth)



ReedyTS<-aggregate(x=DOSatReedy$DOSatReedy, by=list(date=format(DOSatReedy$Date, "%Y-%m")), FUN=median)
JanMed<-median(DOSatReedy$DOSatReedy[DOSatReedy$Month=="Jan"], na.rm=T)
FebMed<-median(DOSatReedy$DOSatReedy[DOSatReedy$Month=="Feb"], na.rm=T)
ReedyTS[32,2]<-JanMed
ReedyTS[78,2]<-FebMed
ReedyTS1<-ts(ReedyTS$x, frequency=12)
f<-decompose(ReedyTS1)
plot(f)
