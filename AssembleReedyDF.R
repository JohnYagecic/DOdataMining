setwd("~/AssessmentsGeneral/DataMiningProject/ReedyIsland")

ReedyDO<-data.frame(read.csv("Dates.csv"))
tides<-data.frame(read.csv("dailytides.csv"))
ReedyDO$Date<-as.Date(ReedyDO$Date, format="%m/%d/%Y")
tides$date<-as.Date(tides$date, format="%Y-%m-%d")
ReedyDO2<-merge(ReedyDO, tides, by.x="Date", by.y="date", all.x=TRUE)
library(dataRetrieval)
siteNo<-"01463500"

TrentParams<-c("00060", "00010", "00095")
TrentDat<-retrieveNWISData(siteNo, TrentParams, "2000-01-01", "2010-12-31")

names(TrentDat)<-c("agency", "site", "datetime", "tempTrenton", "tempcode", "dischargeTrenton", "dischargeCode", "spcTrenton", "spcCode")
data2merge<-c(3,4,6,8)
TrentDat2<-TrentDat[,data2merge]

TrentonDO <- data.frame(read.csv("TrentonDOSatFilled.csv"))
TrentonDO$Date <- as.Date(TrentonDO$Date, format="%m/%d/%Y")
TrentDat3<-merge(TrentDat2, TrentonDO, by.x="datetime", by.y="Date", all.x=TRUE)

ReedyDO3<-merge(ReedyDO2, TrentDat3, by.x="Date", by.y="datetime", all.x=TRUE)

Radiation<-data.frame(read.csv("ConsolidatedDailyDirectRadiation.csv"))
Radiation$Date<-as.Date(Radiation$Date, format="%m/%d/%Y")
ReedyDO4<-merge(ReedyDO3, Radiation, by.x="Date", by.y="Date", all.x=TRUE)

DOSatReedy<-data.frame(read.csv("DOSatReedy.csv"))
DOSatReedy$Date<-as.Date(DOSatReedy$Date, format="%m/%d/%Y")
DOSatReedy$DOSatReedy<-as.numeric(levels(DOSatReedy$DOSatReedy))[DOSatReedy$DOSatReedy]

ReedyDO5<-merge(ReedyDO4, DOSatReedy, byx="Date", by.y="Date", all.x=TRUE)

TrentonpH<-data.frame(read.csv("TrentonpHData.csv"))
TrentonpH$Date<-as.Date(TrentonpH$Date, format="%m/%d/%Y")
ReedyDO6<-merge(ReedyDO5, TrentonpH, by.x="Date", by.y="Date", all.x=TRUE)

weather<-data.frame(read.csv("weatherDat.csv"))
weather$Date<-as.Date(weather$Date, format="%m/%d/%Y")
ReedyDO7<-merge(ReedyDO6, weather, by.x="Date", by.y="Date", all.x=TRUE)

ReedyOther<-data.frame(read.csv("ReedyWQdata.csv"))
ReedyOther$Date<-as.Date(ReedyOther$Date, format="%m/%d/%Y")
ReedyDO8<-merge(ReedyDO7, ReedyOther, by.x="Date", by.y="Date", all.x=TRUE)

ReedyDO8$SCrangeRI<-ReedyDO8$SCMaxRI-ReedyDO8$SCMinRI
ReedyDO8$pHrangeRI<-ReedyDO8$pHMaxRI-ReedyDO8$pHMinRI
ReedyDO8$TempRangeRI<-ReedyDO8$TempMaxRI-ReedyDO8$TempMinRI


cansum<-c("tide.median", "tide.min", "tide.max", "tide.range", "dischargeTrenton", "Radiation", "AirTempAv", "AirTempMax", "AirTempMin", "DewPoint", "Precip", "Gust", "Pressure", "MaxWind", "TempMaxBF", "TempMinBF", "TempMeanBF")
