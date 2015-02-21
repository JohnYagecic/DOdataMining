setwd("~/AssessmentsGeneral/DataMiningProject/ReedyIsland")

ReedyDO9<-data.frame(read.csv("ReedyDO9.csv"))
ReedyDO9<-ReedyDO9[,-46] # get rid of DO Sat Reedy optimized


# Create a new DF of complete explanatory variables only
ReedyDO10<-ReedyDO9[complete.cases(ReedyDO9),]

explans<-c(2:10, 12:70) # column indices of possible explanatory variables
# excludes date, DOSat and others

Best3TermLM<-lm(DOSatReedy ~ SCMinRI_Mean_30_Lag_30 + SCMeanRI + I(AirTempMin_Sum_18_Lag_3^2),
                data=ReedyDO10)

#
#
#
DOpred1<-predict(Best3TermLM, data=ReedyDO10)
plot(ReedyDO10$DOSatReedy, DOpred1, xlab="Observed % DO Saturation",
     ylab="Predicted % DO Saturation", main="3 Term Linear Model (2000-2010)",
     xlim=c(0.5, 1.1), ylim=c(0.5, 1.1))
abline(0,1)

summary(Best3TermLM)$r.squared
sqrt(sum((DOpred1-ReedyDO10$DOSatReedy)^2)/nrow(ReedyDO10)) # RMSE for 3 term linear model

# Model fit assessment for new data
# Read in data from 2011-2014, correct date format

ReedyDO9oos <- read.csv("./NewData/ReedyDO9oos.csv")
ReedyDO9oos$Date <- as.Date(ReedyDO9oos$Date, format="%Y-%m-%d")
ReedyDO9oos <- ReedyDO9oos[,-46] # Get rid of optimized DO sat column
summary(ReedyDO9oos)

ReedyDO10oos <- ReedyDO9oos[complete.cases(ReedyDO9oos),]
summary(ReedyDO10oos)

DOpredoos <- predict(Best3TermLM, newdata=ReedyDO10oos)
plot(ReedyDO10oos$DOSatReedy, DOpredoos, xlab="Observed % DO Saturation",
     ylab="Predicted % DO Saturation", main="3 Term Linear Model, Reedy Island (2011-2014)",
     xlim=c(0.7, 1.1), ylim=c(0.7, 1.1))
abline(0,1)

LM3TforR <- lm(DOpredoos ~ ReedyDO10oos$DOSatReedy)
summary(LM3TforR)$r.squared
sqrt(sum((DOpredoos-ReedyDO10oos$DOSatReedy)^2)/nrow(ReedyDO10oos)) #RMSE