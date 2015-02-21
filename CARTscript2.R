library(tree)
library(DMwR)
library(gplots)
library(rpart)
library(randomForest)


setwd("~/AssessmentsGeneral/DataMiningProject/ReedyIsland")

ReedyDO9<-read.csv("ReedyDO9.csv")
ReedyDO9$Date<-as.Date(ReedyDO9$Date, format="%Y-%m-%d")
ReedyDO9<-ReedyDO9[,-46]
ReedyDO9<-ReedyDO9[,-1] # Remove date
ReedyDO10<-ReedyDO9[complete.cases(ReedyDO9),]


set.seed(1776)

DOtree2 <- tree(DOSatReedy ~ ., data=ReedyDO10)
summary(DOtree2)
plot(DOtree2)
text(DOtree2, font=2)

DOpred<-predict(DOtree2, data=ReedyDO10)
plot(ReedyDO10$DOSatReedy, DOpred)



RFtree <- randomForest(DOSatReedy ~ ., data=ReedyDO10, importance=TRUE, na.action=na.omit)
DOpredRF<-predict(RFtree, data=ReedyDO10)
plot(ReedyDO10$DOSatReedy, DOpredRF, xlab="Observed % DO Saturation",
     ylab="Predicted % DO Saturation", main="Random Forest Model (2000-2010)")
abline(0,1)

sqrt(sum((DOpredRF-ReedyDO10$DOSatReedy)^2)/nrow(ReedyDO10)) #RMSE for RF model
RFforR2 <- lm(DOpredRF~ReedyDO10$DOSatReedy) #lm for determining R-squared
summary(RFforR2)$r.squared

# Model fit assessment for new data
# Read in data from 2011-2014, correct date format

ReedyDO9oos <- read.csv("./NewData/ReedyDO9oos.csv")
ReedyDO9oos$Date <- as.Date(ReedyDO9oos$Date, format="%Y-%m-%d")
ReedyDO9oos <- ReedyDO9oos[,-46] # Get rid of optimized DO sat column
summary(ReedyDO9oos)

ReedyDO10oos <- ReedyDO9oos[complete.cases(ReedyDO9oos),]
summary(ReedyDO10oos)

DORFpredoos <- predict(RFtree, newdata=ReedyDO10oos)
plot(ReedyDO10oos$DOSatReedy, DORFpredoos, xlab="Observed % DO Saturation",
     ylab="Predicted % DO Saturation", main="Random Forest Model, Reedy Island (2011-2014)",
     xlim=c(0.7, 1.1), ylim=c(0.7, 1.1))
abline(0,1)

RFforR <- lm(DORFpredoos ~ ReedyDO10oos$DOSatReedy)
summary(RFforR)$adj.r.squared
sqrt(sum((DORFpredoos-ReedyDO10oos$DOSatReedy)^2)/nrow(ReedyDO10oos)) #RMSE

importance(RFtree)
varImpPlot(RFtree)
