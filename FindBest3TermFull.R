#  A Script for finding the best 3 term model from all
#  available combinations within format limitations
#  
#  Written by John Yagecic in October 2014
#  JYagecic@gmail.com
#
#
#  DANGER DANGER  this script takes 8+ hours on a typical
#  desktop PC
#
# set working directory and read in the previously created ReedyDO9 data frame

setwd("~/AssessmentsGeneral/DataMiningProject/ReedyIsland")

ReedyDO9<-data.frame(read.csv("ReedyDO9.csv"))
ReedyDO9<-ReedyDO9[,-46] # get rid of DO Sat Reedy optimized


AllR<-rep(NA,8489000)
jymod<-NA
jycount<-0

# Create a new DF of complete explanatory variables only
ReedyDO10<-ReedyDO9[complete.cases(ReedyDO9),]

explans<-c(2:10, 12:70) # column indices of possible explanatory variables
                        # excludes date, DOSat and others

# Create data frame to house results of test models
modelstemp<-data.frame(rep(NA,10000), rep(NA, 10000), rep(-999, 10000))
names(modelstemp)<-c("Model", "RMSE", "Train.AdjR2")

termbase <- rep(NA,3) # initialize term base
term <- rep(NA,3) # initialize term

for (i in 1:length(explans)){ # term 1, term base
  for (j in 1:length(explans)){ # term 2, term base
    gc()
    for (k in 1:length(explans)){ # term 3, term base
      termbase[1]<-colnames(ReedyDO10)[explans[i]]
      termbase[2]<-colnames(ReedyDO10)[explans[j]]
      termbase[3]<-colnames(ReedyDO10)[explans[k]]
      

      for (x in 1:3){ # term 1, 3 possible states (as is, squared, log)
        for (y in 1:3){ # term 2
          for (z in 1:3){ # term 3
            if (x==1){
              term[1]<-termbase[1] # pass this term as is
            }
            if (x==2){
              term[1]<-paste0("I(", termbase[1], "^2)") # pass square of this term
            }
            if (x==3){
              term[1] <- paste0("I(log(", termbase[1], "+20))") # pass log of this term
            }
            if (y==1){
              term[2]<-termbase[2]
            }
            if (y==2){
              term[2]<-paste0("I(", termbase[2], "^2)")
            }
            if (y==3){
              term[2] <- paste0("I(log(", termbase[2], "+20))")
            }
            if (z==1){
              term[3]<-termbase[3]
            }
            if (z==2){
              term[3]<-paste0("I(", termbase[3], "^2)")
            }
            if (z==3){
              term[3] <- paste0("I(log(", termbase[3], "+20))")
            }
            jycount <- jycount + 1
            #print(jymod)
            if (jycount %% 1000 ==0){print(jycount)}
            jymod<-paste0("DOSatReedy ~ ", term[1], " + ",term[2], " + ",term[3])
            
            lmx<-lm(jymod, data=ReedyDO10) # create the new model
            AllR[jycount] <- summary(lmx)$adj.r.squared # keep track of all R2
            if (summary(lmx)$adj.r.squared > min(modelstemp$Train.AdjR2)){
              if (summary(lmx)$adj.r.squared > max(modelstemp$Train.AdjR2)){
                bestlm <- lmx
              }
              lastmin<-which.min(modelstemp$Train.AdjR2)
              modelstemp$Train.AdjR2[lastmin] <- summary(lmx)$adj.r.squared
              modelstemp$RMSE[lastmin]<-sqrt(sum((lmx$fitted.values-ReedyDO10$DOSatReedy)^2)/nrow(ReedyDO10))
              modelstemp$Model[lastmin]<-jymod
              
            } 
          }
        }
      }
    }
  }
}

write.table(modelstemp, file = "modelstempFull.csv", sep = ",", row.names=FALSE)
Model5rank<-modelstemp[order(modelstemp$Train.AdjR2),]
tail(Model5rank)

#
#
##