# An R script with a function for expanding the a list of explanatory variables
# by adding lag, summation, mean, lagged summation, and lagged mean.  Script
# compares adjusted R squared values to R squareds for other expanded variables,
# retaining the form with the highest adjusted R squared.
#
# This script is specific to my DF's.  Other uses will require modification.
#  Posted here for safe keeping
#
# Coded by John Yagecic in May-June 2014
# JYagecic@gmail.com
#
NewBestRdf<-data.frame(rep(NA,ncol(ReedyDO8)), rep(-999, ncol(ReedyDO8)))
names(NewBestRdf)<-c("Label", "adjRsq")


NewAddVars<-data.frame(matrix(NA, nrow=nrow(ReedyDO8), ncol=ncol(ReedyDO8)))
colnames(NewAddVars)[1]<-"Date"
NewAddVars$Date <- ReedyDO8$Date



CompareModel<-function(varvect, i, newvarname){
  newlm<-lm(ReedyDO8$DOSatReedy ~ varvect)
  cat(newvarname)
  cat(",")
  cat(summary(newlm)$adj.r.squared)
  cat(",")
  print(i)
  
  if (summary(newlm)$adj.r.squared > NewBestRdf[i,2]){
    NewBestRdf[i,1]<<-newvarname
    NewBestRdf[i,2]<<-summary(newlm)$adj.r.squared
    NewAddVars[,i]<<-varvect
    colnames(NewAddVars)[i]<<-newvarname
  }
}


for (i in 2:ncol(ReedyDO8)){
  if (colnames(ReedyDO8[i])!="DOSatCh"){
    # Loops for Lag only
    for (j in 1:30){ # j is lag length
      mylag<-rep(NA, nrow(ReedyDO8)) # reset the lagged vector each time
      for (k in 1:(nrow(ReedyDO8)-j-1)){ # k is row number
        mylag[(k+j)]<-ReedyDO8[k,i]
      }
    myvarname<-paste0(colnames(ReedyDO8[i]), "_Lag_", j)	
    CompareModel(mylag, i, myvarname)
    }
    # Loops for sums only
    for (j in 1:30){ # sum length
      mysum<-rep(NA, nrow(ReedyDO8)) # reset the summed vector each time
      for (k in (j+1):nrow(ReedyDO8)){
        mysum[k]<-sum(ReedyDO8[((k-j+1):k),i], na.rm=FALSE) 
      }
      mysumname<-paste0(colnames(ReedyDO8[i]),"_Sum_",j)
      CompareModel(mysum, i, mysumname)     
  }
  # Loops for rolling means
  for (j in 1:30){ # mean length
    mymean<-rep(NA, nrow(ReedyDO8)) # reset the summed vector each time
    for (k in (j+1):nrow(ReedyDO8)){
      mymean[k]<-mean(ReedyDO8[((k-j+1):k),i], na.rm=TRUE)
    }
    
    mymeanname<-paste0(colnames(ReedyDO8[i]),"_Mean_",j)
    CompareModel(mymean, i, mymeanname)
}
  # Loops for lagged mean
  for (j in 1:30){  # rolling day mean length -- max is 30, 5 used for test
    for (z in 1:30){ # lag length -- max is 30, 5 used for test
      mylagmean<-rep(NA, nrow(ReedyDO8)) # reset the summed vector each time
      for (k in (j+1):(nrow(ReedyDO8)-z-1)){
      mylagmean[(k+z)]<-mean(ReedyDO8[((k-j+1):k),i], na.rm=TRUE) 
    }
    mylagmeanname<-paste0(colnames(ReedyDO8[i]),"_Mean_",j,"_Lag_",z)
    CompareModel(mylagmean, i, mylagmeanname)
}
}
  # Loops for lagged sum
  for (j in 1:30){  # sum length -- max is 30, 5 used for test
    for (z in 1:30){ # lag length -- max is 30, 5 used for test
      mylagsum<-rep(NA, nrow(ReedyDO8)) # reset the summed vector each time
      for (k in (j+1):(nrow(ReedyDO8)-z-1)){
        mylagsum[(k+z)]<-sum(ReedyDO8[((k-j+1):k),i], na.rm=FALSE)
    }
    mylagsumname<-paste0(colnames(ReedyDO8[i]),"_Sum_",j,"_Lag_",z)
    CompareModel(mylagsum, i, mylagsumname)

}
}
}
}


write.table(NewBestRdf, file = "NewBestRdf.csv", sep = ",", row.names=FALSE)
write.table(NewAddVars, file = "NewAddVars.csv", sep = ",", row.names=FALSE)