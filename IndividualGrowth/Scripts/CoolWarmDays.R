#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/IndividualGrowth")

dat<-read.csv("Data/VBG_TimeInterval.csv")

#############Summer days vector approach
summary(dat$Days.1)
#Day 0 to day 1307 (1308 total days)
summerdays<-vector("numeric", length=1308)

#1 indicates average nightly low is above 40degF
#First Summer
summerdays[1:16]<-1
#Second Summer
summerdays[169:381]<-1
#Third Summer
summerdays[534:746]<-1
#Fourth Summer
summerdays[899:1111]<-1
#Fifth Summer
summerdays[1264:1308]<-1

#For each record, create number of summer days and number of winters days
for(i in 1:dim(dat)[1]){
  dat$dS[i]<-sum(summerdays[dat$Days[i]:dat$Days.1[i]])
  dat$dW[i]<-dat$dT[i]-dat$dS[i]+1
}

write.csv(dat, file="VBG_TimeInterval_SeasonalDaysDuration.csv")

##############Lame-o for loop approach that doesn't work
for(i in 1:dim(dat)[1]){ # Loop through all observations in order to calculate season days
  if(dat$Days[i] <= 15){ # For individuals caught within the first warm season
    if(dat$Days.1[i] <= 15){ # Recaught within first warm season
      dat$dS[i]<-dat$Days.1[i]-dat$Days[i]
      dat$dW[i]<-0
    }
    if(dat$Days.1[i] > 15 & dat$Days.1<=167){ # Recaught within first cool season
      dat$dS[i]<-15-dat$Days[i] + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i] + 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 167 & dat$Days.1<=380){ # Recaught within second warm season
      dat$dS[i]<-15-dat$Days[i] + 1 + dat$Days.1[i]-168 + 1 
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 380 & dat$Days.1<=532){ # Recaught within second cool season
      dat$dS[i]<-15-dat$Days[i] + 1 + 380-168 + 1 
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 532 & dat$Days.1<=745){ # Recaught within third warm season
      dat$dS[i]<-15-dat$Days[i] + 1 + 380-168 + 1 + dat$Days.1[i]-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 745 & dat$Days.1<=897){ # Recaught within third cool season
      dat$dS[i]<-15-dat$Days[i] + 1 + 380-168 + 1 + 745-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-15-dat$Days[i] + 1 + 380-168 + 1 + 745-533 + 1 + dat$Days.1[i]-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-15-dat$Days[i] + 1 + 380-168 + 1 + 745-533 + 1 + 1110-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-15-dat$Days[i] + 1 + 380-168 + 1 + 745-533 + 1 + 1110-898 + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
  }
  if(dat$Days[i] > 15 & dat$Days<=167){ # For individuals caught within the first cool season
    if(dat$Days.1[i] > 15 & dat$Days.1<=167){ # Recaught within first cool season
      dat$dS[i]<-0
      dat$dW[i]<-dat$Days.1[i] - dat$Days[i] + 1
    }
    if(dat$Days.1[i] > 167 & dat$Days.1<=380){ # Recaught within second warm season
      dat$dS[i]<-dat$Days.1[i]-167 + 1 
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1 -dat$Days[i]
    }
    if(dat$Days.1[i] > 380 & dat$Days.1<=532){ # Recaught within second cool season
      dat$dS[i]<-380-168 + 1 
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 532 & dat$Days.1<=745){ # Recaught within third warm season
      dat$dS[i]<-380-168 + 1 + dat$Days.1[i]-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 745 & dat$Days.1<=897){ # Recaught within third cool season
      dat$dS[i]<-380-168 + 1 + 745-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-380-168 + 1 + 745-533 + 1 + dat$Days.1[i]-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-380-168 + 1 + 745-533 + 1 + 1110-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-380-168 + 1 + 745-533 + 1 + 1110-898 + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  if(dat$Days[i] > 167 & dat$Days<=380){ # For individuals caught within the second warm season
    if(dat$Days.1[i] > 167 & dat$Days.1<=380){ # Recaught within second warm season
      dat$dS[i]<-dat$Days.1[i]-dat$Days[i]
      dat$dW[i]<-0
    }
    if(dat$Days.1[i] > 380 & dat$Days.1<=532){ # Recaught within second cool season
      dat$dS[i]<-380-dat$Days[i] + 1 
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 532 & dat$Days.1<=745){ # Recaught within third warm season
      dat$dS[i]<-380-dat$Days[i] + 1 + dat$Days.1[i]-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 745 & dat$Days.1<=897){ # Recaught within third cool season
      dat$dS[i]<-380-dat$Days[i] + 1 + 745-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-380-dat$Days[i] + 1 + 745-533 + 1 + dat$Days.1[i]-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-380-dat$Days[i] + 1 + 745-533 + 1 + 1110-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-380-dat$Days[i] + 1 + 745-533 + 1 + 1110-898 + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  if(dat$Days[i] > 380 & dat$Days<=532){ # For individuals caught in second cool season
    if(dat$Days.1[i] > 380 & dat$Days.1<=532){ # Recaught within second cool season
      dat$dS[i]<-0
      dat$dW[i]<-dat$Days.1[i]-dat$Days[i]
    }
    if(dat$Days.1[i] > 532 & dat$Days.1<=745){ # Recaught within third warm season
      dat$dS[i]<-dat$Days.1[i]-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 745 & dat$Days.1<=897){ # Recaught within third cool season
      dat$dS[i]<-745-533 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-745-533 + 1 + dat$Days.1[i]-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-745-533 + 1 + 1110-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-745-533 + 1 + 1110-898 + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  if(dat$Days[i] > 532 & dat$Days<=745){
    if(dat$Days.1[i] > 532 & dat$Days.1<=745){ # Recaught within third warm season
      dat$dS[i]<-dat$Days.1[i]-dat$Days[i] + 1
      dat$dW[i]<-0
    }
    if(dat$Days.1[i] > 745 & dat$Days.1<=897){ # Recaught within third cool season
      dat$dS[i]<-745-dat$Days[i] + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-745-dat$Days[i] + 1 + dat$Days.1[i]-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-745-dat$Days[i] + 1 + 1110-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-745-dat$Days[i] + 1 + 1110-898 + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  if(dat$Days[i] > 745 & dat$Days<=897){
    if(dat$Days.1[i] > 745 & dat$Days.1<=897){ # Recaught within third cool season
      dat$dS[i]<-0
      dat$dW[i]<-dat$Days.1[i] - dat$Days[i]
    }
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-dat$Days.1[i]-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-1110-898 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-1110-898 + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  if(dat$Days[i] > 897 & dat$Days<=1110){
    if(dat$Days.1[i] > 897 & dat$Days.1<=1110){ # Recaught within fourth warm season
      dat$dS[i]<-dat$Days.1[i]-dat$Days[i] + 1
      dat$dW[i]<-0
    }
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-1110-dat$Days[i] + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-1110-dat$Days[i] + 1 + dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  if(dat$Days[i] > 1110 & dat$Days<=1262){
    if(dat$Days.1[i] > 1110 & dat$Days.1<=1262){ # Recaught within fourth cool season
      dat$dS[i]<-0
      dat$dW[i]<-dat$Days.1[i]-dat$Days[i] + 1
    }
    else{ # Recaught within last warm season
      dat$dS[i]<-dat$Days.1[i]-1263 + 1
      dat$dW[i]<-dat$Days.1[i] - dat$dS[i]+ 1-dat$Days[i]
    }
  }
  else{ # Caught and recaught within last warm season
    dat$dS[i]<-dat$Days.1[i]-dat$Days[i] + 1
    dat$dW[i]<-0
  }
}

