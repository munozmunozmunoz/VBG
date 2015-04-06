#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/IndividualGrowth")

dat<-read.csv("Data/VBG_TimeInterval_SeasonalDaysDuration_Temp.csv")
temp<-read.csv("Data/Temp4Growth.csv")

###### Standardize average temperature values
#str(temp)
#temp$StdTemp<-scale(temp$AvgTemp, center=TRUE, scale=TRUE)
#write.csv(temp, file="Data/Temp4Growth.csv")

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

#Combine summer day indicator with data frame
temp1<-cbind(temp, summerdays)

###### Standardize average temperature values for each season
str(temp)
#Create separate column to standardize summer temperatures
for(q in 1:dim(temp1)[1]){
  if(summerdays[q]>0){
    temp$TempSummer[q]<-temp$AvgTemp[q]
  } else{
    temp$TempSummer[q]<-"NA"
  }
}
#Create separate column to standardize winter temperatures
for(q in 1:dim(temp1)[1]){
  if(summerdays[q]==0){
    temp$TempWinter[q]<-temp$AvgTemp[q]
  } else{
    temp$TempWinter[q]<-"NA"
  }
}

#Z-standardize both temp columns
temp$SummerStdTemp<-scale(as.numeric(temp$TempSummer), center=TRUE, scale=TRUE)
temp$WinterStdTemp<-scale(as.numeric(temp$TempWinter), center=TRUE, scale=TRUE)
#Combine both columns into single final temperature
for(q in 1:dim(temp1)[1]){
  if(summerdays[q]>0){
    temp$FinalStdTemp[q]<-temp$SummerStdTemp[q]
  } else{
    temp$FinalStdTemp[q]<-temp$WinterStdTemp[q]
  }
}

write.csv(temp, file="Data/Temp4Growth.csv")

###### Add mean temperature to time intervals
str(dat)
#For each record, take mean of temp of each season between two days
for(i in 1:dim(dat)[1]){
  dat$TempSummer[i]<-mean(temp$SummerStdTemp[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempWinter[i]<-mean(temp$WinterStdTemp[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  
}

write.csv(dat, file="Data/VBG_TimeInterval_SeasonalDaysDuration_Temp.csv")
