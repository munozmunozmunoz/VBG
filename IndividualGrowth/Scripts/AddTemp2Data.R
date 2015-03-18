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

###### Add mean temperature to time intervals
str(dat)
#For each record, take mean of temp between two days
for(i in 1:dim(dat)[1]){
  dat$Temp[i]<-mean(temp$StdTemp[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  
}

write.csv(dat, file="Data/VBG_TimeInterval_SeasonalDaysDuration_Temp.csv")