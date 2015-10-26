#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/CMH/IndividualGrowth")
temp<-read.csv("Data/Patuxent_Weather_VBGData.csv")
dat<-read.csv("Data/VBG_4SeasonStd.csv")
#############Summer days vector approach
#Day 0 to day 1307 (1308 total days)
summerdays<-vector("numeric", length=1308)
winterdays<-vector("numeric", length=1308)
springdays<-vector("numeric", length=1308)
falldays<-vector("numeric", length=1308)

#1s indicate a fall day based on dates surveyed
#thus each fall has a different number of days
falldays[1:50]<-1
winterdays[51:137]<-1
springdays[138:213]<-1
summerdays[214:325]<-1
falldays[326:415]<-1
winterdays[416:502]<-1
springdays[503:578]<-1
summerdays[579:690]<-1
falldays[691:780]<-1
winterdays[781:868]<-1
springdays[869:944]<-1
summerdays[945:1056]<-1
falldays[1057:1147]<-1
winterdays[1148:1233]<-1
springdays[1234:1308]<-1

temp$StdSp<-0
temp$StdSm<-0
temp$StdF<-0
temp$StdW<-0

#Standardize each winter and summer(single value for all W/Sm days)
sm1<-mean(temp[214:325,5], na.rm=T)
sm2<-mean(temp[579:690,5], na.rm=T)
sm3<-mean(temp[945:1056,5], na.rm=T)
StdSm<-scale(c(60.8340179, sm1, sm2, sm3), center=TRUE, scale=TRUE)

w1<-mean(temp[51:137,5], na.rm=T)
w2<-mean(temp[416:502,5], na.rm=T)
w3<-mean(temp[781:868,5], na.rm=T)
w4<-mean(temp[1148:1233,5], na.rm=T)
StdW<-scale(c(w1,w2,w3,w4), center=TRUE, scale=TRUE)

StdSp<-StdW

StdF<-StdSm

temp$StdSm[214:325]<-StdSm[2]
temp$StdSm[579:690]<-StdSm[3]
temp$StdSm[945:1056]<-StdSm[4]
temp$StdW[51:137]<-StdW[1]
temp$StdW[416:502]<-StdW[2]
temp$StdW[781:868]<-StdW[3]
temp$StdW[1148:1233]<-StdW[4]
temp$StdSp[138:213]<-StdSp[1]
temp$StdSp[503:578]<-StdSp[2]
temp$StdSp[869:944]<-StdSp[3]
temp$StdSp[1234:1308]<-StdSp[4]
temp$StdF[1:50]<-StdF[1]
temp$StdF[326:415]<-StdF[2]
temp$StdF[691:780]<-StdF[3]
temp$StdF[1057:1147]<-StdF[4]

###### Add mean temperature to time intervals
str(dat)

dat$TempSp<-0
dat$TempSm<-0
dat$TempF<-0
dat$TempW<-0
#For each record, take mean of temp of each season between two days
for(i in 1:dim(dat)[1]){
  dat$TempSp[i]<-mean(temp$StdSp[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempSm[i]<-mean(temp$StdSm[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempF[i]<-mean(temp$StdF[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempW[i]<-mean(temp$StdW[dat$Days[i]:dat$Days.1[i]], na.rm=T)  
}

#Write new data files
write.csv(dat, file="Data/VBG_4SeasonStdTemp_Prev.csv")
#write.csv(temp, file="Data/4SeasonTemp.csv")
