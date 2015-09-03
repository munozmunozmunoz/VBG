#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/CMH/IndividualGrowth")
temp<-read.csv("Data/4SeasonTemp.csv")
temp<-temp[,-1]
dat<-read.csv("Data/VBG_4SeasonDiffTemp.csv")
#############Summer days vector approach
#Day 0 to day 1307 (1308 total days)
summerdays<-vector("numeric", length=1308)
winterdays<-vector("numeric", length=1308)
springdays<-vector("numeric", length=1308)
falldays<-vector("numeric", length=1308)

dat$dSp=0
dat$dSm=0
dat$dF=0
dat$dW=0
#1s indicate a fall day based on dates surveyed
#thus each fall has a different number of days
falldays[1:50]<-1
winterdays[51:143]<-1
springdays[144:200]<-1
summerdays[201:325]<-1
falldays[326:399]<-1
winterdays[400:502]<-1
springdays[503:560]<-1
summerdays[561:718]<-1
falldays[719:763]<-1
winterdays[764:872]<-1
springdays[873:929]<-1
summerdays[930:1071]<-1
falldays[1072:1131]<-1
winterdays[1132:1267]<-1
springdays[1268:1308]<-1

#For each record, create number of summer days and number of winters days
for(i in 1:dim(dat)[1]){
  dat$dSp[i]<-sum(springdays[(dat$Days[i]+1):(dat$Days.1[i]+1)])
  dat$dSm[i]<-sum(summerdays[(dat$Days[i]+1):(dat$Days.1[i]+1)])
  dat$dF[i]<-sum(falldays[(dat$Days[i]+1):(dat$Days.1[i]+1)])
  dat$dW[i]<-sum(winterdays[(dat$Days[i]+1):(dat$Days.1[i]+1)])
}
#Individuals caught on first day of season don't experience that season's growth
dat$dSp[which(dat$dSp==1)]<-0
dat$dF[which(dat$dF==1)]<-0

dat<-droplevels(dat)

#Standardize each winter and summer(single value for all W/Sm days)
sm1<-mean(temp[201:325,7], na.rm=T)
sm2<-mean(temp[561:718,7], na.rm=T)
sm3<-mean(temp[930:1071,7], na.rm=T)
StdSm<-scale(c(sm1,sm2,sm3), center=TRUE, scale=TRUE)

w1<-mean(temp[51:143,9], na.rm=T)
w2<-mean(temp[400:502,9], na.rm=T)
w3<-mean(temp[764:872,9], na.rm=T)
w4<-mean(temp[1132:1267,9], na.rm=T)
StdW<-scale(c(w1,w2,w3,w4), center=TRUE, scale=TRUE)

temp$StdSm[201:325]<-StdSm[1]
temp$StdSm[561:718]<-StdSm[2]
temp$StdSm[930:1071]<-StdSm[3]
temp$StdW[51:143]<-StdW[1]
temp$StdW[400:502]<-StdW[2]
temp$StdW[764:872]<-StdW[3]
temp$StdW[1132:1267]<-StdW[4]
###### Standardize low temperature values for spring/fall
#Z-standardize both temp columns
temp$StdSp<-scale(as.numeric(temp$TempSp), center=TRUE, scale=TRUE)
temp$StdF<-scale(as.numeric(temp$TempF), center=TRUE, scale=TRUE)

###### Add mean temperature to time intervals
str(dat)
#For each record, take mean of temp of each season between two days
for(i in 1:dim(dat)[1]){
  dat$TempSp[i]<-mean(temp$StdSp[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempSm[i]<-mean(temp$StdSm[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempF[i]<-mean(temp$StdF[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempW[i]<-mean(temp$StdW[dat$Days[i]:dat$Days.1[i]], na.rm=T)  
}
#Change NAs to zeroes so BUGS doesn't explode
dat$TempSp[which(dat$TempSp=="NaN")]<-0
dat$TempSm[which(dat$TempSm=="NaN")]<-0
dat$TempF[which(dat$TempF=="NaN")]<-0
dat$TempW[which(dat$TempW=="NaN")]<-0

#Write new data files
dat<-dat[,c(-1,-2,-9,-10)]
write.csv(dat, file="Data/VBG_4SeasonDiffTemp.csv")
#write.csv(temp, file="Data/4SeasonTemp.csv")
