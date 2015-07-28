#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/CMH/IndividualGrowth")
temp<-read.csv("Data/4SeasonTemp.csv")
dat<-read.csv("Data/VBG_4SeasonDiffTemp.csv")
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
winterdays[400:501]<-1
springdays[502:560]<-1
summerdays[561:718]<-1
falldays[719:763]<-1
winterdays[764:872]<-1
springdays[873:929]<-1
summerdays[930:1071]<-1
falldays[1072:1132]<-1
winterdays[1133:1268]<-1
springdays[1269:1308]<-1

#For each record, create number of summer days and number of winters days
for(i in 1:dim(dat)[1]){
  dat$dSp[i]<-sum(springdays[dat$Days[i]:dat$Days.1[i]])
  dat$dSm[i]<-sum(summerdays[dat$Days[i]:dat$Days.1[i]])
  dat$dF[i]<-sum(falldays[dat$Days[i]:dat$Days.1[i]])
  dat$dW[i]<-sum(winterdays[dat$Days[i]:dat$Days.1[i]])  

}
#Combine summer day indicator with data frame
temp1<-cbind(temp, summerdays)

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


write.csv(temp, file="Data/Temp4Growth.csv")

###### Add mean temperature to time intervals
str(dat)
#For each record, take mean of temp of each season between two days
for(i in 1:dim(dat)[1]){
  dat$TempSp[i]<-mean(temp$StdSp[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempSm[i]<-mean(temp$StdSm[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempF[i]<-mean(temp$StdF[dat$Days[i]:dat$Days.1[i]], na.rm=T)
  dat$TempW[i]<-mean(temp$StdW[dat$Days[i]:dat$Days.1[i]], na.rm=T)  
}
write.csv(dat, file="Data/VBG_4SeasonDiffTemp.csv")
write.csv(temp, file="Data/4SeasonTemp.csv")
