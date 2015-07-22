#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/CMH/IndividualGrowth")

dat<-read.csv("Data/VBG_TimeInterval.csv")
dat<-dat[,-10]
#############Summer days vector approach
summary(dat$Days.1)
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

write.csv(dat, file="VBG_4SeasonDiff.csv")
