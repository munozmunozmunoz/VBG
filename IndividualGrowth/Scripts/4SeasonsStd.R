#################################
### Add cool/warm season days ###
#################################
setwd("E:/Current/New or Updated Files/PennState/Research/CMH/IndividualGrowth")

dat<-read.csv("Data/VBG_TimeInterval.csv")
dat<-dat[,-c(8:11)];dat<-dat[,-1]
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
write.csv(dat, file="Data/VBG_4SeasonStd.csv")
