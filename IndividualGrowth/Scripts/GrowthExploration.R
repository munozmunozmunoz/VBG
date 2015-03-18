#Individual Growth Exploration
dat<-read.csv("VBG_Data.csv")

str(dat)
plot(dat$SVL)
summary(dat)
plot(dat$SVL~dat$UID)
plot(dat$SVL~dat$Days)

library(mgcv)
fit<-gam(SVL~s(UID, Days, bs="fs", k=2),data=dat2)
fit2<-gam(SVL~Color + s(UID, Days, bs="fs", k=5),data=dat2)
rand<-list(~Days|UID)
fit3<-gamm(SVL~Color, random=list(Days=~1|UID), data=dat2)
dat2<-dat[duplicated(dat$UID),]
str(dat2)
dat2<-droplevels(dat2)
plot(fit3$lme)
summary(fit3$lme)
summary(fit3$gam)
library(lme4)
ranef(fit3$lme)