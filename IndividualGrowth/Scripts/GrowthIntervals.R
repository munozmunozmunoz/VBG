load("Z:/IndividualGrowth/WorkspaceImageGrowth.RData")

gint <- matrix(0,0,6)

IDS <- unique(dat[,"UID"])

for (a in 1:length(IDS)){
  d <- subset(dat, UID == IDS[a])
  if(nrow(d)>1){
  for (b in 1:(nrow(d)-1)){
    e <- data.frame(d[b,c(4,3,2,5)],d[(b+1),c(2,5)])
    gint <- rbind(gint,e)
  }}
}
gint <- data.frame(gint,dT = gint[,5]-gint[,3], dL = gint[,6]-gint[,4])

write.csv(gint, file="VBG_TimeInterval.csv")


