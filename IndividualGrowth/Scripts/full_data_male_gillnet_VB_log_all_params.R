rm(list=ls())

#### Load libraries
library(arm)
library(MASS)
#?mvrnorm
library(MCMCpack)
library(R2jags)
library(xtable)


dat<-read.csv('full_data.csv',na.strings='NA', header=T)

summary(dat)
dim(dat)

table(dat$STUDY_BAY_NO)
dat$STUDY_BAY_NO <- factor(dat$STUDY_BAY_NO)

# Keep STUDY_BAY_NO for which samples >~ 100 
# These do not need to be excluded or subsampled
dat2 <- dat[dat$STUDY_BAY_NO %in% c("103","202","203","205",
                                    "207","605","905"),]
dat2 <- droplevels(dat2)

# Check
table(dat2$STUDY_BAY_NO)

# # Separate regions with high sample size for subsetting
# dat.103 <- subset(x=dat,subset=dat$STUDY_BAY_NO==103)
# dat.205 <- subset(x=dat,subset=dat$STUDY_BAY_NO==205)

# # And separate the rest
# dat.106 <- subset(x=dat,subset=dat$STUDY_BAY_NO==106)
# dat.108 <- subset(x=dat,subset=dat$STUDY_BAY_NO==108)
# dat.202 <- subset(x=dat,subset=dat$STUDY_BAY_NO==202)
# dat.203 <- subset(x=dat,subset=dat$STUDY_BAY_NO==203)
# dat.207 <- subset(x=dat,subset=dat$STUDY_BAY_NO==207)
# dat.903 <- subset(x=dat,subset=dat$STUDY_BAY_NO==903)
# dat.905 <- subset(x=dat,subset=dat$STUDY_BAY_NO==905)
# dat.909 <- subset(x=dat,subset=dat$STUDY_BAY_NO==909)


# # Randomly subet fish from these larger regions; These sample sizes
# # are based on a previous analysis
# #dat.103 <- dat.103[sample(nrow(dat.103), 750, replace=FALSE), ]
# #dat.205 <- dat.205[sample(nrow(dat.205), 500, replace=FALSE), ]


# df <- rbind( dat.103, dat.106, dat.108, dat.202, dat.203, 
# dat.205, dat.207, dat.903, dat.905, dat.909)

# table(df$STUDY_BAY_NO)
# df <- droplevels(df)
# table(df$STUDY_BAY_NO)

# Subsample more (Females, June-Dec and gillnet-caught)
dat3 <- subset(dat2, SEX<2 & MONTH >5)
dat3 <- droplevels(dat3)

dat4 <- dat3[dat3$GEAR_CODE %in% c(2.50, 3.00, 3.50, 4.00, 4.50, 5.00, 5.50, 6.00,
                                   214, 246, 98, 1),]


# # Subsample/truncate females ages for examination of models without larger fish
# df <- subset(df,  FRAC_AGE < 4)
# df <- droplevels(df)

## Get into data prep for Bayesian Analysis
df <- dat4; summary(df)

df$length <- df$TL_mm

# All ages
df$age <- df$FRAC_AGE



# # remove NA's for age data
# I <- !is.na(df$SiteAge)
# df <- df[I,]
# summary(df)
# dim(df)
# 
# # Use more recent data
# df <- df[which(df$year > 2005), ]

# Convert subsetted WtrName back to factor to remove unused levels
df$STUDY_BAY_NO <- factor(df$STUDY_BAY_NO)

# summary(df)

# Number of bays
N <- length(unique(df$STUDY_BAY_NO))


# Set plotting margins
par(mfrow=c(1,1),mar=c(1,1,2,5),oma=c(6,4,2,6),mai=c(0.5,0.5,0,0.5))
# Plot with no data
plot(length ~ age, data=df, pch=16, xlim=c(min(df$age), max(df$age) ),xaxt="n",yaxt="n",type='n',xlab='',ylab='')

axis(side=1,cex.axis=2, mgp=c(1,1,0),tck= -0.01) 
axis(side=2,cex.axis=2,mgp=c(1,1,0),tck= -0.01)

# Add data points
points(jitter(df$age),df$length, cex=2, pch=16,col=df$STUDY_BAY_NO ,bg=as.numeric(df$STUDY_BAY_NO), lwd=2)

# Add axis labels
mtext('Age',side=1,outer=T, adj=0.5, cex=2, line=1)
mtext('Length(mm)', side=2, outer=T, adj=0.5, cex=2, line=1)
box()

df$g <- as.numeric(df$STUDY_BAY_NO)

# ######### nls analysis - use these as starting values for matrix B #####
#  c1 <- array(NA,c(N,3) )
# 
#  for(i in 1:N){
#   vb1 <- nls(length ~ Linf*(1-exp(-k*(age-to))), data=df, subset=g==i, nls.control(maxiter=5000,minFactor=1e-20), start=list(Linf=400, k=0.03, to=-2))
#   c1[i,] <- as.numeric(coef(vb1))
#  }
#  c1
# ################################



sink("modelLogAll.txt")
cat("
    model{
    for(i in 1:n){
    y[i] ~ dnorm(y.hat[i], tau.y)
    y.hat[i] <- Linf[g[i]] * (1-exp(-k[g[i]] * (age[i] - t0[g[i]] )))
    }
    
    tau.y <- pow(sigma.y,-2)
    sigma.y ~ dunif(0,100)
    
    
    
    # Level-2 parameters
    for(j in 1:J){
    Linf[j] <- exp(B[j,1])
    k[j] <- exp(B[j,2]) #### k modeled on log-scale
    t0[j] <- exp(B[j,3])-10 # A constant of 10 is added (subtracted) to t0 to ensure that negative values are possible, becuase t0 is estimated on log-scale
    B[j,1:3] ~ dmnorm(B.hat[j,], Tau.B[,]) #Multivariate normal dist'n;  Tau.B is a precision (tau) matrix
    B.hat[j,1] <- mu.Linf
    B.hat[j,2] <- mu.k
    B.hat[j,3] <- mu.t0 
    }
    
    #priors for level-2 parameters
    mu.Linf ~ dnorm(0,.0001)
    mu.k ~ dnorm(0,.0001)
    mu.t0 ~ dnorm(0,.0001)
    
    # Get grand mean k on untransformed scale
    mu.k.raw <- exp(mu.k)
    linf.raw <- exp(mu.Linf)
    t0.raw <- exp(mu.t0)-10
    
    Tau.B[1:3,1:3] ~ dwish(W[,], df) # Variance-covariance matrix from precision matrix (i.e., 1/tau)
    df <- 4 # df set to 1 greater than dimension of Tau.B (i.e., the number of varying parameters + 1)
    
    Sigma.B[1:3, 1:3] <- inverse(Tau.B[,]) # Calculate variance-covariance matrix Sigma.B
    
    # Obtain sigmas from variance-covariance matrix Sigma.B
    sigma.l <- sqrt(Sigma.B[1,1])
    sigma.k <- sqrt(Sigma.B[2,2])
    sigma.t <- sqrt(Sigma.B[3,3])
    
    #correlation 
    rho[1] <- Sigma.B[1,2]/sqrt(Sigma.B[1,1]*Sigma.B[2,2])
    rho[2] <- Sigma.B[1,3]/sqrt(Sigma.B[1,1]*Sigma.B[3,3])
    rho[3] <- Sigma.B[2,3]/sqrt(Sigma.B[2,2]*Sigma.B[3,3])
    }
    ",fill=TRUE)
sink()



# Create identity matrix for Wishart dist'n
W <- diag(3)

# load data
data <- list(y = df$length, age = df$age, g = df$g, n = dim(df)[1],
             J = length(unique(df$STUDY_BAY_NO)), W=W  )


# Number of parameters
K <- 3
# Number of groups
J <- length(unique(df$STUDY_BAY_NO))

#B <- array(c(rep(log(400) +rnorm(1,0.01,0.01),J),rep(log(0.5)+rnorm(1,0.01,0.1),J),rep(log(0.5+10)+rnorm(1,0.01,0.1),J))
, c(J,K))



# Initial values
inits <- function(){list(mu.Linf = rnorm(1,3,0.001), mu.k = rnorm(1,1,0.001), mu.t0 = rnorm(1,0.7,0.001),
                         sigma.y = runif(1,1,10), 
                         B=array(c(rep(log(600) +rnorm(1,0.01,0.01),J),rep(log(0.4)+rnorm(1,0.001,0.1),J),rep(log(0.5+10)+rnorm(1,0.01,0.1),J)),
                                 c(J,K)), Tau.B = rwish(4,diag(3)) ) }



# Parameters monitored
params1 <- c("mu.Linf", "mu.k", "mu.t0", "sigma.l", "sigma.k", "sigma.t", "sigma.y","rho","B","Sigma.B","mu.k.raw","k",
             "linf.raw","t0.raw","Linf","t0")


# MCMC settings
ni <- 100000
nt <- 3
nb <- 50000
nc <- 3


############################################
###### DO analysis in JAGS and compare times
start.time = Sys.time()         # Set timer

out <- jags(data = data, inits = inits, parameters.to.save = params1, 
            model.file = "modelLogAll.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time


# Summarize the result
print(out, digits = 2)

# #Write output
# fem_all <- out$BUGSoutput$summary
# write.csv(fem_all,"jags_output_fem_all.csv")


##### Plot 

predX <- seq(min(df$age), max(df$age), length=100) # fake data to predict
bayesB <- out$BUGSoutput$summary[c('linf.raw','mu.k.raw','t0.raw'),1] # Extract pop'n average coefficents

# Obtain group-specific estimates
linfs <- out$BUGSoutput$summary[22:28,1]
ks <- out$BUGSoutput$summary[39:45,1]
t0s <- out$BUGSoutput$summary[58:64,1]

# Put group esitmates in matrix
bayesB2 <- cbind(linfs, ks, t0s)

z <- seq(min(df$length),max(df$length),length=100) # fake y-axis


# VonB Equation - pop-Ave effect
y.pred <- bayesB[1] * (1-exp(-bayesB[2]  * (predX -  bayesB[3] )))



#res <- 6
#name_figure <- "VonB.png"
#png(filename = name_figure, height = 500*res, width = 800*res, res=72*res)
#def.par <- par(no.readonly = TRUE)

par(mfrow = c(1,1), mar=c(4,5,1,1))

size.labels = 1
size.text = 1.3
x.label = 'Age (yrs)'
y.label = 'Total length (mm)'


plot(predX,z, ylim=c(min(df$length), max(df$length)),xlim=c(min(df$age), max(df$age)), axes=F, ylab='', xlab='', type='n')
points(jitter(df$age),df$length, cex=0.8, pch=16)

for(i in 1:J){
  y.pred2 <- bayesB2[i,1] * (1-exp(-bayesB2[i,2]  * (predX -  bayesB2[i,3] )))
  lines(predX,y.pred2,lwd=1, col=2,lty=1)
  
}

lines(predX, y.pred, lwd = 5, col="blue", lty = 1)

axis(side=1, cex.axis=size.text,  labels=T, mgp=c(0,0.4,0), tck=-0.01) # at= , tck=0
axis(side=2,cex.axis=size.text,las=1,mgp=c(0,0.4,0), tck= -0.01 ) # 
mtext(x.label, line = 2, side = 1, cex = size.text)
mtext(y.label, line = 3, side = 2, cex = size.text)
box()

# # Label for histogram
# text(9.0,180, "k-values", cex=1) 
# 
# # Add histogram of parameter
# par(fig=c(0.60, 0.8, 0.2, 0.4), new=T,mar=c(0,0,0,0),oma=c(2,2,1,2),mai=c(0,0,0,0)) # Default settings of 0 1 0 1 (the full device width and height)
# # Plot with no data
# hist(bayesB2[,2],breaks=20,col='orange',xlab='',ylab='',axes='F',main='')
# axis(side=1, cex.axis=0.5, mgp=c(0,-0.2,0),tck= -0.01) # plot date on x-axis,labels=F
# axis(side=2, cex.axis=0.5,  mgp=c(0,0.2,0),tck= -0.01, las=1)
# 
# #box()
# #par(def.par)
# #dev.off()

library(mcmcplots)

group <- c("Pamlico Sound, NC", "Winyah Bay,SC","Cape Romain, SC", "Charleston, SC", 
           "ACE Basin, SC","Mobile Bay, AL",  "Matagorda, TX")
## Linfs
par(mfrow = c(2,1), mar=c(4,1,1,1))
caterplot(as.mcmc(out), parms="Linf",labels=group,labels.loc='above',
          quantiles=list(outer=c(0.025,0.975),inner=c(0.1,0.9)),pch=3,
          cex.labels=0.75,lwd=c(2,5),style='plain',col=2)
mtext("Total length (mm)", line = 2.5, side = 1, cex = 1.25)

## Ks
caterplot(as.mcmc(out), parms="k",labels=group,labels.loc='above',
          quantiles=list(outer=c(0.025,0.975),inner=c(0.1,0.9)),pch=3,
          cex.labels=0.75,lwd=c(2,5),style='plain',col=4)
mtext("Growth rate (K)", line = 2.5, side = 1, cex = 1.25)

###### Tables for appendix
xtable(out$BUGSoutput$summary[,1:4])

