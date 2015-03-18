##############################
### Von Bertalanffy Growth ###
##############################

setwd("Z://IndividualGrowth")
dat<-read.csv("VBG_TimeInterval.csv")

str(dat)
library(R2jags)
library(MCMCpack)
#Create empty variance covariance matrix
W<- diag(2)
K<-2
J<-2
# load data
data <- list(Lr=dat$SVL.1, Lm = dat$SVL, dT = dat$dT, n = dim(dat)[1],
             m=dat$Color, J=length(unique(dat$Color)), W=W)

# Initial values
inits <- function(){list(mu.Linf = rnorm(1,0,0.001), mu.k = rnorm(1,1,0.001), sigma.Lr = runif(1,1,5), 
                         B=array(c(rep(log(50) +rnorm(1,0.01,0.01),J),rep(log(0.4)+rnorm(1,0.001,0.1),J)),
                                 c(J,K)), Tau.B = rwish(3,diag(2)) ) }

# Parameters monitored
params1 <- c("mu.Linf", "mu.k", "sigma.l", "sigma.k", "sigma.Lr","rho","B","Sigma.B","mu.k.raw","K",
             "linf.raw","L.inf")

# MCMC settings
ni <- 20000
nt <- 3
nb <- 1000
nc <- 3


############################################
###### DO analysis in JAGS and compare times
start.time = Sys.time()         # Set timer

out <- jags(data = data, inits = inits, parameters.to.save = params1, 
            model.file = "Morph2_VBG.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time


# Summarize the result
print(out, digits = 3)

