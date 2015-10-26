##############################
### Von Bertalanffy Growth ###
##############################

dat<-read.csv("Data/VBG_4SeasonStd.csv")

str(dat)
library(R2jags)

# load data
data <- list(Lr=dat$SVL.1, Lm = dat$SVL, dSp = dat$dSp, dSm = dat$dSm,
             dW = dat$dW, dF= dat$dF, n = dim(dat)[1],
             m=as.numeric(dat$Color), J=length(unique(dat$Color)))


# Initial values
inits <- function(){list(L.inf = rnorm(2,0,0.001))}

# Parameters monitored
params1 <- c("L.inf", "Ksp", "Ksm","Kf","Kw", "sigma.Lr","tau.Lr")


# MCMC settings
ni <- 5000
nt <- 1
nb <- 1000
nc <- 3


############################################
###### DO analysis in JAGS and compare times
start.time = Sys.time()         # Set timer

out <- jags(data = data, inits = inits, parameters.to.save = params1, 
            model.file = "JAGS_Models/Morph_4SeasonsDiff.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 

save(out, file="Results/4SeasonStd.RData")
# Calculate computation time


# Summarize the result
print(out, digits = 3)

