##############################
### Von Bertalanffy Growth ###
##############################

#setwd("Z://IndividualGrowth")
#setwd("Z:/depts/ag_apel/IndividualGrowth")
setwd("E:/Current/New or Updated Files/PennState/Research/IndividualGrowth")

dat<-read.csv("Data/VBG_TimeInterval_SeasonalDaysDuration_Temp.csv")

str(dat)
library(R2jags)

# load data
data <- list(Lr=dat$SVL.1, Lm = dat$SVL, dS = dat$dS, dW = dat$dW, n = dim(dat)[1],
             m=as.numeric(dat$Color), J=length(unique(dat$Color)), temp = dat$Temp)


# Initial values
inits <- function(){list(L.inf = rnorm(2,0,0.001))}

# Parameters monitored
params1 <- c("L.inf", "Ks", "Kw", "B0s", "B0w", "Bts", "Btw", "sigma.Lr","tau.Lr")


# MCMC settings
ni <- 4000
nt <- 1
nb <- 1000
nc <- 2


############################################
###### DO analysis in JAGS and compare times
start.time = Sys.time()         # Set timer

out <- jags(data = data, inits = inits, parameters.to.save = params1, 
            model.file = "JAGS_Models/Morph_SeasonallyVaryingVBG_Temp_Script.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time


# Summarize the result
print(out, digits = 3)

