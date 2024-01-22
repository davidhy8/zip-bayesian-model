library(nimble)

ZIPcode <- nimbleCode({
  p ~ dunif(0,1)
  lambda ~ dgamma(2,2)
  for(i in 1:N){
    y[i] ~ dZIP(lambda, zeroProb=p)
  }
})

dZIP <- nimbleFunction(
  run=function(x = integer(), lambda = double(), zeroProb = double(), log = logical(0, default = 0)){
    returnType(double())
    
    if(x != 0){
      if(log) return(dpois(x, lambda, log=TRUE) + log(1-zeroProb))
      
      else return((1-zeroProb) * dpois(x, lambda, log= FALSE))
    }
    
    totalProbZero <- zeroProb + (1-zeroProb) * dpois(0, lambda, log=FALSE)
    if(log) return(log(totalProbZero))
    return(totalProbZero)
  })

rZIP <- nimbleFunction(
  run = function(n = integer(), lambda = double(), zeroProb = double()){
    returnType(integer())
    isStructuralZero <- rbinom(1, prob = zeroProb, size = 1)
    if(isStructuralZero) return(0)
    return(rpois(1, lambda))
  })

registerDistributions(list(
  dZIP = list(
    BUGSdist = "dZIP(lambda, zeroProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value=integer()', 'lambda=double()', 'zeroProb = double()')
  )))

ZIPmodel <- nimbleModel( ZIPcode, constants = list(N = 576), check = FALSE)

y = list(c(rep.int(0, 229), rep.int(1, 211), rep.int(2, 93), rep.int(3, 35), rep.int(4, 7), 5))
y = y[[1]]

ZIPmodel$setData(list(y=y))
cZIPmodel <- compileNimble(ZIPmodel)
ZIPmcmc <- buildMCMC(ZIPmodel)
cZIPmcmc <- compileNimble(ZIPmcmc, project = ZIPmodel)
cZIPmcmc$run(10000)
samples <- as.matrix(cZIPmcmc$mvSamples)

lambda.EQ = quantile(samples[,1], c(0.025, 0.975))
p.EQ = quantile(samples[,2], c(0.025, 0.975))

library(HDInterval)
lambda.HPD = hdi(samples[,1], credMass=0.95)
p.HPD = hdi(samples[,2], credMass=0.95)

mean(samples[,1])
mean(samples[,2])

# Create density plot
library(ggplot2)
n = 576
sumx = 535
a.gamma=2 
b.gamma=2
theta.gamma = rgamma(10000, sumx+b.gamma, n+a.gamma)

dat <- data.frame(theta=c(theta.gamma, samples[,1]), lines=rep(c("Poisson", "Zero-inflated Poisson")), each=100)
ggplot(dat, aes(x=theta, fill=lines)) + geom_density(alpha=0.5)
