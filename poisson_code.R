n = 576
sumx = 535

# Uniform prior
theta.uniform = rgamma(10000, sumx+1, n)
# Jeffreys prior
theta.jeffreys = rgamma(10000, sumx + 1/2, n)
# Exponential prior with a=2
a.exp=2
theta.exponential = rgamma(10000, sumx+1, n+a.exp)
# Gamma prior with a=2 and b=2
a.gamma=2 
b.gamma=2
theta.gamma = rgamma(10000, sumx+b.gamma, n+a.gamma)
# Chi-square prior with b = 4
b.chi = 4
theta.chisquare = rgamma(10000, sumx+b.chi/2, n + 1/2)

#equal tail intervals
theta.uniform.eq = qgamma(c(0.025, 0.975), sumx+1, n)
theta.jeffreys.eq = qgamma(c(0.025, 0.975), sumx + 1/2, n)
theta.exponential.eq = qgamma(c(0.025, 0.975), sumx+1, n+a.exp)
theta.gamma.eq = qgamma(c(0.025, 0.975), sumx+b.gamma, n+a.gamma)
theta.chisquare.eq = qgamma(c(0.025, 0.975), sumx+b.chi/2, n+1/2)

#HPD intervals
library(HDInterval)
theta.uniform.hpd = hdi(qgamma, credMass = 0.95, shape = sumx+1, rate = n)
theta.jeffreys.hpd = hdi(qgamma, credMass = 0.95, shape=sumx + 1/2, rate=n)
theta.exponential.hpd = hdi(qgamma, credMass = 0.95, shape=sumx+1, rate=n+a.exp)
theta.gamma.hpd = hdi(qgamma, credMass = 0.95, shape=sumx+b.gamma, rate=n+a.gamma)
theta.chisquare.hpd= hdi(qgamma, credMass = 0.95, shape=sumx+b.chi/2, rate=n+1/2)

mean(theta.uniform)
mean(theta.exponential)
mean(theta.gamma)
mean(theta.chisquare)

y = list(c(rep.int(0, 229), rep.int(1, 211), rep.int(2, 93), rep.int(3, 35), rep.int(4, 7), 5))
y = y[[1]]
var(y)