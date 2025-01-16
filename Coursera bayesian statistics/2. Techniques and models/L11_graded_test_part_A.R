# Lesson 11 Graded test part A
setwd("/Users/paula/repos/r_review/Coursera bayesian statistics/2. Techniques and models")

## Question 3
# In previous lessons, we fit models to data representing percent growth in personnel 
# for companies in two industries. Below are attached additional data from the original 
# two industries (with 10 and six companies respectively), as well as three additional industries. 
# Percent growth is reported for a total of 53 companies.

dat = read.csv(file="pctgrowth.csv", header=TRUE)

# Rather than fit five separate models, one for each industry, we can fit a hierarchical model. 
# As before, we assume a normal likelihood and common variance across all observations. 
# Each industry will have it's own mean growth, and each of these means will come 
# from a common distribution, from which we will estimate the overall mean and variability across industries.

## Question 4
# Fit the hierarchical model from Question 3 in JAGS and obtain posterior mean estimates 
# for each industry's mean growth (posterior mean for each sigma_g).
library("rjags")

mod_string = " model {
for (i in 1:length(y)) {
  y[i] ~ dnorm(theta[grp[i]], sig2)
}

for (j in 1:max(grp)) {
  theta[j] ~ dnorm(mu, tau2)
}

mu ~ dnorm(0.0, 1.0/1e6)
inv_tau2 ~ dgamma(1/2, 1*3/2)
inv_sig2 ~ dgamma(2/2, 2*1/2)

tau2 = 1/inv_tau2
sig2 = 1/inv_sig2

} "

set.seed(113)

data_jags = as.list(dat)

params = c("theta", "tau2", "sig2")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

## Posterior mean estimates
pmean_post = colMeans(mod_csim)
means_theta = pmean_post[3:7]

# We are interested in comparing these estimates to those obtained from a model 
# that assumes no hierarchy (the ANOVA cell means model). 
# We can approximate the posterior estimates for the five industry means 
# under a noninformative prior by simply calculating the sample mean growth for the five industries. 
# You can do this in R with:
means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)  
  
# How do these compare with the estimates from the hierarchical model?
# Hint: It might help to plot them with:
plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.
  
