# Graded test for lesson 8: ANOVA

## For Questions 3-8, refer to the plant growth analysis from the lesson.

## Question 3
## Re-fit the JAGS model on plant growth from the lesson with a separate variance 
## for each of the three groups. To do so, modify the model code to index the precision 
## in the normal likelihood by group, just as we did with the mean. 
## Use the same priors as the original model 
## (except in this case it will be three independent priors for the variances).
## Compare the estimates between the original lesson model and this model 
## with the summarysummary function. 
##
## Notice that the posterior means for the three μμ parameters 
## are essentially unchanged. However, the posterior variability 
## for these parameters has changed. The posterior for which group's mean 
## was most affected by fitting separate group variances?

### Data
data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)

### JAGS model

library("rjags")

mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
        sig[j] = sqrt(1.0 / prec[j])
    }
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3, 0.0, 100.0), "prec"=rgamma(3, 1.0, 1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

### Model checking
summary(mod_sim)
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)


## Question 4
## Compute the deviance information criterion (DIC) for each of the two models 
## and save the results as objects dic1dic1 (for the original model) 
## and dic2dic2 (for the new model). 
## What is the difference: DIC1 - DIC2?
## Hint: You can compute this directly with the following code: dic1−dic2dic1−dic2.

### Original model
mod_string_orig = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

set.seed(82)

params_orig = c("mu", "sig")

inits_orig = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod_orig = jags.model(textConnection(mod_string_orig), data=data_jags, inits=inits_orig, n.chains=3)
update(mod_orig, 1e3)

mod_sim_orig = coda.samples(model=mod_orig,
                       variable.names=params_orig,
                       n.iter=5e3)
mod_csim_orig = as.mcmc(do.call(rbind, mod_sim_orig)) # combined chains

### DIC calculations
dic1 = dic.samples(mod, n.iter=1e3)
dic2 = dic.samples(mod_orig, n.iter=1e3)
dic1 - dic2


## Question 6
## Use the original model (single variance) to calculate a 95% interval of 
## highest posterior density (HPD) for μ3−μ1. 
## Which of the following is closest to this interval?
mu3 = mod_csim_orig[,3]
mu1 = mod_csim_orig[,1]
HPDinterval(mu3-mu1)


## Question 8
mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)
