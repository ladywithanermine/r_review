# Part A: fit models

library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

plot(education ~ income, data=Anscombe)

## Basic lm model (point estimate)
basiclm = lm(education ~ income + young + urban, data = Anscombe)
summary(basiclm)

## Jags model
library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)

### Run model
set.seed(72)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1, 1.0, 1.0))
}

mod1 = jags.model(textConnection(mod_string), data=data_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

### Check convergence
plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
summary(mod1_sim)

### Residual plot for the point estimate lm model
plot(basiclm)

# Part B: deviance and alternative models

## Check the DIC 
dic.samples(mod1, n.iter=1e6)

## Alternative models

### Model 1: remove the term in the linear model for urban.
mod_string_alt1 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)

params_alt1 = c("b", "sig")

inits_alt1 = function() {
  inits = list("b"=rnorm(2, 0.0, 100.0), "prec"=rgamma(1, 1.0, 1.0))
}

mod_alt1 = jags.model(textConnection(mod_string_alt1), data=data_jags, 
                      inits=inits_alt1, n.chains=3)
update(mod_alt1, 1000) # burn-in

mod_alt1_sim = coda.samples(model=mod_alt1,
                            variable.names=params_alt1,
                            n.iter=5000)
mod_alt1_csim = do.call(rbind, mod_alt1_sim) # combine multiple chains

#### Check the convergence and DIC
plot(mod_alt1_sim)
gelman.diag(mod_alt1_sim)
autocorr.diag(mod_alt1_sim)
autocorr.plot(mod_alt1_sim)
effectiveSize(mod_alt1_sim)
summary(mod_alt1_sim)

dic.samples(mod_alt1, n.iter=1e6)

### Model 2: remove the term in the linear model for urban AND add an interaction term income*youth
mod_string_alt2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)

params_alt2 = c("b", "sig")

inits_alt2 = function() {
  inits = list("b"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1, 1.0, 1.0))
}

mod_alt2 = jags.model(textConnection(mod_string_alt1), data=data_jags, 
                      inits=inits_alt1, n.chains=3)
update(mod_alt2, 1000) # burn-in

mod_alt2_sim = coda.samples(model=mod_alt2,
                            variable.names=params_alt2,
                            n.iter=5000)
mod_alt2_csim = do.call(rbind, mod_alt1_sim) # combine multiple chains

#### Check the convergence and DIC
plot(mod_alt2_sim)
gelman.diag(mod_alt2_sim)
autocorr.diag(mod_alt2_sim)
autocorr.plot(mod_alt2_sim)
effectiveSize(mod_alt2_sim)
summary(mod_alt2_sim)

dic.samples(mod_alt2, n.iter=1e6)

## Using the model favored by the DIC (the original model),
## obtain a Monte Carlo estimate of the posterior probability 
## that the coefficient for income is positive (greater than 0.0). 

## Burn some more steps to ensure convergence
update(mod1, 10000) # burn-in
mod1_sim2 = coda.samples(model=mod1,
                         variable.names=params1,
                         n.iter=5000)
plot(mod1_sim2)

### Extract posterior samples for the income coefficient
income_samples = as.matrix(mod1_sim)[, "b[1]"]

### Compute the Monte Carlo estimate of the posterior probability
prob_income_positive = mean(income_samples > 0)
