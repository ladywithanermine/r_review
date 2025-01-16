# Lesson 10 Graded test

## Question 1
x1_i = 0.8
x2_i = 1.2
b0 = 1.5
b1 = -0.3
b2 = 1.0

loglam = b0 + b1*x1_i + b2*x2_i
lam = exp(loglam)

## Question 2
# Re-run the JAGS model for the Poisson regression on doctor visits from the lesson. 
# Calculate the DIC for the original model. Now remove the interaction term 
# from the model and fit the simpler additive model. Again compute the DIC. 
# If we use predictive performance as a criterion for selecting models, 
# what do we conclude?

## Data
library("COUNT")

data("badhealth")
?badhealth
head(badhealth)

## JAGS model
library("rjags")

mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

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

## MODEL WITHOUT INTERACTION TERM
mod_string2 = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
} "

params2 = c("int", "b_badh", "b_age")

mod2 = jags.model(textConnection(mod_string2), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim2 = coda.samples(model=mod2,
                       variable.names=params2,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))

## convergence diagnostics
plot(mod_sim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
autocorr.plot(mod_sim2)
effectiveSize(mod_sim2)

## compute DIC
dic2 = dic.samples(mod2, n.iter=1e3)


## Question 4
# In the previous course, we briefly discussed Poisson processes. 
# The mean of a Poisson distribution can be thought of as a rate at which the events
# we count are occurring. Hence, it is natural to imagine that 
# if we are observing for twice as long, we would expect to count about 
# twice as many events (assuming the rate is steady). 
# If t is the amount of time that we observe, and λ is the rate of events per unit of time, 
# then the expected number of events is tλ and the distribution 
# of the number of events in this time interval is Poisson(tλ).
# 
# Suppose that a retail store receives an average of 15 customer calls per hour, 
# and that the calls approximately follow a Poisson process. 
# If we monitor calls for two hours, what is the probability that there will be 
# fewer than 22 calls in this time period? Round your answer to two decimal places.

lamb = 15
t = 2

n = 5e3
y = rpois(n=n, lambda=t*lamb)
plot(table(y)/n, pch=2, ylab="posterior prob.", xlab="visits")
mean(y < 22)


## Question 5
# On average, this retailer receives 0.01 calls per customer per day. 
# They notice, however, that one particular group of customers tends to call more frequently. 
# To test this, they select 90 days to monitor 224 customers, 24 of which belong to this group 
# (call it group 2). Not all customers had accounts for the full 90 day period, 
# but we do know how many of the 90 days each was active. 
# We also have the age of the customer, the group to which the customer belongs, 
# and how many calls the customer placed during the period they were active. 
# The data are attached as callers.csv.

# Try plotting some of the variables to understand some of the relationships. 
# If one of the variables is categorical, a box plot is a good choice.
# 
# Which of the following plots would be most useful to the retailer to informally 
# explore their hypothesis that customers from group 2 call at a higher rate than the other customers?

# Set R's working directory to this folder containing the code and data
setwd("~/repos/r_review/Coursera bayesian statistics/2. Techniques and models")
# Read the data
dat = read.csv(file="callers.csv", header=TRUE)

lamb = 0.01
t = 90
n = 224

### Plots
hist(dat$calls, breaks=20)
boxplot(dat$calls/dat$days_active, dat$age)
boxplot(dat$calls, dat$isgroup2)
boxplot(dat$age, dat$isgroup2)
boxplot(dat$calls/dat$days_active, dat$isgroup2)


## QUESTION 6
# Since we know how many days each customer was active and the data are counts, 
# it seems reasonable to use a Poisson process model. 
# We will assume that the customers' calls are independent, and that the calling rate per day active for person 
# lambda_i is unique to each customer and does not change throughout the monitoring period.
# 
# It makes sense to model lambda_i using our two covariates, age and isgroup2. 
# How would the likelihood specification for this model look in JAGS?

mod_string = " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(days_active[i] * lam[i])
    log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]
  }
  
  b0 ~ dnorm(0.0, 1.0/1e2)
  b1 ~ dnorm(0.0, 1.0/1e2)
  b2 ~ dnorm(0.0, 1.0/1e2)
} "

set.seed(102)

data_jags = as.list(dat)

params = c("b0", "b1", "b2")

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

## Residuals
X = as.matrix(dat[,c(3, 4)])
X_days = as.matrix(dat[, 2])
pmed_coef = apply(mod_csim, 2, median)
llam_hat = pmed_coef["b0"] + X %*% pmed_coef[c("b1", "b2")]
lam_hat = X_days * exp(llam_hat)

# B2 posterior
# Hint: Count how many posterior simulations of b2 are greater than 0 and divide by the total number of simulations.
b2_post = as.matrix(mod_csim)[, 3]
prob_b2_gt_0 = sum(b2_post > 0) / length(b2_post)
prob_b2_gt_0
