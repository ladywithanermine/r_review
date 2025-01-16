# Lesson 9 graded test: logistic regression

## Question 2
# Logistic regression works with binomial likelihoods in addition to Bernoulli likelihoods. 
# If the response y_i is a number of successes in n_i independent trials each with 
# phi_i success probability, we can still model phi_i with a linear model using the logit transformation.
# 
# As an example, consider the OME data in the MASS package in R. 
# The data consist of experimental results from tests of auditory perception in children. 
# Under varying conditions and for multiple trials under each condition, 
# children either correctly or incorrectly identified the source of changing signals.
# 
# Although the independence of the trails and results are questionable, 
# we'll try fitting a logistic regression to these data. 
# First, we'll explore the relationships briefly with the following code:
library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )
pairs(dat)

## Question 3
# Next, we'll fit a reference logistic regression model with noninformative prior in R. 
# We can do this with the glm function, providing the model formula as with the usual lm, 
# except now the response is the observed proportion of correct responses. 
# We must also indicate how many trials were run for each experiment using the weights argument.
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)

# To get an idea of how the model fits, we can create residual 
# (using a special type of residual for non-normal likelihoods) 
# and in-sample prediction plots.
plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)


# Question 4
# JAGS model
# Next, we will fit a similar model in JAGS. 
# To make the results comparable to those of the reference model, 
# we will use the same configuration of covariates. 
# We can extract this information from the reference model using model.matrix.
X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X) 

# Question 5
# Now complete the following code (as well as the code from previous questions) 
# to fit the JAGS model with the fairly noninformative priors given. 
# Use three chains with at least 5,000 iterations in each.

mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).

# Run the model
library(rjags)
set.seed(92)
head(X)

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)
params = c("b0", "b")
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim, ask=TRUE)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)

## calculate DIC
dic1 = dic.samples(mod, n.iter=1e3)

## Raftery and Lewis's diagnostic
raftery.diag(mod_csim)

# Question 7
# Using the posterior mean estimates of the model coefficients, 
# create a point estimate of the probability of correct responses 
# for a child of age 60 months, with high OME, using a coherent stimulus of 50 decibels. 
# Round your answer to two decimal places.
# 
# Hint: First calculate the linear part by multiplying the variables 
# by the coefficients and adding them up (call this xb). 
# Once you have that, apply the inverse of the link function 
# to transform it into a probability estimate.

## A particular example with age=60, ome=high, coherent=0, loud=50
xb2 = pm_coef["b0"] + 60*pm_coef["b[1]"] + 0*pm_coef["b[2]"] + 50*pm_coef["b[3]"] + 0*pm_coef["b[4]"]
phat2 = 1.0 / (1.0 + exp(-xb2))


# Question 8
# Use the posterior mean estimates of the model coefficients to create point 
# estimates of the probability of correct responses for each observation in the original data. 
# To do this, follow the steps outlined in the lesson to create a vector of 
# these probabilities called phat (using our notation from this quiz, it would be phi_hat).
#     
# Once you have phat, calculate the proportion of in-sample observations 
# that are correctly classified according to the following criterion: 
# the model prediction and observed correct response rate are either 
# both higher than 0.7 or both lower than 0.7. 
# Round your answer to two decimal places.

# Calculate probabilties for the complete dataset
pm_coef = colMeans(mod_csim)
xb = pm_coef["b0"] + X %*% pm_coef[1:4]
phat = 1.0 / (1.0 + exp(-xb))
head(phat)

(tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)
