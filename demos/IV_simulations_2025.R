## Laura Dee - IV regression example
# EBIO Spring 2025

library(AER)
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
library(lavaan)
# source("Cluster2way.R")   ##source("http://iangow.me/~igow/code/cluster2.R")

### detecting weak instruments in R: https://diffuseprior.wordpress.com/2013/09/23/detecting-weak-instruments-in-r/

## this compares IV versus OLS and HW errors:
# https://iangow.wordpress.com/2012/01/19/iv-regression-and-two-way-cluster-robust-standard-errors/
# also useful code for implementing clustered errors (2 way) with ivreg which calls the code in Cluster2way.R

###
#Description
#Fit instrumental-variable regression by two-stage least squares. This is equivalent to direct instrumental variables
#estimation when the number of instruments is equal to the number of predictors.
#Usage
#ivreg(formula, instruments, data, subset, na.action, weights, offset,
#     contrasts = NULL, model = TRUE, y = TRUE, x = FALSE, ...)
#ivreg is the high-level interface to the work-horse function ivreg.fit, a set of standard methods
#(including print, summary, vcov, anova, hatvalues, predict, terms, model.matrix, bread,
# estfun) is available and described on summary.ivreg.

#### simulations
set.seed(0)
# first generate exogeneous variables: o and z
z <- rnorm(100, 3, 7)
o <- rnorm(100, 3, 5)
u <- rnorm(100, 0, 7)  # assuming random error and uncorrelated by generating this structure

# now generate endogenous variables
#assume it depends linearly on o and z with some noise and with some coefficients
# if I made the coefficient on z and, depending on the noise (if small), then its a strong instrument
# bc most of x will be explained by z. #how much can we tell about x from z?
beta_zx = 5
beta_ox = 5  #size of bias from omitted variable o depends on this coefficient as well as beta_oy (they compound)
x  <- beta_zx * z + beta_ox*o + rnorm(100, 0, 1)

#generate y, the dependent variable
beta_xy = 1
beta_oy = 50
y = beta_xy*x + beta_oy*o + u

### ENDOGENIETY: OMMITED VARIABLES BIAS OF o ###
## Now let's just do a linear model of x --> y
OLS <- lm(y ~ x)
coeftest(OLS)  #biased up, because positive effect of o on y and on x.

#Now let's do a model where we control for o., assuming you observe o.
OLS2 <- lm(y ~ x + o)
coeftest(OLS2)

# Third model: IV; we don't observe o but we have data on z.
iv <- ivreg(y ~ x | z)
summary(iv)

## test that beta_xy = 1
# with iv not significantly different than true value (which = 1); rhs is specifying that hypothesis that the true val =1
linearHypothesis(iv, c(0,1), rhs=1)
# issue iv gives a lot of noise but not significantly diff than real value

# with the OLS model with an omitted variable; it is significantly different.
linearHypothesis(OLS, c(0,1), rhs=1)

############################################################################
## Stepping into reverse causality ########################################
###############################################################################

# need a set of rules (in equations) for how they are generated (co-determined and related) in real life...
# to solve for how they are related.

##############################################################################################
## Stepping into reverse causality with an instrument  ########################################
###################################################################################################

# Regenerate data with instrument ###
# Our simultaneous model looks like this:
#   (1) Y1 = a0 + a1*Y2 + a2*X1 + e1
#   (2) Y2 = g0 + g1*Y1 + g2*X2 + e2
# where a0,a1,a2,g0 and g1 are parameters that we specify explicitly while e1 and e2 are error terms.
#
# We can re-write the system of equations like this using simple substitution:
#   Y1 = (a0 + a1*g0 + a1*e2 + a2*x1 + a1*g2*x2 + e1)/(1 – a1*g1)
#   Y2 = (g0 + g1*a0 + g1*a2*x1 + g1*e1 + g2*x2 + e2)/(1 – g1*a1)
# and use this to simulate the data

# Set size of dataset
n <- 1000

# Set up parameters
a0 <- 1
a1 <- 0.1
# a1 = 1 # Determines strength of simultaneity in Equation (1); close to 0 is weak
a2 <- 1      # Determines how strong of an instrument x2 is for Equation (2); close to 0 is weak
g0 <- 5
g1 <- -2     # Determines strength of simultaneity in Equation (2); close to 0 is weak
g2 <- 1      # Determines how strong of an instrument x1 is for Equation (1); close to 0 is weak

# Randomly generate data: Normal(0,1), generate outcomes, save to dataframe
# defaults to mean = 0, sd = 1
x1 <- rnorm(n)
x2 <- rnorm(n)
e1 <- rnorm(n)
e2 <- rnorm(n)

y1 <- (a0 + a1*g0 + a1*e2 + a2*x1 + a1*g2*x2 + e1)/(1-a1*g1)
y2 <- (g0 + g1*a0 + g1*a2*x1 + g1*e1 + g2*x2 + e2)/(1-g1*a1)

dtf <- data.frame(y1,y2,x1,x2)

## run models to compare iv and OLS and SEM
m1 <- lm(y1 ~ y2, data=dtf)
# plot(y1 ~ y2)
summary(m1)
m2 <- lm(y1 ~ y2 + x1, data=dtf)
summary(m2)

#IV
ivmod1 <- ivreg(y1 ~ y2 + x1 | x2 + x1, data=dtf)
summary(ivmod1, diagnostics = T)

#SEM
sem.naive1 <- 'y1 ~ y2 + x1 '
sem.1.fit <- sem(sem.naive1, dtf)
summary(sem.1.fit)

## does not fix the biased estimate for how y2 affects y1.
sem.reverse <- 'y1 ~ y2 + x1
               y2 ~ x2 '
sem.reverse.fit <- sem(sem.reverse, dtf)
summary(sem.reverse.fit)

#########################################################################
#### Vary the noise and instrument strength ###########################
#########################################################################

# Set up parameters
a0 <- 1
a1 <- 0.1
# a1 = 1 # Determines strength of simultaneity in Equation (1); close to 0 is weak
a2 <- 1      # Determines how strong of an instrument x2 is for Equation (2); close to 0 is weak
g0 <- 5
g1 <- -2     # Determines strength of simultaneity in Equation (2); close to 0 is weak
g2 <- 1      # Determines how strong of an instrument x1 is for Equation (1); close to 0 is weak

# Randomly generate data: Normal(0,1), generate outcomes, save to dataframe
# defaults to mean = 0, sd = 1
x1 <- rnorm(n)
x2 <- rnorm(n)
e1 <- rnorm(100, 0, 7)
e2 <- rnorm(100, 0, 1)

y1 <- (a0 + a1*g0 + a1*e2 + a2*x1 + a1*g2*x2 + e1)/(1-a1*g1)
y2 <- (g0 + g1*a0 + g1*a2*x1 + g1*e1 + g2*x2 + e2)/(1-g1*a1)

dtf <- data.frame(y1,y2,x1,x2)

## run models to compare iv and OLS and SEM
m1 <- lm(y1 ~ y2, data=dtf)
# plot(y1 ~ y2)
summary(m1)
m2 <- lm(y1 ~ y2 + x1, data=dtf)
summary(m2)

#using library(AER)
ivmod1 <- ivreg(y1 ~ y2 + x1 | x2 + x1, data=dtf)
summary(ivmod1, diagnostics = T)



