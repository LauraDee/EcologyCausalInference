#### Endogeniety and Omitted Variables Bias

#load paclages
library(AER)
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
library(lavaan)

## Simulate the Data
set.seed(0)
# first generate exogeneous variables: o and z and the error term u:
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
summary(OLS)
#Now let's do a model where we control for o., assuming you observe o.
OLS2 <- lm(y ~ x + o)
coeftest(OLS2)

summary(OLS2)

#Now let's do a model where we control for z, assuming you don't observe o
OLS2 <- lm(y ~ x + z)
coeftest(OLS2)
