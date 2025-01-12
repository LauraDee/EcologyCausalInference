############################################################
# Classical measurement error and attenuation bias ##########
# Written by Brendan Hobart and Laura Dee - Spring 2023 ####################
###########################################################

library(tidyverse)

alpha <- c(5)
beta <- c(2)
x <- rep(seq(1:10),10)
x_err1 <- rep(seq(1:10)) + rnorm(100, mean=0, sd=2)
x_err2 <- rep(seq(1:10)) + rnorm(100, mean=0, sd=4)
x_err3 <- rep(seq(1:10)) + rnorm(100, mean=0, sd=6)

y <- alpha + (beta*x)
dat <- data.frame(x=x, y=y, x_err1=x_err1, x_err2=x_err2, x_err3=x_err3)

slr1 <- lm(y~x)
slr1$coefficients[1]
slr1$coefficients[2]

slr2 <- lm(y~x_err1)
slr2$coefficients[1]
slr2$coefficients[2]

slr3 <- lm(y~x_err2)
slr3$coefficients[1]
slr3$coefficients[2]

slr4 <- lm(y~x_err3)
slr4$coefficients[1]
slr4$coefficients[2]

ggplot() +
  ylim(5,26) +
  xlim(-13,25) +
  geom_jitter(data=dat, aes(x=x,y=y),
              height=.1, width=.1, size=2) +
  geom_abline(intercept=slr1$coefficients[1], slope=slr1$coefficients[2],
              lwd=1.0) +
  geom_jitter(data=dat, aes(x=x_err1, y=y), col='blue',
              width=0, height=0.1) +
  geom_abline(intercept=slr2$coefficients[1], slope=slr2$coefficients[2],
              lwd=1.0, col='blue') +
  geom_jitter(data=dat, aes(x=x_err2, y=y), col='red',
              width=0, height=.1) +
  geom_abline(intercept=slr3$coefficients[1], slope=slr3$coefficients[2],
              lwd=1.0, col='red') +
  geom_jitter(data=dat, aes(x=x_err3, y=y), col='green',
              width=0, height=.1) +
  geom_abline(intercept=slr4$coefficients[1], slope=slr4$coefficients[2],
              lwd=1.0, col='green')