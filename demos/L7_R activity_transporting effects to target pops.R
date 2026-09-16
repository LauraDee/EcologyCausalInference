## Basic transportability methods:


### Selection diagram overview:
# X->Y
# S->Z->Y

## Target population differs from study sample with regard to the distribution of Z, due to researcher choices. 

## Z is an effect modifier of the treatment X and Z influences Y.

## How do we transport the effect of X on Y into this new population?
## How do we evaluate success based on out-of-sample predictive accuracy?

library(ggplot2)
set.seed(123)

### 1) Simulate data: #######

## Note: we are just simulating 1 x in this scenario, so pattern can be a little noisy!

## Sample population:
N <- 100
sample <- data.frame(
  Z = rnorm(N, mean = 0, sd = 4),
  X = rbinom(N, 1, prob = 0.50),
  S = "study sample"
)
sample$true_effect <- 0.5 * sample$Z
sample$Y <- rnorm(N, mean = 10 + sample$true_effect * sample$X + sample$Z)

## Target population:
target <- data.frame(
  Z = rnorm(N, mean = 4, sd = 4),
  X = rbinom(N, 1, prob = 0.50),
  S = "target pop"
)
target$true_effect <- 0.5 * target$Z
target$Y <- rnorm(N, mean = 10 + target$true_effect * target$X + target$Z)


df <- rbind(sample, target)


# Compare the distribution of Z in study sample v. target pop
ggplot(df, aes(x=Z, fill=as.factor(S))) + 
  geom_density(alpha=0.6) +
  xlab("Value of Z") + theme_minimal()



## 2) Calculate treatment effect and evaluate "naive" transport: ####

naivemod <- lm(Y~X, data=sample)


# Predict into the target with the "naive" model fit to the sample:
ate_from_model <- function(model, newdata) {
  mean(predict(model, newdata = transform(newdata, X = 1)) -
      predict(model, newdata = transform(newdata, X = 0)))
}

naive_ate <- coef(naivemod)["X"]

# Naive transport: with no Z in the model, how well does naivemodel predict ATE for target?
naive_target_ATE <- ate_from_model(naivemod, target)


## compare:
naive_target_ATE
mean(target$true_effect) ## True ATE for target



## 3) Reweigh treatment effect for target population, using transportability methods ####

# R = [ P(Y| do(X), Z) * P*(Z) ] = [ P(Y| do(X), Z) * P*(Z) ]

# modify the model to account for variables with S->Y paths:
modified_model <- lm(Y~X*Z, data=sample)

# Transported effect following the g-formula above:
pred_treated   <- predict(modified_model, newdata = transform(target, X = 1))
pred_untreated <- predict(modified_model, newdata = transform(target, X = 0))
transported_effect <- mean(pred_treated - pred_untreated)


# Compare the transported effects between the naive model and updated estimate using transport equation:
naive_target_ATE      # naive estimate without considering Z
transported_effect           # transported treatment estimate, calculated with Z
mean(target$true_effect)     # true effect for target population

