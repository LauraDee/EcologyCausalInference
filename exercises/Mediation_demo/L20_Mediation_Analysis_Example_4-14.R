# Perform mediation analysis using mediation package in R
# Mediation analysis used to understand mechanism through which an independent
# variable (X) affects a dependent variable (Y) through a mediator variable (M).

# Set Up ----------------------------------------------------------------------
# Check if the mediation package is installed, if not, install it
if (!requireNamespace("mediation", quietly = TRUE)) {
  install.packages("mediation")
}

# Load the mediation package
library(mediation)

# check is tidyverse package is installed, if not, install it
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

# Load the tidyverse package
library(tidyverse)

# # Load other necessary packages
# library(lmtest) # for hypothesis testing
# library(sandwich) # for robust standard errors


#' TODO
#' mediator & exposure/outcome interaction

# Example data ----------------------------------------------------------------
# Simulate some data for demonstration purposes
set.seed(123)
n <- 100 # Number of observations


# Partial mediation
# Both X and M contribute to Y.
# There is a direct effect of X on Y and an indirect effect through M.
data_partial <- data.frame(
  X = rnorm(n),
  M = rnorm(n),
  Y = rnorm(n)
)
data_partial$M <- 0.7 * data_partial$X + data_partial$M  # Stronger effect of X on M
data_partial$Y <- 0.4 * data_partial$X + 0.5 * data_partial$M + data_partial$Y  # Adjusted effects of X and M on Y

# Full mediation
# Only M contributes to Y.
# The effect of X on Y is entirely mediated by M.
data_full <- data.frame(
  X = rnorm(n),
  M = rnorm(n),
  Y = rnorm(n)
)
data_full$M <- 0.8 * data_full$X + data_full$M  # Stronger effect of X on M
data_full$Y <- 0.9 * data_full$M + data_full$Y  # Stronger effect of M on Y (no direct effect of X on Y)

# No mediation
# Only X contributes to Y.
# There is no effect of M on Y.
data_no <- data.frame(
  X = rnorm(n),
  M = rnorm(n),
  Y = rnorm(n)
)
data_no$M <- 0.6 * data_no$X + data_no$M  # Moderate effect of X on M
data_no$Y <- 0.5 * data_no$X + data_no$Y  # Moderate effect of X on Y (no effect of M on Y)

# Print summaries of the datasets
cat("Partial mediation:\n")
summary(data_partial)

cat("\nFull mediation:\n")
summary(data_full)

cat("\nNo mediation:\n")
summary(data_no)

# Mediation Analysis ----------------------------------------------------------
# Let's assume we have a dataset `data` with variables:
# X (independent variable), M (mediator), and Y (dependent variable)

data <- data_partial
data <- data_full
data <- data_no

# Step 0: Outcome by predictor model ------------------------------------------
base_model <- lm(Y ~ X, data = data)
summary(base_model)

# checking there is some relationship (usual done with p-value) between treatment (X) & outcome (Y)

# Step 1: Fit mediator model --------------------------------------------------
# This model predicts the mediator (M) using the independent variable (X)
mediator_model <- lm(M ~ X, data = data)
summary(mediator_model)

# Step 2: Fit outcome model ---------------------------------------------------
# This model predicts the dependent variable (Y) using both the independent variable (X) and the mediator (M)
outcome_model <- lm(Y ~ X + M, data = data)
summary(outcome_model)

# Step 3: Perform mediation analysis ------------------------------------------
# This step combines the two models to estimate the mediation effect
mediation_result <- mediate(mediator_model,
                            outcome_model,
                            treat = "X",
                            mediator = "M",
                            boot = TRUE,
                            sims = 100) # default simulation type is quasi-Bayesian Monte Carlo method based on normal approximation (Imai et al. 2010a)

# Step 4: Summarize results ---------------------------------------------------
# point estimates, confidence intervals, and the p-values, for
# average direct, indirect, & total eﬀects
summary(mediation_result)



# Is the total effect significant?
# Is the mediation effect significant?


# Step 5: Plot results --------------------------------------------------------
# average causal mediation effect (ACME)
# average direct effect (ADE)
plot(mediation_result) +
  # title('Partial Mediation')
  # title('Full Mediation')
  title('No Mediation')





# Your Turn -------------------------------------------------------------------
# Try mediation analysis with example dataset
# & figure out which dataset has which type of mediation (full, partial, no mediation)


# load example data -----------------------------------------------------------
# set file path
# file_path <- "path/to/your/data.csv"
# file_path <- "mediation_example_data.csv"

# Read the CSV file into a data frame
mediation_data <- read.csv(file_path)

# examine the data
str(mediation_data)

# subset dataset into three datasets
data_A <- mediation_data %>% filter(dataset == "A")
data_B <- mediation_data %>% filter(dataset == "B")
data_C <- mediation_data %>% filter(dataset == "C")


# Perform mediation analysis on the example data
data <- data_A
data <- data_B
data <- data_C

# Step 0: Outcome by predictor model ------------------------------------------
base_model <- lm(Y ~ X, data = data)
summary(base_model)

# Step 1: Fit mediator model --------------------------------------------------
# This model predicts the mediator (M) using the independent variable (X)
mediator_model <- lm(M ~ X, data = data)
summary(mediator_model)

# Step 2: Fit outcome model ---------------------------------------------------
# This model predicts the dependent variable (Y) using both the independent variable (X) and the mediator (M)
outcome_model <- lm(Y ~ X + M, data = data)
summary(outcome_model)

# Step 3: Perform mediation analysis ------------------------------------------
# This step combines the two models to estimate the mediation effect
mediation_result <- mediate(mediator_model,
                            outcome_model,
                            treat = "X",
                            mediator = "M",
                            boot = TRUE,
                            sims = 1000)

# Step 4: Summarize results ---------------------------------------------------
summary(mediation_result)

# Is the total effect significant?
# Is the mediation effect significant?


# Step 5: Plot results --------------------------------------------------------
# average causal mediation effect (ACME)
# average direct effect (ADE)
plot(mediation_result)

# What are the average causal mediation effect (ACME) and average direct effect (ADE) for each dataset?

# Repeat with each dataset ----------------------------------------------------

# Which dataset has full mediation?
# Which dataset has partial mediation?
# Which dataset has no mediation?







# Mediation Extension ---------------------------------------------------------

# Try mediation analysis with interaction terms


# Try mediation analysis with multiple mediators


# extensions copied from R package vignette

# load new example dataset
data("framing", package = "mediation")

# Moderated mediation
med.fit <- lm(emo ~ treat * age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat * age + emo * age + educ + gender +
                 income, data = framing, family = binomial("probit"))

med.age20 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 20), sims = 100)
med.age60 <- mediate(med.fit, out.fit, treat = "treat",
                     mediator = "emo", covariates = list(age = 60), sims = 100)

summary(med.age20)
summary(med.age60)

med.init <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", 
                    sims = 2)
test.modmed(med.init, covariates.1 = list(age = 20),
            covariates.2 = list(age = 60), sims = 100)

# Non-binary treatment variables
med.fit <- lm(emo ~ cond + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + cond + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med23.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 2, treat.value = 3, sims = 100)
summary(med23.out)

med14.out <- mediate(med.fit, out.fit, treat = "cond", mediator = "emo",
                     control.value = 1, treat.value = 4, sims = 100)
summary(med14.out)

# Sensitivity analysis for sequential ignorability
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,
               data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo",
                   robustSE = TRUE, sims = 100)
sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)

plot(sens.out, sens.par = "rho", main = "Anxiety", ylim = c(-0.2, 0.2))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")
