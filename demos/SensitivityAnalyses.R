## 

# Load necessary libraries
library(tidyverse)
library(lmtest)

# 1. Placebo Tests in R
# 
# Placebo tests are used to check if a treatment effect appears where it should not exist, which can indicate spurious correlation or model misspecification.
# 
# Example 1: Placebo Test with Pre-Treatment Outcomes
# We check if a policy or treatment has an effect before it was implemented. If significant effects are found before the actual intervention, the causal claim is questionable.
# 
# Scenario:
#   
#   A state implements a minimum wage increase in 2018, and we estimate its effect on employment. A placebo test checks if a pre-2018 placebo treatment also shows an effect.

##Example 1



# Simulated data
set.seed(123)
data <- tibble(
  state = rep(1:50, each = 10),
  year = rep(2009:2018, times = 50),
  treated = ifelse(state <= 25, 1, 0), # 25 treated states
  post_treatment = ifelse(year >= 2018, 1, 0), # Policy starts in 2018
  employment = rnorm(500, mean = 100, sd = 10) - 2 * treated * post_treatment # Simulated policy effect
)

# Run regression for actual treatment
model_actual <- lm(employment ~ treated * post_treatment + factor(state) + factor(year), data = data)
summary(model_actual)

# Placebo Test: Using 2016 as the fake treatment year
data$placebo_post <- ifelse(data$year >= 2016, 1, 0)
model_placebo <- lm(employment ~ treated * placebo_post + factor(state) + factor(year), data = data)
summary(model_placebo)


Refined Interpretation:
  
  If the placebo treatment (2016) shows a statistically significant effect, this suggests that pre-existing trends might explain the observed employment changes, rather than the actual 2018 policy.
If the placebo treatment shows no effect, the 2018 policy estimate is more credible.

# #Example 2: Placebo Test with Fake Treatment Groups
# Assign the treatment randomly and check if a significant effect appears.
# 
# Scenario:
# We analyze the effect of an education program on students' test scores, but we randomly assign treatment as a placebo test.
# 
# # Simulated Data
set.seed(456)
data <- tibble(
  student_id = 1:1000,
  treated_real = sample(0:1, 1000, replace = TRUE),  # Real treatment
  treated_placebo = sample(0:1, 1000, replace = TRUE),  # Fake treatment
  pre_score = rnorm(1000, 50, 10),  # Pre-treatment test scores
  post_score = pre_score + treated_real * 5 + rnorm(1000, 0, 3)  # Treatment adds 5 points on avg
)

# Real treatment effect
model_real <- lm(post_score ~ treated_real + pre_score, data = data)
summary(model_real)

# Placebo test: Fake treatment group
model_placebo <- lm(post_score ~ treated_placebo + pre_score, data = data)
summary(model_placebo)
