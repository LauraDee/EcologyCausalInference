### STANDARD ERRORS WITH SANDWICH ##
# from https://evalf21.classes.andrewheiss.com/example/standard-errors/

library(tidyverse)       # For ggplot, dplyr, and friends
library(broom)  # To Convert model objects into data frames
install.packages("palmerpenguins")
library(palmerpenguins) # Palmer LTER penguin data
library(sandwich)  # Adjust standard errors
library(lmtest)    # Recalculate model errors with sandwich functions with coeftest()
library(modelsummary)  # Make nice tables and plots for models

# Get rid of rows with missing values
penguins <- penguins %>% drop_na(sex)
head(penguins)

model1 <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm + species,
             data = penguins)
tidy(model1, conf.int = TRUE)
# This confidence interval is the coefficient estimate ± (1.96 × the standard error) (or 60.1 ± (1.96 × 7.21)),

# plot the residuals
# Plug the original data into the model and find fitted values and
# residuals/errors
fitted_data <- augment(model1, data = penguins)

# Look at relationship between fitted values and residuals
ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  geom_smooth(method = "lm")

#clustered by species and or island:
ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm")

ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point(aes(color = island)) +
  geom_smooth(method = "lm")

ggplot(fitted_data, aes(x = .resid)) +
  geom_histogram(binwidth = 100, color = "white", boundary = 3000)


## sandwich comes with a bunch of standard error 
#correction functions, like vcovHC() for heteroskedasticity-consistent (HC) errors, 
#vcovHAC() for heteroskedastiticy- and autocorrelation-consistent (HAC) errors, and vcovCL() for clustered errors (see their website for all the different ones). Within each of these different functions, there are different types (again, things that fancy smart statisticians figure out). If you want to replicate Stata’s , robust option exactly, you can use vcovHC(type = "HC1").

# Robust standard errors with lm()
model1_robust <- coeftest(model1, 
                          vcov = vcovHC)

# Stata's robust standard errors with lm()
model1_robust_stata <- coeftest(model1, 
                                vcov = vcovHC,
                                type = "HC1")

tidy(model1_robust) %>% filter(term == "bill_length_mm")
## # A tibble: 1 × 5
##   term           estimate std.error statistic  p.value
##   <chr>             <dbl>     <dbl>     <dbl>    <dbl>
## 1 bill_length_mm     60.1      6.52      9.22 3.67e-18

tidy(model1_robust_stata) %>% filter(term == "bill_length_mm")


# Clustered robust standard errors with lm()
model1_robust_clustered <- coeftest(model1,
                                    vcov = vcovCL,
                                    type = "HC1",
                                    cluster = ~species)

tidy(model1_robust_clustered, conf.int = TRUE) %>% 
  filter(term == "bill_length_mm")

# Clustered robust standard errors with lm(), correcting for small sample (finite sample correction )
model1_robust_clustered_corrected <- coeftest(model1,
                                              vcov = vcovCL,
                                              type = "HC1",
                                              df = 2,  # There are 3 species, so 3-1 = 2
                                              cluster = ~species)

tidy(model1_robust_clustered_corrected, conf.int = TRUE) %>% 
  filter(term == "bill_length_mm")


model_basic <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm + species,
                  data = penguins)

# Add an extra row with the error names
se_info <- tibble(term = "Standard errors", "Regular", "Robust", "Stata", "Clustered by species")

modelsummary(model_basic, 
             # Specify how to robustify/cluster the model
             vcov = list("iid", "robust", "stata", function(x) vcovCL(x, cluster = ~ species)),
             # Get rid of other coefficients and goodness-of-fit (gof) stats
             coef_omit = "species|flipper|Intercept", gof_omit = ".*",
             add_rows = se_info)


modelplot(
  list('lm_robust(se_type = "stata", clusters = species)' = model_lmrobust_clustered,
       'feols(se = "cluster", cluster = ~species)' = model_feols_clustered,
       'feols(se = "hetero")' = model_feols_hetero,
       'lm() + vcovCL(cluster = "species") [small sample corrected]' = model1_robust_clustered_corrected,
       'lm() + vcovCL(cluster = "species")' = model1_robust_clustered,
       'lm() + vcovHC(type = "HC1") [Stata]' = model1_robust_stata,
       "lm() + vcovHC() [robust]" = model1_robust,
       "Basic lm() model" = model1),
  coef_omit = "species|flipper|Intercept") + 
  guides(color = guide_legend(reverse = TRUE))

#apply newey west Ses and clustered robust with coef test
coeftest(model1, vcov = NeweyWest)
coeftest(model1, vcov = vcovCL(model1, ~species))


