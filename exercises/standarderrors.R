### STANDARD ERRORS WITH SANDWICH ##
# from https://evalf21.classes.andrewheiss.com/example/standard-errors/

library(tidyverse)       # For ggplot, dplyr, and friends
library(broom)  # To Convert model objects into data frames
install.packages("palmerpenguins")

library(palmerpenguins) # Palmer LTER penguin data
library(sandwich)

# Get rid of rows with missing values
penguins <- penguins %>% drop_na(sex)

model1 <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm + species,
             data = penguins)
tidy(model1, conf.int = TRUE)
