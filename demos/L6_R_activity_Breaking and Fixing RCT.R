## Causal inference: "Breaking" RCT:

library(tidyverse) ## You need these two packages
library(broom)



## We are interested in the effect of bison reintroduction on soil carbon sequestration.
# However, let's say we know that there are many other drivers of soil carbon in natural settings (e.g., precip, soil type) that could confound our estimates of bison on soil C, so we decide to do an experiment, using fences to exclude bison.


### 1) Randomized treatment assignment: #######
#First, let's explore how an additional driver of soil C (precip) influences estimation of the treatment effect in a randomized design:

# Make some vectors to store our simulation results
ate_random <- c() # ATE = average treatment effect
sig_random <- c() # We will also store our p-values


set.seed(1234)

# Our loop to simulate experiment occurring 100 times with the following scenario:

for (i in 1:100) {  ## For i in 100 simulations of fake data
    N <- 12 # Set sample size to 12 plots.
    
    # Sample size is divided among our 2 treatment levels (1=fenced; 0=unfenced)
    fenced <- rep(c(0,1), N/2)
    
    # precipitation varies from 1cm to 5cm in our study area. 
    # (runinf() generates random draws from the uniform distribution).
    precip <- runif(N, min=1, max=5) 
    
    # Now, let's make a fake response variable, based on our generative model: 
    ## Fencing exclosures influence soil carbon with a "true" effect of -5 
    ## (removing bison reduces carbon)
    ## HOWEVER, there is no relationship between bison and precip at this time (Randomized!)
    
    mu <- 10 + 10*precip - 5*fenced ## This is B0 + B1*X1 + B2*X2
    sigma <- 5 ## this is the sd for our normal distribution
    carbon <- rnorm(N, mu, sigma)  ## This simulates carbon using the normal probability distribution
    random.design <- data.frame(precip, fenced, carbon) ## Make it into a dataframe

  # Fit model that ignores precipitation. Just condition on the bison fencing treatment:
    mymodel <- lm(carbon~fenced, data=random.design)
    
    # Extract estimates for treatment effect & p-vals
    ate_random[i] <- coef(mymodel)["fenced"]
    sig_random[i] <- tidy(mymodel)$p.value[2] <= .05
}


# If we randomize, can we accurately estimate the true effect of species richness without including precip, which is also a driver of soil C?
## This plot will show us, across all 100 simulations of 'fake data' which estimates of the treatment effect we ended up with...

## Plot the treatment effect:
tibble(ate = ate_random) %>%
  ggplot(., aes(ate)) + 
  geom_density(fill="#8ab5a5") + # density plot for our estimates of ATE
  # Aesthetic stuff below:
  geom_vline(aes(xintercept=mean(ate_random)), size=1) + # plot a line for the mean est.
  geom_vline(aes(xintercept=-5), # plot a line for the true effect
             linetype="dashed", color="#c94402", size=1) +
  theme_bw()+
  xlab("Estimates of Treatment Effect\nfrom Randomized Design")


## How often did we detect a "significant" effect?
sum(sig_random)/100


## Discuss:
## 1a) How well does this "perfectly" randomized design estimate the true treatment effect?
## 1b) Try it!: Add precip to the model (AFTER fenced) above and re-run the loop. Does adding the precip variable to our model change the estimates of the treatment effects? Why or why not? Does it change the number of times we detect a "significant" effect?


  
  
### 2) Breaking RCT assumptions: Confounding ######

# Let's add a confound to our scenario, using the same loop as above, with two small changes:
set.seed(12345)

for (i in 1:100) {  
    N <- 12 
    
    ## Confounder = Microbial activity
    microbes <- runif(N, min=0, max=1) ##
    ### Now, let's make microbes covary with bison fencing treatment
    fenced <- rep(c(0,1), N/2) + round(microbes)
    
    ## Everything below is the same as before:
    mu <- 10 + 10*microbes - 5*fenced # Microbes also impacts the response...
    sigma <- 5 
    carbon <- rnorm(N, mu, sigma)  
    random.design <- data.frame(microbes, fenced, carbon) 
    
    # Fit model that ignores the confounder, microbes. Just an effect for treatment.
    mymodel <- lm(carbon~fenced, data=random.design)
    
    # Extract estimates for treatment effect & p-vals
    ate_random[i] <- coef(mymodel)["fenced"] 
    sig_random[i] <- tidy(mymodel)$p.value[2] <= .05
  }
  

#Plot the treatment effect:
tibble(ate = ate_random) %>%
    ggplot(., aes(ate)) + 
    geom_density(fill="#8ab5a5") + # density plot for treatment effect
    # Aesthetic stuff below:
    geom_vline(aes(xintercept=mean(ate_random)), size=1) + # plot a line for the mean est.
    geom_vline(aes(xintercept=-5), # plot a line for the true effect
               linetype="dashed", color="#c94402", size=1) +
    theme_bw()+
    xlab("Estimates of Treatment Effect\nfrom Randomized Design")
  

## Discuss:
## 2) How does the omitted confounder challenge estimation of the treatment effect? What would you add to result in an unbiased treatment effect?




### 3) Breaking RCT assumptions: Excludability Violation ######
# Let's add a competing "mediator" to our scenario:
set.seed(1234)

for (i in 1:100) {  
  N <- 12 

  ### Make a single fencing treatment variable that acts on 
  ## both bison abundance and rabbit abundance, 
  ## which are the direct mechanisms impacting soil carbon:
  
  fenced <- rep(c(0,1), N/2) 
  bison <- runif(N, min=0, max=1) - fenced # Fences reduce bison
  rabbits <- runif(N, min=0, max=1) - fenced # Fences reduce rabbits
  
  mu <- 10 + 5*rabbits + 5*bison # Both bison and rabbits will impact our response:
  sigma <- 5 
  carbon <- rnorm(N, mu, sigma)  
  random.design <- data.frame(rabbits, bison, fenced, carbon) 
  
  # Fit model that ignores the mediator, rabbits. Just an effect for treatment.
  mymodel <- lm(carbon~fenced, data=random.design)
  
  # Extract estimates for treatment effect & p-vals
  ate_random[i] <- coef(mymodel)["fenced"] 
  sig_random[i] <- tidy(mymodel)$p.value[2] <= .05
}


## Plot the treatment effects:
tibble(ate = ate_random) %>%
  ggplot(., aes(ate)) + 
  geom_density(fill="#8ab5a5") + # density plot for treatment effect
  # Aesthetic stuff below:
  geom_vline(aes(xintercept=mean(ate_random)), size=1) + # plot a line for the mean est.
  geom_vline(aes(xintercept=-5), # plot a line for the true effect
             linetype="dashed", color="#c94402", size=1) +
  theme_bw()+
  xlab("Estimates of Treatment Effect\nfrom Randomized Design")


## Discuss:
## 3a) How does the unaccounted mediator influence estimation of the effects of bison?
## 3b) What would you need to add to this model to estimate an unbiased effect of bison?




### 4) SUTVA violations and "noncompliance" ######
# SUTVA encompasses: No multiple versions of treatment and no "spillover" or interference.
# Non-Compliance functionally acts on the same mechanisms...

## All three of these result in scenarios in which the treatment differs from the one we actually record (0/1 for fenced/not) -- fenced plots actually receive bison herbivory when they shouldn't (i.e., noncompliance), or unfenced areas are accidentally excluded from herbivory because of neighboring fences (i.e., interference), or fences are differently effective in different areas (i.e., multiple treatments)...

## Since these functionally do similar things, we'll focus on one: "Interference"


for (i in 1:100) {  
  N <- 12 
  fenced <- rep(c(0,1), N/2) # Plots are fenced or not... but....
  
  ## Neighboring fences interfere with bison activity at unfenced plots...
  # Find each plot's two neighbors on the ring (plot 1 and plot 12 touch)
  left  <- c(N, 1:(N - 1))
  right <- c(2:N, 1)
  fenced_neighbors <- fenced[left] + fenced[right]   # 0, 1, or 2
  spill <- 0.25   # + : bison pushed out of fenced plots crowd into unfenced neighbors
  
  baseline <- runif(N, min = 0.5, max = 1.5) # Bison a plot would have with no fences anywhere
  
  # Spillover only reaches UNFENCED plots, from each FENCED neighbor
  spillover <- spill * fenced_neighbors * (1 - fenced)
  
  # Fenced plots exclude bison entirely; unfenced plots get baseline + spillover
  bison <- (1 - fenced) * baseline + spillover

  mu <- 10 + 5*bison
  sigma <- 5
  carbon <- rnorm(N, mu, sigma)
  random.design <- data.frame(fenced, fenced_neighbors, bison, carbon)
  
  # Standard experimental analysis: fenced vs. unfenced plots
  mymodel <- lm(carbon ~ fenced , data = random.design)
  # Extract estimates for treatment effect & p-vals
  ate_random[i] <- coef(mymodel)["fenced"] # (2nd coefficient is our treatment)
  sig_random[i] <- tidy(mymodel)$p.value[2] <= .05
}


## Plot the treatment effect:
tibble(ate = ate_random) %>%
  ggplot(., aes(ate)) + 
  geom_density(fill="#8ab5a5") + # density plot for treatment effect
  # Aesthetic stuff below:
  geom_vline(aes(xintercept=mean(ate_random)), size=1) + # plot a line for the mean est.
  geom_vline(aes(xintercept=-5), # plot a line for the true effect
             linetype="dashed", color="#c94402", size=1) +
  theme_bw()+
  xlab("Estimates of Treatment Effect\nfrom Randomized Design")


## Discuss:
## 4a) Fencing was randomized, so the fenced and unfenced plots are alike on average. Why does the estimate still miss the effect of fencing the whole landscape by about half?
## 4b) Try It! What would you need to add to this model to estimate an unbiased effect of fencing?





# BONUS: This simulation demonstrates the implications of measurement error in the treatment (no noncompliance):


for (i in 1:100) { 
  N <- 12 
  fenced <- rep(c(0,1), N/2)
  precip <- runif(N, min=1, max=5)
  
  ## add measurement error to treatment by setting some to zero
  reps_to_zero <- sample(length(fenced), 2)
  fenced_noncompliant[reps_to_zero] <- 0  
  mu <- 10 + 10*precip - 5*fenced_noncompliant
  
  sigma <- 5 
  carbon <- rnorm(N, mu, sigma)  
  random.design <- data.frame(precip, fenced, carbon) 
  mymodel <- lm(carbon~fenced, data=random.design)
  ate_random[i] <- coef(mymodel)["fenced"]
  sig_random[i] <- tidy(mymodel)$p.value[2] <= .05
}



## Plot the treatment effect:
tibble(ate = ate_random) %>%
  ggplot(., aes(ate)) + 
  geom_density(fill="#8ab5a5") + # density plot for treatment effect
  # Aesthetic stuff below:
  geom_vline(aes(xintercept=mean(ate_random)), size=1) + # plot a line for the mean est.
  geom_vline(aes(xintercept=-5), # plot a line for the true effect
             linetype="dashed", color="#c94402", size=1) +
  theme_bw()+
  xlab("Estimates of Treatment Effect\nfrom Randomized Design")
