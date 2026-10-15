# ==============================================================================
# SYNTHETIC CONTROL DEMONSTRATION CODE
# ==============================================================================

# 1. Load Packages -------------------------------------------------------------
library(tidyverse)
library(tidysynth)

# 2. Load Data -----------------------------------------------------------------
city_data <- read_csv("sc_demo_data.csv")

# 3. Create & Fit Synthetic Control Model --------------------------------------
gbifcity_out <- city_data %>%
  
  # Initial synthetic control specification
  synthetic_control(
    outcome           = n.obs45km,       # Outcome variable
    unit              = City,            # Unit index in panel data
    time              = year,            # Time index in panel data
    i_unit            = "Philadelphia",  # Treated unit
    i_time            = 2021,            # Intervention year
    generate_placebos = TRUE             # Generate placebos for inference
  ) %>%
  
  # Aggregate predictors across full time window
  generate_predictor(
    time_window        = 2015:2023,
    Population         = mean(Population, na.rm = TRUE),
    "Unique Observers" = mean(unique_observers, na.rm = TRUE)
  ) %>%
  
  # Single-year predictors at intervention
  generate_predictor(
    time_window          = 2021,
    "Temperature"        = Temp,
    "Precipitation"      = Prec,
    "Impervious surface" = imperv45km,
    "Area (km2)"         = area_km2,
    "Median HH Income"   = Median_HH_Income
  ) %>%
  
  # Generate optimal weights for synthetic control unit
  generate_weights(
    optimization_window = 2015:2021,  # Pre-intervention fitting period
    margin_ipop         = 0.02,       # Optimizer margin
    sigf_ipop           = 7,          # Optimizer precision
    bound_ipop          = 6           # Optimizer boundary constraint
  ) %>%
  
  # Construct the synthetic control unit
  generate_control()

# 4. Visualizations & Diagnostics ----------------------------------------------

# Trend Plot: Observed vs. Synthetic Control trajectory over time
trend_plot <- gbifcity_out %>% 
  plot_trends() +
  labs(title = "Trend plot", x = "Year", y = "Bee observations")
trend_plot

# Difference Plot: Trajectory difference (Observed vs. Synthetic)
diff_plot <- gbifcity_out %>% 
  plot_differences() +
  labs(title = NULL, x = "Year", y = "(Estimated - Observed) Bee observations")
diff_plot

# C. Weight Plot: Donor unit weight contributions
weight_plot <- gbifcity_out %>% 
  plot_weights() +
  labs(title = NULL, y = "Weight")
weight_plot

# D. Balance Table: Compare predictor values between treated and synthetic unit
balance_tab <- gbifcity_out %>%
  grab_balance_table() %>%
  mutate(
    synthetic_percent_diff = ((synthetic_Philadelphia - Philadelphia) / Philadelphia) * 100,
    donor_percent_diff     = ((donor_sample - Philadelphia) / Philadelphia) * 100
  )
balance_tab

# E. In-Space Placebos: Permutation test against untreated control units
placebos <- gbifcity_out %>% 
  plot_placebos(prune = FALSE) +
  labs(title = NULL, x = "Year", y = "(Estimated - Observed) Bee observations")
placebos

# F. Fisher's Exact Test / Significance Table: Units ordered by MSPE ratio
# (Note: Requires at least 20 donor units to achieve p < 0.05)
significance_tab <- gbifcity_out %>% grab_significance()
significance_tab
