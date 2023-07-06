# --------------------
# Model Parameters
# --------------------

lws_0 <- 0.0            # Initial landwater storage trend value applying to first projection year (m).
first_projection_year <- NULL  # First model projection year (necessary because calibrating to Church & White data that removed landwater storage trend).
lws_random_sample <- NULL  # Samples from Normal distribution for annual land water storage trend.

# --------------------
# Model Variables
# --------------------

lws_sea_level <- NULL  # Cumulative sea level contribution from land water storage changes (m).

# --------------------
# Model Equations
# --------------------

landwater_storage_run_timestep <- function(t) {
  
  if (t < first_projection_index) {
    
    # Anthropogenic landwater storage values set to 0 during model calibration period (sea level observations removed land water storage contribution).
    lws_sea_level[t] <<- 0.0
    
  } else if (t == first_projection_index) {
    
    # Set initial condition for first model projection year.
    lws_sea_level[t] <<- lws_0
    
  } else {
    
    # Add landwater storage values for projection period.
    lws_sea_level[t] <<- lws_sea_level[t-1] + lws_random_sample[t]
    #lws_sea_level <<- lws_sea_level[t-1] + rnorm(n = 1, mean = lws_random_sample, sd = 1)  # another way to sample from Normal distribution in R
  }
}




