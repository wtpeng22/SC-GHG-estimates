# --------------------
# Model Parameters
# --------------------

# Define parameters
gsic_beta_0 <- NULL
gsic_v_0 <- NULL
gsic_s_0 <- NULL
gsic_n <- NULL
gsic_teq <- NULL
global_surface_temperature <- NULL

# Define variables
gsic_sea_level <- NULL

# Define function for model equations
glaciers_small_icecaps_run_timestep <- function(t) {
  if (t==1) {
    # Set initial sea level contribution from glaciers and small ice caps.
    gsic_sea_level[t] <<- gsic_s_0
  } else {
    # Calculate sea level contribution from glaciers and small ice caps (with a check in case they have fully melted).
    if (gsic_sea_level[t-1] < gsic_v_0) {
      gsic_sea_level[t] <<- gsic_sea_level[t-1] + gsic_beta_0 * (global_surface_temperature[t-1] - gsic_teq) * (1 - gsic_sea_level[t-1] / gsic_v_0) ^ gsic_n
    } else {
      gsic_sea_level[t] <<- gsic_v_0
    }
  }
}


