# --------------------
# Model Parameters
# --------------------
slr_glaciers_small_ice_caps <- vector(length = length(brick_time)) # Sea level contribution from glaciers and small ice caps (m).
slr_greeland_icesheet <- vector(length = length(brick_time)) # Sea level contribution from Greenland ice sheet (m).
slr_antartic_icesheet <- vector(length = length(brick_time)) # Sea level contribution from Antarctic ice sheet (m).
slr_thermal_expansion <- vector(length = length(brick_time)) # Sea level contribution from thermal expansion (m).
slr_landwater_storage <- vector(length = length(brick_time)) # Sea level contribution from landwater storage (m).

gsic_sea_level = slr_glaciers_small_ice_caps
greenland_sea_level = slr_greeland_icesheet
ais_sea_level = slr_antartic_icesheet
te_sea_level = slr_thermal_expansion
lws_sea_level = slr_landwater_storage

# --------------------
#   Model Variables
# --------------------
sea_level_rise <- vector(length = length(brick_time)) # total sea level rise from all components (includes landwater storage for projection periods).

# --------------------
#   Model Equations
# --------------------
global_sea_level_run_timestep <- function(t) {
 
    # Calculate global mean sea level rise as sum of sea level contributions from individual BRICK components.
    sea_level_rise[t] <<- gsic_sea_level[t] + greenland_sea_level[t] + ais_sea_level[t] + te_sea_level[t] + lws_sea_level[t]
}


