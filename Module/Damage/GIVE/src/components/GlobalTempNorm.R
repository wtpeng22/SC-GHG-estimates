# ------------------------------------------------------------------------------
# Normalize global temperature to a provided list of years - template component to be used by different components
# ------------------------------------------------------------------------------
# Define GlobalTempNorm component

global_temperature_norm <- matrix(nrow = length(timestep_climate),ncol = 1) # Global temperature deviation normalized to the new baseline (°C).

GlobalTempNorm_run_timestep <- function(norm_range_start, norm_range_end, t) {
  norm_range_start_value <<- norm_range_start - model_first + 1
  norm_range_end_value <<- norm_range_end - model_first + 1
  
  # if (t >= norm_range_end_value) {
  if (t == norm_range_start_value) {
    t_values <<- seq(norm_range_start_value, norm_range_end_value, by = 1) # Mimi errors if you use a `:` to index with timesteps. This is a workaround for now.
    global_temperature_norm_range_mean <<- mean(global_temperature[t_values])
  }
  
  # if (t >= norm_range_end_value) {
  if (t >= norm_range_start_value) {
    global_temperature_norm[t] <<- global_temperature[t] - global_temperature_norm_range_mean
  }
}


