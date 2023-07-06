# ------------------------------------------------------------------------------
# Accumulate the Ocean Heat Content from BRICK Over Time
# ------------------------------------------------------------------------------
# Define the component
### Parameter
# del_ohc = matrix(nrow = length(time),ncol = 1)

### Variable
del_ohc_accum = matrix(nrow = length(time),ncol = 1)

OceanHeatAccumulator_run_timestep <- function(t){
  if (t==1) {
    del_ohc_accum[t] <<- 0 # FAIR won't provide del_ohc for first period so leave at 0.
  } else {
    del_ohc_accum[t] <<- del_ohc_accum[t-1] + (del_ohc[t] / 1e22)
  }
}
