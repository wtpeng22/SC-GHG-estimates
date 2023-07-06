inputregions <- Index()
outputregions <- Index()

input_output_mapping <- Parameter(index = inputregions) # one element per input region containing it's corresponding output region
input_output_mapping_int <- Variable(index = inputregions) # internally computed for speed up

input_region_names <- Parameter(index = inputregions)
output_region_names <- Parameter(index = outputregions)

input <- Parameter(index = c(time, inputregions))
output <- Variable(index = c(time, outputregions))

init <- function(p, v, d) {
  idxs <- match(p$input_output_mapping, p$output_region_names)
  if (any(is.na(idxs))) {
    stop("All provided region names in the RegionAggregatorSum's input_output_mapping Parameter must exist in the output_region_names Parameter.")
  }
  v$input_output_mapping_int[] <- idxs
}

run_timestep <- function(p, v, d, t) {
  v$output[t, ] <- 0
  
  for (i in d$inputregions) {
    v$output[t, v$input_output_mapping_int[i]] <- v$output[t, v$input_output_mapping_int[i]] + p$input[t, i]
  }
}
