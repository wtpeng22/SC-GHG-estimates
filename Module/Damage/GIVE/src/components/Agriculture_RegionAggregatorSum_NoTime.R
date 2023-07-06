# -------------------------------------------------------------------------------------------------
#   Aggregate from countries to FUND regions using sum function
# -------------------------------------------------------------------------------------------------

ag_mapping_input_regions <- model_ag_mapping_input_regions
ag_mapping_output_regions <- model_ag_mapping_output_regions

input_output_mapping <- model_ag_mapping
input_output_mapping_int <- matrix(nrow = length(ag_mapping_input_regions),ncol = 1)# internally computed for speed up

input_region_names = model_ag_mapping_input_regions
output_region_names = model_ag_mapping_output_regions

idxs <- match(input_output_mapping, output_region_names)

if (is.null(which(is.na(idxs)))) {
  stop("All provided region names in the Agriculture_RegionAggregatorSum's input_output_mapping Parameter must exist in the output_region_names Parameter.")
}
input_output_mapping_int[] <- idxs

output_pop_notime <- matrix(nrow = length(ag_mapping_output_regions),ncol = 1)
output_gdp_notime <- matrix(nrow = length(ag_mapping_output_regions),ncol = 1)

Agsumnotime_pop_run_timestep <- function(population) {
  # fill in the data because there's no time dimensions
  output_pop_notime[] <- 0.
  input <- population
  for (i in 1:length(ag_mapping_input_regions)) {
    output_pop_notime[input_output_mapping_int[i]] <- output_pop_notime[input_output_mapping_int[i]] + input[i]
  }
  return(output_pop_notime)
}
Agsumnotime_gdp_run_timestep <- function(gdp) {
  # fill in the data because there's no time dimensions
  output_gdp_notime[] <- 0.
  input <- gdp
  for (i in ag_mapping_input_regions) {
    output_gdp_notime[input_output_mapping_int[i]] <- output_gdp_notime[input_output_mapping_int[i]] + input[i]
  }
  return(output_gdp_notime)
}
