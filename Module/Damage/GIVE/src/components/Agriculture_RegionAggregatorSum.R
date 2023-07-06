# -------------------------------------------------------------------------------------------------
#   Aggregate from countries to FUND regions using sum function
# -------------------------------------------------------------------------------------------------

ag_mapping_input_regions <- model_ag_mapping_input_regions
ag_mapping_output_regions <- model_ag_mapping_output_regions

input_output_mapping <- model_ag_mapping
input_output_mapping_int <- matrix(nrow = length(ag_mapping_input_regions),ncol = 1)# internally computed for speed up

input_region_names = model_ag_mapping_input_regions
output_region_names = model_ag_mapping_output_regions

input <- matrix(nrow = length(timestep_damage),ncol = length(ag_mapping_input_regions))
output_pop <- matrix(nrow = length(timestep_damage),ncol = length(ag_mapping_output_regions))
output_gdp <- matrix(nrow = length(timestep_damage),ncol = length(ag_mapping_output_regions))

idxs <- match(input_output_mapping, output_region_names)
if (any(is.na(idxs))) {
  stop("All provided region names in the Agriculture_RegionAggregatorSum's input_output_mapping Parameter must exist in the output_region_names Parameter.")
}
input_output_mapping_int[] <- idxs


Agsum_pop_run_timestep <- function( t) {
  output_pop[t, ] <<- 0
  input <<- population
  for (i in 1:length(ag_mapping_input_regions)) {
    output_pop[t, input_output_mapping_int[i]] <<- output_pop[t, input_output_mapping_int[i]] + input[t, i]
  }
}

Agsum_gdp_run_timestep <- function( t) {
  output_gdp[t, ] <<- 0
  input <<- gdp
  input[1:15,169] <<- input[16,169]/1.03^(15:1)
  for (i in 1:length(ag_mapping_input_regions)) {
    output_gdp[t, input_output_mapping_int[i]] <<- output_gdp[t, input_output_mapping_int[i]] + input[t, i]
  }
}


