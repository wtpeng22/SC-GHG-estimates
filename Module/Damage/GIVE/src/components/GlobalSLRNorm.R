# ------------------------------------------------------------------------------
# Normalize global SLR to a provided list of years - template component to be used by different components
# ------------------------------------------------------------------------------
#' GlobalSLRNorm component definition
#'
#' Global sea level rise deviation normalized to the new baseline
#'
#' @param global_slr total sea level rise from all components (includes landwater storage for projection periods)
#' @param norm_range_start the first year of the range of years to normalize to
#' @param norm_range_end the last year of the range of years to normalize to
#'
#' @return Global sea level rise deviation normalized to the new baseline
#' @export
#'
#' @examples
#' # Define a GlobalSLRNorm component
#' GlobalSLRNorm(global_slr = NA_real_, norm_range_start = NA_real_, norm_range_end = NA_real_)
#' 
#' # Run the component with input data
#' library(magrittr)
#' comp <- GlobalSLRNorm(global_slr = c(0.1, 0.2, 0.3, 0.4), norm_range_start = 1, norm_range_end = 2)
#' run(comp)
#'
#' # Get the output
#' get_output(comp, "global_slr_norm")
#' 
#' @importFrom mimi gettime is_first TimestepValue
#' @importFrom stats mean
#' @importFrom units unit_set

# Define parameters

# if (is.na(norm_range_start)) {
#   stop("norm_range_start must be provided")
# }
# 
# if (is.na(norm_range_end)) {
#   stop("norm_range_end must be provided")
# }
#   
# if (length(global_slr) == 0) {
#   stop("global_slr must be provided")
# }
  
# Define variables
global_slr_norm <<- matrix(nrow = length(sea_level_rise),ncol = 1)
global_slr_norm_range_mean <<- NA
  
# Define function to run at each time step
GlobalSLRNorm_run_timestep <- function(t) {
  t_values <<- seq(slr_norm_range_start,slr_norm_range_end,1) - brick_first+1
  global_slr_norm_range_mean <<- mean(sea_level_rise[t_values])
  global_slr_norm[t] <<- sea_level_rise[t] - global_slr_norm_range_mean;
}
  
  
