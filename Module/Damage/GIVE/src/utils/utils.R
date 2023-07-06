# Load required packages

# Define function
get_module_name <- function(m, comp_name) {
  return (m$md$namespace[[comp_name]]$comp_id$module_obj)
}
