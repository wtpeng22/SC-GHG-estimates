library(R6)
# -------------------------------------------------------------------------------------------------
#   Aggregate from countries to FUND regions using sum function
# -------------------------------------------------------------------------------------------------


Agriculture_RegionAggregatorSum <- R6Class("Agriculture_RegionAggregatorSum",
                                           public = list(
                                             ag_mapping_input_regions = NULL,
                                             ag_mapping_output_regions = NULL,
                                             input_output_mapping = NULL,
                                             input_output_mapping_int = NULL,
                                             input_region_names = NULL,
                                             output_region_names = NULL,
                                             input = NULL,
                                             output = NULL,
                                             
                                             initialize = function() {
                                               self$ag_mapping_input_regions <- NULL
                                               self$ag_mapping_output_regions <- NULL
                                               self$input_output_mapping <- NULL
                                               self$input_output_mapping_int <- NULL
                                               self$input_region_names <- NULL
                                               self$output_region_names <- NULL
                                               self$input <- NULL
                                               self$output <- NULL
                                             },
                                             
                                             init = function() {
                                               idxs <- match(self$input_output_mapping, self$output_region_names)
                                               if (any(is.na(idxs))) {
                                                 stop("All provided region names in the Agriculture_RegionAggregatorSum's input_output_mapping Parameter must exist in the output_region_names Parameter.")
                                               }
                                               self$input_output_mapping_int <- idxs
                                             },
                                             
                                             run_timestep = function(t) {
                                               self$output[t, ] <- 0
                                               
                                               for (i in self$ag_mapping_input_regions) {
                                                 self$output[t, self$input_output_mapping_int[i]] <- self$output[t, self$input_output_mapping_int[i]] + self$input[t, i]
                                               }
                                             }
                                           ),
                                           
                                           private = list(
                                             time = NULL
                                           )
)

  # same as above but without a time dimension
Agriculture_RegionAggregatorSum_NoTime <- R6Class("Agriculture_RegionAggregatorSum_NoTime",
                                                  public = list(
                                                    ag_mapping_input_regions = NULL,
                                                    ag_mapping_output_regions = NULL,
                                                    input_output_mapping = NULL,
                                                    input_output_mapping_int = NULL,
                                                    input_region_names = NULL,
                                                    output_region_names = NULL,
                                                    input = NULL,
                                                    output = NULL,
                                                    
                                                    initialize = function() {
                                                      self$ag_mapping_input_regions <- NULL
                                                      self$ag_mapping_output_regions <- NULL
                                                      self$input_output_mapping <- NULL
                                                      self$input_output_mapping_int <- NULL
                                                      self$input_region_names <- NULL
                                                      self$output_region_names <- NULL
                                                      self$input <- NULL
                                                      self$output <- NULL
                                                    },
                                                    
                                                    init = function() {
                                                      idxs <- match(self$input_output_mapping, self$output_region_names)
                                                      if (any(is.na(idxs))) {
                                                        stop("All provided region names in the Agriculture_RegionAggregatorSum_NoTime's input_output_mapping Parameter must exist in the output_region_names Parameter.")
                                                      }
                                                      self$input_output_mapping_int <- idxs
                                                      
                                                      # fill in the data because there's no time dimensions
                                                      self$output <- rep(0, length(self$output_region_names))
                                                      for (i in self$ag_mapping_input_regions) {
                                                        self$output[self$input_output_mapping_int[i]] <- self$output[self$input_output_mapping_int[i]] + self$input[i]
                                                      }
                                                    },
                                                    
                                                    run_timestep = function(t) {
                                                      # blank
                                                    }
                                                  )
)

  