# 1-generate_paramdata.R
# Generate samples for each Hector parameter that's going to be sampled
# The resulting `vardata` and `rundata` files are applied to EACH SCENARIO
# Ben Bond-Lamberty January 2016

source("0-functions.R")  # support functions

SCRIPTNAME  	<- "1-generate_paramdata.R"
PROBLEM       <- FALSE

# The S (climate sensitivity) distribution we generate will be 
# made using the following possible values (using `seq`)
TVALS_MIN <- 0.001 # i.e. don't generate any S smaller than this
TVALS_MAX <- 10    # i.e. don't generate any S larger than this
TVALS_N <- 1000

# -----------------------------------------------------------------------------
# Equation 3 from Roe and Baker (2007) in Science
hT_roebaker <- function(DT, DT0, fbar, sigma) { 
  (1 / (sigma * sqrt(2 * pi)) * DT0 / DT ^ 2 * 
     exp(-1/2 * ((1 - fbar - DT0 / DT) / sigma) ^ 2))
} # hT_roebaker

roebaker <- function(S_params, n_samples) {
  s_mean <- S_params$mean
  s_sd <- S_params$SD
  
  # We start by generating a range of T_vals, temperatures that S could possibly be
  # From Roe & Baker (2007) 
  T_vals <- seq(TVALS_MIN, TVALS_MAX, length.out = TVALS_N)
  
  # Use equation 3 from R&B to generate the corresponding PDF for S over these temperatures
  S_pdf <- hT_roebaker(DT = T_vals, DT0 = 1.2, s_mean, s_sd)  # DT=1.2 C, constant from paper
  
  # Sample from the temperature values according to the PDF
  sample(x = T_vals, n_samples, replace = TRUE, prob = S_pdf)
}

# -----------------------------------------------------------------------------
# Custom S
# In this case, we open the file given in the 'reference' field and draw from there
custom <- function(S_params, n_samples) {
  S_vals <- readr::read_csv(S_params$reference, comment = "#")
  
  sample(unlist(S_vals[1]), n_samples, replace = TRUE)
}


# ==============================================================================
# Main 

openlog(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), sink = TRUE)

printlog("Welcome to", SCRIPTNAME)
printlog("Random seed =", RANDOM_SEED)
set.seed(RANDOM_SEED)

runset_data <- read_csv(RUNSET_FILE, comment = "#", col_types = "cicc")

if(file.exists(file.path(RUNDATA_FILE))) {
  printlog("Removing existing", RUNDATA_FILE)
  file.remove(RUNDATA_FILE)
}

rundata <- list()
for(rs in seq_len(nrow(runset_data))) {
  S_params <- read_csv(runset_data$climate_sens_file[rs], col_types = "cddc", comment = "#")
  n_samples <- runset_data$n_samples[rs]
  
  # Generate as many distributions for S as we have parameters, make `n_samples`
  # random draws from that distribution, and write output files
  
  # S_params has a 'type' field that is used to call the appropriate function
  # e.g. type 'roebaker' or 'custom'
  vardata <- list()
  for(i in seq_len(nrow(S_params))) {
    fntype <- S_params$type[i]
    printlog(SEPARATOR, i, fntype)
    
    fn <- match.fun(fntype)
    S_vals <- fn(S_params[i,], n_samples) %>% round(3)
    
    vardata[[i]] <- data.frame(temperature.S = S_vals, param_set = i)
  }
  rundata[[rs]] <- bind_rows(vardata)
  rundata[[rs]]$run_number <- rs
  rundata[[rs]]$runname_mc <- runset_data$runname_mc[rs]
}

# ------------ Make final data sets, write out, and visualize

printlog(SEPARATOR)
printlog("Collapsing rundata into a single data frame...")
rundata <- bind_rows(rundata)
print_dims(rundata)

printlog("Making rundata...")
S_params %>%
  mutate(param_set = seq_len(nrow(S_params))) %>%
  select(type, mean, SD, param_set) %>%
  left_join(rundata, by = "param_set") ->
  rundata

printlog("Writing", RUNDATA_FILE)
cat("# This file lists the individual Hector runs to be performed for each emissions scenario",
    "# The first two columns (mean, SD) give information about the S distribution used;",
    "# `param_set` gives the relevant line number in the S data file;",
    "# `temperature.S`` is the random draw from that distribution;",
    "# and the last (run_number) a sequentially increasing run number, i.e. line number in runset file.", 
    sep = "\n", file = RUNDATA_FILE)
write_csv(rundata, RUNDATA_FILE, append = TRUE, col_names = TRUE)

rundata$set <- paste0(rundata$type, " (µ=", rundata$mean, ", sd=", rundata$SD, ")")
p <- ggplot(rundata, aes(temperature.S, color = set)) + geom_density()
p <- p + ggtitle(RUNSET_NAME) + xlab("S")
p <- p + scale_color_discrete("")
print(p)
save_plot(paste0(RUNSET_NAME, "_S_value_densities"))

# ------------ Done

printlog(SEPARATOR)
printlog("All done with", SCRIPTNAME)
closelog()

if(PROBLEM) warning("There was a problem - see log")
