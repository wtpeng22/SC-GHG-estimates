# Common function definitions.
# Most critically, this file easy ggplot and data saving; 
# logged csv[.gz|zip] read/write; and a few other handy things.
INPUT_DIR     <- "data/"
OUTPUT_DIR		<- "outputs/"
RANDOM_SEED		<- 1234
#CHECKPOINTDATE	<- "2015-03-05" # comment out to not use checkpoint
SEPARATOR		<- "-------------------"

# -----------------------------------------------------------------------------
library(luzlogr)  # 0.2.0
library(ggplot2)  # 2.1.0
theme_set(theme_bw())
library(dplyr)    # 0.5.0
library(magrittr) # 1.5
library(tidyr)    # 0.6.0
library(readr)    # 1.0.0

# =============================================================================
# Settings
# We read a file `runset.txt`, the first line of which specifies runset to do
# For runset X, there should be a file `runsets/X.csv`, the lines of which
# specify which Hector emissions file to use, and which file of S distributions
readLines("runset.txt", n = 1) %>% 
  trimws -> 
  RUNSET_NAME

if(target_scenario == "rffsps"){
  RUNSET_NAME = "n20_rffsps"
}

if(target_scenario == "ssp245"){
  RUNSET_NAME = "n20_ssp245"
}

# -----------------------------------------------------------------------------
# Return output directory (perhaps inside a script-specific folder)
# If caller specifies `scriptfolder=FALSE`, return OUTPUT_DIR
# If caller specifies `scriptfolder=TRUE` (default), return OUTPUT_DIR/SCRIPTNAME
outputdir <- function(scriptfolder=TRUE) {
  output_dir <- OUTPUT_DIR
  if(scriptfolder) output_dir <- file.path(output_dir, sub(".R$", "", SCRIPTNAME))
  if(!file.exists(output_dir)) dir.create(output_dir)
  output_dir
} # outputdir

# ---------- settings you probably don't want to change
RUNSET_FILE <- paste0(RUNSET_NAME, ".csv") %>%
  file.path("runsets", .)
RUNDATA_FILE    <- paste0(RUNSET_NAME, "_rundata.csv") %>%
  file.path(outputdir(scriptfolder = FALSE), .)
RUNSET_OUTPUT_DIR <- file.path(OUTPUT_DIR, RUNSET_NAME)

# =============================================================================
# Other support functions

# -----------------------------------------------------------------------------
# Print dimensions of data frame
print_dims <- function(d, dname=deparse(substitute(d))) {
  stopifnot(is.data.frame(d))
  printlog(dname, "rows =", nrow(d), "cols =", ncol(d))
} # print_dims

# -----------------------------------------------------------------------------
# Save a ggplot figure
save_plot <- function(pname, p=last_plot(), ptype=".pdf", scriptfolder=TRUE, ...) {
  fn <- file.path(outputdir(scriptfolder), paste0(pname, ptype))
  printlog("Saving", fn)
  ggsave(fn, p, ...)
} # save_plot

# -----------------------------------------------------------------------------
# Save a data frame
save_data <- function(df, fname=paste0(deparse(substitute(df)), ".csv"), scriptfolder=TRUE, gzip=FALSE, ...) {
  fn <- file.path(outputdir(scriptfolder), fname)
  if(gzip) {
    printlog("Saving", fn, "[gzip]")    
  } else {
    printlog("Saving", fn)    
  }
  readr::write_csv(df, fn, ...)
  
  if(gzip & require(R.utils)) {
    R.utils::gzip(fn, overwrite = TRUE)
  }
} # save_data

# -----------------------------------------------------------------------------
# Open a (possibly compressed) csv file and return data
read_csv <- function(fn, datadir=".", ...) {
  if(is.null(datadir)) {  # NULL signifies absolute path
    fqfn <- fn 
  } else {
    fqfn <- file.path(datadir, fn)      
  }
  printlog("Opening", fqfn)
  invisible(readr::read_csv(fqfn, ...))
} # read_csv

# -----------------------------------------------------------------------------
is_outlier <- function(x, devs=3.2) {
  # See: Davies, P.L. and Gather, U. (1993).
  # "The identification of multiple outliers" (with discussion)
  # J. Amer. Statist. Assoc., 88, 782-801.
  x <- na.omit(x)
  lims <- median(x) + c(-1, 1) * devs * mad(x, constant = 1)
  x < lims[ 1 ] | x > lims[2]
} # is_outlier

# -----------------------------------------------------------------------------
# save a plot to the diagnostic plot folder (used by README.md)
save_diagnostic <- function(p, pname, printit = TRUE, ...) {
  print(p)
  printlog("Saving diagnostic for", pname)
  ggsave(paste0("qc_plots/", pname, ".png"))
  save_plot(pname, ...)
}

# =============================================================================
# Misc code

if(!file.exists(OUTPUT_DIR)) {
  printlog("Creating", OUTPUT_DIR)
  dir.create(OUTPUT_DIR)
}
if(!file.exists(RUNSET_OUTPUT_DIR)) {
  printlog("Creating", RUNSET_OUTPUT_DIR)
  dir.create(RUNSET_OUTPUT_DIR)
}

# -----------------------------------------------------------------------------
#if(exists("CHECKPOINTDATE") & require(checkpoint))
#    try(checkpoint(CHECKPOINTDATE)) # 'try' b/c errors w/o network (issue #171)
