# define constants
years <- seq(2000, 2300, by = 10)
default_horizon <- 2300
pulse_year <- 2020
USD2005to1995 <- 0.819710818
alpha_order <- c("ANZ", "CAM", "CAN", "CEE", "CHI", "FSU", "JPK", "MDE", "NAF", "SAM", "SAS", "SEA", "SIS", "SSA", "USA", "WEU")
alpha_order[c(4, 9, 10)] <- c("EEU", "MAF", "LAM")
fund_regions <- c("USA", "CAN", "WEU", "JPK", "ANZ", "EEU", "FSU", "MDE", "CAM", "LAM", "SAS", "SEA", "CHI", "MAF", "SSA", "SIS")
switch_region_indices <- sapply(fund_regions, function(region) which(alpha_order == region))

# define function to get GTAP data for a given dataframe in FUND regional order
get_gtap_df <- function(gtap) {
  stopifnot(gtap %in% gtaps)
  gtap_dir <- file.path("../data/GTAP DFs")
  gtap_data <- read.csv(file.path(gtap_dir, paste0(gtap, ".csv")))
  gtap_data[, switch_region_indices]
}

# define constant gtap_df_all
gtap_df_all <- array(NA, dim = c(16, 3, 5))
for (i in 1:5) {
  gtap_df_all[,,i] <- t(get_gtap_df(gtaps[i]))
}

# define function linear_interpolate
linear_interpolate <- function(values, original_domain, new_domain) {
  itp <- stats::approx(x = original_domain, y = values, xout = new_domain, method = "linear", rule = 2, ties = "ordered")
  return(itp$y)
}

# TODO: implement quadratic interpolation correctly
# quadratic_interpolate <- function(values, original_domain, new_domain) {
#   itp <- splinefun(original_domain, values, method = "natural")
#   return(itp(new_domain))
# }
