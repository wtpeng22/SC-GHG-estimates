library(JSON)
library(CSVFiles)
library(DataFrames)

# Process all FAIR Monte Carlo Simulation constrained parameter sets from JSON
# to CSV files to use directly by functions in main_mcs.jl

# Pairings of parameters to csv files names, for reference, also produced by
# `get_fair_mcs_params_map` in `main_mcs.jl`

# Dict(
#     :β_CO                          => "b_aero_CO",
#     :scale_CH₄                     => "scale_CH4",
#     :F_solar                       => "F_solar",
#     :Ψ_CH₄                         => "b_tro3_CH4",
#     :scale_N₂O                     => "scale_N2O",
#     :CO₂_pi                        => "C_pi",
#     :deep_ocean_efficacy           => "deep_ocean_efficacy",
#     :scale_bcsnow                  => "scale_bcsnow",
#     :scale_aerosol_direct_OC       => "scale_aerosol_direct_OC",
#     :b_SOx                         => "ghan_params_SOx",
#     :feedback                      => "ozone_feedback",
#     :scale_O₃                      => "scale_O3",
#     :b_POM                         => "ghan_params_b_POM",
#     :r0_co2                        => "r0",
#     :β_NH3                         => "b_aero_NH3",
#     :lambda_global                 => "lambda_global",
#     :scale_landuse                 => "scale_landuse",
#     :scale_volcanic                => "scale_volcanic",
#     :scale_aerosol_direct_SOx      => "scale_aerosol_direct_SOx",
#     :β_NOx                         => "b_aero_NOx",
#     :Ψ_N₂O                         => "b_tro3_N2O",
#     :ocean_heat_capacity           => "ocean_heat_capacity",
#     :β_OC                          => "b_aero_OC",
#     :scale_solar                   => "scale_solar",
#     :rC_co2                        => "rc",
#     :scale_aerosol_direct_BC       => "scale_aerosol_direct_BC",
#     :scale_CH₄_H₂O                 => "scale_CH4_H2O",
#     :scale_aerosol_indirect        => "scale_aerosol_indirect",
#     :scale_ods                     => "scale_ods",
#     :Ψ_CO                          => "b_tro3_CO",
#     :scale_aerosol_direct_NOx_NH3  => "scale_aerosol_direct_NOx_NH3",
#     :scale_other_ghg               => "scale_other_ghg",
#     :Ψ_NMVOC                       => "b_tro3_NMVOC",
#     :F2x                           => "F2x",
#     :β_SOx                         => "b_aero_SOx",
#     :β_NMVOC                       => "b_aero_NMVOC",
#     :rT_co2                        => "rt",
#     :β_BC                          => "b_aero_BC",
#     :scale_CO₂                     => "scale_CO2",
#     :Ψ_ODS                         => "b_tro3_ODS",
#     :scale_aerosol_direct_CO_NMVOC => "scale_aerosol_direct_CO_NMVOC",
#     :Ψ_NOx                         => "b_tro3_NOx",
#     :ocean_heat_exchange           => "ocean_heat_exchange",
#     :ϕ                             => "ghan_params_Pi"
# )
library(jsonlite)

n <- 2237 # total number of available samples
fair_params <- fromJSON(file.path("..", "..", "data", "FAIR_mcs", "fair-1.6.2-wg3-params.json"))

# Names of minor greenhouse gases and ozone-depleting substances (used or indexing).
other_ghg_names <- c("CF4", "C2F6", "C6F14", "HFC23", "HFC32", "HFC43_10", "HFC125", "HFC134a", "HFC143a", "HFC227ea", "HFC245fa", "SF6")
ods_names <- c("CFC_11", "CFC_12", "CFC_113", "CFC_114", "CFC_115", "CARB_TET", "MCF", "HCFC_22", "HCFC_141B", "HCFC_142B", "HALON1211", "HALON1202", "HALON1301", "HALON2402", "CH3BR", "CH3CL")

# Carbon cycle
for (p in c("r0", "rt", "rc")) {
  df <- data.frame(p = sapply(fair_params, function(x) x[[p]]))
  write.csv(df, file.path("..", "..", "data", "FAIR_mcs", paste0("fair_mcs_params_", p, ".csv")), row.names = FALSE)
}

# Forcing from a doubling of CO₂.
for (p in c("F2x")) {
  df <- data.frame(p = sapply(fair_params, function(x) x[[p]]))
  write.csv(df, file.path("..", "..", "data", "FAIR_mcs", paste0("fair_mcs_params_", p, ".csv")), row.names = FALSE)
}

# Ozone radiative forcing feedback.
for (p in c("ozone_feedback")) {
  df <- data.frame(p = sapply(fair_params, function(x) x[[p]]))
  write.csv(df, file.path("..", "..", "data", "FAIR_mcs", paste0("fair_mcs_params_", p, ".csv")), row.names = FALSE)
}

# Solar radiative forcing.
for (p in c("C_pi")) {
  df <- data.frame(p = sapply(fair_params, function(x) x[[p]][1]))
  write.csv(df, file.path("..", "..", "data", "FAIR_mcs", paste0("fair_mcs_params_", p, ".csv")), row.names = FALSE)
}

# Pre-industrial CO₂ concentration (other concentrations fixed across samples).
for (p in c("F_solar")) {
  arr <- sapply(fair_params, function(x) x[[p]])
  arr <- t(simplify2array(arr)) # 361 years per sample - flatten out from vector of vectors to a matrix
  df <- data.frame(arr)
  colnames(df) <- 1750:2110 # TODO are these the correct years?
  write.csv(df, file.path("..", "..", "data", "FAIR_mcs", paste0("fair_mcs_params_", p, ".csv")), row.names = FALSE)
}

# Temperature component
for (p in c("ocean_heat_exchange", "deep_ocean_efficacy", "lambda_global", "ocean_heat_capacity")) {
  if (p == "ocean_heat_capacity") {
    arr <- sapply(fair_params, function(x) x[[p]])
    arr <- t(arr) # 2 members (deep and mixed) per sample - flatten out from vector of vectors to a matrix
    df <- data.frame(arr)
    colnames(df) <- c("1", "2")
  } else {
    df <- data.frame(p = sapply(fair_params, function(x) x[[p]]))
  }
  write.csv(df, file.path("..", "..", "data", "FAIR_mcs", paste0("fair_mcs_params_", p, ".csv")), row.names = FALSE)
}

# "ghan_params" for aerosol indirect forcing effect.
library(tidyverse)

df_ghan_params_Pi <- data.frame(ghan_params_Pi = map_dbl(fair_params, ~ .x$ghan_params[1]))
write_csv(df_ghan_params_Pi, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_ghan_params_Pi.csv"))

df_ghan_params_SOx <- data.frame(ghan_params_SOx = map_dbl(fair_params, ~ .x$ghan_params[2]))
write_csv(df_ghan_params_SOx, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_ghan_params_SOx.csv"))

df_ghan_params_b_POM <- data.frame(ghan_params_b_POM = map_dbl(fair_params, ~ .x$ghan_params[3]))
write_csv(df_ghan_params_b_POM, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_ghan_params_b_POM.csv"))

# Radiative forcing scaling terms (based on ordering of forcing agents in Python code). - select from a vector of 45 elements
# TODO: :scale_contrails !!! Default FAIR has contrail forcing switched off. But they sample a scaling term. Not including for now.
df_scale_CO2 <- data.frame(scale_CO2 = map_dbl(fair_params, ~ .x$scale[1]))
write_csv(df_scale_CO2, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_CO2.csv"))

df_scale_CH4 <- data.frame(scale_CH4 = map_dbl(fair_params, ~ .x$scale[2]))
write_csv(df_scale_CH4, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_CH4.csv"))

df_scale_N2O <- data.frame(scale_N2O = map_dbl(fair_params, ~ .x$scale[3]))
write_csv(df_scale_N2O, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_N2O.csv"))

df_scale_O3 <- data.frame(scale_O3 = map_dbl(fair_params, ~ .x$scale[32]))
write_csv(df_scale_O3, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_O3.csv"))

df_scale_CH4_H2O <- data.frame(scale_CH4_H2O = map_dbl(fair_params, ~ .x$scale[34]))
write_csv(df_scale_CH4_H2O, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_CH4_H2O.csv"))

df_scale_aerosol_direct_SOx <- data.frame(scale_aerosol_direct_SOx = map_dbl(fair_params, ~ .x$scale[36]))
write_csv(df_scale_aerosol_direct_SOx, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_aerosol_direct_SOx.csv"))

df_scale_aerosol_direct_CO_NMVOC <- data.frame(scale_aerosol_direct_CO_NMVOC = map_dbl(fair_params, ~ .x$scale[37]))
write_csv(df_scale_aerosol_direct_CO_NMVOC, file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_aerosol_direct_CO_NMVOC.csv"))

data_frame(scale_aerosol_direct_NOx_NH3 = sapply(fair_params, function(x) x$scale[38])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_aerosol_direct_NOx_NH3.csv"), row.names = FALSE)

data_frame(scale_aerosol_direct_BC = sapply(fair_params, function(x) x$scale[39])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_aerosol_direct_BC.csv"), row.names = FALSE)

data_frame(scale_aerosol_direct_OC = sapply(fair_params, function(x) x$scale[40])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_aerosol_direct_OC.csv"), row.names = FALSE)

data_frame(scale_aerosol_indirect = sapply(fair_params, function(x) x$scale[41])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_aerosol_indirect.csv"), row.names = FALSE)

data_frame(scale_bcsnow = sapply(fair_params, function(x) x$scale[42])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_bcsnow.csv"), row.names = FALSE)

data_frame(scale_landuse = sapply(fair_params, function(x) x$scale[43])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_landuse.csv"), row.names = FALSE)

data_frame(scale_volcanic = sapply(fair_params, function(x) x$scale[44])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_volcanic.csv"), row.names = FALSE)

data_frame(scale_solar = sapply(fair_params, function(x) x$scale[45])) %>%
  write.csv(file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_scale_solar.csv"), row.names = FALSE)

# 保存其他温室气体和臭氧破坏物的缩放参数
scale_other_ghg <- matrix(unlist(lapply(fair_params, function(p) p$scale[4:15])), ncol=12, byrow=TRUE)
colnames(scale_other_ghg) <- other_ghg_names
scale_other_ghg_df <- as.data.frame(scale_other_ghg)
save(scale_other_ghg_df, file.path(dirname(dirname(dirname(dirname(getwd())))), "data", "FAIR_mcs", "fair_mcs_params_scale_other_ghg.csv"))

scale_ods <- matrix(unlist(lapply(fair_params, function(p) p$scale[16:31])), ncol=16, byrow=TRUE)
colnames(scale_ods) <- ods_names
scale_ods_df <- as.data.frame(scale_ods)
save(scale_ods_df, file.path(dirname(dirname(dirname(dirname(getwd())))), "data", "FAIR_mcs", "fair_mcs_params_scale_ods.csv"))

# Ozone radiative forcing - select from a vector of 6 elements
b_tro3_CH4 <- sapply(fair_params, function(x) x$b_tro3[1])
write.csv(data.frame(b_tro3_CH4), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_tro3_CH4.csv"))

b_tro3_N2O <- sapply(fair_params, function(x) x$b_tro3[2])
write.csv(data.frame(b_tro3_N2O), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_tro3_N2O.csv"))

b_tro3_ODS <- sapply(fair_params, function(x) x$b_tro3[3])
write.csv(data.frame(b_tro3_ODS), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_tro3_ODS.csv"))

b_tro3_CO <- sapply(fair_params, function(x) x$b_tro3[4])
write.csv(data.frame(b_tro3_CO), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_tro3_CO.csv"))

b_tro3_NMVOC <- sapply(fair_params, function(x) x$b_tro3[5])
write.csv(data.frame(b_tro3_NMVOC), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_tro3_NMVOC.csv"))

b_tro3_NOx <- sapply(fair_params, function(x) x$b_tro3[6])
write.csv(data.frame(b_tro3_NOx), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_tro3_NOx.csv"))

# Aerosol direct forcing.
b_aero_SOx <- sapply(fair_params, function(x) x$b_aero[1])
write.csv(data.frame(b_aero_SOx), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_SOx.csv"))

b_aero_CO <- sapply(fair_params, function(x) x$b_aero[2])
write.csv(data.frame(b_aero_CO), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_CO.csv"))

b_aero_NMVOC <- sapply(fair_params, function(x) x$b_aero[3])
write.csv(data.frame(b_aero_NMVOC), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_NMVOC.csv"))

b_aero_NOx <- sapply(fair_params, function(x) x$b_aero[4])
write.csv(data.frame(b_aero_NOx), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_NOx.csv"))

b_aero_BC <- sapply(fair_params, function(x) x$b_aero[5])
write.csv(data.frame(b_aero_BC), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_BC.csv"))

b_aero_OC <- sapply(fair_params, function(x) x$b_aero[6])
write.csv(data.frame(b_aero_OC), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_OC.csv"))

b_aero_NH3 <- sapply(fair_params, function(x) x$b_aero[7])
write.csv(data.frame(b_aero_NH3), file = file.path("..", "..", "data", "FAIR_mcs", "fair_mcs_params_b_aero_NH3.csv"))




