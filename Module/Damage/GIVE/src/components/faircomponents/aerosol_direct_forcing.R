beta_SOx <- NULL              # Sulfur oxides radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
beta_CO <- NULL               # Carbon monoxide radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
beta_NMVOC <- NULL            # Non-methane volatile organic compounds radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
beta_NOx <- NULL              # Nitrogon oxides radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
beta_BC <- NULL               # Black carbon radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
beta_OC <- NULL               # Organic carbon radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
beta_NH3 <- NULL              # Ammonia radiative efficiency: Wm⁻²(Mt yr⁻¹)⁻¹.
# rf_scale_aero_direct <- NULL  # Scaling factor to capture effective radiative forcing uncertainty.
SOx_emiss <- matrix(nrow = length(time),ncol = 1) # Emissions (MtS yr⁻¹).
CO_emiss <- matrix(nrow = length(time),ncol = 1)  # Emissions (MtCO yr⁻¹).
NMVOC_emiss <- matrix(nrow = length(time),ncol = 1) # Emissions (Mt yr⁻¹).
NOx_emiss <- matrix(nrow = length(time),ncol = 1) # Emissions (MtN yr⁻¹).
BC_emiss <- matrix(nrow = length(time),ncol = 1)  # Emissions (Mt yr⁻¹).
OC_emiss <- matrix(nrow = length(time),ncol = 1)  # Emissions (Mt yr⁻¹).
NH3_emiss <- matrix(nrow = length(time),ncol = 1) # Emissions (MtN yr⁻¹).

#Variable
F_SOx_aero <- matrix(nrow = length(time),ncol = 1)  # Sulfur oxides forcing contribution.
F_CO_aero <- matrix(nrow = length(time),ncol = 1)   # Carbon monoxide forcing contribution.
F_NMVOC_aero <- matrix(nrow = length(time),ncol = 1) # Non-methane volatile organic compounds forcing contribution.
F_NOx_aero <- matrix(nrow = length(time),ncol = 1)  # Nitrogon oxides forcing contribution.
F_BC_aero <- matrix(nrow = length(time),ncol = 1)   # Sulfur black carbon forcing contribution.
F_OC_aero <- matrix(nrow = length(time),ncol = 1)   # Organic carbon forcing contribution.
F_NH3_aero <- matrix(nrow = length(time),ncol = 1)  # Ammonia forcing contribution.
F_aerosol_direct <- matrix(nrow = length(time),ncol = 1) # Direct radiative forcing from aerosols (Wm⁻²).

aerosol_direct_forcing_run_timestep <- function(t) {
  # Calculate forcing contributions from individual emissions.
  F_SOx_aero[t] <<- beta_SOx * SOx_emiss[t]
  F_CO_aero[t] <<- beta_CO * CO_emiss[t]
  F_NMVOC_aero[t] <<- beta_NMVOC * NMVOC_emiss[t]
  F_NOx_aero[t] <<- beta_NOx * NOx_emiss[t]
  F_BC_aero[t] <<- beta_BC * BC_emiss[t]
  F_OC_aero[t] <<- beta_OC * OC_emiss[t]
  F_NH3_aero[t] <<- beta_NH3 * NH3_emiss[t]
  
  # Total direct aerosol forcing based on linear relationships between emissions and forcing in Aerocom models.
  # Reference: Myhre et al., 2013: https://www.atmos-chem-phys.net/13/1853/2013
  F_aerosol_direct[t] <<- F_SOx_aero[t] + F_CO_aero[t] + F_NMVOC_aero[t] + F_NOx_aero[t] + F_BC_aero[t] + F_OC_aero[t] + F_NH3_aero[t]
}





