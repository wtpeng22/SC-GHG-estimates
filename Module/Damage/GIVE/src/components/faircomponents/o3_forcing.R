# --------------------------------------------------
# Ozone Radiative Forcing
# --------------------------------------------------

# Note from original Python code: "Calculates total ozone forcing from precursor emissions and
# concentrations based on AerChemMIP and CMIP6 Historical behaviour [Skeie et al. (2020) &
# Thornhill et al. (2021)]. Unlike Stevenson CMIP5 no distinction is made for tropospheric and
# stratospheric. With this formulation, ozone forcing depends on concentrations of CH4, N2O,
# ozone-depleting halogens, and emissions of CO, NVMOC and NOx.

# ozone_depleting_substances <- Index() # Index for ozone-depleting substances.

Br <- matrix(nrow = length(ozone_depleting_substances),ncol = 1) # Number of bromine atoms in each ozone-depleting species.
Cl <- matrix(nrow = length(ozone_depleting_substances),ncol = 1) # Number of chlorine atoms in each ozone-depleting species.
FC <- matrix(nrow = length(ozone_depleting_substances),ncol = 1) # Fractional stratospheric release for ozone-depleting substances (Reference: Daniel, J. and Velders, G.: A focus on information and options for policymakers, in: Scientific Assessment of Ozone Depletion, WMO, 2011).

ods_pi <- matrix(nrow = length(ozone_depleting_substances),ncol = 1) # Pre-industrial concentrations for ozone_depleting_substances (ppt).
conc_ods <- matrix(nrow = length(time), ncol = length(ozone_depleting_substances)) # Atmospheric concentrations of ozone_depleting_substances (ppt).

CH4 <- matrix(nrow = length(time),ncol = 1) # Atmospheric methane concentration (ppb).
N2O <- matrix(nrow = length(time),ncol = 1) # Atmospheric nitrous oxide concentration (ppb).

CO2 <- matrix(nrow = length(time),ncol = 1) # Atmospheric carbon dioxide concentration (ppm).
CH4_pi <- NULL # Pre-industrial atmospheric methane concentration (ppb).
N2O_pi <- NULL # Pre-industrial atmospheric nitrous oxide concentration (ppb).

CO2_pi <- NULL # Pre-industrial atmospheric carbon dioxide concentration (ppm).
total_forcing_O3_0 <- NULL # Initial condition for ozone radiative forcing in first model period.

NOx_emiss_pi <- NULL # Pre-industrial nitrogen oxides emissions (MtN yr⁻¹).
CO_emiss_pi <- NULL # Pre-industrial carbon monoxide emissions (MtCO yr⁻¹).
NMVOC_emiss_pi <- NULL # Pre-industrial global non-methane volatile organic compounds emissions (Mt yr⁻¹).
NOx_emiss <- matrix(nrow = length(time),ncol = 1) # Nitrogen oxides emissions (MtN yr⁻¹).
CO_emiss <- matrix(nrow = length(time),ncol = 1) # Carbon monoxide emissions (MtCO yr⁻¹).
NMVOC_emiss <- matrix(nrow = length(time),ncol = 1) # Global non-methane volatile organic compounds emissions (Mt yr⁻¹).

temperature <- matrix(nrow = length(time),ncol = 1) # Global mean surface temperature anomaly (K).
feedback <- NULL # Temperature feedback on ozone radiative forcing (W m⁻²K⁻¹).

psi_CH4 <- NULL # Radiative efficiency coefficient for methane concentrations (W m⁻²ppb⁻¹).
psi_N2O <- NULL # Radiative efficiency coefficient for nitrous_oxide concentrations (W m⁻²ppb⁻¹).
psi_ODS <- NULL # Radiative efficiency coefficient for ozone-depleting substance concentrations in EESC (W m⁻² ppt⁻¹).
psi_CO <- NULL # Radiative efficiency coefficient for carbon monoxide emissions (W m⁻² (Mt yr⁻¹)⁻¹).
psi_NMVOC <- NULL # Radiative efficiency coefficient for non-methane volatile organic compound emissions (Wm⁻²(Mt yr⁻¹)⁻¹).
psi_NOx <- NULL # Radiative efficiency coefficient for nitrogen oxides emissions (Wm⁻²(MtN yr⁻¹)⁻¹).

F_CH4 <- matrix(nrow = length(time),ncol = 1) # Radiative forcing contribution from methane (Wm⁻²).
F_N2O <- matrix(nrow = length(time),ncol = 1) # Radiative forcing contribution from nitrous_oxide (Wm⁻²).
F_ODS <- matrix(nrow = length(time),ncol = 1) # Radiative forcing contribution from ozone-depleting substances (Wm⁻²).
F_CO <- matrix(nrow = length(time),ncol = 1) # Radiative forcing contribution from carbon monoxide (Wm⁻²).
F_NMVOC <- matrix(nrow = length(time),ncol = 1) # Radiative forcing contribution from non-methane volatile organic compounds (Wm⁻²).
F_NOx <- matrix(nrow = length(time),ncol = 1) # Radiative forcing contribution from nitrogen oxides (Wm⁻²).
int_eecs <- matrix(nrow = length(time), ncol = length(ozone_depleting_substances)) # Intermediate terms to calculate equivalent effective stratospheric chlorine (EESC) for ozone-depleting substances (just for convenience).
eecs <- matrix(nrow = length(time),ncol = 1) # Equivalent effective stratospheric chlorine (EESC) for ozone-depleting substances.
total_forcing_O3 <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from stratospheric ozone (Wm⁻²).

o3_forcing_run_timestep <- function(t) {
  if (t==1) {
    total_forcing_O3[t] <<- total_forcing_O3_0
  } else {
    for (g in 1:length(ozone_depleting_substances)) {
      # Calculate intermediate variables for EECS (calculations done for each individual ozone-depleting substance).
      int_eecs[t, g] <<- Cl[g] * (conc_ods[t, g] - ods_pi[g]) * (FC[g] / FC[1]) + 45 * Br[g] * (conc_ods[t, g] - ods_pi[g]) * (FC[g] / FC[1])
    }
    
    # Calculate equivalent effective stratospheric chlorine.
    eecs[t] <<- max((FC[1] * sum(int_eecs[t, ])), 0.0)
    
    # Calculate forcing contributions from individual agents.
    F_ODS[t] <<- psi_ODS * eecs[t]
    F_CH4[t] <<- psi_CH4 * (CH4[t] - CH4_pi)
    F_N2O[t] <<- psi_N2O * (N2O[t] - N2O_pi)
    #F_CO2[t] <<- psi_CO2 * (CO2[t] - CO2_pi)
    F_CO[t] <<- psi_CO * (CO_emiss[t] - CO_emiss_pi)
    F_NMVOC[t] <<- psi_NMVOC * (NMVOC_emiss[t] - NMVOC_emiss_pi)
    F_NOx[t] <<- psi_NOx * (NOx_emiss[t] - NOx_emiss_pi)
    
    # Calculate total ozone radiative forcing with (potential) temperature feedback.
    total_forcing_O3[t] <<- F_ODS[t] + F_CH4[t] + F_N2O[t] + F_CO[t] + F_NMVOC[t] + F_NOx[t] + feedback * temperature[t - 1]
  }
}

