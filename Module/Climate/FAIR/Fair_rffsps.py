import fair
import numpy as np
import pandas as pd
import copy
import numpy
from scipy import stats
from fair.tools.ensemble import tcrecs_generate
from fair.forward import fair_scm
import os
from scipy.stats import truncnorm

os.chdir('/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module')

sample_num = 10000
target_year = 2020

ecs_ar5_sample = pd.read_csv('Sample/ecs_sample.csv')
ecs_ar5_sample = ecs_ar5_sample["x"]

scenario_sample = pd.read_csv('Sample/climate_scenario_sample.csv')
scenario_sample = scenario_sample["x"]

ecs = np.array(ecs_ar5_sample)
rwf = truncnorm.rvs(-3, 3, loc=0.6, scale=0.1, size=10000, random_state=301)
tcr = rwf*ecs

rffsp_co2_emissions = pd.read_csv("rffsps_v5/emissions/rffsp_co2_emissions.csv")
rffsp_ch4_emissions = pd.read_csv("rffsps_v5/emissions/rffsp_ch4_emissions.csv")
rffsp_n2o_emissions = pd.read_csv("rffsps_v5/emissions/rffsp_n2o_emissions.csv")

T_base = np.zeros((736,sample_num))
T_extra_CO2 = np.zeros((736,sample_num))
T_extra_CH4 = np.zeros((736,sample_num))
T_extra_N2O = np.zeros((736,sample_num))

C_base = np.zeros((736,sample_num))
C_extra_CO2 = np.zeros((736,sample_num))
C_extra_CH4 = np.zeros((736,sample_num))
C_extra_N2O = np.zeros((736,sample_num))

for i in range(sample_num):
    print(i)
    scenario_sample_target = scenario_sample[i]
    emissions_rffsps = pd.read_csv("Climate/FAIR/rffsps/FAIR_rffsps_template.csv")
    emissions_rffsps = np.array(emissions_rffsps)
    emissions_rffsps = emissions_rffsps[3:emissions_rffsps.shape[0], :]
    emissions_rffsps = np.array(emissions_rffsps, dtype=np.float32)
    emissions_rffsps = np.delete(emissions_rffsps, 6, 1)

    emissions_rffsps_base = copy.copy(emissions_rffsps)
    emissions_rffsps_base[range(target_year-1765,2300-1765+1),1] = rffsp_co2_emissions.loc[(rffsp_co2_emissions['sample'] == scenario_sample_target)]["value"] - emissions_rffsps[range(target_year-1765,2300-1765+1),2]
    emissions_rffsps_base[range(target_year-1765,2300-1765+1),3] = rffsp_ch4_emissions.loc[(rffsp_co2_emissions['sample'] == scenario_sample_target)]["value"]
    emissions_rffsps_base[range(target_year-1765,2300-1765+1),4] = rffsp_n2o_emissions.loc[(rffsp_co2_emissions['sample'] == scenario_sample_target)]["value"]

    C, _, T_base[:,i] = fair.forward.fair_scm(
        emissions=emissions_rffsps_base,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_base[:, i] = C[:,0]

    emissions_rffsps_extraCO2 = copy.copy(emissions_rffsps_base)
    emissions_rffsps_extraCO2[2020-1765,1] = emissions_rffsps_base[2020-1765,1]+100*12/44

    C, _, T_extra_CO2[:,i] = fair.forward.fair_scm(
        emissions=emissions_rffsps_extraCO2,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_extra_CO2[:, i] = C[:, 0]

    emissions_rffsps_extraCH4 = copy.copy(emissions_rffsps_base)
    emissions_rffsps_extraCH4[2020-1765,3] = emissions_rffsps_base[2020-1765,3]+1000

    C, _, T_extra_CH4[:,i] = fair.forward.fair_scm(
        emissions=emissions_rffsps_extraCH4,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_extra_CH4[:, i] = C[:, 0]

    emissions_rffsps_extraN2O = copy.copy(emissions_rffsps_base)
    emissions_rffsps_extraN2O[2020-1765,4] = emissions_rffsps_base[2020-1765,4]+100*28/44

    C, _, T_extra_N2O[:,i] = fair.forward.fair_scm(
        emissions=emissions_rffsps_extraN2O,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_extra_N2O[:, i] = C[:, 0]

C_base_pd = pd.DataFrame(C_base[range(0,(2300-1765+1)),])
T_base_pd = pd.DataFrame(T_base[range(0,(2300-1765+1)),])
C_extra_CO2_pd = pd.DataFrame(C_extra_CO2[range(0,(2300-1765+1)),])
C_extra_CH4_pd = pd.DataFrame(C_extra_CH4[range(0,(2300-1765+1)),])
C_extra_N2O_pd = pd.DataFrame(C_extra_N2O[range(0,(2300-1765+1)),])
T_extra_CO2_pd = pd.DataFrame(T_extra_CO2[range(0,(2300-1765+1)),])
T_extra_CH4_pd = pd.DataFrame(T_extra_CH4[range(0,(2300-1765+1)),])
T_extra_N2O_pd = pd.DataFrame(T_extra_N2O[range(0,(2300-1765+1)),])


C_base_pd.to_csv('Climate/FAIR/outputs/rffsps/C_base.csv')
T_base_pd.to_csv('Climate/FAIR/outputs/rffsps/T_base.csv')
C_extra_CO2_pd.to_csv('Climate/FAIR/outputs/rffsps/C_extra_CO2.csv')
C_extra_CH4_pd.to_csv('Climate/FAIR/outputs/rffsps/C_extra_CH4.csv')
C_extra_N2O_pd.to_csv('Climate/FAIR/outputs/rffsps/C_extra_N2O.csv')
T_extra_CO2_pd.to_csv('Climate/FAIR/outputs/rffsps/T_extra_CO2.csv')
T_extra_CH4_pd.to_csv('Climate/FAIR/outputs/rffsps/T_extra_CH4.csv')
T_extra_N2O_pd.to_csv('Climate/FAIR/outputs/rffsps/T_extra_N2O.csv')
