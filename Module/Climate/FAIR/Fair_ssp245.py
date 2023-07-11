import fair
import numpy as np
import pandas as pd
import copy
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
    emissions_ssp245 = pd.read_csv("Climate/FAIR/ssp2/FAIR_ssp245.csv")
    emissions_ssp245 = np.array(emissions_ssp245)
    emissions_ssp245 = emissions_ssp245[3:emissions_ssp245.shape[0], :]
    emissions_ssp245 = np.array(emissions_ssp245, dtype=np.float32)
    emissions_ssp245 = np.delete(emissions_ssp245, 6, 1)

    emissions_ssp245_base = copy.copy(emissions_ssp245)

    C, _, T_base[:,i] = fair.forward.fair_scm(
        emissions=emissions_ssp245_base,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_base[:, i] = C[:,0]

    emissions_ssp245_extraCO2 = copy.copy(emissions_ssp245_base)
    emissions_ssp245_extraCO2[2020-1765,1] = emissions_ssp245_base[2020-1765,1]+100*12/44

    C, _, T_extra_CO2[:,i] = fair.forward.fair_scm(
        emissions=emissions_ssp245_extraCO2,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_extra_CO2[:, i] = C[:, 0]

    emissions_ssp245_extraCH4 = copy.copy(emissions_ssp245_base)
    emissions_ssp245_extraCH4[2020-1765,3] = emissions_ssp245_base[2020-1765,3]+1000

    C, _, T_extra_CH4[:,i] = fair.forward.fair_scm(
        emissions=emissions_ssp245_extraCH4,
        tcrecs=np.array([max(1,min(tcr[i],2.5)), ecs[i]]))
    C_extra_CH4[:, i] = C[:, 0]

    emissions_ssp245_extraN2O = copy.copy(emissions_ssp245_base)
    emissions_ssp245_extraN2O[2020-1765,4] = emissions_ssp245_base[2020-1765,4]+100*28/44

    C, _, T_extra_N2O[:,i] = fair.forward.fair_scm(
        emissions=emissions_ssp245_extraN2O,
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

C_base_pd.to_csv('Climate/Fair/outputs/ssp245/C_base.csv')
T_base_pd.to_csv('Climate/Fair/outputs/ssp245/T_base.csv')
C_extra_CO2_pd.to_csv('Climate/Fair/outputs/ssp245/C_extra_CO2.csv')
C_extra_CH4_pd.to_csv('Climate/Fair/outputs/ssp245/C_extra_CH4.csv')
C_extra_N2O_pd.to_csv('Climate/Fair/outputs/ssp245/C_extra_N2O.csv')
T_extra_CO2_pd.to_csv('Climate/Fair/outputs/ssp245/T_extra_CO2.csv')
T_extra_CH4_pd.to_csv('Climate/Fair/outputs/ssp245/T_extra_CH4.csv')
T_extra_N2O_pd.to_csv('Climate/Fair/outputs/ssp245/T_extra_N2O.csv')
