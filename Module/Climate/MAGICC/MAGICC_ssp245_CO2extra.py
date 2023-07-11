from pymagicc.scenarios import rcp45
import pymagicc
import scmdata
import pandas as pd
import numpy
import numpy as np
import os

os.chdir('/media/wangtianpeng/Datadisk/wangtianpeng/Downloads/NCC-revise/Module')

sample_num = 10000;

ecs_ar5_sample = pd.read_csv('Sample/ecs_sample.csv')
ecs_ar5_sample = ecs_ar5_sample["x"]

scenario_sample = pd.read_csv('Sample/climate_scenario_sample.csv')
scenario_sample = scenario_sample["x"]

T_base = np.zeros((536,sample_num))
T_extra_CO2 = np.zeros((536,sample_num))
T_extra_CH4 = np.zeros((536,sample_num))
T_extra_N2O = np.zeros((536,sample_num))

C_base = np.zeros((536,sample_num))
C_extra_CO2 = np.zeros((536,sample_num))
C_extra_CH4 = np.zeros((536,sample_num))
C_extra_N2O = np.zeros((536,sample_num))

for i in range(sample_num):
    print(i)
    ssp_path = "Climate/MAGICC/ssp2/MAGICC_ssp245.csv"
    emissions_ssp = pd.read_csv(ssp_path, header=None)
    emissions_ssp = np.array(emissions_ssp)
    rcp45.values[0:161, ] = emissions_ssp[0:161, ]
    rcp45.time_points.values[6] = numpy.datetime64('2010-01-01T00:00:00')
    rcp45.time_points.values[7] = numpy.datetime64('2019-01-01T00:00:00')
    rcp45.time_points.values[8] = numpy.datetime64('2020-01-01T00:00:00')
    rcp45.time_points.values[9] = numpy.datetime64('2021-01-01T00:00:00')

    # add CO2 emissions in 2020 100Gt
    rcp45.values[0,8] = rcp45.values[0,8]+100*12/44
    rcp45.values[138,8] = rcp45.values[138,8]+100*12/44

    # add CH4 emissions in 2020 1Gt
    # rcp45.values[2,8] = rcp45.values[2,8]+1000
    # rcp45.values[140,8] = rcp45.values[140,8]+1000

    # add N2O emissions in 2020 0.1Gt
    # rcp45.values[3,8] = rcp45.values[3,8]+100*28/44
    # rcp45.values[141,8] = rcp45.values[141,8]+100*28/44

    results = []
    results_scen = pymagicc.run(rcp45, CORE_CLIMATESENSITIVITY=ecs_ar5_sample[i], endyear=2300)
    results.append(results_scen)
    results = scmdata.run_append(results)
    temperature_rel_to_1850_1900 = (
        results
            .filter(variable="Surface Temperature", region="World")
            .relative_to_ref_period_mean(year=range(1850, 1900 + 1))
    )

    CO2_concentration = (
        results
            .filter(variable="Atmospheric Concentrations|CO2", region="World")
    )

    T_base[:,i] = temperature_rel_to_1850_1900.values
    C_base[:,i] = CO2_concentration.values

T_base_pd = pd.DataFrame(T_base)
C_base_pd = pd.DataFrame(C_base)

# C_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/C_base.csv')
# T_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/T_base.csv')
C_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/C_extra_CO2.csv')
T_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/T_extra_CO2.csv')
# C_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/C_extra_CH4.csv')
# T_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/T_extra_CH4.csv')
# C_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/C_extra_N2O.csv')
# T_base_pd.to_csv('Climate/MAGICC/outputs/ssp2/T_extra_N2O.csv')
