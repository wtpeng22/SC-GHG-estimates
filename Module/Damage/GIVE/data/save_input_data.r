# 1. Read in variables from Delavane

load("From Fran Moore/SCCinputs.rdata")

# 2. Save USG2 socioeconomics pathways

usg2_population = USGscen$USG2$pop_F
write.csv(usg2_population, "FUND params/usg2_population.csv")

usg2_income = USGscen$USG2$ref_gdp_F
write.csv(usg2_income, "FUND params/usg2_income.csv")

# 3. Save DICE temperature and pulsed temperature pathways

dice_temp = USGscen$USG2$climate$temp    # from 2000 to 2300 by tens
write.table(dice_temp, "DICE climate output/dice_temp.csv", sep=",", row.names = FALSE, col.names = FALSE)

dice_temp_pulse = USGscen$USG2$climate$temppulsed
write.table(dice_temp_pulse, "DICE climate output/dice_temp_pulse.csv", sep=",", row.names = FALSE, col.names = FALSE)
