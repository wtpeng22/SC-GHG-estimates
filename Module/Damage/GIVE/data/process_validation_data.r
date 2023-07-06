library(dplyr)
library(tidyr)

# 1. Load output data from Fran Moore

load("From Fran Moore/AGincDam.RDat")

# 2. Get just the Agriculture results
validation.df = incDam %>%
  filter(sector == "Agriculture") %>%
  filter(model != "FUNDAg") %>%
  filter(IAWGscenario == "deterministic USG2 (incremental)") %>%
  spread(region, value)
write.csv(validation.df, "validation/regions_agriculture.csv", row.names=FALSE)

# Sum to agricultural scc values
scc.df = incDam %>%
  filter(sector == "Agriculture") %>%
  filter(model != "FUNDAg") %>%
  filter(IAWGscenario == "deterministic USG2 (incremental)") %>%
  group_by(model, discountrate) %>%
  summarise(scc = sum(value))
write.csv(scc.df, "validation/ag_scc.csv", row.names=FALSE)
