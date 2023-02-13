# Download MSI Flags from Scorecard
setwd("~/Dropbox/EdTrust")

library(tidyverse)

# ingest raw scorecard data
scorecard <- read_csv("Scorecard-Latest-Institution.csv")

# select only relevant variables for MSI flags
MSI <- scorecard %>%
  select(INSTNM, OPEID, UNITID, HBCU, PBI, ANNHI, TRIBAL, AANAPII, HSI, NANTI)

# write as MSI.csv
write_csv(MSI, "HEERF_Git/Raw Data/MSI.csv")
