setwd("~/Dropbox/EdTrust/HEERF_Git")
library(tidyverse)
library(readxl)

# ------------------------------------------------------------------------------
# Step 1: Download data and DED from https://covid-relief-data.ed.gov/
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 2: Review DED to familiarize yourself with the type of data available
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 3: Ingest 2020 HEERF Raw Data
# ------------------------------------------------------------------------------

HEERF_2021 <- read_excel("Raw Data/2021 ESF Data Files/heer-2021-public-2023-02.xlsx",
                    sheet= "Prime")

Institutional_Information_2021 <- read_excel("Raw Data/2021 ESF Data Files/heer-2021-public-2023-02.xlsx",
                                        sheet= "Enriched")
MSI <- read_csv("Raw Data/MSI.csv", 
                col_types = cols(HBCU = col_double(), 
                                 PBI = col_double(), 
                                 ANNHI = col_double(), 
                                 TRIBAL = col_double(), 
                                 AANAPII = col_double(), 
                                 HSI = col_double(), 
                                 NANTI = col_double())) 

Texas_2021_HEERF <- HEERF_2021 %>%
  filter(state == "TEXAS") %>%
  left_join(Institutional_Information_2021, by = c("dunsNumber"="duns"))

# I(nstitutions with no duns)
x <- Texas_2021_HEERF %>%
  filter(is.na(instnm))
