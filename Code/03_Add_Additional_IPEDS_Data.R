# Add IPEDS variables (Retention Rates and Institutional Characteristics) to Analytic Dataset
# Last Updated: 1/31/2023

library(tidyverse)

# ------------------------------------------------------------------------------
#  Ingest Data
# ------------------------------------------------------------------------------
  # Institutional Characteristics and Select Relevant Vars
  HD2020 <- read_csv("Raw Data/hd2020.csv")

  # Retention (Fall Enrollment) Dataframe
  EF_Retention <- read_csv("Raw Data/IPEDS_Retention_2017_2021.csv")

# ------------------------------------------------------------------------------
#  Clean Data
# ------------------------------------------------------------------------------

# Clean Retention (Fall Enrollment) Dataframe 
EF_Retention <- EF_Retention %>%
  transmute(Year, UNITID,
         New_UG = UGENTERN,
         New_Cohort = GRCOHRT,
         FT_Previous_Cohort = RRFTCT,
         FT_Previous_Cohort_Adj = RRFTCTA,
         FT_Retention_N = RET_NMF,
         FT_Retention_Rate = RET_PCF,
         PT_Previous_Cohort = RRPTCT,
         PT_Previous_Cohort_Adj = RRPTCTA,
         PT_Retention_N = RET_NMP,
         PT_Retention_Rate = RET_PCP
         )

EF_Retention <- EF_Retention %>% 
  group_by(UNITID) %>% 
  mutate(Count = Year) %>% 
  gather("Year", "New_UG", "New_Cohort", "FT_Previous_Cohort", "FT_Previous_Cohort_Adj", 
         "FT_Retention_N", "FT_Retention_Rate", "PT_Previous_Cohort", "PT_Previous_Cohort_Adj",
         "PT_Retention_N", "PT_Retention_Rate", key = variable, value = number) %>% 
  unite(combi, variable, Count) %>% 
  spread(combi, number) %>%
  select(-starts_with("Year_"))

# Select Relevant Institutional Characteristics
 
Inst_Characteristics <- HD2020 %>%
  select(UNITID, INSTNM, ADDR, CITY, STABBR, ZIP, FIPS, OPEID, 
         SECTOR, ICLEVEL, CONTROL, INSTCAT, 
         LANDGRNT, INSTSIZE, LOCALE, POSTSEC, LONGITUD, LATITUDE) %>%
  mutate(SECTOR = case_when(SECTOR == 0 ~ "Administrative Unit",
                            SECTOR == 1 ~ "Public, 4-year or above",
                            SECTOR == 2 ~ "Private not-for-profit, 4-year or above",
                            SECTOR == 3 ~ "Private for-profit, 4-year or above",
                            SECTOR == 4 ~ "Public, 2-year",
                            SECTOR == 5 ~ "Private not-for-profit, 2-year",
                            SECTOR == 6 ~ "Private for-profit, 2-year",
                            SECTOR == 7 ~ "Public, less-than 2-year",
                            SECTOR == 8 ~ "Private not-for-profit, less-than 2-year",
                            SECTOR == 9 ~ "Private for-profit, less-than 2-year",
                            SECTOR == 99 ~ "Sector unknown (not active)",
                            TRUE ~ NA_character_),
         ICLEVEL = case_when(ICLEVEL == 1 ~ "Four or more years",
                             ICLEVEL == 2 ~ "At least 2 but less than 4 years",
                             ICLEVEL == 3 ~ "Less than 2 years (below associate)",
                             ICLEVEL == -3 ~ "Not Available",
                             TRUE ~ NA_character_),
         CONTROL = case_when(CONTROL == 1 ~ "Public",
                             CONTROL == 2 ~ "Private not-for-profit",
                             CONTROL == 3 ~ "Private for-profit",
                             CONTROL == -3 ~ "Not Available",
                             TRUE ~ NA_character_),
         INSTCAT = case_when(INSTCAT == 1 ~ "Degree-granting, graduate with no undergraduate degrees",
                             INSTCAT == 2 ~ "Degree-granting, primarily baccalaureate or above",
                             INSTCAT == 3 ~ "Degree-granting, not primarily baccalaureate or above",
                             INSTCAT == 4 ~ "Degree-granting, associate's and certificates",
                             INSTCAT == 5 ~ "Nondegree-granting, above the baccalaureate",
                             INSTCAT == 6 ~ "Nondegree-granting, sub-baccalaureate",
                             INSTCAT == -1 ~ "Not reported",
                             INSTCAT == -2 ~ "Not applicable",
                             TRUE ~ NA_character_),
         LANDGRNT = case_when(LANDGRNT == 1 ~ "Land Grant Institution",
                              LANDGRNT == 2 ~ "Not a Land Grant Institution",
                              TRUE ~ NA_character_),
         INSTSIZE = case_when(INSTSIZE == 1 ~ "Under 1,000",
                              INSTSIZE == 2 ~ "1,000 - 4,999",
                              INSTSIZE == 3 ~ "5,000 - 9,999",
                              INSTSIZE == 4 ~ "10,000 - 19,999",
                              INSTSIZE == 5 ~ "20,000 and above",
                              INSTSIZE == -1 ~ "Not reported",
                              INSTSIZE == -2 ~ "Not applicable",
                              TRUE ~ NA_character_),
         LOCALE = case_when(LOCALE == 11 ~ "City: Large",
                            LOCALE == 12 ~ "City: Midsize",
                            LOCALE == 13 ~ "City: Small",
                            LOCALE == 21 ~ "Suburb: Large",
                            LOCALE == 22 ~ "Suburb: Midsize",
                            LOCALE == 23 ~ "Suburb: Small",
                            LOCALE == 31 ~ "Town: Fringe",
                            LOCALE == 32 ~ "Town: Distant",
                            LOCALE == 33 ~ "Town: Remote",
                            LOCALE == 41 ~ "Rural: Fringe",
                            LOCALE == 42 ~ "Rural: Distant",
                            LOCALE == 43 ~ "Rural: Remote",
                            LOCALE == -3 ~ "Not available",
                            TRUE ~ NA_character_))

# Ingest Current Texas Dataframe
Texas_HEERF_2020 <- read_csv("Out/Texas_HEI_HEERF.csv") %>%
  rename(UNITID = unitid)

# Merge Institutional Characteristics and Retention Vars on to Current TX dataframe
Final_TX_HEERF_2020 <- Texas_HEERF_2020 %>%
  left_join(Inst_Characteristics, by = "UNITID") %>%
  left_join(EF_Retention, by = "UNITID")

# Write Final Dataframe to /Out
write_csv(Final_TX_HEERF_2020, "Out/Texas_2020_HEERF_IC.csv")


