# Add IPEDS variables (Retention Rates and Institutional Characteristics) to Analytic Dataset
# Last Updated: 1/31/2023

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
         SECTOR, ICLEVEL, CONTROL, HLOFFER, UGOFFER, GROFFER, INSTCAT, 
         LANDGRNT, INSTSIZE, LOCALE, POSTSEC, PSEFLAG, PSET4FLG, LONGITUD, LATITUDE)

# Ingest Current Texas Dataframe
Texas_HEERF <- read_csv("Out/Texas_HEI_HEERF.csv") %>%
  rename(UNITID = unitid)

# Merge Institutional Characteristics and Retention Vars on to Current TX dataframe
Final_TX_HEERF <- Texas_HEERF %>%
  left_join(Inst_Characteristics, by = "UNITID") %>%
  left_join(EF_Retention, by = "UNITID")

# Write Final Dataframe to /Out



