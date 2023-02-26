setwd("~/Dropbox/EdTrust/HEERF_Git")
library(tidyverse)
library(readxl)

# ------------------------------------------------------------------------------
# Ingest Data: 2021 HEERF, 2021 Additional Inst Info, MSI Flags from Scorecard
# ------------------------------------------------------------------------------

HEERF_2021 <- read_excel("Raw Data/2021 ESF Data Files/heer-2021-public-2023-02.xlsx",
                    sheet= "Prime")

Institutional_Information_2021 <- read_excel("Raw Data/2021 ESF Data Files/heer-2021-public-2023-02.xlsx",
                                        sheet= "Enriched")
MSI <- read_csv("Raw Data/MSI.csv", 
                col_types = cols(UNITID = col_character(),
                                 HBCU = col_double(), 
                                 PBI = col_double(), 
                                 ANNHI = col_double(), 
                                 TRIBAL = col_double(), 
                                 AANAPII = col_double(), 
                                 HSI = col_double(), 
                                 NANTI = col_double())) 

Texas_2021_HEERF <- HEERF_2021 %>%
  select(iheName,
         opeIds,
         dunsNumber,
         unitIds,
         state,
         awardedAmountsStudentAid,
         awardedAmountsInstitutional,
         awardedAmountsHbcu,
         awardedAmountsTccu,
         awardedAmountsMsi,
         awardedAmountsSip,
         awardedAmountsSaihe,
         awardedAmountsFipse,
         awardedAmountsProprietary,
         awards,
         ugFtPellEnrolled,
         ugFtNonPellEnrolled,
         ugPtPellEnrolled,
         ugPtNonPellEnrolled,
         gFtEnrolled,
         gPtEnrolled,
         allEnrolledStudents,
         ugFtPellHeerRecipients,
         ugFtNonPellHeerRecipients,
         ugPtPellHeerRecipients,
         gFtHeerRecipients,
         gPtHeerRecipients,
         allStudentsHeerRecipients,
         a1InstitutionalDirectTotal,
         a2DirectUgFtPell,
         a2DirectUgFtNonPell,
         a2DirectUgPtPell,
         a2DirectUgPtNonPell,
         a2DirectGFt,
         a2DirectGPt,
         a2DirectTotal,
         a3DirectUgFtPell,
         a3DirectUgFtNonPell,
         a3DirectUgPtPell,
         a3DirectUgPtNonPell,
         a3DirectGFt,
         a3DirectGPt,
         a3DirectTotal,
         a4DirectUgFtPell,
         a4DirectUgFtNonPell,
         a4DirectUgPtPell,
         a4DirectUgPtNonPell,
         a4DirectGFt,
         a4DirectGPt,
         a4DirectTotal,
         minimumAwardUgFtPell,
         minimumAwardUgFtNonPell,
         minimumAwardUgPtPell,
         minimumAwardUgPtNonPell,
         minimumAwardGFt,
         minimumAwardGPt,
         minimumAward,
         maximumAwardUgFtPell,
         maximumAwardUgFtNonPell,
         maximumAwardUgPtPell,
         maximumAwardUgPtNonPell,
         maximumAwardGFt,
         maximumAwardGPt,
         maximumAward,
         ugFtPellGrantAmountTotal,
         ugFtNonPellGrantAmountTotal,
         ugPtPellGrantAmountTotal,
         ugPtNonPellGrantAmountTotal,
         gFtGrantAmountTotal,
         gPtGrantAmountTotal,
         allStudentsGrantAmountTotal,
         ugFtPellAverageHeerAmount,
         ugFtNonPellAverageHeerAmount,
         ugPtPellAverageHeerAmount,
         ugPtNonPellAverageHeerAmount,
         gFtAverageHeerAmount,
         gPtAverageHeerAmount,
         allStudentsAverageHeerAmount,
         nEnrolledStudentsNotEligibleTitleIV,
         pEnrolledStudentsNotEligibleTitleIV,
         nStudentsReceivedGrantsNotEligibleTitleIV,
         pStudentsGrantedHeerNotEligibleTitleIV,
         aiEnrolledStudentCount,
         aiReceivedAidGrant,
         aiTotalAmountOfGrants,
         aiAverageHeerfAward,
         aEnrolledStudentCount,
         aReceivedAidGrant,
         aTotalAmountOfGrants,
         aAverageHeerfAward,
         bEnrolledStudentCount,
         bReceivedAidGrant,
         bTotalAmountOfGrants,
         bAverageHeerfAward,
         hEnrolledStudentCount,
         hReceivedAidGrant,
         hTotalAmountOfGrants,
         hAverageHeerfAward,
         nhEnrolledStudentCount,
         nhReceivedAidGrant,
         nhTotalAmountOfGrants,
         nhAverageHeerfAward,
         wEnrolledStudentCount,
         wReceivedAidGrant,
         wTotalAmountOfGrants,
         wAverageHeerfAward,
         tmrEnrolledStudentCount,
         tmrReceivedAidGrant,
         tmrTotalAmountOfGrants,
         tmrAverageHeerfAward,
         urEnrolledStudentCount,
         urReceivedAidGrant,
         urTotalAmountOfGrants,
         urAverageHeerfAward,
         nrEnrolledStudentCount,
         nrReceivedAidGrant,
         nrTotalAmountOfGrants,
         nrAverageHeerfAward,
         ncrEnrolledStudentCount,
         ncrReceivedAidGrant,
         ncrTotalAmountOfGrants,
         ncrAverageHeerfAward,
         mEnrolledStudentCount,
         mReceivedAidGrant,
         mTotalAmountOfGrants,
         mAverageHeerfAward,
         woEnrolledStudentCount,
         woReceivedAidGrant,
         woTotalAmountOfGrants,
         woAverageHeerfAward,
         ncgEnrolledStudentCount,
         ncgReceivedAidGrant,
         ncgTotalAmountOfGrants,
         ncgAverageHeerfAward,
         tfoEnrolledStudentCount,
         tfoReceivedAidGrant,
         tfoTotalAmountOfGrants,
         tfoAverageHeerfAward,
         tfyEnrolledStudentCount,
         tfyReceivedAidGrant,
         tfyTotalAmountOfGrants,
         tfyAverageHeerfAward,
         enroll2021Ug,
         enroll2021UgW,
         enroll2021G,
         enroll2021GW,
         enroll2020Ug,
         enroll2020UgW,
         enroll2020G,
         enroll2020GW,
         enroll2019Ug,
         enroll2019UgW,
         enroll2019UgC,
         enroll2019UgSe,
         enroll2019G,
         enroll2019GW) %>%
  filter(state == "TEXAS") %>%
  left_join(Institutional_Information_2021, by = c("dunsNumber"="duns")) %>%
  left_join(MSI, by = c("unitid" = "UNITID")) %>% # unitid is from Inst Information 2021
  # *****!!!! remove 3 institutions with no match on duns (261 -> 258)
  filter(!is.na(instnm)) %>%
  # exclude institutions with data issues -- none, use allEnrolledStudents, not ipeds numIheStudents (not 12 month)
  mutate(distributed_to_more_than_enrolled = ifelse(allEnrolledStudents * 1.10 < allStudentsHeerRecipients, 1, 0)) %>%
  select(unitid, opeid, instnm, stabbr, iheType, iheControl, pctNative, pctAsian, pctBlack, pctHispanic, # Inst Information 2021
         pctIslander, pctWhite, pctMultRaces, pctFullTime, pctPartTime, pctPell, hbcu, pbi, annhi, # Inst Information 2021
         tribal, aanapii, hsi, nanti,  ## Inst Information 2021
         HBCU, PBI, ANNHI, TRIBAL, AANAPII, HSI, NANTI, # from scorecard
        state, contains("awardedAmounts"), awards, ends_with("PellEnrolled"), ends_with("Enrolled"),
        ends_with("HeerRecipients"), starts_with("a1"), starts_with("a2"), starts_with("a3"),
        starts_with("a4"), starts_with("minimum"), starts_with("maximum"), ends_with("GrantAmountTotal"),
        ends_with("AverageHeerAmount"), ends_with("EligibleTitleIV"), ends_with("EnrolledStudentCount"),
        ends_with("ReceivedAidGrant"), ends_with("TotalAmountOfGrants"), ends_with("AverageHeerfAward"),
        starts_with("enroll2021"), starts_with("enroll2020"), starts_with("enroll2019")) %>%
  # in awardedAmounts, force NA to 0
  mutate(across(starts_with("awardedAmounts"), ~replace(., is.na(.), 0)))
        # across(ends_with("HeerRecipients"), ~replace(., is.na(.), 0)))




# Issues:
# - Dallas Institute of Funeral Service listed 0 Pell Recipients, but all of their Grants went to Pell Students...
pell_issues <- Texas_2021_HEERF %>%
  mutate(PellAmount = ifelse(ugFtPellGrantAmountTotal > 0 | ugPtPellGrantAmountTotal > 0, 1, 0),
         No_FT_Pell_Listed = ifelse(is.na(ugFtPellHeerRecipients) | ugFtPellHeerRecipients == 0, 1, 0),
         No_PT_Pell_Listed = ifelse(is.na(ugPtPellHeerRecipients) | ugPtPellHeerRecipients == 0, 1, 0)) %>%
  filter(PellAmount == 1 & No_PT_Pell_Listed == 1 & No_FT_Pell_Listed == 1) %>%
  select(unitid, instnm, PellAmount, No_FT_Pell_Listed, No_PT_Pell_Listed)

test <- Texas_2021_HEERF %>%
  filter(unitid == "491996") %>%
  select(instnm, ugFtPellAverageHeerAmount, ugPtPellAverageHeerAmount, ugFtPellGrantAmountTotal, ugPtPellGrantAmountTotal,
         ugFtPellHeerRecipients, ugPtPellHeerRecipients)

# said they gave money to race/ethnic group, but did not list recipients
race_eth_issues <- Texas_2021_HEERF %>%
  select(unitid, instnm, ends_with("ReceivedAidGrant"), ends_with("TotalAmountOfGrants"), ends_with("AverageHeerfAward")) %>%
  mutate(No_AIGrantees = ifelse(is.na(aiReceivedAidGrant) | aiReceivedAidGrant == 0, 1, 0),
         AI_Monies = ifelse(aiTotalAmountOfGrants > 0, 1, 0),
         AI_Pos_Avg = ifelse(aiAverageHeerfAward > 0, 1, 0),
         No_AGrantees = ifelse(is.na(aReceivedAidGrant) | aReceivedAidGrant == 0, 1, 0),
         A_Monies = ifelse(aTotalAmountOfGrants > 0, 1, 0),
         A_Pos_Avg = ifelse(aAverageHeerfAward > 0, 1, 0),
         No_BGrantees = ifelse(is.na(bReceivedAidGrant) | bReceivedAidGrant == 0, 1, 0),
         B_Monies = ifelse(bTotalAmountOfGrants > 0, 1, 0),
         B_Pos_Avg = ifelse(bAverageHeerfAward > 0, 1, 0),
         No_HGrantees = ifelse(is.na(hReceivedAidGrant) | hReceivedAidGrant == 0, 1, 0),
         H_Monies = ifelse(hTotalAmountOfGrants > 0, 1, 0),
         H_Pos_Avg = ifelse(hAverageHeerfAward > 0, 1, 0),
         No_NHGrantees = ifelse(is.na(nhReceivedAidGrant) | hReceivedAidGrant == 0, 1, 0),
         NH_Monies = ifelse(nhTotalAmountOfGrants > 0, 1, 0),
         NH_Pos_Avg = ifelse(nhAverageHeerfAward > 0, 1, 0),
         No_WGrantees = ifelse(is.na(wReceivedAidGrant) | wReceivedAidGrant == 0, 1, 0),
         W_Monies = ifelse(wTotalAmountOfGrants > 0, 1, 0),
         W_Pos_Avg = ifelse(wAverageHeerfAward > 0, 1, 0),
         No_TMRGrantees = ifelse(is.na(tmrReceivedAidGrant) | tmrReceivedAidGrant == 0, 1, 0),
         TMR_Monies = ifelse(tmrTotalAmountOfGrants > 0, 1, 0),
         TMR_Pos_Avg = ifelse(tmrAverageHeerfAward > 0, 1, 0)) %>%
  mutate(issues = case_when(No_AIGrantees == 1 & (AI_Monies == 1 | AI_Pos_Avg == 1) ~ 1,
                            No_AGrantees == 1 & (A_Monies == 1 | A_Pos_Avg == 1) ~ 1,
                            No_BGrantees == 1 & (B_Monies == 1 | B_Pos_Avg == 1) ~ 1,
                            No_HGrantees == 1 & (H_Monies == 1 | H_Pos_Avg == 1) ~ 1, 
                            No_NHGrantees == 1 & (NH_Monies == 1 | NH_Pos_Avg == 1) ~ 1,
                            No_WGrantees == 1 & (W_Monies == 1 | W_Pos_Avg == 1) ~ 1,
                            No_TMRGrantees == 1 & (TMR_Monies == 1 | TMR_Pos_Avg == 1) ~ 1,
                            TRUE ~ 0),
         AI_Issue = ifelse(No_AIGrantees == 1 & (AI_Monies == 1 | AI_Pos_Avg == 1), 1, 0),
         A_Issue = ifelse(No_AGrantees == 1 & (A_Monies == 1 | A_Pos_Avg == 1), 1, 0), 
         B_Issue = ifelse(No_BGrantees == 1 & (B_Monies == 1 | B_Pos_Avg == 1), 1, 0),
         H_Issue = ifelse(No_HGrantees == 1 & (H_Monies == 1 | H_Pos_Avg == 1), 1, 0), 
         NH_Issue = ifelse(No_NHGrantees == 1 & (NH_Monies == 1 | NH_Pos_Avg == 1), 1, 0), 
         W_Issue = ifelse(No_WGrantees == 1 & (W_Monies == 1 | W_Pos_Avg == 1), 1, 0),
         TMR_Issue = ifelse(No_TMRGrantees == 1 & (TMR_Monies == 1 | TMR_Pos_Avg == 1), 1, 0))



### In cases where an institution is missing Pell or Race/Ethnicity Information, exclude them from the counts
# i.e., when calculating pell, exclude pell issues
# when calculating race/ethnicity averages
# Additional Cleaning Steps:
# Force NA to 0
# Calculate student portion distributed total - including emergency grants
# Add additional IPEDS characteristics: localeType, etc

# ==============================================================================
# Functions
# ==============================================================================

get_pell_averages <- function(demo){
  
  # Get List of Inst With Pell Issues
  issues <- issues %>% pull(unitid)
  
  Texas_2021_HEERF %>%
    filter(!unitid %in% c(issues)) %>%
    mutate(across(ends_with("TotalAmountOfGrants"), ~replace(., is.na(.), 0)),
           across(ends_with("ReceivedAidGrant"), ~replace(., is.na(.), 0))) %>%
    group_by({{demo}}) %>%
    summarise(n = n(),
              Total_Pell_Distribution = sum(ugFtPellGrantAmountTotal, ugPtPellGrantAmountTotal, na.rm = T),
              Total_Pell_Students = sum(ugFtPellEnrolled, ugPtPellEnrolled, na.rm = T),
              Pell_Recipients = sum(ugFtPellHeerRecipients, ugPtPellHeerRecipients, na.rm = T),
              Average_Pell_Distribution = sum(Total_Pell_Distribution, na.rm = T)/sum(Pell_Recipients, na.rm = T))
  
}


get_race_eth_averages <- function(demo){
  Texas_2021_HEERF %>%
  #filter(!unitid %in% c(AI_issues)) %>% See note above
  mutate(across(ends_with("TotalAmountOfGrants"), ~replace(., is.na(.), 0)),
         across(ends_with("ReceivedAidGrant"), ~replace(., is.na(.), 0))) %>%
  group_by({{demo}}) %>%
  summarise(n = n(),
            Average_AI_Distribution = sum(aiTotalAmountOfGrants, na.rm = T) / sum(aiReceivedAidGrant, na.rm = T),
            Average_Asian_Distribution = sum(aTotalAmountOfGrants, na.rm = T) / sum(aReceivedAidGrant, na.rm = T),
            Average_Black_Distribution = sum(bTotalAmountOfGrants, na.rm = T) / sum(bReceivedAidGrant, na.rm = T),
            Average_Latinx_Distribution = sum(hTotalAmountOfGrants, na.rm = T) / sum(hReceivedAidGrant, na.rm = T),
            Average_NH_Distribution = sum(nhTotalAmountOfGrants, na.rm = T) / sum(nhReceivedAidGrant, na.rm = T),
            Average_White_Distribution = sum(wTotalAmountOfGrants, na.rm = T) / sum(wReceivedAidGrant, na.rm = T),
            Average_TMR_Distribution = sum(tmrTotalAmountOfGrants, na.rm = T) / sum(tmrReceivedAidGrant, na.rm = T))
}

# ==============================================================================
# Analysis 
# ==============================================================================

# Overall Spending for IHE in Sample
Texas_2021_HEERF %>%
  group_by(state) %>%
  summarise(n = n(),
            Total_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T),
            Total_Student_Recipients = sum(allStudentsHeerRecipients, na.rm = T), 
            Average_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T)/sum(allStudentsHeerRecipients, na.rm = T))


# Overall Spending by Institution Type
Texas_2021_HEERF %>%
  group_by(iheType, iheControl) %>%
  summarise(n = n(),
            Total_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T),
            Total_Student_Recipients = sum(allStudentsHeerRecipients, na.rm = T), 
            Average_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T)/sum(allStudentsHeerRecipients, na.rm = T))

# Overall Spending by MSI Type

Texas_2021_HEERF %>%
  group_by(AANAPII) %>%
  summarise(n = n(),
            Total_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T),
            Total_Student_Recipients = sum(allStudentsHeerRecipients, na.rm = T), 
            Average_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T)/sum(allStudentsHeerRecipients, na.rm = T)) %>%
  filter(AANAPII == 1)

Texas_2021_HEERF %>%
  group_by(HBCU) %>%
  summarise(n = n(),
            Total_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T),
            Total_Student_Recipients = sum(allStudentsHeerRecipients, na.rm = T), 
            Average_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T)/sum(allStudentsHeerRecipients, na.rm = T)) %>%
  filter(HBCU == 1)

Texas_2021_HEERF %>%
  group_by(HSI) %>%
  summarise(n = n(),
            Total_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T),
            Total_Student_Recipients = sum(allStudentsHeerRecipients, na.rm = T), 
            Average_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T)/sum(allStudentsHeerRecipients, na.rm = T)) %>%
  filter(HSI == 1)


## Pell ------------------------------------------------------------------------

# ALL (TX) 
Pell_Spending_State <- get_pell_averages(state)

# INSTITUTION TYPE 
Pell_Spending_by_Inst_Type <- Texas_2021_HEERF %>%
  filter(!unitid %in% c(issues)) %>%
  mutate(across(ends_with("TotalAmountOfGrants"), ~replace(., is.na(.), 0)),
         across(ends_with("ReceivedAidGrant"), ~replace(., is.na(.), 0))) %>%
  group_by(iheType, iheControl) %>%
  summarise(n = n(),
            Total_Pell_Distribution = sum(ugFtPellGrantAmountTotal, ugPtPellGrantAmountTotal, na.rm = T),
            Total_Pell_Students = sum(ugFtPellEnrolled, ugPtPellEnrolled, na.rm = T),
            Pell_Recipients = sum(ugFtPellHeerRecipients, ugPtPellHeerRecipients, na.rm = T),
            Average_Pell_Distribution = sum(Total_Pell_Distribution, na.rm = T)/sum(Pell_Recipients, na.rm = T))


# MSI
# use MSI flags from scorecard, not Inst Information !!!
Pell_Spending_HBCU <- get_pell_averages(HBCU) %>%
  mutate(MSI_Type = "HBCU") %>%
  rename(flag = HBCU)

Pell_Spending_PBI <- get_pell_averages(PBI) %>%
  mutate(MSI_Type = "PBI") %>%
  rename(flag = PBI)

Pell_Spending_ANNHI <- get_pell_averages(ANNHI) %>%
  mutate(MSI_Type = "ANNHI") %>%
  rename(flag = ANNHI)

Pell_Spending_Tribal <- get_pell_averages(TRIBAL) %>%
  mutate(MSI_Type = "TRIBAL") %>%
  rename(flag = TRIBAL)

Pell_Spending_AANAPII <- get_pell_averages(AANAPII) %>%
  mutate(MSI_Type = "AANAPII") %>%
  rename(flag = AANAPII)

Pell_Spending_HSI <- get_pell_averages(HSI) %>%
  mutate(MSI_Type = "HSI") %>%
  rename(flag = HSI)

Pell_Spending_NANTI <- get_pell_averages(NANTI) %>%
  mutate(MSI_Type = "NANTI") %>%
  rename(flag = NANTI)

Pell_Spending_by_MSI_Type <- rbind(
  Pell_Spending_HBCU, Pell_Spending_PBI, Pell_Spending_ANNHI, Pell_Spending_Tribal,
  Pell_Spending_AANAPII, Pell_Spending_HSI, Pell_Spending_NANTI
) %>%
  filter(flag == 1)

## Race ------------------------------------------------------------------------
# exclude race/ethnicity issues
# calculate averages by Race/Ethnicity
# Conlcusion: even after removing inst with issues by race, still get NaN for some groups--
# NaN occurs when you divide by 0, so no need to exclude inst and do each separately.
# Issues with Institution Averages, but since we are recalculating averages and there are no issues with
# count and total distribution by race/ethnicity, should be fine to include everything. 
# Limitation is we cant recreate the averages they provided for inst where they dont give the # of recipients and total amount given

# AI_issues <- race_eth_issues %>% filter(AI_Issue == 1) %>% pull(unitid)



# ALL (TX)
RaceEth_Spending_State <- get_race_eth_averages(state)

# INSTITUTION TYPE
RaceEth_Spending_by_Inst_Type <- Texas_2021_HEERF %>%
  #filter(!unitid %in% c(AI_issues)) %>% See note above
  mutate(across(ends_with("TotalAmountOfGrants"), ~replace(., is.na(.), 0)),
         across(ends_with("ReceivedAidGrant"), ~replace(., is.na(.), 0))) %>%
  group_by(iheType, iheControl) %>%
  summarise(n = n(),
            Average_AI_Distribution = sum(aiTotalAmountOfGrants, na.rm = T) / sum(aiReceivedAidGrant, na.rm = T),
            Average_Asian_Distribution = sum(aTotalAmountOfGrants, na.rm = T) / sum(aReceivedAidGrant, na.rm = T),
            Average_Black_Distribution = sum(bTotalAmountOfGrants, na.rm = T) / sum(bReceivedAidGrant, na.rm = T),
            Average_Latinx_Distribution = sum(hTotalAmountOfGrants, na.rm = T) / sum(hReceivedAidGrant, na.rm = T),
            Average_NH_Distribution = sum(nhTotalAmountOfGrants, na.rm = T) / sum(nhReceivedAidGrant, na.rm = T),
            Average_White_Distribution = sum(wTotalAmountOfGrants, na.rm = T) / sum(wReceivedAidGrant, na.rm = T),
            Average_TMR_Distribution = sum(tmrTotalAmountOfGrants, na.rm = T) / sum(tmrReceivedAidGrant, na.rm = T))

# MSI
RaceEth_Spending_HBCU <- get_race_eth_averages(HBCU) %>%
  rename(flag = HBCU) %>%
  mutate(MSI_Type = "HBCU")

RaceEth_Spending_AANAPII <- get_race_eth_averages(AANAPII) %>%
  rename(flag = AANAPII) %>%
  mutate(MSI_Type = "AANAPII") 

RaceEth_Spending_HSI <- get_race_eth_averages(HSI) %>%
  rename(flag = HSI) %>%
  mutate(MSI_Type = "HSI")

RaceEth_Spending_by_MSI_Type <- rbind(
  RaceEth_Spending_AANAPII, RaceEth_Spending_HSI, RaceEth_Spending_HBCU
) %>% filter(flag == 1)



### OUTCOME: Still Enrolled

# Check out variables
Outcome <- Texas_2021_HEERF %>%
  select(instnm, starts_with("enroll2019"))

# Withdrawal Rate (Only ~ 50 institutions have this info...)
Withdraw_Rate <- Texas_2021_HEERF %>%
  select(instnm, starts_with("enroll")) %>%
  mutate(UG_Withdraw_Rate_2019 = (enroll2019UgW/enroll2019Ug) * 100,
         UG_Withdraw_Rate_2020 = (enroll2020UgW/enroll2020Ug) * 100,
         UG_Withdraw_Rate_2021 = (enroll2021UgW/enroll2021Ug) * 100) %>%
  select(instnm, contains("Withdraw_Rate"))

# Since only roughly 50 percent of institutions have this information, it may be better to use IPEDS data. 
# The data is likely biased since only 20% of institutions filled in this information. 
# Maybe only institutions who knew they did well shared this info. Or maybe only more resourced institutions. 
# Either way, its biased so prefer not to use. 



### SCRATCH



Spending_by_Inst_Type <- Texas_2021_HEERF %>%
  mutate(across(ends_with("TotalAmountOfGrants"), ~replace(., is.na(.), 0)),
         across(ends_with("ReceivedAidGrant"), ~replace(., is.na(.), 0))) %>%
  group_by(iheType, iheControl) %>%
  summarise(
    n = n(),
    # All
    Total_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T),
    Total_Student_Recipients = sum(allStudentsHeerRecipients, na.rm = T), 
    Average_Student_Distribution = sum(allStudentsGrantAmountTotal, na.rm = T)/sum(allStudentsHeerRecipients, na.rm = T),
    # Pell
    Total_Pell_Distribution = sum(ugFtPellGrantAmountTotal, ugPtPellGrantAmountTotal, na.rm = T),
    Total_Pell_Students = sum(ugFtPellEnrolled, ugPtPellEnrolled, na.rm = T),
    Pell_Recipients = sum(ugFtPellHeerRecipients, ugPtPellHeerRecipients, na.rm = T),
    Average_Pell_Distribution = sum(Total_Pell_Distribution, na.rm = T)/sum(Pell_Recipients, na.rm = T),
    # Race/Ethnicity
    Average_AI_Distribution = sum(aiTotalAmountOfGrants, na.rm = T) / sum(aiReceivedAidGrant, na.rm = T),
    Average_Asian_Distribution = sum(aTotalAmountOfGrants, na.rm = T) / sum(aReceivedAidGrant, na.rm = T),
    Average_Black_Distribution = sum(bTotalAmountOfGrants, na.rm = T) / sum(bReceivedAidGrant, na.rm = T),
    Average_Latinx_Distribution = sum(hTotalAmountOfGrants, na.rm = T) / sum(hReceivedAidGrant, na.rm = T),
    Average_NH_Distribution = sum(nhTotalAmountOfGrants, na.rm = T) / sum(nhReceivedAidGrant, na.rm = T),
    Average_White_Distribution = sum(wTotalAmountOfGrants, na.rm = T) / sum(wReceivedAidGrant, na.rm = T),
    Average_TMR_Distribution = sum(tmrTotalAmountOfGrants, na.rm = T) / sum(tmrReceivedAidGrant, na.rm = T))
)

# Check Pell Distribution for 2-year Private Not-For-Profit -- tried to remove inst. (see issues above) where they distributed monies to pell students but didnt list any pell recipients
PnfP <- Texas_2021_HEERF %>%
  filter(iheType == "2 Year", iheControl == "Private Not-For-Profit") %>%
  select(instnm, aiTotalAmountOfGrants, aiReceivedAidGrant, aTotalAmountOfGrants, aReceivedAidGrant, allStudentsGrantAmountTotal, allStudentsHeerRecipients, ugFtPellEnrolled, ugPtPellEnrolled, ugFtPellHeerRecipients,
         ugPtPellHeerRecipients, ugFtPellGrantAmountTotal, ugPtPellGrantAmountTotal, ugFtPellAverageHeerAmount)



