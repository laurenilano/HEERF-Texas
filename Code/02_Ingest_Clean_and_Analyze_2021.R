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
        contains("awardedAmounts"), awards, ends_with("PellEnrolled"), ends_with("Enrolled"),
        ends_with("HeerRecipients"), starts_with("a1"), starts_with("a2"), starts_with("a3"),
        starts_with("a4"), starts_with("minimum"), starts_with("maximum"), ends_with("GrantAmountTotal"),
        ends_with("AverageHeerAmount"), ends_with("EligibleTitleIV"), ends_with("EnrolledStudentCount"),
        ends_with("ReceivedAidGrant"), ends_with("TotalAmountOfGrants"), ends_with("AverageHeerfAward"),
        starts_with("enroll2021"), starts_with("enroll2020"), starts_with("enroll2019")) %>%
  # in awardedAmounts, force NA to 0
  mutate(across(starts_with("awardedAmounts"), ~replace(., is.na(.), 0)),
         across(ends_with("HeerRecipients"), ~replace(., is.na(.), 0)))

# Additional Cleaning Steps:
# Force NA to 0
# Calculate student portion distributed total - including emergency grants
# Add additional IPEDS characteristics: localeType, etc

# Analysis ---------------------------------------------------------------------

## HEERF Spending by Institution Type (Sector, Control, locale)

## Spending by MSI Type

## Institution Level


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




