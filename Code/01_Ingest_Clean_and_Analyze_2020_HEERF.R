setwd("~/Dropbox/EdTrust/HEERF_Git")
library(tidyverse)
library(readxl)

# ------------------------------------------------------------------------------
# Step 1: Download data and DED from https://covid-relief-data.ed.gov/
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 2: Review DED to familiarize yourself with the type of data available
# ------------------------------------------------------------------------------

# list of needed variables

# `State Name`
# stateCode
# reportingYear
# iheName (Institution Name)
# opeids 
# awardedAmountsStudent
# awardedAmountsInstitutional
# awardedAmountsHbcu
# awardedAmountsTccu
# awardedAmountsMsi
# awardedAmountsSip
# awardedAmountsFipse
# awardedAmountsProprietary
# awardedAmount (total awarded -- sum of previous categories)
# didStudentsApplyForFunds (inst required students to apply for funds)

### FACTORS USED IN DETERMINING STUDENT AMOUNT
# isApplicationUsedToDetermineGrantAmount (app was used to determine students emergency fin aid funding)
# isStudentAccessToFood (IHE prioritized access to food when determining aid amount)
# isStudentAccessToHousing (IHE prioritized access to housing when determining aid amount)
# isStudentAccessToCourseMaterials (IHE prioritized access to course materials when determining aid amount)
# isStudentAccessToTechnology (IHE prioritized access to tech when determining aid amount)
# isStudentAccessToHealthCare (IHE prioritized access to healthcare when determining aid amount)
# isStudentAccessToChildCare (IHE prioritized access to childcare when determining aid amount)
# otherAttendanceExpenses (IHE prioritized other attendance expenses when determining aid amount)
# wasPreExistingDataUsed (IHE used pre-existing administrative data when determining aid amount)
# enrollmentIntensity
# isLocationFactorGrantDetermination
# isPellGrantEligible
# isFafsaFactorGrantDetermination
# fafsaFamilyIncome
# fafsaEstimatedFamilyContribution
# fafsaIndependentOrDependentStatus
# isCampusOrDistanceEducationStatus
# isOnOrOffCampusLivingArrangement
# isAcademicLevelFactorGrantDetermination
# isOtherFactorGrantDetermination

## ELIGIBLE STUDENTS
# eligibleStudentsTotal
# eligibleStudentsUGRDFTPellRecipient (Undergrad Full Time Pell Eligible)
# eligibleStudentsUGRDFTNonPellRecipient (Undergrad Full Time NON Pell Eligible)
# eligibleStudentsUGRDPTPellRecipient (Undergrad Part Time Pell Eligible)
# eligibleStudentsUGRDPTNonPellRecipient (Undergrad Part Time NON Pell Eligible)
# eligibleStudentsGRDFTStudents (Grad Full Time Eligible)
# eligibleStudentsGRDPTStudents (Grad Part Time Eligible)

## ACTUAL RECIPIENTS
# studentsRecipientsTotal 
# studentsRecipientsUGRDFTPellRecipient (Undergrad Full Time Pell)
# studentsRecipientsUGRDFTNonPellRecipient (Undergrad Full Time NON Pell)
# studentsRecipientsUGRDPTPellRecipient (UG Part Time Pell)
# studentsRecipientsUGRDPTNonPellRecipient (UG Part Time NON Pell)
# studentsRecipientsGRDFTStudents (Grad Full Time)
# studentsRecipientsGRDPTStudents (Grad Part Time)
# studentPortionDisbursedTotal (Total amount distributed to students through Sec 18004(a)(1)
# wereAllFundsExpended

# ------------------------------------------------------------------------------
# Step 3: Ingest 2020 HEERF Raw Data
# ------------------------------------------------------------------------------

HEERF <- read_excel("Raw Data/2020 ESF Data Files/HEERF_Collection_Data_Public-2020-20211119.xlsx")
Institutional_Information <- read_excel("Raw Data/2020 ESF Data Files/HEERF_Collection_Data_Public-2020-20211119.xlsx", sheet = "Additional IHE Information")
MSI <- read_csv("Raw Data/MSI.csv", 
                col_types = cols(HBCU = col_double(), 
                                 PBI = col_double(), 
                                 ANNHI = col_double(), 
                                 TRIBAL = col_double(), 
                                 AANAPII = col_double(), 
                                 HSI = col_double(), 
                                 NANTI = col_double())) 

# note: Institutional_Information was missing for 3 institutions from original raw dataset.
# In order to merge on MSI flags, I manually changed the UNITID in in the raw excel file. 
# The raw file available with the project assets has the UNITIDs updated for the following HEIs:
  # American Medical Institute, Pearlands Innovative school of beauty, San Antonio College,
  # Austin Career institute, UT Austin 


# ------------------------------------------------------------------------------
# Step 4: Get Final Analytic Dataset
# ------------------------------------------------------------------------------

Texas_HEERF <- HEERF %>%
  select(`State Name`, 
         stateCode, 
         reportingYear,
         iheName, # (Institution Name)
         dunsNumber,
         opeids,
         awardedAmountsStudent,
         awardedAmountsInstitutional,
         awardedAmountsHbcu,
         awardedAmountsTccu,
         awardedAmountsMsi,
         awardedAmountsSip,
         awardedAmountsFipse,
         awardedAmountsProprietary,
         awardedAmount,
         eligibleStudentsTotal,
         eligibleStudentsUGRDFTPellRecipient, # (Undergrad Full Time Pell Eligible)
         eligibleStudentsUGRDFTNonPellRecipient, # (Undergrad Full Time NON Pell Eligible)
         eligibleStudentsUGRDPTPellRecipient, # (Undergrad Part Time Pell Eligible)
         eligibleStudentsUGRDPTNonPellRecipient, # (Undergrad Part Time NON Pell Eligible)
         eligibleStudentsGRDFTStudents, # (Grad Full Time Eligible)
         eligibleStudentsGRDPTStudents, # (Grad Part Time Eligible)
         studentsRecipientsTotal,
         studentsRecipientsUGRDFTPellRecipient, # (Undergrad Full Time Pell)
         studentsRecipientsUGRDFTNonPellRecipient, # (Undergrad Full Time NON Pell)
         studentsRecipientsUGRDPTPellRecipient, #  (UG Part Time Pell)
         studentsRecipientsUGRDPTNonPellRecipient, #  (UG Part Time NON Pell)
         studentsRecipientsGRDFTStudents, #(Grad Full Time)
         studentsRecipientsGRDPTStudents, #(Grad Part Time)
         studentPortionDisbursedTotal,
         additionalGrantsA1Funds, # additional institutional funds spent on emergency student grants
         additionalGrantsA2Funds, # additional institutional funds spent on emergency student grants
         additionalGrantsA3Funds) %>% # additional institutional funds spent on emergency student grants
  filter(stateCode == "TX") %>%
  left_join(Institutional_Information, by = c("dunsNumber")) %>%
  mutate(UNITID = as.numeric(unitid)) %>%
  left_join(MSI, by = c("UNITID")) %>% # Had to update some Unitids from raw data file to match
  mutate(MSI_Count = HBCU + PBI + ANNHI + TRIBAL + AANAPII + HSI + NANTI) %>%
  mutate(MSI_Flag = ifelse(MSI_Count > 0, 1, 0)) %>%
  # exclude institutions with data issues
  mutate(distributed_to_more_than_enrolled = ifelse(numIheStudents * 1.10 < studentsRecipientsTotal, 1, 0)) %>%
  filter(distributed_to_more_than_enrolled == 0 | is.na(distributed_to_more_than_enrolled)) %>%
  # recalculate student distribution to include additional institutional funds disbursed to students
  mutate(across(starts_with("additionalGrantsA"), ~replace(., is.na(.), 0))) %>%
  mutate(across(starts_with("studentPortionDisbursed"), ~replace(., is.na(.), 0))) %>%
  mutate(studentPortionDisbursedTotal1 = studentPortionDisbursedTotal,
         studentPortionDisbursedTotal_A1 = studentPortionDisbursedTotal + additionalGrantsA1Funds,
         totalAdditionalStudentGrants = additionalGrantsA1Funds + additionalGrantsA2Funds + additionalGrantsA3Funds) %>%
  mutate(studentPortionDisbursedTotal = totalAdditionalStudentGrants + studentPortionDisbursedTotal)

# check additional funds
# texas_funds <- Texas_HEERF %>% select(iheName, studentPortionDisbursedTotal, studentPortionDisbursedTotal1, studentPortionDisbursedTotal_A1,
#                      totalAdditionalStudentGrants, additionalGrantsA1Funds, additionalGrantsA2Funds, additionalGrantsA3Funds)

# ------------------------------------------------------------------------------
# Step 5. Analysis 
# ------------------------------------------------------------------------------

### State Level Questions ------------------------------------------------------

# How much did Texas Higher Education Institutions Get?
sum(Texas_HEERF$awardedAmount.x) 

# How much did was Disbursed to Texas Students?
sum(Texas_HEERF$studentPortionDisbursedTotal, na.rm = TRUE) 

# How many students received something?
sum(Texas_HEERF$studentsRecipientsTotal, na.rm = T)

# Average amount distributed per student
sum(Texas_HEERF$studentPortionDisbursedTotal, na.rm = TRUE) / sum(Texas_HEERF$studentsRecipientsTotal, na.rm = TRUE)

###  HEERF Spending by Institution Type ----------------------------------------
Texas_HEERF %>%
  group_by(iheType, iheControl) %>%
  summarise(totalaward = sum(awardedAmount.x),
            totalStudentAward = sum(awardedAmountsStudent, na.rm = T),
            totalStudents = sum(numIheStudents, na.rm = T),
            totalStudentDistribution = sum(studentPortionDisbursedTotal, na.rm = T)) %>%
  ungroup() %>%
  mutate(totalAwardsToStudents = totalStudentDistribution/totalStudentAward * 100,
         avgDistributedPerStudent = totalStudentDistribution / totalStudents)

### Weighted Avg of HEERF Spending by MSI Status -------------------------------
MSI_Weighted_Avgs <- Texas_HEERF %>%
  group_by(MSI_Flag) %>%
  summarise(totalaward = sum(awardedAmount.x),
            totalStudentAward = sum(awardedAmountsStudent, na.rm = T),
            totalStudents = sum(numIheStudents, na.rm = T),
            totalStudentDistribution = sum(studentPortionDisbursedTotal, na.rm = T),
            totalIhe = n(),
            eligibleStudents = sum(eligibleStudentsTotal, na.rm = T),
            studentRecipients = sum(studentsRecipientsTotal, na.rm = T)) %>%
  ungroup() %>%
  mutate(totalAwardsDistributedToStudents = totalStudentDistribution/totalStudentAward * 100,
         avgDistributedPerStudent = totalStudentDistribution / studentRecipients,
         avgPercentRecipients = studentRecipients/eligibleStudents * 100) %>%
  mutate(MSI_Type = case_when(MSI_Flag == 1 ~ "All MSIs",
                              MSI_Flag == 0 ~ "Not an MSI",
                              TRUE ~ NA_character_)) %>%
  select(-MSI_Flag)

### Weighted Avg of HEERF Spending by MSI Type --------------------------------

get_weighted_avg_for_MSI_Type <- function(MSI_Var, msi_string){
  Texas_HEERF %>%
  group_by({{MSI_Var}}) %>%
  summarise(totalaward = sum(awardedAmount.x),
            totalStudentAward = sum(awardedAmountsStudent, na.rm = T),
            totalStudents = sum(numIheStudents, na.rm = T),
            totalStudentDistribution = sum(studentPortionDisbursedTotal, na.rm = T),
            totalIhe = n(),
            eligibleStudents = sum(eligibleStudentsTotal, na.rm = T),
            studentRecipients = sum(studentsRecipientsTotal, na.rm = T)) %>%
  ungroup() %>%
  mutate(totalAwardsDistributedToStudents = totalStudentDistribution/totalStudentAward * 100,
         avgDistributedPerStudent = totalStudentDistribution / studentRecipients,
         avgPercentRecipients = studentRecipients/eligibleStudents * 100) %>%
    mutate(MSI_Type = msi_string) %>%
    filter({{MSI_Var}} == 1) %>%
    select(-{{MSI_Var}})
}

# HBCU + PBI + ANNHI + TRIBAL + AANAPII + HSI + NANTI
HBCU_HEERF <- get_weighted_avg_for_MSI_Type(HBCU, "HBCU")
PBI_HEERF <- get_weighted_avg_for_MSI_Type(PBI, "PBI")
ANNHI_HEERF <- get_weighted_avg_for_MSI_Type(ANNHI, "ANNHI")
TRIBAL_HEERF <- get_weighted_avg_for_MSI_Type(TRIBAL, "TRIBAL")
AANAPII_HEERF <- get_weighted_avg_for_MSI_Type(AANAPII, "AANAPII")
HSI_HEERF <- get_weighted_avg_for_MSI_Type(HSI, "HSI")
NANTI_HEERF <- get_weighted_avg_for_MSI_Type(NANTI, "NANTI")

HEERF_MSI_Type <- rbind(MSI_Weighted_Avgs, HBCU_HEERF, PBI_HEERF, ANNHI_HEERF, TRIBAL_HEERF, AANAPII_HEERF, HSI_HEERF, NANTI_HEERF) %>%
                  select(MSI_Type, totalIhe, totalaward, totalStudentAward, totalStudents, eligibleStudents, 
                         studentRecipients, totalStudentDistribution, totalAwardsDistributedToStudents, 
                         avgDistributedPerStudent, avgPercentRecipients)

write_csv(HEERF_MSI_Type, "Out/HEERF_Spending_by_MSI_Type.csv")

### HEERF Institution Level Analysis -------------------------------------------
Inst_Level <- Texas_HEERF %>%
  # overall receipt percentages (by eligibility, by total headcount)
  mutate(pct_enrolled_who_were_eligible = eligibleStudentsTotal / numIheStudents * 100,
         pct_enrolled_who_received_aid_by_inst = studentsRecipientsTotal / numIheStudents * 100,
         pct_eligible_who_received_aid_by_inst = studentsRecipientsTotal / eligibleStudentsTotal * 100) %>%
  # by pell
  # get aggregated eligible Pell/Non Pell Counts
  mutate(eligiblePellUG = eligibleStudentsUGRDFTPellRecipient + eligibleStudentsUGRDPTPellRecipient,
         eligibleNonPellUG = eligibleStudentsUGRDFTNonPellRecipient + eligibleStudentsUGRDPTNonPellRecipient,
         eligibleUG = eligibleStudentsTotal - (eligibleStudentsGRDFTStudents + eligibleStudentsGRDPTStudents)) %>%
  # get aggregated received Pell/Non Pell Counts
  mutate(receivedPellUG = studentsRecipientsUGRDFTPellRecipient + studentsRecipientsUGRDPTPellRecipient,
         receivedNonPellUG = studentsRecipientsUGRDFTNonPellRecipient + studentsRecipientsUGRDPTNonPellRecipient,
         receivedUG = studentsRecipientsTotal - (studentsRecipientsGRDFTStudents + studentsRecipientsGRDPTStudents)) %>%
  mutate(pct_received_who_are_pell = receivedPellUG / receivedUG * 100,
         avgStudentAward = studentPortionDisbursedTotal / studentsRecipientsTotal) %>%
  select(unitid, iheName, MSI_Flag, studentsRecipientsTotal, eligibleStudentsTotal, studentPortionDisbursedTotal, avgStudentAward, pct_eligible_who_received_aid_by_inst,
         pct_enrolled_who_received_aid_by_inst, pct_enrolled_who_were_eligible, pctPell, pct_received_who_are_pell,
         eligiblePellUG, eligibleNonPellUG, eligibleUG, receivedPellUG, receivedNonPellUG, receivedUG)

Inst_Level %>% 
  group_by(MSI_Flag) %>%
  summarise(n = n(),
            totalPellUG = sum(eligiblePellUG, na.rm = T),
            totalNonPellUG = sum(eligibleNonPellUG, na.rm = T),
            totalUG = sum(eligibleUG, na.rm = T)) %>%
  ungroup() %>%
  mutate(pct_pell = totalPellUG/totalUG * 100)     


# Write out final inst dataset
write_csv(Inst_Level, "Out/Texas_HEI_HEERF.csv")



