setwd("~/Dropbox/EdTrust/HEERF")
library(tidyverse)
library(readxl)

# Step 1: Download data and DED from https://covid-relief-data.ed.gov/

# Step 2: Review DED to familiarize yourself with the type of data available

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


# Step 3: Ingest HEERF Raw Data
HEERF <- read_excel("Raw Data/HEERF_Collection_Data_Public-2021-11-19.xlsx")
Institutional_Information <- read_excel("Raw Data/HEERF_Collection_Data_Public-2021-11-19.xlsx", sheet = "Additional IHE Information")
MSI <- read_csv("Raw Data/MSI.csv", 
                col_types = cols(HBCU = col_double(), 
                                 PBI = col_double(), 
                                 ANNHI = col_double(), 
                                 TRIBAL = col_double(), 
                                 AANAPII = col_double(), 
                                 HSI = col_double(), 
                                 NANTI = col_double())) 

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
         studentPortionDisbursedTotal) %>%
  filter(stateCode == "TX") %>%
  left_join(Institutional_Information, by = c("dunsNumber")) %>%
  mutate(UNITID = as.numeric(unitid)) %>%
  left_join(MSI, by = c("UNITID")) %>%
  mutate(MSI_Count = HBCU + PBI + ANNHI + TRIBAL + AANAPII + HSI + NANTI) %>%
  mutate(MSI_Flag = ifelse(MSI_Count > 0, 1, 0)) %>%
  # exclude institutions with data issues
  mutate(distributed_to_more_than_enrolled = ifelse(numIheStudents * 1.10 < studentsRecipientsTotal, 1, 0)) %>%
  filter(distributed_to_more_than_enrolled == 0 | is.na(distributed_to_more_than_enrolled))


# Texas Percent of Eligible Who Received Funds by Pell Status
Texas_recipients <- Texas_HEERF %>%
  mutate(
        # Calculate what percent of all students who were eligible were pell recipients  
         eligible_pct_UG_FT_Pell = eligibleStudentsUGRDFTPellRecipient/eligibleStudentsTotal * 100,
         eligible_pct_UG_PT_Pell = eligibleStudentsUGRDPTPellRecipient/eligibleStudentsTotal * 100,
         eligible_pct_UG_PT_NonPell = eligibleStudentsUGRDPTNonPellRecipient/eligibleStudentsTotal * 100,
         eligible_pct_UG_FT_NonPell = eligibleStudentsUGRDFTNonPellRecipient/eligibleStudentsTotal * 100,
         eligible_pct_GR_FT = eligibleStudentsGRDFTStudents/eligibleStudentsTotal * 100,
         eligible_pct_GR_PT = eligibleStudentsGRDPTStudents/eligibleStudentsTotal * 100,
         # calculate the percent of eligible students who were actually awarded aid by Pell Status
         received_aid_pct_UG_FT_Pell = studentsRecipientsUGRDFTPellRecipient/eligibleStudentsUGRDFTPellRecipient * 100,
         received_aid_pct_UG_FT_NonPell = studentsRecipientsUGRDFTNonPellRecipient/eligibleStudentsUGRDFTNonPellRecipient * 100
         ) %>%
  select(iheName, contains("eligible_pct_"), contains("received_aid_"))

# By HEI, how many eligible students actually received aid?
Texas_HEERF %>%
  mutate(pct_eligible_who_received_aid = studentsRecipientsTotal/ eligibleStudentsTotal * 100) %>%
  mutate(pct_eligible_who_received_aid_neat = paste0(round(pct_eligible_who_received_aid,2), "% (n=", eligibleStudentsTotal, ")")) %>%
  select(iheName, pct_eligible_who_received_aid, pct_eligible_who_received_aid_neat, studentsRecipientsTotal)




# For example, was the percentage of Pell students receiving emergency aid proportionate to their enrollment?
Receipt_Pct_by_Pell <- Pell %>%
  mutate(PctPellReceived = receivedPellUG / eligiblePellUG * 100,
         PctNonPellReceived = receivedNonPellUG / eligibleNonPellUG * 100) %>%
  select(iheName, receivedPellUG, eligiblePellUG, PctPellReceived, receivedNonPellUG, eligibleNonPellUG, PctNonPellReceived)

Pell_Enrollment <- Texas_HEERF %>%
  mutate(TotalPellEnrollment = pctPell * numIheStudents,
         pctPellEnroll = pctPell * 100) %>%
  select(iheName, TotalPellEnrollment, pctPellEnroll, numIheStudents) %>%
  left_join(Receipt_Pct_by_Pell, by = "iheName") %>%
  left_join(Pell, by = "iheName")


### State Level Questions
  # How much did Texas Higher Education Institutions Get?
  sum(Texas_HEERF$awardedAmount.x) 
  
  # How much did was Disbursed to Texas Students?
  sum(Texas_HEERF$studentPortionDisbursedTotal, na.rm = TRUE) 
  
  # How many students received something?
  sum(Texas_HEERF$studentsRecipientsTotal, na.rm = T)
  
  # Average amount distributed per student
  sum(Texas_HEERF$studentPortionDisbursedTotal, na.rm = TRUE) / sum(Texas_HEERF$studentsRecipientsTotal, na.rm = TRUE)

### Institution Type Disagg
  # Private/Public 4-yr 2-yr total amounts
  Texas_HEERF %>%
    group_by(iheType, iheControl) %>%
    summarise(totalaward = sum(awardedAmount.x),
              totalStudentAward = sum(awardedAmountsStudent, na.rm = T),
              totalStudents = sum(numIheStudents, na.rm = T),
              totalStudentDistribution = sum(studentPortionDisbursedTotal, na.rm = T)) %>%
    ungroup() %>%
    mutate(totalAwardsToStudents = totalStudentDistribution/totalStudentAward * 100,
           avgDistributedPerStudent = totalStudentDistribution / totalStudents)
  
  # MSI vs non MSI average amount per student
    ## QUESTION: DOES THE STUDENT PORTION DISTRIBUTED TOTAL INCLUDE THE EXTRA FIPS/HBCU MONEY?? NO
    ## WHY IS THE AVG PER STUDENT LOWER FOR MSIs?
  
  # some inst missing MSI flag--had to manually enter in IPEDS ID for some institutions because inst sheet missing data
  # UT Austin and San Antonio College had IPEDS code for system office (changed),
  # some institutions were missing from MSI file so will redownload new file from Scorecard
 # fixed 12/16
   #missing_MSI <- Texas_HEERF %>%
   # filter(is.na(MSI_Flag))
  
  
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
           avgPercentRecipients = studentRecipients/eligibleStudents * 100)
  
  MSI <- Texas_HEERF %>%
    filter(MSI_Flag == 1) %>% 
    select(iheName, contains("awardedAmounts"), eligibleStudentsTotal, studentsRecipientsTotal, studentPortionDisbursedTotal)

# Institution
### How much did the institutions give to students?
  # What percent of all enrolled students received something?

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



