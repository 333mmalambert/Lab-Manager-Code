#######################
# ASSIST Scoring
# Date: 6/18/2026; Updated: 8/3/2026
# Author: Emma Lambert
#######################

# Importing packages
library(tidyverse)

# Setting Working Directory
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/Damme, Katherine Steffen's files - ThriveWithMe/Phase 1 (18-25)/Data/Part 1/REDCap")

# Reading in TWM Part 1 Data (This will need to be updated with each new data report)
TWM_Data <- read_csv("ThriveWithMeScreener_DATA_2026-08-03_1007.csv")

#################
# Selecting Data
#################

# Selecting ASSIST-Relevant Columns
ASSIST <- TWM_Data %>%
  select(participant_id, contains("assist")) 

######################
# Scoring ASSIST Data
######################

ASSIST <- ASSIST %>%
 mutate(inclusion_status = if_else(
    assist_cannabis_use >= 4 | # In the past three months, how often have you used Cannabis?
     assist_cocaine_use >= 4 |  # In the past three months, how often have you used Cocaine?
     assist_prescriptionstimulants_use >= 4 | # In the past three months, how often have you used Prescription Stimulants?
     assist_methamphetamines_use >= 4 | # In the past three months, how often have you used Methamphetamines?
     assist_inhalants_use >= 4 | # In the past three months, how often have you used Inhalants?
     assist_sedatives_use >= 4 | # In the past three months, how often have you used Sedatives?
     assist_hallucinogens_use >= 4 | # In the past three months, how often have you used Hallucinogens?
     assist_streetopioids_use >= 4 | # In the past three months, how often have you used Street Opioids?
     assist_prescriptionopioids_use >= 4 | # In the past three months, how often have you used Prescription Opioids?
     assist_choicedrug_use >= 4, # In the past three months, how often have you used the substance you specified?
     true = 0, # 0 = Excluded
     false = 1, # 1 = Included
     missing = 1)) # The above questions are gated, so a majority of the data is NA. 

# Creating Column that indicates if a participant DID NOT answer frequency item, but DID answer use item
ASSIST <- ASSIST %>%
  mutate(freqNA_Y1N0 = case_when(
    assist_substance_use___1 == 1 & is.na(assist_cannabis_use) ~ 1, 
    assist_substance_use___2 == 1 & is.na(assist_cocaine_use) ~ 1,
    assist_substance_use___3 == 1 & is.na(assist_prescriptionstimulants_use) ~ 1,
    assist_substance_use___4 == 1 & is.na(assist_methamphetamines_use) ~ 1,
    assist_substance_use___5 == 1 & is.na(assist_inhalants_use) ~ 1,
    assist_substance_use___6 == 1 & is.na(assist_sedatives_use) ~ 1,
    assist_substance_use___7 == 1 & is.na(assist_hallucinogens_use) ~ 1,
    assist_substance_use___8 == 1 & is.na(assist_streetopioids_use) ~ 1,
    assist_substance_use___9 == 1 & is.na(assist_prescriptionopioids_use) ~ 1,
    assist_substance_use___10 == 1 & is.na(assist_choicedrug_use) ~ 1,
    TRUE ~ 0))

# Marking Participants as Excluded if they do not answer frequency item, but answered use item
ASSIST <- ASSIST %>%
  mutate(inclusion_status = case_when(
    freqNA_Y1N0 == 1 ~ 0,
    TRUE ~ inclusion_status))

ASSIST_Clean <- ASSIST %>%
  select(participant_id, inclusion_status)

#################
# Writing to CSV
#################

# Pathway to Output folder
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/Damme, Katherine Steffen's files - ThriveWithMe/Phase 1 (18-25)/Data/Part 1/Scored Data/Output/Phase 2 Eligibility (ASSIST)")

# Writing to .csv
write_csv (ASSIST_Clean,'Phase2_Inclusion_Status.csv')

