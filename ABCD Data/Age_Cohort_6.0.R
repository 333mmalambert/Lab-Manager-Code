##########################
# Age_Cohort_6.0.R
# Author: Emma Lambert
# Date: 9/4/2026
##########################

# The purpose of this code is to calculate a participant's time in the ABCD study at each session. 

sourcepath <- "Insert Data Path Here"  
devpath <- "Insert Data Path Here"  

library(tidyverse)
library(lubridate)

Ages <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))

######################################
# IN THE BELOW CODE BLOCK
# Clean and prep data for calculation
#####################################

# Selecting relevant age columns + calculate age in months
Age_Cohort <- Age_Cohort %>%
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age) %>%
  rename(age = ab_g_dyn__visit_age) %>%
  mutate(age_months = age *12)

######################################
# IN THE BELOW CODE BLOCK
# Calculate time in ABCD study
#####################################

Age_Cohort <- Age_Cohort %>%
  group_by(participant_id) %>%
  mutate(baseline_age_m = age_months[session_id == "ses-00A"], # Marking baseline age as age at ses-00A
         time_in_study = age_months - baseline_age_m) %>% # Calculating time since joining ABCD study
  select(-c(baseline_age_m)) # Dropping unnecessary columns
  

######################################
# IN THE BELOW CODE BLOCK
# Write to .tsv
#####################################

write_tsv(Age_Cohort, paste0(devpath, 'Output/Age_Cohort_6.0.tsv'))