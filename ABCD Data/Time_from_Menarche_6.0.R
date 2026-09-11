##########################
# Time_from_Menarche_6.0.R
# Author: Emma Lambert
# Date: 9/4/2026
##########################

# The purpose of this code is to calculate time to/from menarche at each session.

sourcepath <- "Insert Data Path Here"  
devpath <- "Insert Data Path Here"  

library(tidyverse)
library(lubridate)

Ages <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))
Sex <-  read_tsv(paste0(sourcepath, '/abcd_general/ab_g_stc.tsv'))
Menarche_Ages <- read_csv(paste0(devpath, '/ABCD_6.0/Outputs/abcd_6.0_menarche_ReadableSubset_1_9_26.csv'))

######################################
# IN THE BELOW CODE BLOCK
# Clean and prep data for calculation
#####################################

# Selecting only relevant dynamic columns and calculating age in months
Ages <- Ages %>%
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age) %>%
  rename(age = ab_g_dyn__visit_age) %>%
  mutate(age_months = age *12)


# Selecting only relevant static columns
Sex <- Sex %>%
  select(participant_id,
         ab_g_stc__cohort_sex) %>% # Participant sex
  rename(sex = ab_g_stc__cohort_sex) %>%
  filter(sex == 2)

# Selecting only relevant menarche columns and calculating age in months
Menarche_Ages <- Menarche_Ages %>%
  select(participant_id, 
         AgeOfMenarche_ConsistentOnly_ParentReport, 
         AgeOfMenarche_ConsistentOnly_YouthReport) %>%
  mutate(AgeOfMenarche_P_Months = AgeOfMenarche_ConsistentOnly_ParentReport * 12,
         AgeOfMenarche_Y_Months = AgeOfMenarche_ConsistentOnly_YouthReport * 12)

# Merging Age and Sex Data + Dropping NA values (remove non-females)
Age_Sex <- merge(Ages, Sex, by = c("participant_id"), all.x = TRUE) %>% 
  drop_na()

# Merging Age/Sex Data + Arranging Chronologically
Men_Time_Var <- merge(Age_Sex, Menarche_Ages, by = c("participant_id"), all.x = TRUE) %>%
   arrange(participant_id, session_id)

######################################
# IN THE BELOW CODE BLOCK
# Calculate time in ABCD study
#####################################

Men_Time_Var <- Men_Time_Var %>%
  group_by(participant_id) %>%
  mutate(time_from_menarche_p = age_months - AgeOfMenarche_P_Months,
         time_from_menarche_y = age_months - AgeOfMenarche_Y_Months) 

######################################
# IN THE BELOW CODE BLOCK
# Clean up and Write to .tsv
#####################################

Men_Time_Var <- Men_Time_Var %>%
  select(participant_id, session_id, age, age_months, sex, AgeOfMenarche_P_Months, AgeOfMenarche_Y_Months, time_from_menarche_p, time_from_menarche_y)

outpath <- "Insert Data Path Here"  

write_tsv(Men_Time_Var, paste0(outpath, '/Time_from_Menarche_6.0.tsv'))