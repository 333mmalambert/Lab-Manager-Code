###################################
# NDA_Submission_Check
# Author: Emma Lambert
# 6/17/2026
######################################

library(dplyr)
library(lubridate)

# Setting source path
sourcepath_6.0 <- "Insert ABCD 6.0 Path"
sourcepath_nda <- "Insert Prior NDA Submissions"
sourcepath_demo <- "Insert 6.0 Demo Data"

# Read in all volumes/demographics
volumes_6.0 <- read_csv(paste0(sourcepath_6.0, 'All_6.0_Volumes.csv'))
nda_vols <- read_csv(paste0(sourcepath_nda, 'NDA_Vol_Resubmission_Jan26.csv'))
nda_demo <- read_csv(paste0(sourcepath_nda, 'NDA_Demo_Resubmission_Jan26.csv'))
demo <- read_tsv(paste0(sourcepath_demo, 'ab_g_stc.tsv'))
interview <- read_tsv(paste0(sourcepath_demo, 'ab_g_dyn.tsv'))

# Selecting only relevant columns
ids_6.0 <- volumes_6.0 %>%
  select(participant_id)

nda_ids <- nda_vols %>%
  select(src_subject_id)

demo <- demo %>%
  select(participant_id, ab_g_stc__cohort_sex, ab_g_stc__cohort_race__nih, ab_g_stc__cohort_ethn)

interview <- interview %>%
  select(participant_id, session_id, ab_g_dyn__visit_dtt, ab_g_dyn__visit_age)

# Update Bio Sex info and Filter for only Females
demo <- demo %>%
  mutate(sex = case_when(
         ab_g_stc__cohort_sex == 1 ~ "M",
         ab_g_stc__cohort_sex == 2 ~ "F",
         TRUE ~ NA_character_
       )) %>%

filter(sex == "F") %>%
  select(participant_id, sex, ab_g_stc__cohort_race__nih, ab_g_stc__cohort_ethn)
 
# Merge 6.0 Vols Demo Data
female_vols_6.0 <- ids_6.0 %>%
  inner_join(demo, by = "participant_id") %>%
    distinct()
 
# Rename Race and Ethnicity Columns
female_vols_6.0 <- female_vols_6.0 %>%
  rename(race = ab_g_stc__cohort_race__nih) %>%
  rename(ethnic_group = ab_g_stc__cohort_ethn)

# Recoding Race Values
female_vols_6.0 <- female_vols_6.0 %>%
  mutate(race = recode(race,
                       '2' = 'White',
                       '3' = 'Black or African American',
                       '4' = 'Asian',
                       '5' = 'American Indian/Alaska Native',
                       '6' = 'Hawaiian or Pacific Islander',
                       '8' = 'More than one race',
                       '13' = 'Unknown or not reported'))

# Recoding Ethnicity Values
female_vols_6.0 <- female_vols_6.0 %>%
  mutate(ethnic_group = na_if(ethnic_group, 'n/a')) %>%
    mutate(ethnic_group = recode(ethnic_group,
                                 '1' = 'Hispanic or Latino',
                                 '2' = 'Not Hispanic or Latino'))
  
# Update naming convention
nda_ids <- nda_ids %>%
  mutate(participant_id = str_replace(src_subject_id,'NDAR_INV', 'sub-')) %>%
    unique()
 
# Comparing 6.0 to Jan26 NDA Submission
newvols <- female_vols_6.0 %>%
  filter(!participant_id %in% nda_ids$participant_id)
 
# Merging session and volume data back in
newvols <- newvols %>%
 inner_join(volumes_6.0, by = "participant_id")
 
# Merging in interview date and age
newvols <- newvols %>%
  inner_join(interview, by = c("participant_id", "session_id"))
 
# Renaming interview date and age columns to match NDA guidelines
newvols <- newvols %>%
  rename(interview_date = ab_g_dyn__visit_dtt) %>%
    rename(interview_age = ab_g_dyn__visit_age)
 
# Adjusting Data format to match NDA guidelines
newvols <- newvols %>%
  mutate(interview_date = as.Date(interview_date)) %>%
    mutate(interview_date = format(as.Date(interview_date), "%m/%d/%Y"))

# Adjusting Age format to match NDA guidelines 
newvols <- newvols %>%
  mutate(interview_age = interview_age * 12) %>%
    mutate(interview_age = round(interview_age, digits = 0))

# Renaming + Reformating Subject IDs to NDA format
newvols <- newvols %>%
  rename(src_subject_id = participant_id) %>%
    mutate(src_subject_id = str_replace(src_subject_id, 'sub-', 'NDAR_INV')) %>%
      mutate(subjectkey = src_subject_id)
    
# Dropping Session ID Column
newvols <- subset(newvols, select = -c(session_id))

# Reorder columns
newvols <- newvols %>%
  select(subjectkey, everything())
newvols <- newvols %>%
  relocate(interview_date:interview_age, .after = src_subject_id)

# Append New Volumes to Old NDA Session
nda_vol_submission <- bind_rows(nda_vols, newvols)

# Drop Race and Ethnic Group Columns
nda_vol_submission <- subset(nda_vol_submission, select = -c(race, ethnic_group))

# Creating NDA Demographics Submission
newdemo <- newvols %>%
  select(subjectkey, src_subject_id, interview_date, interview_age, sex, race, ethnic_group)

# Adding new NDA Columns and filling with expected value
newdemo$phenotype <- "N/A"
newdemo$phenotype_description <- "N/A"
newdemo$twins_study <- "No"
newdemo$sibling_study <- "No"
newdemo$family_study <- "No"
newdemo$sample_taken <- "No"

# Append New Demo to Old NDA Session
nda_demo_submission <- bind_rows(nda_demo, newdemo)

# Write to .csv
write_csv(nda_vol_submission, paste0(sourcepath_nda, 'NDA_Vol_Resubmission_Jun26.csv'))
write_csv(nda_demo_submission, paste0(sourcepath_nda, 'NDA_Demo_Resubmission_Jun26.csv'))

