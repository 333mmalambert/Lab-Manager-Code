#############################
# 7.0_Priority_List.R
# Author: Emma Lambert
# Date: 7/13/2026
#############################

# The purpose of this code is to identify NEW usable t1 scans for ABCD 7.0.

# Import data and libraries
sourcepath_7.0 <- "Insert ABCD 7.0 Data Path"
sourcepath_6.0 <- "Insert ABCD 6.0 Data Path"
sourcepath_phen <- "Insert Phenotype Data Path"

library(tidyverse)

usable_scans_7.0 <- read_csv(paste0(sourcepath_7.0, '7.0_Usable_Scan_List.csv'))
usable_scans_6.0 <- read_csv(paste0(sourcepath_6.0, '6.0_Usable_Scan_List.csv'))
demo <- read_tsv(paste0(sourcepath_phen, 'ab_g_stc.tsv'))
ples <- read_tsv(paste0(sourcepath_phen, 'mh_y_pps.tsv'))

# Selecting relevant columns
demo <- demo %>%
  select(participant_id, ab_g_stc__cohort_sex)

ples <- ples %>%
  select(participant_id, session_id, mh_y_pps__severity_score) 

# Merging 6.0 and 7.0 data
usable_scans_merged <- usable_scans_7.0 %>%
  left_join(usable_scans_6.0, by = c("participant_id", "session_id"), suffix = c("_7.0", "_6.0")) %>%
    arrange(participant_id, session_id)

# Count # of scans per participant
usable_scans_merged <- usable_scans_merged %>%
  group_by(participant_id) %>%
  mutate(num7.0 = sum(!is.na(filename_7.0))) %>%
  mutate(num6.0 = sum(!is.na(filename_6.0)))

# Selecting relevant columns
usable_scans_merged <- usable_scans_merged %>%
  select(participant_id, session_id, filename_7.0, acq_time_6.0, acq_time_7.0, num7.0, num6.0)

# Counting number of new scans
usable_scans_merged <- usable_scans_merged %>%
  mutate(newscans = num7.0 - num6.0)

# Merging Biological Sex Data
usable_scans_merged <- usable_scans_merged %>%
  left_join(demo, by = "participant_id") %>%
  rename(sex = ab_g_stc__cohort_sex)

# Recode Biological Sex Data
usable_scans_merged <- usable_scans_merged %>%
  mutate(sex = recode(sex,
                      '1' = 'M',
                      '2' = 'F'))
# Merging in PLE Data
usable_scans_merged <- usable_scans_merged %>%
  left_join(ples, by = c("participant_id", "session_id")) %>%
  rename(ple_severity = mh_y_pps__severity_score)

# Separate 7.0 longitudinal and single timepoint
long_7.0 <- usable_scans_merged %>%
  group_by(participant_id) %>%
  filter(num7.0 > 1)

single_7.0 <- usable_scans_merged %>%
  group_by(participant_id) %>%
  filter(num7.0 == 1)

# Filtering for ONLY females
usable_scans_fem <- long_7.0 %>%
  group_by(participant_id) %>%
    filter(sex == "F")

# # Filtering for ONLY male
usable_scans_male <- long_7.0 %>%
  group_by(participant_id) %>%
  filter(sex == "M")

# Filtering by # of New Scans (Females)
fem_prio_list <- usable_scans_fem %>%
  group_by(participant_id) %>%
    filter(newscans > 0)

# Filtering by # of New Scans (Males)
male_prio_list <- usable_scans_male %>%
  group_by(participant_id) %>%
  filter(newscans > 0)

# Filtering by PLE Severity (Females)
fem_prio_list <- fem_prio_list %>%
  mutate(ple_severity = sum(ple_severity)) %>%
    arrange(-ple_severity)

# Filtering by PLE Severity (Males)
male_prio_list <- male_prio_list %>%
  mutate(ple_severity = sum(ple_severity)) %>%
  arrange(-ple_severity)

# Filtering for only new scans
fem_prio_list <- fem_prio_list %>%
  group_by(participant_id) %>%
    filter(is.na(acq_time_6.0))

male_prio_list <- male_prio_list %>%
  group_by(participant_id) %>%
  filter(is.na(acq_time_6.0))

# Filtering out Run-02 Scans
fem_prio_list_01 <- fem_prio_list %>%
  group_by(participant_id) %>%
   filter(grepl("run-01", filename_7.0))

fem_prio_list_02 <- fem_prio_list %>%
  group_by(participant_id) %>%
    filter(grepl("run-02", filename_7.0))

male_prio_list_01 <- male_prio_list %>%
  group_by(participant_id) %>%
  filter(grepl("run-01", filename_7.0))

male_prio_list_02 <- male_prio_list %>%
  group_by(participant_id) %>%
  filter(grepl("run-02", filename_7.0))

# Dropping Unnecessary Columns 
fem_prio_list_01 <- subset(fem_prio_list_01, select = -c(acq_time_6.0, num6.0, newscans))
fem_prio_list_02 <- subset(fem_prio_list_02, select = -c(acq_time_6.0, num6.0, newscans))

male_prio_list_01 <- subset(male_prio_list_01, select = -c(acq_time_6.0, num6.0, newscans))
male_prio_list_02 <- subset(male_prio_list_02, select = -c(acq_time_6.0, num6.0, newscans))


# Write to .tsv
write_tsv(fem_prio_list_01, paste0(sourcepath_7.0, '7.0_Hippo_Fem_Prio_List.tsv'))
write_tsv(male_prio_list_01, paste0(sourcepath_7.0, '7.0_Hippo_Male_Prio_List.tsv'))



