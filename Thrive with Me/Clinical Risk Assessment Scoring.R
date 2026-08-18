###################################
# Clinical Risk Assessment Scoring
# Date: 6/18/2026; Updated 8/3/2026
# Author: Emma Lambert
####################################

# Importing packages
library(tidyverse)

# Setting Working Directory
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/Damme, Katherine Steffen's files - ThriveWithMe/Phase 1 (18-25)/Data/Part 1/REDCap")

# Reading in TWM Part 1 Data (This will need to be updated with each new data report)
TWM_Data <- read_csv("ThriveWithMeScreener_DATA_2026-08-03_1007.csv")


##############################################
# Selecting and Cleaning Beck Depression Data
#############################################

# Selecting relevant columns
BDI <- TWM_Data %>%
  select(participant_id, contains("bdi"))


# Creating vector of columns
bdi_cols <- c("bdi_q1_sadness", 
              "bdi_q2_discouraged", 
              "bdi_q3_failure",
              "bdi_q4_satisfaction", 
              "bdi_q5_guilt", 
              "bdi_q6_punishment", 
              "bdi_q7_disappointment", 
              "bdi_q8_blame",
              "bdi_q9_cry",
              "bdi_q10_irritable",
              "bdi_q11_interest",
              "bdi_q12_decisions",
              "bdi_q13_physicality",
              "bdi_q14_work_ethic",
              "bdi_q15_sleep",
              "bdi_q16_tired",
              "bdi_q17_appetite",
              "bdi_q18_weight", 
              "bdi_q19_health_worry",
              "bdi_q20_sex_interest")

###########################
# Scoring and Categorizing
##########################

# Summing Total BDI Score
BDI <- BDI %>%
  mutate(bdi_score = rowSums(across(all_of(bdi_cols)), na.rm = TRUE))

# Categorizing as Clinical High Risk or Healthy Control 
BDI <- BDI %>%
  mutate(chr_or_hc_dep = if_else(
    bdi_score >= 17, 1, 0)) # 1 = CHR, 0 = HC

# Moving necessary columns to CHR vs. HC dataframe
CHR_HC <- BDI %>%
  select(participant_id, chr_or_hc_dep)


#############################################
# # Selecting and Cleaning Beck Anxiety Data
############################################

# Selecting relevant columns
BAI <- TWM_Data %>%
  select(participant_id, contains("bai"))

# Creating vector of columns
bai_cols <- c("bai_1_numbness",
         "bai_2_hot",
         "bai_3_wobble",
         "bai_4_relaxation",
         "bai_5_fear_worst",
         "bai_6_dizzy",
         "bai_7_heart_race",
         "bai_8_unsteady",
         "bai_9_terrified",
         "bai_10_nervousness",
         "bai_11_choking",
         "bai_12_trembling",
         "bai_13_unsteady",
         "bai_14_losing_control",
         "bai_15_breathing",
         "bai_16_fear_dying",
         "bai_17_scared",
         "bai_18_indigestion",
         "bai_19_lightheaded",
         "bai_20_flushed",
         "bai_21_sweats")

###########################
# Scoring and Categorizing
##########################

# Summing Total BAI Score
BAI <- BAI %>%
  mutate(bai_score = rowSums(across(all_of(bai_cols)), na.rm = TRUE))

# Categorizing as Clinical High Risk or Healthy Control 
BAI <- BAI %>%
  mutate(chr_or_hc_anx = if_else(
    bai_score >= 16, 1, 0)) # 1 = CHR, 0 = HC

# Merging BAI columns to CHR vs. HC dataframe
CHR_HC <- merge(CHR_HC, BAI, by = "participant_id")

####################################
# # Selecting and Cleaning CAPE Data
####################################

# Selecting relevant columns
CAPE <- TWM_Data %>%
  select(participant_id, contains("cape"))

# Creating vector of frequency columns
freq_cols <- c("cape42_q2_double_meanings",
               "cape42_q5_tv",
               "cape42_q6_doubt",
               "cape42_q7_persecute",
               "cape42_q10_conspiracy",
               "cape42_q11_destiny",
               "cape42_q13_special",
               "cape42_q15_telepathy",
               "cape42_q17_electronics",
               "cape42_q20_supernatural",
               "cape42_q22_appearance",
               "cape42_q24_thoughts",
               "cape42_q26_other_thoughts",
               "cape42_q28_vivid",
               "cape42_q30_echo",
               "cape42_q31_control",
               "cape42_q33_voice",
               "cape42_q34_two_voices",
               "cape42_q41_double",
               "cape42_q42_hallucinate")

# Creating vector of distress columns
dist_cols <- c("cape42_q2b_mean_distress",
               "cape42_q5b_tv_distress",
               "cape42_q6b_doubt_distress",
               "cape42_q7b_per_distress",
               "cape42_q10b_conspir_distress",
               "cape42_q11b_destiny_distress",
               "cape42_q13b_special_distress",
               "cape42_q15b_tele_distress",
               "cape42_q17b_elec_distress",
               "cape42_q20b_super_distress",
               "cape42_q22b_appear_distress",
               "cape42_q24b_thoughts_distress",
               "cape42_q26b_other_thoughts_distress",
               "cape42_q28b_vivid_distress",
               "cape42_q30b_echo_distress",
               "cape42_q31b_control_distress",
               "cape42_q33b_voice_distress",
               "cape42_q34b_two_voices_distress",
               "cape42_q41b_double_distress",
               "cape42_q42b_hallucinate_distress")

###########################
# Scoring and Categorizing
##########################

# Summing Frequency Score
CAPE <- CAPE %>%
  mutate(frequency_score = rowSums(across(all_of(freq_cols)), na.rm = TRUE))

# Summing Distress Score
CAPE <- CAPE %>%
  mutate(distress_score = rowSums(across(all_of(dist_cols)), na.rm = TRUE))

# Counting # of Distressing Experiences 
CAPE <- CAPE %>%
  mutate(distress_count = rowSums(across(all_of(dist_cols), ~ .x > 0), na.rm = TRUE))

# Categorizing as Clinical High Risk or Healthy Control 
CAPE <- CAPE %>%
  mutate(chr_or_hc_psy = if_else(
    distress_count >= 9, 1, 0)) # 1 = CHR, 0 = HC

# Moving necessary columns to CHR vs. HC dataframe
CHR_HC <- merge(CHR_HC, CAPE, by = "participant_id")

# Cleaning CHR vs. HC 
CHR_HC <- CHR_HC %>%
  select(participant_id, contains("chr"))
  
####################################
# # Selecting and Cleaning HPS Data
####################################

# Selecting relevant columns
HPS <- TWM_Data %>%
  select(participant_id, contains("hps"))


# Creating vector of columns
hps_cols <- c("hps_q1_average",
              "hps_q2_clown",
              "hps_q3_hyper",
              "hps_q4_comedian",
              "hps_q5_rapid_ideas",
              "hps_q6_center_attention",
              "hps_q7_assertive",
              "hps_q8_restless",
              "hps_q9_eccentric",
              "hps_q10_emotion_intensity",
              "hps_q11_elated",
              "hps_q12_milestone",
              "hps_q13_clever",
              "hps_q14_self_aware",
              "hps_q15_random_happiness",
              "hps_q16_book",
              "hps_q17_avg_mood",
              "hps_q18_high_moods",
              "hps_q19_various_interest",
              "hps_q20_little_sleep",
              "hps_q21_mood_flux",
              "hps_q22_do_it_all",
              "hps_q23_high_aspiration",
              "hps_q24_happy_reason",
              "hps_q25_social_comfort",
              "hps_q26_actor",
              "hps_q27_normal",
              "hps_q28_writing_ideas",
              "hps_q29_adventurous",
              "hps_q30_politician",
              "hps_q31_slow_down",
              "hps_q32_considered_hyper",
              "hps_q33_giddy",
              "hps_q34_success",
              "hps_q35_above_rules",
              "hps_q36_charm",
              "hps_q37_mood_change",
              "hps_q38_racing_thoughts",
              "hps_q39_controlling",
              "hps_q40_sociability",
              "hps_q41_inspiration",
              "hps_q42_persuasion",
              "hps_q43_forget_needs",
              "hps_q44_irate",
              "hps_q45_happy_irate_mix",
              "hps_q46_talking",
              "hps_q47_ordinary",
              "hps_q48_forgotten")

###########################
# Scoring and Categorizing
##########################

# Summing Total HPS Score
HPS <- HPS %>%
  mutate(hps_score = rowSums(across(all_of(hps_cols)), na.rm = TRUE))

# Categorizing as Clinical High Risk or Healthy Control 
HPS <- HPS %>%
  mutate(chr_or_hc_bpd = if_else(
    hps_score >= 36, 1, 0)) # 1 = CHR, 0 = HC

# Moving necessary columns to CHR vs. HC dataframe
CHR_HC <- merge(CHR_HC, HPS, by = "participant_id")

# Cleaning CHR vs. HC 
CHR_HC <- CHR_HC %>%
  select(participant_id, contains("chr"))


###########################################
# Finalizing CHR vs. HC and Writing to .csv
###########################################

# Setting Working Directory
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/Damme, Katherine Steffen's files - ThriveWithMe/Phase 1 (18-25)/Data/Part 1/Scored Data/Output/Phase 2 Eligibility (ASSIST)")

# Reading in Inclusion Status Data 
inclusion <- read.csv("Phase2_Inclusion_Status.csv")

# Vector of chr_or_hc columns
chr_hc_cols <- c("chr_or_hc_dep",
                 "chr_or_hc_anx",
                 "chr_or_hc_psy",
                 "chr_or_hc_bpd")

# Creating a Clinical High Risk vs. Healthy Control column
CHR_HC <- CHR_HC %>%
  mutate(chr_Y1N0 = if_else(
    if_any(any_of(chr_hc_cols), ~ .x == 1), 1, 0)) # 1 = CHR, 0 = HC

# Merging Inclusion and CHR vs. HC Data
CHR_HC <- merge(CHR_HC, inclusion, by = "participant_id")

# Selecting only included participants
CHR_HC <- filter(CHR_HC, inclusion_status == 1)

# Count of participants in each psychopathology group
# Depression
CHR_HC %>%
  count(chr_or_hc_dep) # n = 8
# Anxiety
CHR_HC %>%
  count(chr_or_hc_anx) # n = 8
# Psychosis
CHR_HC %>%
  count(chr_or_hc_psy) # n = 3
# Bipolar Disorder
CHR_HC %>%
  count(chr_or_hc_bpd) # n = 1
# CHR or HC  
CHR_HC %>%
  count(chr_Y1N0) # CHR: n = 11, HC: n = 16

# Setting Output Directory
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/Damme, Katherine Steffen's files - ThriveWithMe/Phase 1 (18-25)/Data/Part 1/Scored Data/Output/CHR vs. HC")

# Writing to .csv
write_csv(CHR_HC,'CHR_or_HC.csv')
