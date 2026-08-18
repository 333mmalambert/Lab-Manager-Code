#########################################
# CALC CBCL 6-18 Scoring 
# Author: Emma Lambert
# Date: 1/12/2026
#########################################

# Import libraries
library(tidyverse)
library(dplyr)

#Define source path
pathway <- "Insert Path to Data Here"

#Read in .csv
cbclCALC <- read_csv(paste0(pathway,'your_scores.csv'))

# Selecting only relevant Year 4 columns
cbcl_yr4_CALC <- cbclCALC %>%
  select(record_id,starts_with("yr4")) %>% #355 columns removed
   select(-c(2:65)) %>% # Identifiers and Competence Scales removed
     select(-contains(c("describe", "not_listed", "covid", "years_complete"), ignore.case = TRUE)) #86 columns removed
    
###################################
# Defining Syndrome Scale Sets 
###################################

# Anxious/Depressed Items
AD_items <- c("yr4_cbcl_618_cries", "yr4_cbcl_618_fearnotschool", "yr4_cbcl_618_fearschool",
                "yr4_cbcl_618_fearbad", "yr4_cbcl_618_perfect","yr4_cbcl_618_nolove", "yr4_cbcl_618_inferior",
                  "yr4_cbcl_618_tense", "yr4_cbcl_618_anxious", "yr4_cbcl_618_guilty",
                    "yr4_cbcl_618_selfconscious", "yr4_cbcl_618_suicide", "yr4_cbcl_618_worries")

# Withdrawn/Depressed Items 
WD_items <- c("yr4_cbcl_618_enjoy", "yr4_cbcl_618_alone", "yr4_cbcl_618_refusetalk",
                "yr4_cbcl_618_secretive", "yr4_cbcl_618_shy", "yr4_cbcl_618_underactive",
                  "yr4_cbcl_618_unhappy", "yr4_cbcl_618_withdrawn")

# Somatic Complaint Items
SC_items <- c("yr4_cbcl_618_nightmare", "yr4_cbcl_618_constipated", "yr4_cbcl_618_dizzy",
                "yr4_cbcl_618_overtired", "yr4_cbcl_618_aches", "yr4_cbcl_618_headaches",
                  "yr4_cbcl_618_nausea", "yr4_cbcl_618_eyes", "yr4_cbcl_618_rashes",
                      "yr4_cbcl_618_stomachache", "yr4_cbcl_618_vomit", "yr4_cbcl_618_physicalprob_other")

# Social Problem Items
SP_items <- c("yr4_cbcl_618_cling", "yr4_cbcl_618_lonley", "yr4_cbcl_618_get_along",
                "yr4_cbcl_618_jealous", "yr4_cbcl_618_outtoget", "yr4_cbcl_618_hurt",
                   "yr4_cbcl_618_teased", "yr4_cbcl_618_notliked", "yr4_cbcl_618_clumsy",
                     "yr4_cbcl_618_younger_kids", "yr4_cbcl_618_speech")

# Thought Problem Items
TP_items <- c("yr4_cbcl_618_obsess", "yr4_cbcl_618_selfharm", "yr4_cbcl_618_voices",
                "yr4_cbcl_618_twitch", "yr4_cbcl_618_picks", "yr4_cbcl_618_sexparts_public",
                  "yr4_cbcl_618_sexparts_much", "yr4_cbcl_618_compulsion", "yr4_cbcl_618_seesthings",
                    "yr4_cbcl_618_sleepsless", "yr4_cbcl_618_stores", "yr4_cbcl_618_strangebehavior",
                      "yr4_cbcl_618_strangeideas", "yr4_cbcl_618_sleepwalk", "yr4_cbcl_618_sleep")

# Attention Problem Items
AP_items <- c("yr4_cbcl_618_acts_young", "yr4_cbcl_618_finish", "yr4_cbcl_618_concentrate",
                "yr4_cbcl_618_hyper", "yr4_cbcl_618_confused", "yr4_cbcl_618_destroys_daydream",
                  "yr4_cbcl_618_impulsive", "yr4_cbcl_618_schoolwork", "yr4_cbcl_618_inattentive",
                    "yr4_cbcl_618_blankstare")

# Rule-Breaking Items
RB_items <- c("yr4_cbcl_618_alcohol", "yr4_cbcl_618_noguilt", "yr4_cbcl_618_breakrule",
                "yr4_cbcl_618_otherstrouble", "yr4_cbcl_618_lying", "yr4_cbcl_618_olderkids",
                  "yr4_cbcl_618_runsaway", "yr4_cbcl_618_fires", "yr4_cbcl_618_sexualprobs",
                    "yr4_cbcl_618_stealshome", "yr4_cbcl_618_stealsoutside", "yr4_cbcl_618_swears",
                      "yr4_cbcl_618_thinkssex", "yr4_cbcl_618_tobacco", "yr4_cbcl_618_truancy",
                        "yr4_cbcl_618_drugs", "yr4_cbcl_618_vandalism")

# Aggressive Behavior Items
AB_items <- c("yr4_cbcl_618_argues", "yr4_cbcl_618_bully", "yr4_cbcl_618_attention",
                "yr4_cbcl_618_destroyown", "yr4_cbcl_618_destroyother", "yr4_cbcl_618_disobedienthome",
                  "yr4_cbcl_618_disobedientschool", "yr4_cbcl_618_fights", "yr4_cbcl_618_attack",
                    "yr4_cbcl_618_screams", "yr4_cbcl_618_stubborn", "yr4_cbcl_618_moodchange",
                      "yr4_cbcl_618_sulks", "yr4_cbcl_618_suspicious", "yr4_cbcl_618_tease",
                        "yr4_cbcl_618_temper", "yr4_cbcl_618_threatens", "yr4_cbcl_618_loud")

# Other Problem Items
other_items <- c("yr4_cbcl_618_outsidetoilet", "yr4_cbcl_618_brag", "yr4_cbcl_618_cruel",
                      "yr4_cbcl_618_eat", "yr4_cbcl_618_bitenails", "yr4_cbcl_618_overeat",
                        "yr4_cbcl_618_overweight", "yr4_cbcl_618_physicalprob_other", "yr4_cbcl_618_showoff",
                          "yr4_cbcl_618_sleepsmore", "yr4_cbcl_618_talk", "yr4_cbcl_618_suckthumb",
                            "yr4_cbcl_618_wetsself", "yr4_cbcl_618_whine", "yr4_cbcl_618_wishesoppositesex")

###################################
# Defining DSM-Oriented Scale Sets
###################################

# Depressive Problem Items
dep_items <- c("yr4_cbcl_618_enjoy", "yr4_cbcl_618_cries", "yr4_cbcl_618_selfharm",
                "yr4_cbcl_618_eat", "yr4_cbcl_618_inferior", "yr4_cbcl_618_guilty",
                  "yr4_cbcl_618_overtired", "yr4_cbcl_618_sleepsless", "yr4_cbcl_618_sleepsmore",
                    "yr4_cbcl_618_suicide", "yr4_cbcl_618_sleep", "yr4_cbcl_618_underactive",
                      "yr4_cbcl_618_unhappy")

# Anxiety Problem Items
anx_items <- c("yr4_cbcl_618_cling", "yr4_cbcl_618_fearnotschool", "yr4_cbcl_618_fearschool",
                "yr4_cbcl_618_fearbad", "yr4_cbcl_618_tense", "yr4_cbcl_618_nightmare",
                 "yr4_cbcl_618_anxious", "yr4_cbcl_618_selfconscious", "yr4_cbcl_618_worries")

# Somatic Problem Items
som_items <- c("yr4_cbcl_618_constipated", "yr4_cbcl_618_dizzy","yr4_cbcl_618_overtired", 
                "yr4_cbcl_618_aches", "yr4_cbcl_618_headaches", "yr4_cbcl_618_nausea", 
                  "yr4_cbcl_618_eyes", "yr4_cbcl_618_rashes","yr4_cbcl_618_stomachache", 
                    "yr4_cbcl_618_vomit", "yr4_cbcl_618_physicalprob_other")

# Attention Deficit Items
attn_items <- c("yr4_cbcl_618_finish", "yr4_cbcl_618_concentrate", "yr4_cbcl_618_hyper",
                  "yr4_cbcl_618_impulsive", "yr4_cbcl_618_inattentive", "yr4_cbcl_618_talk", "yr4_cbcl_618_loud")

# Oppositional Defiant Items
oppdef_items <- c("yr4_cbcl_618_argues", "yr4_cbcl_618_disobedienthome", "yr4_cbcl_618_disobedientschool",
                    "yr4_cbcl_618_stubborn", "yr4_cbcl_618_temper")

# Conduct Problem Items
conduct_items <- c("yr4_cbcl_618_cruel", "yr4_cbcl_618_bully", "yr4_cbcl_618_destroyother",
                    "yr4_cbcl_618_noguilt", "yr4_cbcl_618_breakrule", "yr4_cbcl_618_fights",
                        "yr4_cbcl_618_otherstrouble", "yr4_cbcl_618_lying", "yr4_cbcl_618_attack",
                          "yr4_cbcl_618_runsaway", "yr4_cbcl_618_fires", "yr4_cbcl_618_stealshome",
                            "yr4_cbcl_618_stealsoutside", "yr4_cbcl_618_stealsoutside", "yr4_cbcl_618_swears",
                                "yr4_cbcl_618_threatens", "yr4_cbcl_618_truancy", "yr4_cbcl_618_vandalism")

# Sluggish Cognitive Tempo Items
slugcog_items <- c("yr4_cbcl_618_confused", "yr4_cbcl_618_destroys_daydream", "yr4_cbcl_618_blankstare",
                    "yr4_cbcl_618_underactive")

# Obsessive-Compulsive Items
obcomp_items <- c("yr4_cbcl_618_obsess", "yr4_cbcl_618_fearbad", "yr4_cbcl_618_perfect",
                    "yr4_cbcl_618_guilty", "yr4_cbcl_618_compulsion", "yr4_cbcl_618_strangebehavior",
                        "yr4_cbcl_618_strangeideas", "yr4_cbcl_618_worries")

# Stress Items
stress_items <- c("yr4_cbcl_618_argues", "yr4_cbcl_618_concentrate", "yr4_cbcl_618_obsess",
                    "yr4_cbcl_618_cling", "yr4_cbcl_618_fearbad", "yr4_cbcl_618_outtoget",
                      "yr4_cbcl_618_tense", "yr4_cbcl_618_nightmare", "yr4_cbcl_618_anxious", 
                        "yr4_cbcl_618_guilty", "yr4_cbcl_618_secretive", "yr4_cbcl_618_moodchange",
                          "yr4_cbcl_618_unhappy", "yr4_cbcl_618_withdrawn")

############################
# Summing of Syndrome Scales 
############################

# Summing Anxious/Depressed Subscale
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_AD_sum = rowSums(select(., all_of(AD_items)), na.rm = TRUE))

# QC: Flagging rows with impossible values (13 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_AD_sum < 0 | yr4_AD_sum > 26) # 0 rows

# Summing Withdrawn/Depressed Subscale
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_WD_sum = rowSums(select(., all_of(WD_items)), na.rm = TRUE))

# QC: (8 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_WD_sum < 0 | yr4_WD_sum > 16) # 0 rows

# Summing Somatic Complaints Subscales
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_SC_sum = rowSums(select(., all_of(SC_items)), na.rm = TRUE))

# QC: (12 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_SC_sum < 0 | yr4_SC_sum > 24) # 0 rows

# Summing Social Problems Subscales
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_SP_sum = rowSums(select(., all_of(SP_items)), na.rm = TRUE))

# QC: (11 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_SP_sum < 0 | yr4_SP_sum > 22) # 0 rows

# Summing Thought Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_TP_sum = rowSums(select(., all_of(TP_items)), na.rm = TRUE))

# QC: (15 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_TP_sum < 0 | yr4_TP_sum > 30) # 0 rows

# Summing Attention Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_AP_sum = rowSums(select(., all_of(AP_items)), na.rm = TRUE))

# QC: (10 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_AP_sum < 0 | yr4_AP_sum > 20) # 0 rows

# Summing Rule-Breaking Behaviors
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_RB_sum = rowSums(select(., all_of(RB_items)), na.rm = TRUE))

# QC: (17 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_RB_sum < 0 | yr4_RB_sum > 34) # 0 rows

# Summing Aggressive Behaviors
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_AB_sum = rowSums(select(., all_of(AB_items)), na.rm = TRUE))

# QC: (18 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_AB_sum < 0 | yr4_AB_sum > 36) # 0 rows

###################################################################
# Summing of Internalizing, Externalizing, and Other Problem Scales
###################################################################

# Summing Internalizing Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_internalizing_sum = rowSums(cbcl_yr4_CALC[,
    c("yr4_AD_sum", "yr4_WD_sum", "yr4_SC_sum")],
     na.rm = TRUE))

# QC: (32 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_internalizing_sum < 0 | yr4_internalizing_sum > 64) # 0 rows

# Summing Externalizing Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_externalizing_sum = rowSums(cbcl_yr4_CALC[,
   c("yr4_RB_sum", "yr4_AB_sum")],
    na.rm = TRUE))

# QC: (35 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_externalizing_sum < 0 | yr4_externalizing_sum > 70) # 0 rows

# Summing Other Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_other_sum = rowSums(select(., all_of(other_items)), na.rm = TRUE))

# QC: (16 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_other_sum < 0 | yr4_other_sum > 32) # 0 rows

###############################
# Summing of DSM-Oriented Scales
###############################

# Summing Depressive Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_dep_sum = rowSums(select(., all_of(dep_items)), na.rm = TRUE))

# QC: (13 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_dep_sum < 0 | yr4_dep_sum > 26) # 0 rows

# Summing Anxiety Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_anx_sum = rowSums(select(., all_of(anx_items)), na.rm = TRUE))

# QC: (9 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_anx_sum < 0 | yr4_anx_sum > 18) # 0 rows

# Summing Somatic Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_somatic_sum = rowSums(select(., all_of(som_items)), na.rm = TRUE))

# QC: (8 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_somatic_sum < 0 | yr4_somatic_sum > 16) # 0 rows

# Summing Attention-Deficit Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_attndef_sum = rowSums(select(., all_of(attn_items)), na.rm = TRUE))

# QC: (7 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_attndef_sum < 0 | yr4_attndef_sum > 14) # 0 rows

# Summing Oppositional Defiant Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_oppdef_sum = rowSums(select(., all_of(oppdef_items)), na.rm = TRUE))

# QC: (5 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_oppdef_sum < 0 | yr4_oppdef_sum > 10) # 0 rows

# Summing Conduct Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_conduct_sum = rowSums(select(., all_of(conduct_items)), na.rm = TRUE))

# QC: (17 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_conduct_sum < 0 | yr4_conduct_sum > 34) # 0 rows

# Summing Sluggish Cognitive Tempo Items
cbcl_yr4_CALC<- cbcl_yr4_CALC %>%
  mutate(yr4_slugcog_sum = rowSums(select(., all_of(slugcog_items)),na.rm = TRUE))

# QC: (4 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_slugcog_sum < 0 | yr4_slugcog_sum > 8) # 0 rows

# Summing Obsessive-Compulsive Problems
cbcl_yr4_CALC <-cbcl_yr4_CALC %>%
  mutate(yr4_obcomp_sum = rowSums(select(., all_of(obcomp_items)), na.rm = TRUE))

# QC: (8 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_obcomp_sum < 0 | yr4_obcomp_sum > 16) # 0 rows

# Summing Stress Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(yr4_stress_sum = rowSums(select(., all_of(stress_items)), na.rm = TRUE))

# QC: (14 items total; scored 0, 1, or 2) 
cbcl_yr4_CALC %>%
  filter(yr4_attndef_sum < 0 | yr4_attndef_sum > 28) # 0 rows

############################################
# Summing Total Problems and Writing to .csv 
############################################

# Identifying CBCL item columns by name containing "618" 
item_cols <- cbcl_yr4_CALC %>%
  select(matches("618", ignore.case = TRUE)) 

# Summing Total Problems
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
    mutate(yr4_totalprobs_sum = rowSums(item_cols, na.rm = TRUE))

#Flagging rows with impossible values (119 items total; scored 0, 1, or 2) 
impossible <- cbcl_yr4_CALC %>%
  filter(yr4_totalprobs_sum < 0 | yr4_totalprobs_sum > 238)

# Marking rows as NA based on CBCL 6-18 Items 1 + 2
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  mutate(across(
    -record_id,  #excludes Record ID column
    ~ifelse(
      is.na(yr4_cbcl_618_acts_young) & is.na(yr4_cbcl_618_alcohol),
      NA, .)))

# Count # of participant's with missing Year 4 data
sum(is.na(cbcl_yr4_CALC$yr4_totalprobs_sum))  # n = 139

# Select only relevant scored columns
cbcl_yr4_CALC <- cbcl_yr4_CALC %>%
  select(record_id, contains("sum"))

# Write Year 4 Data to .csv
write_csv(cbcl_yr4_CALC,paste0(pathway,'yours_scored.csv'))


  

