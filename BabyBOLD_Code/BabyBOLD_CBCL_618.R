#########################################
# BabyBOLD CBCL 6-18 Scoring 
# Author: Emma Lambert
# Date: 1/22/2026
#########################################

# Import libraries
library(tidyverse)
library(dplyr)

#Define source path
pathway <- # Insert Data Path #

#Read in .csv
cbclBEG <- read_csv(paste0(pathway,'your.csv'))
cbclREA <- read_csv(paste0(pathway,'your.csv'))

# Removing columns not relevant to scoring
cbclBEG <- cbclBEG %>%
  select(-c(2:65)) %>% # Identifiers and Competence Scales removed
    select(-contains(c("describe", "not_listed", "covid", "years_complete"), ignore.case = TRUE)) #22 columns removed

cbclREA <- cbclREA %>%
  select(-c(2:65)) %>% # Identifiers and Competence Scales removed
   select(-contains(c("describe", "not_listed","covid", "years_complete"), ignore.case = TRUE)) #22 columns removed

###################################
# Defining Syndrome Scale Sets 
###################################

# Anxious/Depressed Items
AD_items <- c("cbcl_cries", "cbcl_fearnotschool", "cbcl_fearschool",
              "cbcl_fearbad", "cbcl_perfect","cbcl_nolove", "cbcl_inferior",
              "cbcl_tense", "cbcl_anxious", "cbcl_guilty",
              "cbcl_selfconscious", "cbcl_suicide", "cbcl_worries")

# Build AD items for Beginner Reader (BEG) and Reader (REA) time points
AD_items_BEG <- paste0("beg_", AD_items)
AD_items_REA <- paste0("rea_", AD_items)

# Withdrawn/Depressed Items 
WD_items <- c("cbcl_enjoy", "cbcl_alone", "cbcl_refusetalk",
              "cbcl_secretive", "cbcl_shy", "cbcl_underactive",
              "cbcl_unhappy", "cbcl_withdrawn")

# Build WD items for BEG and REA time points
WD_items_BEG <- paste0("beg_", WD_items)
WD_items_REA <- paste0("rea_", WD_items)

# Somatic Complaint Items
SC_items <- c("cbcl_nightmare", "cbcl_constipated", "cbcl_dizzy",
              "cbcl_overtired", "cbcl_aches", "cbcl_headaches",
              "cbcl_nausea", "cbcl_eyes", "cbcl_rashes",
              "cbcl_stomachache", "cbcl_vomit", "cbcl_physicalprob_other")

# Build SC items for BEG and REA time points
SC_items_BEG <- paste0("beg_", SC_items)
SC_items_REA <- paste0("rea_", SC_items)

# Social Problem Items
SP_items <- c("cbcl_cling", "cbcl_lonley", "cbcl_get_along",
              "cbcl_jealous", "cbcl_outtoget", "cbcl_hurt",
              "cbcl_teased", "cbcl_notliked", "cbcl_clumsy",
              "cbcl_younger_kids", "cbcl_speech")

# Build SP items for BEG and REA time points
SP_items_BEG <- paste0("beg_", SP_items)
SP_items_REA <- paste0("rea_", SP_items)

# Thought Problem Items
TP_items <- c("cbcl_obsess", "cbcl_selfharm", "cbcl_voices",
              "cbcl_twitch", "cbcl_picks", "cbcl_sexparts_public",
              "cbcl_sexparts_much", "cbcl_compulsion", "cbcl_seesthings",
              "cbcl_sleepsless", "cbcl_stores", "cbcl_strangebehavior",
              "cbcl_strangeideas", "cbcl_sleepwalk", "cbcl_sleep")

# Build TP items for BEG and REA time points
TP_items_BEG <- paste0("beg_", TP_items)
TP_items_REA <- paste0("rea_", TP_items)

# Attention Problem Items
AP_items <- c("cbcl_acts_young", "cbcl_finish", "cbcl_concentrate",
              "cbcl_hyper", "cbcl_confused", "cbcl_destroys_daydream",
              "cbcl_impulsive", "cbcl_schoolwork", "cbcl_inattentive",
              "cbcl_blankstare")

# Build AP items for BEG and REA time points
AP_items_BEG <- paste0("beg_", AP_items)
AP_items_REA <- paste0("rea_", AP_items)

# Rule-Breaking Items
RB_items <- c("cbcl_alcohol", "cbcl_noguilt", "cbcl_breakrule",
              "cbcl_otherstrouble", "cbcl_lying", "cbcl_olderkids",
              "cbcl_runsaway", "cbcl_fires", "cbcl_sexualprobs",
              "cbcl_stealshome", "cbcl_stealsoutside", "cbcl_swears",
              "cbcl_thinkssex", "cbcl_tobacco", "cbcl_truancy",
              "cbcl_drugs", "cbcl_vandalism")

# Build RB items for BEG and REA time points
RB_items_BEG <- paste0("beg_", RB_items)
RB_items_REA <- paste0("rea_", RB_items)

# Aggressive Behavior Items
AB_items <- c("cbcl_argues", "cbcl_bully", "cbcl_attention",
              "cbcl_destroyown", "cbcl_destroyother", "cbcl_disobedienthome",
              "cbcl_disobedientschool", "cbcl_fights", "cbcl_attack",
              "cbcl_screams", "cbcl_stubborn", "cbcl_moodchange",
              "cbcl_sulks", "cbcl_suspicious", "cbcl_tease",
              "cbcl_temper", "cbcl_threatens", "cbcl_loud")

# Build AB items for BEG and REA time points
AB_items_BEG <- paste0("beg_", AB_items)
AB_items_REA <- paste0("rea_", AB_items)

# Other Problem Items
other_items <- c("cbcl_outsidetoilet", "cbcl_brag", "cbcl_cruel",
                 "cbcl_eat", "cbcl_bitenails", "cbcl_overeat",
                 "cbcl_overweight", "cbcl_physicalprob_other", "cbcl_showoff",
                 "cbcl_sleepsmore", "cbcl_talk", "cbcl_suckthumb",
                 "cbcl_wetsself", "cbcl_whine", "cbcl_wishesoppositesex")

# Build Other items for BEG and REA time points
other_items_BEG <- paste0("beg_", other_items)
other_items_REA <- paste0("rea_", other_items)

###################################
# Defining DSM-Oriented Scale Sets
###################################

# Depressive Problem Items
dep_items <- c("cbcl_enjoy", "cbcl_cries", "cbcl_selfharm",
               "cbcl_eat", "cbcl_inferior", "cbcl_guilty",
               "cbcl_overtired", "cbcl_sleepsless", "cbcl_sleepsmore",
               "cbcl_suicide", "cbcl_sleep", "cbcl_underactive",
               "cbcl_unhappy")

# Build Depressive items for BEG and REA time points
dep_items_BEG <- paste0("beg_", dep_items)
dep_items_REA <- paste0("rea_", dep_items)

# Anxiety Problem Items
anx_items <- c("cbcl_cling", "cbcl_fearnotschool", "cbcl_fearschool",
               "cbcl_fearbad", "cbcl_tense", "cbcl_nightmare",
               "cbcl_anxious", "cbcl_selfconscious", "cbcl_worries")

# Build Anxiety items for BEG and REA time points
anx_items_BEG <- paste0("beg_", anx_items)
anx_items_REA <- paste0("rea_", anx_items)

# Somatic Problem Items
som_items <- c("cbcl_constipated", "cbcl_dizzy","cbcl_overtired", 
               "cbcl_aches", "cbcl_headaches", "cbcl_nausea", 
               "cbcl_eyes", "cbcl_rashes","cbcl_stomachache", 
               "cbcl_vomit", "cbcl_physicalprob_other")

# Build Somatic items for BEG and REA time points 
som_items_BEG <- paste0("beg_", som_items)
som_items_REA <- paste0("rea_", som_items)

# Attention Deficit Items
attn_items <- c("cbcl_finish", "cbcl_concentrate", "cbcl_hyper",
                "cbcl_impulsive", "cbcl_inattentive", "cbcl_talk", "cbcl_loud")

# Build Attention items for BEG and REA time points
attn_items_BEG <- paste0("beg_", attn_items)
attn_items_REA <- paste0("rea_", attn_items)

# Oppositional Defiant Items
oppdef_items <- c("cbcl_argues", "cbcl_disobedienthome", "cbcl_disobedientschool",
                  "cbcl_stubborn", "cbcl_temper")

# Build Oppositional items for BEG and REA time points
oppdef_items_BEG <- paste0("beg_", oppdef_items)
oppdef_items_REA <- paste0("rea_", oppdef_items)

# Conduct Problem Items
conduct_items <- c("cbcl_cruel", "cbcl_bully", "cbcl_destroyother",
                   "cbcl_noguilt", "cbcl_breakrule", "cbcl_fights",
                   "cbcl_otherstrouble", "cbcl_lying", "cbcl_attack",
                   "cbcl_runsaway", "cbcl_fires", "cbcl_stealshome",
                   "cbcl_stealsoutside", "cbcl_stealsoutside", "cbcl_swears",
                   "cbcl_threatens", "cbcl_truancy", "cbcl_vandalism")

# Build Conduct items for BEG and REA time points
conduct_items_BEG <- paste0("beg_", conduct_items)
conduct_items_REA <- paste0("rea_", conduct_items)

# Sluggish Cognitive Tempo Items
slugcog_items <- c("cbcl_confused", "cbcl_destroys_daydream", "cbcl_blankstare",
                   "cbcl_underactive")

# Build Cognitive items for BEG and REA time points
slugcog_items_BEG <- paste0("beg_", slugcog_items)
slugcog_items_REA <- paste0("rea_", slugcog_items)

# Obsessive-Compulsive Items
obcomp_items <- c("cbcl_obsess", "cbcl_fearbad", "cbcl_perfect",
                  "cbcl_guilty", "cbcl_compulsion", "cbcl_strangebehavior",
                  "cbcl_strangeideas", "cbcl_worries")

# Build Obsessive items for BEG and REA time points
obcomp_items_BEG <- paste0("beg_", obcomp_items)
obcomp_items_REA <- paste0("rea_", obcomp_items)

# Stress Items
stress_items <- c("cbcl_argues", "cbcl_concentrate", "cbcl_obsess",
                  "cbcl_cling", "cbcl_fearbad", "cbcl_outtoget",
                  "cbcl_tense", "cbcl_nightmare", "cbcl_anxious", 
                  "cbcl_guilty", "cbcl_secretive", "cbcl_moodchange",
                  "cbcl_unhappy", "cbcl_withdrawn")

# Build Stress items for BEG and REA time points
stress_items_BEG <- paste0("beg_", stress_items)
stress_items_REA <- paste0("rea_", stress_items)

############################
# Summing of Syndrome Scales 
############################

# Summing Anxious/Depressed Items
cbclBEG <- cbclBEG %>%
  mutate(BEG_AD_sum = rowSums(select(., all_of(AD_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_AD_sum = rowSums(select(., all_of(AD_items_REA)), na.rm = TRUE))

# Summing Withdrawn/Depressed Items
cbclBEG <- cbclBEG %>%
  mutate(BEG_WD_sum = rowSums(select(., all_of(WD_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_WD_sum = rowSums(select(., all_of(WD_items_REA)), na.rm = TRUE))

# Summing Somatic Complaints
cbclBEG <- cbclBEG %>%
  mutate(BEG_SC_sum = rowSums(select(., all_of(SC_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_SC_sum = rowSums(select(., all_of(SC_items_REA)), na.rm = TRUE))

# Summing Social Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_SP_sum = rowSums(select(., all_of(SP_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_SP_sum = rowSums(select(., all_of(SP_items_REA)), na.rm = TRUE))

# Summing Thought Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_TP_sum = rowSums(select(., all_of(TP_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_TP_sum = rowSums(select(., all_of(TP_items_REA)), na.rm = TRUE))

# Summing Attention Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_AP_sum = rowSums(select(., all_of(AP_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_AP_sum = rowSums(select(., all_of(AP_items_REA)), na.rm = TRUE))

# Summing Rule-Breaking Behavior Items
cbclBEG <- cbclBEG %>%
  mutate(BEG_RB_sum = rowSums(select(., all_of(RB_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_RB_sum = rowSums(select(., all_of(RB_items_REA)), na.rm = TRUE))

# Summing Aggressive Behavior Items
cbclBEG <- cbclBEG %>%
  mutate(BEG_AB_sum = rowSums(select(., all_of(AB_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_AB_sum = rowSums(select(., all_of(AB_items_REA)), na.rm = TRUE))


###################################################################
# Summing of Internalizing, Externalizing, and Other Problem Scales
###################################################################

# Summing Internalizing Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_internalizing_sum = rowSums(select(.,
    all_of(c("BEG_AD_sum", "BEG_WD_sum", "BEG_SC_sum"))), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_internalizing_sum = rowSums(select(.,
    all_of(c("REA_AD_sum", "REA_WD_sum", "REA_SC_sum"))), na.rm = TRUE))

# Summing Externalizing Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_externalizing_sum = rowSums(select(.,
    all_of(c("BEG_RB_sum", "BEG_AB_sum"))), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_externalizing_sum = rowSums(select(.,
   all_of(c("REA_RB_sum", "REA_AB_sum"))), na.rm = TRUE))

# Summing Other Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_other_sum = rowSums(select(., all_of(other_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_other_sum = rowSums(select(., all_of(other_items_REA)), na.rm = TRUE))

#################################
# Summing of DSM-Oriented Scales
#################################

# Summing Depressive Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_dep_sum = rowSums(select(., all_of(dep_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_dep_sum = rowSums(select(., all_of(dep_items_REA)), na.rm = TRUE))

# Summing Anxiety Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_anx_sum = rowSums(select(., all_of(anx_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_anx_sum = rowSums(select(., all_of(anx_items_REA)), na.rm = TRUE))

# Summing Somatic Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_somatic_sum = rowSums(select(., all_of(som_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_somatic_sum = rowSums(select(., all_of(som_items_REA)), na.rm = TRUE))

# Summing Attention Deficit Items
cbclBEG <- cbclBEG %>%
  mutate(BEG_attndef_sum = rowSums(select(., all_of(attn_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_attndef_sum = rowSums(select(., all_of(attn_items_REA)), na.rm = TRUE))

# Summing Oppositional Defiant Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_oppdef_sum = rowSums(select(., all_of(oppdef_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_oppdef_sum = rowSums(select(., all_of(oppdef_items_REA)), na.rm = TRUE))

# Summing Conduct Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_conduct_sum = rowSums(select(., all_of(conduct_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_conduct_sum = rowSums(select(., all_of(conduct_items_REA)), na.rm = TRUE))

# Summing Sluggish Cognitive Tempo Items
cbclBEG <- cbclBEG %>%
  mutate(BEG_slugcog_sum = rowSums(select(., all_of(slugcog_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_slugcog_sum = rowSums(select(., all_of(slugcog_items_REA)), na.rm = TRUE))

# Summing Obsessive-Compulsive Problems 
cbclBEG <- cbclBEG %>%
  mutate(BEG_obcomp_sum = rowSums(select(., all_of(obcomp_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_obcomp_sum = rowSums(select(., all_of(obcomp_items_REA)), na.rm = TRUE))

# Summing Stress Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_stress_sum = rowSums(select(., all_of(stress_items_BEG)), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_stress_sum = rowSums(select(., all_of(stress_items_REA)), na.rm = TRUE))

############################################
# Summing Total Problems and Writing to .csv 
############################################

# Summing Total Problems
cbclBEG <- cbclBEG %>%
  mutate(BEG_totalprobs_sum = rowSums(pick(matches("beg_cbcl_")), na.rm = TRUE))
cbclREA <- cbclREA %>%
  mutate(REA_totalprobs_sum = rowSums(pick(matches("rea_cbcl_")), na.rm = TRUE))

# Flagging rows with impossible values (119 items total; scored 0, 1, or 2) 
cbclBEG %>%
  filter(BEG_totalprobs_sum < 0 | BEG_totalprobs_sum > 238) # n = 0
cbclREA %>%
  filter(REA_totalprobs_sum < 0 | REA_totalprobs_sum > 238) # n = 0

# Marking rows as NA based on CBCL 6-18 Items 1 + 2
cbclBEG <- cbclBEG %>%
  mutate(across(
    -studyid,  #excludes Study ID column
    ~ifelse(
      is.na(beg_cbcl_acts_young) & is.na(beg_cbcl_alcohol),
      NA, .)))

cbclREA <- cbclREA %>%
  mutate(across(
    -studyid,  #excludes Study ID column
    ~ifelse(
      is.na(rea_cbcl_acts_young) & is.na(rea_cbcl_alcohol),
      NA, .)))

# Counting # of missing participants in BEG time point
sum(is.na(cbclBEG$BEG_totalprobs_sum)) #n = 8

# Counting # of missing participants in REA time point
sum(is.na(cbclREA$REA_totalprobs_sum)) #n = 6

# Removing duplicate rows in Reader time point (INF032)
cbclREA <- cbclREA %>%
  distinct(studyid, .keep_all = TRUE)

# Selecting only scored columns
cbclBEG <- cbclBEG %>%
  select(studyid, contains("sum"))
cbclREA <- cbclREA %>%
  select(studyid, contains("sum"))

# Writing BEG Data to .csv
write_csv(cbclBEG, paste0(pathway, 'your_scored.csv'))

# Writing REA Data to .csv
write_csv(cbclREA, paste0(pathway, 'your_scored.csv'))

# Joining data frames and Writing to .csv
cbclBEG_REA <- full_join(cbclBEG, cbclREA, by = "studyid")

write_csv(cbclBEG_REA, paste0(pathway, 'your_scored.csv'))




