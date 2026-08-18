#########################################
# BabyBOLD CBCL Z-Scores 
# Author: Emma Lambert
# Date: 1/22/2026
#########################################

# Import from library 
library(tidyverse)
library(dplyr)

#Define source path
pathway <- # Insert Data Path #

# Read in .csv files
TOD_PRE <- read_csv(paste0(pathway, 'your_scored.csv'))
BEG_REA <- read_csv(paste0(pathway, 'your_scored.csv'))

###############################
# Preparing data for scoring
###############################

# Joining data frames
TODtoREA <- full_join(TOD_PRE, BEG_REA, by = "studyid")

# Selecting only relevant internalizing and externalizing columns
TODtoREA <- TODtoREA %>%
  select(studyid, 
         matches("(internalizing|externalizing|dep|anx|attndef|oppdef)"))

#################################################################
# Calculating z-scores for Internalizing and Externalizing Scales
#################################################################

# Calculating z-score for Internalizing Problems
TODtoREA <- TODtoREA %>%
  mutate(TOD_internalizing = as.numeric(scale(TOD_internalizing_sum)),
         PRE_internalizing = as.numeric(scale(PRE_internalizing_sum)),
         BEG_internalizing = as.numeric(scale(BEG_internalizing_sum)),
         REA_internalizing = as.numeric(scale(REA_internalizing_sum)))

# Calculating z-score for Externalizing Problems
TODtoREA <- TODtoREA %>%
  mutate(TOD_externalizing = as.numeric(scale(TOD_externalizing_sum)),
         PRE_externalizing = as.numeric(scale(PRE_externalizing_sum)),
         BEG_externalizing = as.numeric(scale(BEG_externalizing_sum)),
         REA_externalizing = as.numeric(scale(REA_externalizing_sum)))

# Calculating z-score for Depressive Problems 
TODtoREA <- TODtoREA %>%
  mutate(TOD_depression = as.numeric(scale(TOD_dep_sum)),
         PRE_depression = as.numeric(scale(PRE_dep_sum)),
         BEG_depression = as.numeric(scale(BEG_dep_sum)),
         REA_depression = as.numeric(scale(REA_dep_sum)))

# Calculating z-score for Anxiety Problems
TODtoREA <- TODtoREA %>%
  mutate(TOD_anxiety = as.numeric(scale(TOD_anx_sum)),
         PRE_anxiety = as.numeric(scale(PRE_anx_sum)),
         BEG_anxiety = as.numeric(scale(BEG_anx_sum)),
         REA_anxiety = as.numeric(scale(REA_anx_sum)))

# Calculating z-score for Attention Deficit/Hyperactivity Problems
TODtoREA <- TODtoREA %>%
  mutate(TOD_attndef = as.numeric(scale(TOD_attndef_sum)),
         PRE_attndef = as.numeric(scale(PRE_attndef_sum)),
         BEG_attndef = as.numeric(scale(BEG_attndef_sum)),
         REA_attndef = as.numeric(scale(REA_attndef_sum)))

# Calculating z-score for Oppositional Defiant Problems
TODtoREA <- TODtoREA %>%
  mutate(TOD_oppdef = as.numeric(scale(TOD_oppdef_sum)),
         PRE_oppdef = as.numeric(scale(PRE_oppdef_sum)),
         BEG_oppdef = as.numeric(scale(BEG_oppdef_sum)),
         REA_oppdef = as.numeric(scale(REA_oppdef_sum)))

###################################
# Cleaning and Writing to .csv
###################################

# Selecting only relevant columns
TODtoREA <- TODtoREA %>%
  select(studyid, !contains('sum'))

# Pivoting the data longer
TODtoREA <-TODtoREA %>%
  pivot_longer(cols = -studyid,
               names_to = c("timepoint", "subscale"),
               names_sep = "_",
               values_to = "z_score")

# Pivoting the data wider
TODtoREA <- TODtoREA %>%
  pivot_wider(
    id_cols = c(studyid, timepoint),
    names_from = subscale,
    values_from = z_score)

# Writing Toddler Data to .csv
TOD <- TODtoREA[TODtoREA$timepoint == "TOD",]
write_csv(TOD, paste0(pathway, 'your_zscores.csv'))

# Writing Pre-Reader Data to .csv
PRE <- TODtoREA[TODtoREA$timepoint == "PRE",]
write_csv(PRE, paste0(pathway, 'your_zscores.csv' ))

# Writing Beginner Reader Data to .csv
BEG <- TODtoREA[TODtoREA$timepoint == "BEG",]
write_csv(BEG, paste0(pathway, 'your_zscores.csv'))

# Writing Reader Data to .csv
REA <- TODtoREA[TODtoREA$timepoint == "REA",]
write_csv(REA, paste0(pathway, 'your_zscores.csv'))

# Writing all data to .csv
write_csv(TODtoREA, paste0(pathway, 'BabyBOLD_cbcl_zscores.csv'))


