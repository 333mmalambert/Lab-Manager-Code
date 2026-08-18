#########################################
# CALC CBCL Z-Scores
# Author: Emma Lambert
# Date: 1/22/2026
#########################################

# Import from library 
library(tidyverse)
library(dplyr)

#Define source path
pathway <- "Insert Data Path Here"

# Read in .csv files
years1to3 <- read_csv(paste0(pathway, 'your_scored.csv'))
year4 <- read_csv(paste0(pathway, 'yourL_scored.csv'))

###############################
# Preparing data for scoring
###############################

# Merge into one data frame
years1to4 <- merge(years1to3, year4, by = "record_id")

# Selecting only relevant internalizing and externalizing columns
years1to4 <- years1to4 %>%
  select(record_id, 
         matches("(internalizing|externalizing|dep|anx|attndef|oppdef)"))

#################################################################
# Calculating z-scores for Internalizing and Externalizing Scales
#################################################################

# Calculating z-score for Internalizing Problems
years1to4 <- years1to4 %>%
  mutate(yr1_internalizing_zscore = as.numeric(scale(yr1_internalizing_sum)),
         yr2_internalizing_zscore = as.numeric(scale(yr2_internalizing_sum)),
         yr3_internalizing_zscore = as.numeric(scale(yr3_internalizing_sum)),
         yr4_internalizing_zscore = as.numeric(scale(yr4_internalizing_sum)))

# Calculating z-score for Externalizing Problems
years1to4 <- years1to4 %>%
  mutate(yr1_externalizing_zscore = as.numeric(scale(yr1_externalizing_sum)),
         yr2_externalizing_zscore = as.numeric(scale(yr2_externalizing_sum)),
         yr3_externalizing_zscore = as.numeric(scale(yr3_externalizing_sum)),
         yr4_externalizing_zscore = as.numeric(scale(yr4_externalizing_sum)))

# Calculating z-score for Depressive Problems 
years1to4 <- years1to4 %>%
  mutate(yr1_depression_zscore = as.numeric(scale(yr1_dep_sum)),
         yr2_depression_zscore = as.numeric(scale(yr2_dep_sum)),
         yr3_depression_zscore = as.numeric(scale(yr3_dep_sum)),
         yr4_depression_zscore = as.numeric(scale(yr4_dep_sum)))

# Calculating z-score for Anxiety Problems
years1to4 <- years1to4 %>%
  mutate(yr1_anxiety_zscore = as.numeric(scale(yr1_anx_sum)),
         yr2_anxiety_zscore = as.numeric(scale(yr2_anx_sum)),
         yr3_anxiety_zscore = as.numeric(scale(yr3_anx_sum)),
         yr4_anxiety_zscore = as.numeric(scale(yr4_anx_sum)))

# Calculating z-score for Attention Deficit/Hyperactivity Problems
years1to4 <- years1to4 %>%
  mutate(yr1_attndef_zscore = as.numeric(scale(yr1_attndef_sum)),
         yr2_attndef_zscore = as.numeric(scale(yr2_attndef_sum)),
         yr3_attndef_zscore = as.numeric(scale(yr3_attndef_sum)),
         yr4_attndef_zscore = as.numeric(scale(yr4_attndef_sum)))

# Calculating z-score for Oppositional Defiant Problems
years1to4 <- years1to4 %>%
  mutate(yr1_oppdef_zscore = as.numeric(scale(yr1_oppdef_sum)),
         yr2_oppdef_zscore = as.numeric(scale(yr2_oppdef_sum)),
         yr3_oppdef_zscore = as.numeric(scale(yr3_oppdef_sum)),
         yr4_oppdef_zscore = as.numeric(scale(yr4_oppdef_sum)))

###################################
# Cleaning and Writing to .csv
###################################

# Selecting only relevant columns
years1to4 <- years1to4 %>%
  select(record_id, !contains("sum"))

# Pivoting the data longer
years1to4 <-years1to4 %>%
  pivot_longer(cols = -record_id,
               names_to = c("timepoint", "subscale"),
               names_prefix = "yr",
               names_sep = "_",
               names_transform = list(timepoint = as.numeric),
               values_to = "z_score")

# Pivoting the data wider
years1to4 <- years1to4 %>%
  pivot_wider(
    id_cols = c(record_id, timepoint),
    names_from = subscale,
    values_from = z_score)

# Writing Year 1 z-scores to .csv
year1 <- years1to4[years1to4$timepoint == 1,]
write_csv(year1, paste0(pathway,'your_zscores.csv'))

# Writing Year 2 z-scores to .csv
year2 <- years1to4[years1to4$timepoint == 2,]
write_csv(year2, paste0(pathway,'your_zscores.csv'))

# Writing Year 3 z-scores to .csv
year3 <- years1to4[years1to4$timepoint == 3,]
write_csv(year3, paste0(pathway, 'your_zscores.csv'))

# Writing Year 4 z-scores to .csv
year4 <- years1to4[years1to4$timepoint == 1,]
write_csv(year4, paste0(pathway,'your_zscores.csv'))

# Writing Years 1-4 to .csv
write_csv(years1to4, paste0(pathway,'your_zscores.csv'))