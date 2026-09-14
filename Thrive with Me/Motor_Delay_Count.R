###################################
# Motor_Delay_Count.R
# Date: 9/14/2026
# Author: Emma Lambert
####################################

# Importing packages
library(tidyverse)

# Setting Working Directory
setwd("Insert Path Here")

# Reading in TWM Part 1 Data (This will need to be updated with each new data report)
TWM_Data <- read_csv("ThriveWithMeDATA.csv")

##############################################
# Selecting and Cleaning Motor Delay Data
#############################################

# Selecting relevant columns from Developmental History + MAPR
Motor <- TWM_Data %>%
  select(participant_id, 
         dhs_q10_motor, # Compared to your peers, would you say your motor development (sitting, crawling, walking) was:
         dhs_q10a_confidence, # How confident are you in the accuracy of your above answer?
         dhs_q12a_roll_over, # At approximately what age (in months) were you first able to roll over?
         dhs_q12a_confidence,  # How confident are you in the accuracy of your above answer?
         dhs_q12b_sit_up, # At approximately what age (in months) were you first able to sit without assistance?
         dhs_q12b_confidence,  # How confident are you in the accuracy of your above answer?
         dhs_q12c_walk, # At approximately what age (in months) were you first able to walk without assistance?
         dhs_q12c_confidence,  # How confident are you in the accuracy of your above answer?
         mapr_q1_delays) # Are you aware of any delays in walking, talking, or toilet training when you were an infant or toddler?

# Renaming for clarity
Motor <- Motor %>%
  rename(
    motor_devl = dhs_q10_motor,
    motor_devl_conf = dhs_q10a_confidence,
    age_roll_over = dhs_q12a_roll_over,
    age_roll_over_conf = dhs_q12a_confidence,
    age_sit_up = dhs_q12b_sit_up,
    age_sit_up_conf = dhs_q12b_confidence,
    age_walk = dhs_q12c_walk,
    age_walk_conf = dhs_q12c_confidence,
    mapr_delays = mapr_q1_delays)

###########################
# Scoring and Categorizing
##########################

# Scoring Motor Delay data from Developmental History
Motor <- Motor %>%
  mutate(motor_delayY1N0 = if_else(
    motor_devl <= 2,1,0)) # If a participant answered "Very late" or "Somewhat late", they are marked as having a motor delay 

# Updating score if a participant was NOT confident
Motor <- Motor %>%
  mutate(motor_delayY1N0 = case_when(
    motor_devl_conf == 1 ~ 0,
    TRUE ~ motor_delayY1N0)) # If a participant answered "Not at all confident", they are no longer marked as having a motor delay 

# Scoring Motor Delay data from MAP-R
Motor <- Motor %>%
  mutate(mapr_delayY1N0 = if_else(
    motor_devl == 1,1,0)) %>% # If a participant answered "Some delays" they are marked as having a motor delay 
  mutate(mapr_delayY1N0 = case_when(
    motor_devl == 2 ~ 1,
    TRUE ~ mapr_delayY1N0)) # If a participant answered "Numerous delays" they are marked as having a motor delay 

# Creating a flag to note discrepancies between DHS and MAP-R
Motor <- Motor %>%
  mutate(delay_flagY1N0 = if_else(
    motor_delayY1N0 != mapr_delayY1N0, 1, 0))

#################
# Writing to CSV
#################

# Selecting only relevant columns
Motor_Clean <- Motor %>%
  select(participant_id, 
         motor_delayY1N0, 
         mapr_delayY1N0, 
         delay_flagY1N0)

# Final Count of Motor Delays
Motor_Clean %>%
  count(motor_delayY1N0) # n = 2

# Setting Output Directory
setwd("Insert Path Here")

# Writing to .csv
write_csv(Motor_Clean,'TWM_Motor_Delays.csv')
