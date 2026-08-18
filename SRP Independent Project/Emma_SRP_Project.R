########################
# SRP Project
# Date: 4/6/2026
# Author: Emma Lambert
#######################

# Importing packages
library(tidyverse)
library(stats)
library(EnvStats)
library(ggplot2)
library(broom)

# Setting data path
pathway <- "/path/to/ABCD_6.0/Data/"

###############################
# Selecting and Cleaning Data
###############################

gen_path <- file.path(pathway,"abcd_general")

# Reading in Visit/Demographic tsv files
SRP_Data <- read_tsv(paste0(gen_path, '/ab_g_dyn.tsv'))
SRP_Data <-  SRP_Data %>%
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age) # Participant age @ each session
SRP_Data <- filter(SRP_Data, session_id == "ses-00A") # Filtering for only Baseline data
SRP_Data <- SRP_Data %>%
  rename(age = ab_g_dyn__visit_age)

stc <- read_tsv(paste0(gen_path, '/ab_g_stc.tsv'))
stc <- stc %>%
  select(participant_id, 
         ab_g_stc__cohort_sex, # Participant sex
         ab_g_stc__cohort_ethnrace__leg, # Participant ethnicity 
         ab_g_stc__design_famrel) # Relationship of the participant in their family
SRP_Data <-  merge(SRP_Data, stc, by = "participant_id")
SRP_Data <- SRP_Data[SRP_Data$ab_g_stc__design_famrel < 2,] # Removed all twins and triples (n = 2164)
SRP_Data <- SRP_Data %>%
  rename(sex = ab_g_stc__cohort_sex,
         ethnrace = ab_g_stc__cohort_ethnrace__leg)
SRP_Data <- SRP_Data %>%
  mutate(sex = recode(sex,
                      '1' = 'Male',
                      '2' = 'Female'))
SRP_Data <- SRP_Data %>%
  mutate(ethnrace = recode(ethnrace, 
                           '1'= 'Hispanic',
                           '2' = 'White',
                           '3' = 'Black',
                           '4' = 'Asian',
                           '13' = 'Other'))

demo <- read_tsv(paste0(gen_path, '/ab_p_demo.tsv'))
demo <- demo %>%
  select(participant_id,
         session_id,
         ab_p_demo__income__hhold_001) # Total combined family income for the past 12 months
SRP_Data <- merge(SRP_Data, demo, by = c("participant_id", "session_id"), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(fam_income = ab_p_demo__income__hhold_001)
SRP_Data <- SRP_Data %>%
  mutate(fam_income = na_if(fam_income, 'n/a'),
         fam_income = na_if(fam_income, '999'),
         fam_income = na_if(fam_income, '777'))
SRP_Data <- SRP_Data %>%
  mutate(fam_income = factor(SRP_Data$fam_income, 
                             levels = c("1","2","3","4","5","6","7","8","9","10"), 
                             labels = c("Less than $5,000",
                                        "$5,000 through $11,999",
                                        "$12,000 through $15,999", 
                                        "$16,000 through $24,999", 
                                        "$25,000 through $34,999", 
                                        "$35,000 through $49,999", 
                                        "$50,000 through $74,999", 
                                        "$75,000 through $99,999", 
                                        "$100,000 through $199,999", 
                                        "$200,000 and greater"), ordered=TRUE))


####################################
# Reading in project-specific  files
####################################

#########################
# Obstetric Complications
########################

# OCs path
oc_path <- "/path/to/derivative_data/6.0/"

# Obstetric Complications (Selecting, Renaming, Recoding)
OCs <- read_csv(paste0(oc_path, '/Obstetric_Complications_9_12_25.csv'))
OCs <- OCs %>%
  select(participant_id,
         C_Section_Y1N0,
         Bleeding_Preg_Y1N0,
         Pre_Eclampsia_Y1N0,
         Blue_at_Birth_Y1N0,
         Required_Resusc_Y1N0,
         Neonatal_Apnea_Y1N0,
         Planned_Preg_Y1N0,
         UTI_Y1N0,
         Rubella_Y1N0,
         Relevant_Med_1_Y1N0,
         Relevant_Med_2_Y1N0,
         Relevant_Med_3_Y1N0,
         Relevant_Med_4_Y1N0,
         Relevant_Med_5_Y1N0,
         Relevant_Med_6_Y1N0,
         Relevant_Med_7_Y1N0,
         Relevant_Med_8_Y1N0,
         All_Relevant_Med_Count,
         Hypoxia_Flag,
         Infection_Flag,
         Stress_Flag)
SRP_Data <- merge(SRP_Data, OCs, by = "participant_id", all.x = TRUE)
SRP_Data <- SRP_Data %>% # Recoding "Don't Know' values to NA to aid in future Summing
  mutate(C_Section_Y1N0 = na_if(C_Section_Y1N0, 999),
         Bleeding_Preg_Y1N0 = na_if(Bleeding_Preg_Y1N0, 999),
         Blue_at_Birth_Y1N0 = na_if(Blue_at_Birth_Y1N0, 999),
         Required_Resusc_Y1N0 = na_if(Required_Resusc_Y1N0, 999),
         Neonatal_Apnea_Y1N0 = na_if(Neonatal_Apnea_Y1N0, 999),
         Planned_Preg_Y1N0 = na_if(Planned_Preg_Y1N0, 999),
         UTI_Y1N0 = na_if(UTI_Y1N0, 999),
         Rubella_Y1N0 = na_if(Rubella_Y1N0, 999))
       
# PH path
ph_path <- file.path(pathway, "Physical_Health")
    
# Substance Abuse/Other Obstetric Complications
Add_OCs <- read_tsv(paste0(ph_path, '/ph_p_dhx.tsv'))
Add_OCs <- Add_OCs %>%
  select(participant_id,
         session_id,
         ph_p_dhx__alc_001a, # Before: Alcohol?
         ph_p_dhx__alc_001b, # After: Alcohol?
         ph_p_dhx__mj_001a, # Before: MJ
         ph_p_dhx__mj_001b, # After: MJ   
         ph_p_dhx__nic_001a, # Before: Nic
         ph_p_dhx__nic_001b) # After: Nic

SRP_Data <- merge(SRP_Data, Add_OCs, by = c("participant_id", 'session_id'), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(alc_before = ph_p_dhx__alc_001a,
         alc_after = ph_p_dhx__alc_001b,
         mj_before = ph_p_dhx__mj_001a,
         mj_after = ph_p_dhx__mj_001b,
         nic_before = ph_p_dhx__nic_001a,
         nic_after = ph_p_dhx__nic_001b)

SRP_Data <- SRP_Data %>%
  mutate(alc_before = na_if(alc_before, "n/a"),
         alc_before = na_if(alc_before, "999"),
         alc_after = na_if(alc_after, "n/a"),
         alc_after = na_if(alc_after, "999"),
         mj_before = na_if(mj_before, "n/a"),
         mj_before = na_if(mj_before, "999"),
         mj_after = na_if(mj_after, "n/a"),
         mj_after = na_if(mj_after, "999"),
         nic_before = na_if(nic_before, "n/a"),
         nic_before = na_if(nic_before, "999"),
         nic_after = na_if(nic_after, "n/a"),
         nic_after = na_if(nic_after, "999"))

SRP_Data <- SRP_Data %>%
  mutate_at(c('alc_before',
              'alc_after',
              'mj_before',
              'mj_after',
              'nic_before',
              'nic_after'), as.numeric)

SRP_Data <- SRP_Data %>%
  mutate(alc_exposure = if_else(alc_before == 1 | alc_after == 1, 1, 0),
         mj_exposure = if_else(mj_before == 1 | mj_after == 1, 1, 0),
         nic_exposure = if_else(nic_before == 1 | nic_after == 1, 1, 0))

###########################
# Psychopathology Measures
###########################

# MH Path
mh_path <- file.path(pathway, "Mental_Health")

# CBCL Int and Ext Scores (Selecting, Renaming, Recoding)
cbcl <- read_tsv(paste0(mh_path, '/mh_p_cbcl.tsv'))
cbcl <- cbcl %>%
  select(participant_id,
         session_id,
         mh_p_cbcl__synd__tho_sum,   # Sum of Thought Problems
         mh_p_cbcl__synd__attn_sum,)  # Sum of Attention Problems
SRP_Data <- merge(SRP_Data, cbcl, by = c("participant_id","session_id"), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(thought_sum = mh_p_cbcl__synd__tho_sum,
         attn_sum = mh_p_cbcl__synd__attn_sum)


# PLE Scores (Selecting, Renaming, Recoding)
PLEs <- read_tsv(paste0(mh_path, '/mh_y_pps.tsv'))
PLEs <- PLEs %>%
  select(participant_id,
         session_id,
         mh_y_pps__severity_score) #PPS Severity Score
SRP_Data <- merge(SRP_Data, PLEs, by = c("participant_id", "session_id"), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(ple_severity = mh_y_pps__severity_score)

#########################
# Cognition (NIH Toolbox)
#########################

# Cog Path
cog_path <- file.path(pathway, "Neurocognition")

# NIH Toolbox Scores (Selecting, Renaming, Recoding)
Cog_scores <- read_tsv(paste0(cog_path, '/nc_y_nihtb.tsv'))
Cog_scores <- Cog_scores %>%
  select(participant_id,
         session_id,
         nc_y_nihtb__comp__tot__agecor_score, # NIH Toolbox: Cognition total composite - Age-corrected standard score
         nc_y_nihtb__comp__cryst__agecorr_score, # NIH Toolbox: Crystallized composite - Age-corrected standard score
         nc_y_nihtb__comp__fluid__agecorr_score, # NIH Toolbox: Cognition fluid composite - Age-corrected standard score
         nc_y_nihtb__lswmt__agecor_score) # List Sorting Working Memory Task (NIH Toolbox): Age-Corrected Standard Score
SRP_Data <- merge(SRP_Data, Cog_scores, by = c("participant_id", "session_id"), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(nihtb_comp = nc_y_nihtb__comp__tot__agecor_score,
         nihtb_cryst = nc_y_nihtb__comp__cryst__agecorr_score,
         nihtb_fluid = nc_y_nihtb__comp__fluid__agecorr_score,
         wm_score = nc_y_nihtb__lswmt__agecor_score)

SRP_Data <- SRP_Data %>%
  mutate(nihtb_comp = na_if(nihtb_comp, "n/a"),
         nihtb_cryst = na_if(nihtb_cryst, "n/a"),
         nihtb_fluid = na_if(nihtb_fluid, "n/a"),
         wm_score = na_if(wm_score, "n/a"))

SRP_Data <- SRP_Data %>%
  mutate_at(c('nihtb_comp',
              'nihtb_cryst',
              'nihtb_fluid',
              'wm_score'), as.numeric)


######################
# Hippocampal Volumes
######################

# Hippo Path
hippo_path <- "/path/to/derivative_data/6.0/"

# Hippocampal Volumes (Selecting, Renaming)
Hippo_vol <- read_csv(paste0(hippo_path, '/All_Hippo_Volumes.csv'))
Hippo_vol <- Hippo_vol %>%
  select(participant_id,
         session_id,
         left_whole_hippocampus,
         right_whole_hippocampus)
SRP_Data <- merge(SRP_Data, Hippo_vol, by = c("participant_id", "session_id"), all.x = TRUE)

# Rosner's Test for Outliers 
rosnerTest(SRP_Data$left_whole_hippocampus)
rosnerTest(SRP_Data$right_whole_hippocampus, k = 5)

# Marking Outliers as NA
SRP_Data[3668, 41] <- NA
SRP_Data[3387, 41] <- NA
SRP_Data[364, 42] <- NA
SRP_Data[3387, 42] <- NA
SRP_Data[1241, 42] <- NA

# Creating Whole Hippocampal Volumes
SRP_Data <- SRP_Data %>%
  mutate(hippo_vol = left_whole_hippocampus + right_whole_hippocampus)

# WHB Path
whb_path <- file.path(pathway, "Imaging", "Structural_MRI")

# Whole Brain Volume
WHB_vol <- read_tsv(paste0(whb_path, '/mr_y_smri__vol__aseg.tsv'))
WHB_vol <- WHB_vol %>%
  select(participant_id,
         session_id,
         mr_y_smri__vol__aseg__whb_sum) # Total volume of Subcortical ROI: whole brain
SRP_Data <- merge(SRP_Data, WHB_vol, by = c("participant_id", "session_id"), all.x = TRUE)
SRP_Data <- SRP_Data %>%
  rename(whb_volume = mr_y_smri__vol__aseg__whb_sum)

# Creating Hippocampal/Whole Brain Percentages
SRP_Data <- SRP_Data %>%
  mutate(hippo_whb_percent = hippo_vol/whb_volume)



###################
# Editing the Data
###################

# Creating Categories and Totals for OCs 
SRP_Data <- SRP_Data %>%
  mutate(Hypoxia_Total = rowSums(cbind(C_Section_Y1N0, 
                                       Bleeding_Preg_Y1N0,
                                       Pre_Eclampsia_Y1N0,
                                       Blue_at_Birth_Y1N0,
                                       Required_Resusc_Y1N0,
                                       Neonatal_Apnea_Y1N0), na.rm = TRUE),
         Maternal_Stress_Total = as.factor(Stress_Flag),
         Prenatal_Infec_Total = rowSums(cbind(UTI_Y1N0,
                                              Rubella_Y1N0,
                                              All_Relevant_Med_Count), na.rm =  TRUE),
         Substance_Exposure_Total = rowSums(cbind(alc_exposure,
                                                  mj_exposure,
                                                  nic_exposure,
                                                  Stress_Flag), na.rm = TRUE),
         OCs_Total = rowSums(cbind(Hypoxia_Total,
                                   Stress_Flag,
                                   Prenatal_Infec_Total), na.rm = TRUE),
         OCs_SE_Total = rowSums(cbind(Hypoxia_Total,
                                   Prenatal_Infec_Total,
                                   Substance_Exposure_Total), na.rm = TRUE))

# Z-Scores for CBCL
SRP_Data <- SRP_Data %>%
  mutate(Thought_ZScore = as.numeric(scale(thought_sum)),
         Attn_ZScore = as.numeric(scale(attn_sum)))


###########
# Analysis
###########

# Aim 1. Examine the relationship between obstetric complication burden and type in hippocampal volume. 

# Relationship between OC Burden and Hippocampal Volume % 
summary(lm(formula = hippo_whb_percent ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Type and Hippocampal Volume % 
summary(lm(formula = hippo_whb_percent ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Burden and Hippocampal Volume (Using WHB Variable)
summary(lm(formula = hippo_vol ~ OCs_Total + whb_volume + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Type and Hippocampal Volume (Using WHB Variable)
summary(lm(formula = hippo_vol ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + whb_volume + age + sex + ethnrace + fam_income, data = SRP_Data))


# Aim 2. Determine if the burden of obstetric complications results in higher rates of psychotic-like experiences (PLEs) compared to other symptoms of psychopathology. 

# Relationship between OC Burden and PLE Severity (***)
prac_model <- lm(formula = ple_severity ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data)

# Relationship between OC Type and PLE Severity (Hypoxia, Maternal Stress)
summary(lm(formula = ple_severity ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Burden and Thought Problems (***)
summary(lm(formula = Thought_ZScore ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Type and Thought Problems (Hypoxia, Maternal Stress, Prenatal Infection, Substance Exposure)
summary(lm(formula = Thought_ZScore ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Burden and Attention Problems (***)
summary(lm(formula = Attn_ZScore ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data))
    
# Relationship between OC Type and Attention Problems (Hypoxia, Maternal Stress, Prenatal Infection, Substance Exposure)
summary(lm(formula = Attn_ZScore ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))


# Aim ?. Examine the relationship between obstetric complication burden and type in cognition. 

# Relationship between OC Burden and Cognition Composite (***)
summary(lm(formula = nihtb_comp ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Type and Cognition Composite (Hypoxia, Maternal Stress, Substance Exposure)
summary(lm(formula = nihtb_comp ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Burden and Fluid Cognition (***)
summary(lm(formula = nihtb_fluid ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Type and Fluid Cognition (Hypoxia, Maternal Stress)
summary(lm(formula = nihtb_fluid ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Burden and Fluid Cognition (***)
summary(lm(formula = nihtb_cryst ~ OCs_Total + age + sex + ethnrace + fam_income, data = SRP_Data))

# Relationship between OC Type and Fluid Cognition (Maternal Stress, Prenatal Infection, Substance Exposure)
summary(lm(formula = nihtb_cryst ~ Hypoxia_Total + Maternal_Stress_Total + Prenatal_Infec_Total + Substance_Exposure_Total + age + sex + ethnrace + fam_income, data = SRP_Data))


# Aim 3. Is this relationship moderated by effects on hippocampal volume/connectivity (from Aim 1) 

# Is the relationship between OC Burden and PLE Severity moderated by hippocampal volume?
summary(lm(formula = ple_severity ~ OCs_Total*hippo_vol + age + sex + ethnrace + fam_income, data = SRP_Data))


