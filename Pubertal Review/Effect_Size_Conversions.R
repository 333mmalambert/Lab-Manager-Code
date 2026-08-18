#############################################################
# Effect Size Transformations and Standard Error Calculations
# Author: Emma Lambert
# Date: 2/4/2025
##############################################################

# Import Libraries
library(esc)
library(effectsize)
library(psychometric)
library(metafor)

# Correlation Coefficient (r) to Fisher's Z (Hochman & Lewine, 2004) 
# Total Sample (N = 64)
convert_r2z(0.26)       # Age at first hospitalization
convert_r2z(0.26)       # Age at first symptom
convert_r2z(0.105)      # Total SAPS
convert_r2z(0.237)      # Total SANS

# Restricted Sample (n = 57)   
convert_r2z(0.082)      # Age at first hospitalization
convert_r2z(0.163)      # Age at first symptom
convert_r2z(0.156)      # Total SAPS
convert_r2z(0.324)      # Total SANS

# Calculating Standard Error of Fisher's Z Prime (Hochman & Lewine, 2004)
# Total Sample (N = 64)
SEz(64)

# Restricted Sample (n = 57)   
SEz(57)

# Correlation Coefficient (r) to Fisher's Z (Kilicaslan et al., 2014)     
# Removed negative sign to aid in directionality 
convert_r2z(0.375)    # Age at first odd behavior
convert_r2z(0.382)    # Age at first psychotic symptom
convert_r2z(0.215)    # Age at first hospitalization

# Calculating Standard Error of Fisher's Z Prime (Kilicaslan et al., 2014)
SEz(289) # Which makes sense to use? 
SEz(259)

# Correlation Coefficient (r) to Fisher's Z (Cohen et al., 1999)
# Removed negative sign to aid in directionality 
convert_r2z(0.31)        # Age at first odd behavior (F)
convert_r2z(0.55)        # Age at first psychotic symptom (F) 
convert_r2z(0.57)        # Age at first hospitalization (F) 

# Calculating Standard Error of Fisher's Z (Cohen et al., 1999)
SEz(35) 

# Beta Coefficient to Standardized Mean Difference (Larson et al., 2025)
# Added negative sign to aid in directionality 
esc_beta(beta = -0.31,      # PQ-BC PLE Score
         sdy = 2.70,        # SD of Total PLEs, Year 3
         grp1n = 915,       # Early Pubertal Timing Group (F)    
         grp2n = 3770,      # Total Sample Size
         es.type = "r")

# Correlation Coefficient to Fisher's Z (Larson et al., 2025)
convert_r2z(-0.3099956)

# Calculating Standard Error of Fisher's Z Prime (Larson et al., 2025)
SEz(915)

# Odds Ratio to Standardized Mean Difference (d) (Pries et al., 2025)
# Added negative sign to aid in directionality 
oddsratio_to_d(
  OR = 0.68,   # PQ-BC PLE Score
  log = FALSE) # Model 1 

oddsratio_to_d(
  OR = 0.77,  # PQ-BC PLE Score
  log = FALSE) # Model 2

# Standardized Mean Difference (d) to Correlation Coefficient 
d_to_r(-0.2126269) # Model 1
d_to_r(-0.1440979) # Model 2

# Correlation Coefficient to Fisher's Z (Pries et al., 2025)
convert_r2z(-0.1057177)      # Model 1
convert_r2z(-0.07186267)      # Model 2

# Calculating Standard Error of Fisher's Z Prime (Pries et al., 2025)
SEz(2947) #individuals who had reached menarche

# Partial Eta-squared to Standardized Mean Difference (Barrau-Sastre et al., 2022)
pes_to_cohens_d = function(pes, n) sqrt( ((n-1)/n) * (pes/(1-pes))) # Creating function
pes_to_cohens_d(0.142, 42)        # PSYRATS Hallucinations 
pes_to_cohens_d(0.097, 42)       # PSYRATS Delusions
pes_to_cohens_d(0.114, 42)      # PANSS: Positive
pes_to_cohens_d(0.037, 42)     # PANSS: Negative
pes_to_cohens_d(0.042, 42)    # PANSS: General

# Standardized Mean Difference (d) to Correlation Coefficient 
d_to_r(0.142, 42)        # PSYRATS Hallucinations 
d_to_r(0.097, 42)       # PSYRATS Delusions
d_to_r(0.114, 42)      # PANSS: Positive
d_to_r(0.037, 42)     # PANSS: Negative
d_to_r(0.042, 42)    # PANSS: General

# Correlation Coefficient to Fisher's Z (Barrau-Sastre et al., 2022)
convert_r2z(0.137)           # Age at illness onset
convert_r2z(0.07167581)      # PSYRATS Hallucinations 
convert_r2z(0.04902887)      # PSYRATS Delusions
convert_r2z(0.05759517)      # PANSS: Positive
convert_r2z(0.01872097)      # PANSS: Negative
convert_r2z(0.02124976)      # PANSS: General

# Calculating Standard Error of Fisher's Z Prime (Barrau-Sastre et al., 2022)
SEz(42)

# Odds Ratio to Standardized Mean Difference (d) (Yuan et al., 2023)
# Added negative sign to aid in directionality 
oddsratio_to_d(
    OR = 2.31,
    n1 = 112, # Early menarche = yes
    n2 = 824, # Early menarche = no
    log = FALSE)

# Standardized Mean Difference (d) to Correlation Coefficient (Yuan et al., 2023)
d_to_r(0.4615988, 112, 824)

# Correlation Coefficient (r) to Fisher's Z (Yuan et al., 2023)
convert_r2z(0.1483186)

# Calculating Standard Error of Fisher's Z Prime (Yuan et al., 2023)
SEz(936)

# F-Value to Standardized Mean Difference (d) (Yazici et al., 2013)
esc_f(f = 1.40, # Onset of illness
      grp1n = 20, # Onset before 16
      grp2n = 41, # Onset after 16
      es.type = "d")

# Standardized Mean Difference (d) to Correlation Coefficient (Yazici et al., 2013)
d_to_r(0.3227, 20, 41)

# Correlation Coefficient (r) to Fisher's Z (Yazici et al., 2013)
convert_r2z(0.152238)

# Calculating Standard Error of Fisher's Z Prime
# Schizophrenia Group (n = 61)
SEz(61)

# Correlation Coefficient (r) to Fisher's Z (Frazier et al., 1997)
convert_r2z(0.74)          # Age at development of secondary sex characteristics
convert_r2z(0.44)          # Age at onset of puberty
convert_r2z(0.08)           # Age at menarche

# Calculating Standard Error of Fisher's Z Prime (Frazier et al., 1997)
# Menarche group (n = 10)
SEz(10)

# Correlation Coefficient (r) to Fisher's Z (Galdos et al., 1993)    
convert_r2z(0.54)            # Age at first psychotic symptom

# Calculating Standard Error of Fisher's Z Prime (Galdos et al., 1993)    
# Menarche group (n = 32)
SEz(32)

# Correlation Coefficient (r) to Fisher's Z (Rubio-Abadal et al., 2014)
convert_r2z(0.061)         # Age at first psychotic symptom
convert_r2z(0.059)         # Age at first psychiatry contact
convert_r2z(-0.036)        # Age at first hospitalization
convert_r2z(0.054)         # PANSS Total Score

# Calculating Standard Error of Fisher's Z Prime (Rubio-Abadal et al., 2014)
SEz(42)
SEz(39) # Age at first hospitalization

# Correlation Coefficient (r) to Fisher's Z (Ruiz et al., 2000)
convert_r2z(0.076)            # Age at illness onset

# Calculating Standard Error of Fisher's Z Prime (Ruiz et al., 2000)
SEz(105)

# T-Value to Correlation Coefficient (r) (Damme et al., 2024)
# Added negative sign to aid in directionality 
esc_t(
  t = 4.64,
  totaln = 4422,
  es.type = "r") #OG Model

esc_t(
  t = 4.95,
  totaln = 4422,
  es.type = "r") #Moderation Model

# Calculating Standard Error of Fisher's Z Prime (Damme et al., 2024)
SEz(4422)





