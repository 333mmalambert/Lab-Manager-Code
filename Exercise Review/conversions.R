########################################################################
# Effect Size Transformations and Standard Error Calculations (Exercise)
# Authors: Emma Lambert, Faith Crighton, and Donna Vaghefikia
# Date: 8/7/2026
########################################################################

### Negative Signs have been added/removed to aid in directionality where needed ###

# Import Libraries
library(esc)
library(effectsize)
library(psychometric)
library(metafor)

# Setting functions
# Formula: r = √(F * df1)/(F * df1 + df2)
f_to_r <- function(F, df1, df2) {
  sqrt((F * df1) / (F * df1 + df2))}

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to Standardized Mean Difference (d) (Damme et al., 2022)
esc_f(f = -5.31, # Aerobic Exercise + Positive Symptoms
      grp1n = 13, # Exercise Condition
      grp2n = 12, # Waitlist Condition
      es.type = "d")
# Standardized Mean Difference (d) = -0.9225

# F-Value to Standardized Mean Difference (d) (Damme et al., 2022)
esc_f(f = -0.33, # Aerobic Exercise + Negative Symptoms
      grp1n = 13, # Exercise Condition
      grp2n = 12, # Waitlist Condition
      es.type = "d")
# Standardized Mean Difference (d) = -0.2300

# Standardized Mean Difference (d) to Correlation Coefficient (Damme et al., 2022)
d_to_r(-0.9225, 13, 12) # Aerobic Exercise + Positive Symptoms
# Correlation Coefficient (r) = -0.4330984
d_to_r(-0.2300, 13, 12) # Aerobic Exercise + Negative Symptoms
# Correlation Coefficient (r) = -0.1189493

# F-Value to to Correlation Coefficient (r) (Damme et al., 2022)
f_to_r <- function(F, df1, df2) {
  sqrt((F * df1) / (F * df1 + df2))}

r_rise <- f_to_r(14.81, 1, 13) # Aerobic Exercise + RiSE
# r = 0.7297549
r_hippo_con <- f_to_r(4.67, 1, 21) # Aerobic Exercise + hippocampal-occipital connectivity
# r = 0.426526
r_sub_con <- f_to_r(3.12, 1, 28) # Aerobic Exercise +  right subiculum body
# r = 0.316634

# Correlation Coefficient (r) to Fisher's Z (Damme et al., 2022)
convert_r2z(-0.4330984) # Aerobic Exercise + Positive Symptoms
# Fisher's Z = -0.4637042
convert_r2z(-0.1189493) # Aerobic Exercise + Negative Symptoms
# Fisher's Z = -0.1195151
convert_r2z(0.7297549) # Aerobic Exercise + RiSE
# Fisher's Z = 0.9282028
convert_r2z(0.426526) # Aerobic Exercise + hippocampal-occipital connectivity
# Fisher's Z = 0.4556424
convert_r2z(0.316634) # Aerobic Exercise +  right subiculum body
# Fisher's Z = 0.3279016

# Calculating Standard Error of Fisher's Z Prime
SEz(25)
# Standard Error of Fishers z prime: 0.2132007

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Correlation Coefficient (r) to Fisher's Z (Mittal et al., 2014)
convert_r2z(.02) # Total Physical Activity x Positive Symptoms
# Fisher's Z = 0.02000267
convert_r2z(-.16) # Total Physical Activity x Negative Symptoms
# Fisher's Z = -0.1613867
convert_r2z(.44) # Total Physical Activity x Right Parahippocampal Gyrus
# Fisher's Z = 0.4722308
convert_r2z(.51) # Total Physical Activity x Left Parahippocampal Gyrus
# Fisher's Z = 0.5627298

# Calculating Standard Error of Fisher's Z Prime
SEz(29)
# Standard Error of Fishers z prime: 0.1961161

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0
# Standardized Mean Difference (d) to Correlation Coefficient (Dean et al., 2017)
d_to_r(-0.61, 9, 9) # Exercise x Positive Symptoms
# Correlation Coefficient (r) = -0.3077961

d_to_r(-0.47, 9, 9) # Exercise x Negative Symptoms
# Correlation Coefficient (r) = -0.2418553

d_to_r(0.92, 9, 9) # Exercise x Working Memory
# Correlation Coefficient (r) = 0.4384954

d_to_r(0.47, 9, 9) # Exercise x Reasoning and Problem Solving
# Correlation Coefficient (r) = 0.2418553

d_to_r(1.3, 9, 9) # Exercise x Processing Speed
# Correlation Coefficient (r) = 0.5676068

d_to_r(.63, 9, 9) # Exercise x Verbal Learning
# Correlation Coefficient (r) = 0.3168889

d_to_r(1.74, 9, 9) # Exercise x Cognition Composite
# Correlation Coefficient (r) = 0.67816

d_to_r(.18, 9, 9) # Exercise x Left Hippo Vol
# Correlation Coefficient (r) = 0.09502743

d_to_r(.31, 9, 9) # Exercise x Right Hippo Vol
# Correlation Coefficient (r) = 0.1622246

# T-Value to Correlation Coefficient (r) + Fisher's Z (Dean et al., 2017)
esc_t(
  t = 9.71,
  totaln = 9,
  es.type = "r") # Left Hippocampal Seed x Left Occipital Pole
# r: 0.9648
# Fisher's z: 2.0114

esc_t(
  t = 10.79,
  totaln = 9,
  es.type = "r") # Left Hippocampal Seed x Right Occipital Pole
# r: 0.9712
# Fisher's z: 2.1135

esc_t(
  t = 0.9721,
  totaln = 9,
  es.type = "r") # Left Hippocampal Seed x Lateral Occipital Cortex
# r: 0.9721
# Fisher's z: 2.1287


# Correlation Coefficient (r) to Fisher's Z (Dean et al., 2017)
convert_r2z(-0.3077961) # Exercise x Positive Symptoms
# Fisher's Z = -0.318109

convert_r2z(-0.2418553) # Exercise x Negative Symptoms
# Fisher's Z =-0.2467437

convert_r2z(0.4384954) # Exercise x Working Memory
# Fisher's Z = 0.4703665

convert_r2z(0.2418553) # Exercise x Reasoning and Problem Solving
# Fisher's Z = 0.2467437

convert_r2z(0.5676068) # Exercise x Processing Speed
# Fisher's Z = 0.643985

convert_r2z(0.3168889) # Exercise x Verbal Learning
# Fisher's Z = 0.3281849

convert_r2z(0.67816) # Exercise x Cognition Composite
# Fisher's Z = 0.8256994

convert_r2z(0.09502743) # Exercise x Left Hippo Vol
# Fisher's Z = 0.09531503

convert_r2z(0.1622246) # Exercise x Right Hippo Vol
# Fisher's Z = 0.1636706


# Calculating Standard Error of Fisher's Z Prime
SEz(9)
# Standard Error of Fishers z prime: 0.4082483

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (Damme and Mittal, 2026)
d_to_r(-0.76, 17, 13) # Exercise x Dysphoric Mood (Neg Symp)
# Correlation Coefficient (r) = -0.363204

# Correlation Coefficient (r) to Fisher's Z (Damme and Mittal, 2026)
convert_r2z(-0.363204) # Exercise x Dysphoric Mood (Neg Symp)
# Fisher's Z = -0.3805719

# Calculating Standard Error of Fisher's Z Prime
SEz(30)
# Standard Error of Fishers z prime: 0.1924501

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# T-Value to Correlation Coefficient (r) + Fisher's Z (Damme et al., 2024)
esc_t(
  t = 5.49,
  totaln = 4514,
  es.type = "r") # PLE Severity x Time Spent Sedentary
# r: 0.0815
# Fisher's z: 0.0816

esc_t(
  t = -2.70,
  totaln = 4522,
  es.type = "r") # PLE Severity x Time Spent in Moderate to Intense Physical Activity
# r: -0.0401
# Fisher's z: -0.0401

# Calculating Standard Error of Fisher's Z Prime
SEz(4514)
# Standard Error of Fishers z prime: 0.01488893
SEz(4522)
# Standard Error of Fishers z prime: 0.01487575

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Spearman's Rank Correlation Coefficient to Fisher's Z (Walther et al., 2023) 
convert_r2z(-.32) # Activity Levels x PANSS Negative Scores
# Fisher's Z: -0.3316471
convert_r2z(.20) # Activity Levels x PANSS Positive Scores
# Fisher's Z: 0.2027326

# Calculating Standard Error of Fisher's Z Prime
SEz(52)
# Standard Error of Fishers z prime: 0.1428571

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Calculate Pooled Standard Deviation (Firth et al., 2016) (1)
# Formula: SDPooled = √(n1-1)*s1^2 + (n2-1)*s2^2 / (n1+n2-2)

SDpooled_total <- sqrt(((25-1)*14.9^2 + (7-1)*5.2^2)/(25+7-2)) # PANSS Total
SDpooled_pos <- sqrt(((25-1)*3.5^2 + (7-1)*3.2^2)/(25+7-2)) # PANSS Positive
SDpooled_neg <- sqrt(((25-1)*5.1^2 + (7-1)*1.4^2)/(25+7-2)) # PANSS Negative
SDpooled_gen <- sqrt(((25-1)*8.8^2 + (7-1)*2.9^2)/(25+7-2)) # PANSS General

# Calculate Cohen's D (Firth et al., 2016) (1)
# Formula: d = (Mean1 - Mean2)/SDPooled

d_total <- ((-13.1) - (-3.3))/SDpooled_total
# Cohen's D: -0.7244052
d_pos <- ((-2.9) - (-0.9))/SDpooled_pos
# Cohen's D: -0.5810419
d_neg <- ((-4.0) - (-1.0))/SDpooled_neg
# Cohen's D: -0.6515584
d_gen <- ((-6.2) - (-1.4))/SDpooled_gen
# Cohen's D: -0.601723

# Standardized Mean Difference (d) to Correlation Coefficient (Firth et al., 2016) (1)
d_to_r(-0.7244052) # PANSS Total
# Correlation Coefficient (r) = -0.3405522
d_to_r(-0.5810419) # PANSS Positive
# Correlation Coefficient (r) = -0.2789859
d_to_r(-0.6515584) # PANSS Negative
# Correlation Coefficient (r) = -0.3097561
d_to_r(-0.601723) # PANSS General
# Correlation Coefficient (r) = -0.2881047
d_to_r(0.4) # Processing Speed (Trail A)
# Correlation Coefficient (r) = 0.1961161
d_to_r(0.65) # Processing Speed (Trail B)
# Correlation Coefficient (r) = 0.3090861
d_to_r(-0.17) # Processing Speed (Digit-Symbol Coding)
# Correlation Coefficient (r) = -0.08469459
d_to_r(-0.48) # Executive Function (Stockings of Cambridge)
# Correlation Coefficient (r) = -0.233373
d_to_r(-0.07) # Executive Function (Spatial span)
# Correlation Coefficient (r) = -0.03497858
d_to_r(-0.88) # STM
# Correlation Coefficient (r) = -0.4027386

# Correlation Coefficient (r) to Fisher's Z (Firth et al., 2016) (1)
convert_r2z(-0.3405522) # PANSS Total
# Fisher's Z:  -0.354717
convert_r2z(-0.2789859) # PANSS Positive
# Fisher's Z: -0.286582
convert_r2z(-0.3097561) # PANSS Negative
# Fisher's Z: -0.3202756
convert_r2z(-0.2881047) # PANSS General
# Fisher's Z: -0.2964982
convert_r2z(0.1961161) # Processing Speed (Trail A)
# Fisher's Z: 0.2964982
convert_r2z(0.3090861) # Processing Speed (Trail B)
# Fisher's Z: 0.3195347
convert_r2z(0.08469459) # Processing Speed (Digit-Symbol Coding)
# Fisher's Z: 0.08489798
convert_r2z(0.233373)  # Executive Function (Stockings of Cambridge)
# Fisher's Z: 0.2377538
convert_r2z(0.03497858) # Executive Function (Spatial span)
# Fisher's Z: 0.03499286
convert_r2z(0.4027386) # STM
# Fisher's Z: 0.4269134

# Calculating Standard Error of Fisher's Z Prime
SEz(32)
# Standard Error of Fishers z prime: 0.1856953

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to Standardized Mean Difference (d) (Erickson et al., 2010)
esc_f(f = 2.08, # Aerobic Exercise x Positive Symptoms
      grp1n = 60, # Aerobic Group
      grp2n = 60, # Stretching Control Group
      es.type = "d")
# Cohen's D: 0.2633

# Standardized Mean Difference (d) to Correlation Coefficient (Erickson et al., 2010)
d_to_r(0.2633, 60, 60)
# Correlation Coefficient (r) = 0.1316062

# F-Value to to Correlation Coefficient (r) (Erickson et al., 2010)
r_left <- f_to_r(8.25, 2, 114) # Aerobic Exercise x Left Hippocampus Vol
# r = 0.3555795
r_right <- f_to_r(10.41, 2, 114) # Aerobic Exercise x Right Hippocampus Vol
# r = 0.3929734

# Correlation Coefficient (r) to Fisher's Z (Erickson et al., 2010)
convert_r2z(0.1316062) # Aerobic Exercise x Positive Symptoms
# Fisher's Z: 0.132374
convert_r2z(0.3555795) # Aerobic Exercise x Left Hippocampus Vol
# Fisher's Z: 0.3718164
convert_r2z(0.3929734) # Aerobic Exercise x Right Hippocampus Vol
# Fisher's Z: 0.4153116

# Calculating Standard Error of Fisher's Z Prime
SEz(120)
# Standard Error of Fishers z prime:  0.09245003

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to to Correlation Coefficient (r) (Silva et al., 2015)
r_pos <- f_to_r(4.455, 2, 31) # Concurrent/Resistance Exercise x PANSS Positive
# r = 0.4724958
r_neg <- f_to_r(2.965, 2, 31) # Concurrent/Resistance Exercise x PANSS Negative
# r = 0.4007169

# Correlation Coefficient (r) to Fisher's Z (Silva et al., 2015)
convert_r2z(0.4724958)
# Fisher's Z: 0.5132786
convert_r2z(0.4007169)
# Fisher's Z: 0.4245027

# Calculating Standard Error of Fisher's Z Prime
SEz(34)
# Standard Error of Fishers z prime:  0.1796053

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to Standardized Mean Difference (d) (Pajonk et al., 2015)
esc_f(f = 13.8,  # Aerobic Exercise x Hippocampal
      grp1n = 8, # SCZ Exercise Group
      grp2n = 8, # SCZ Control Group
      es.type = "d")
# Cohen's D: 1.8574

esc_f(f = 4.95,  # Aerobic Exercise x STM Scores
      grp1n = 8, # SCZ Exercise Group
      grp2n = 8, # SCZ Control Group
      es.type = "d")
# Cohen's D: 1.1124

esc_f(f = 6.76,  # Aerobic Exercise x PANSS Total
      grp1n = 8, # SCZ Exercise Group
      grp2n = 8, # SCZ Control Group
      es.type = "d")
# Cohen's D: 1.3000

# Standardized Mean Difference (d) to Correlation Coefficient (Pajonk et al., 2015)
d_to_r(1.8574, 8, 8) # Aerobic Exercise x Hippocampal Vol
# Correlation Coefficient (r) = 0.7045553

d_to_r(1.1124, 8, 8) # Aerobic Exercise x STM Scores
# Correlation Coefficient (r) = 0.5110806

d_to_r(1.3000, 8, 8) # Aerobic Exercise x PANSS Total
# Correlation Coefficient (r) = 0.5706367

# Correlation Coefficient (r) to Fisher's Z (Pajonk et al., 2015)
convert_r2z(0.7045553) # Aerobic Exercise x Hippocampal Vol
# Fisher's Z: 0.8762889

convert_r2z(0.5110806) # Aerobic Exercise x STM Scores
# Fisher's Z: 0.5641913

convert_r2z(0.5706367) # Aerobic Exercise x PANSS Total
# Fisher's Z: 0.5706367

# Calculating Standard Error of Fisher's Z Prime
SEz(16) # All reported analyses done WITHIN SCZ group
# Standard Error of Fishers z prime: 0.2773501

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to Standardized Mean Difference (d) (Nuechterlein et al., 2022)
esc_f(f = 3.33,  # Exercise x Cognition Composite
      grp1n = 24, # Cog Training + Exercise
      grp2n = 23, # Cog Training Only
      es.type = "d")
# Cohen's D:  0.5325

esc_f(f = 3.01,  # Exercise x BDNF
      grp1n = 24, # Cog Training + Exercise
      grp2n = 23, # Cog Training Only
      es.type = "d")
# Cohen's D:  0.5062

# Standardized Mean Difference (d) to Correlation Coefficient (Nuechterlein et al., 2022)
d_to_r(0.5325, 24, 23) # Exercise x Cognition
# Correlation Coefficient (r) = 0.2625008
d_to_r(0.5062, 24, 23) # Exercise x BDNF
# Correlation Coefficient (r) = 0.2503684

# Correlation Coefficient (r) to Fisher's Z (Nuechterlein et al., 2022)
convert_r2z(0.2625008) # Exercise x Cognition
# Fisher's Z: 0.2687924
convert_r2z(0.2503684) # Exercise x BDNF
# Fisher's Z: 0.2558058

# Calculating Standard Error of Fisher's Z Prime
SEz(47) 
# Standard Error of Fishers z prime: 0.1507557

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

####PANSS d to r to z (Lin et al., 2015)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)
d_to_r(0.51) # Yoga x PANSS Positive
#Correlation Coefficient r = 0.2470929
d_to_r(0.91) # Yoga x PANSS Negative
#Correlation Coefficient r = 0.4141458
d_to_r(0.77) # Yoga x Working Memory (DS Forwards Test)
#Correlation Coefficient r = 0.3592917
d_to_r(0.71) # Yoga x Working Memory (DS Backwards Test)
#Correlation Coefficient r = 0.3345448
d_to_r(0.12) # Yoga x Executive Function (Stroop Incongruent)
#Correlation Coefficient r = 0.05989229
d_to_r(0.40) # Yoga x Verbal Retention (HKLLT)
#Correlation Coefficient r = 0.1961161
d_to_r(0.29) # Aerobic Exercise x PANSS Positive
#Correlation Coefficient r = 0.1434993
d_to_r(0.61) # Aerobic Exercise x PANSS Negative
#Correlation Coefficient r = 0.2917325
d_to_r(0.59) # Aerobic Exercise x Working Memory (DS Forwards Test)
#Correlation Coefficient r = 0.2829451
d_to_r(1.08) # Aerobic Exercise x Working Memory (DS Backwards Test)
#Correlation Coefficient r = 0.4751489
d_to_r(0.36) # Aerobic Exercise x Executive Function (Stroop Incongruent)
#Correlation Coefficient r = 0.177153
d_to_r(0.56) # Aerobic Exercise x Verbal Retention (HKLLT)
#Correlation Coefficient r = 0.2696299

# Calculating Degrees of Freedom (Lin et al., 2015)
# Formulas: df1 = t - 1, df2 = (n -1)(t-1)
df1 <- 3 - 1
df2 <- (63 - 1)*(3-1)

# F-Value to Correlation Coefficient (r) (Lin et al., 2015)
# Formula: r = √(F * df1)/(F * df1 + df2)
r_aero <- f_to_r(7.52, df1, df2) # Aerobic Exercise x Total Hippo Vol
# r = 0.3288926
r_yoga <- f_to_r(1.07, df1, df2) # Yoga x Total Hippo Vol
# r = 0.1302508

##step 2: Correlation Coefficient (r) to Fisher's Z (z)
convert_r2z(0.2470929) #  Yoga x PANSS Positive
# Fisher's Z:  0.2523143
convert_r2z(0.4141458) # Yoga x PANSS Negative
# Fisher's Z:  0.440605
convert_r2z(0.3592917) #  Yoga x Working Memory (DS Forwards Test)
# Fisher's Z:  0.3760724
convert_r2z(0.3345448) # Yoga x Working Memory (DS Backwards Test)
# Fisher's Z:  0.3479371
convert_r2z(0.05989229) #  Yoga x Executive Function (Stroop Incongruent)
# Fisher's Z:  0.05996406
convert_r2z(0.1961161) #  Yoga x Verbal Retention (HKLLT)
# Fisher's Z:  0.1986901
convert_r2z(0.1302508) # Yoga x Total Hippo Vol
# Fisher's Z:  0.130995
convert_r2z(0.1434993) # Aerobic Exercise x PANSS Positive
# Fisher's Z:  0.1444966
convert_r2z(0.2917325) # Aerobic Exercise x PANSS Negative
# Fisher's Z:  0.3004589
convert_r2z(0.2829451) # Aerobic Exercise x Working Memory (DS Forwards Test)
# Fisher's Z:  0.2908806
convert_r2z(0.4751489) # Aerobic Exercise x Working Memory (DS Backwards Test)
# Fisher's Z:  0.5166998
convert_r2z(0.177153) # Aerobic Exercise x Executive Function (Stroop Incongruent)
# Fisher's Z:  0.1790419
convert_r2z(0.2696299) # Aerobic Exercise x Verbal Retention (HKLLT)
# Fisher's Z:  0.2764647
convert_r2z(0.3288926) # Aerobic Exercise x Total Hippo Vol
# Fisher's Z:  0.341586

##step 3: Calculating Standard Error of Fisher's Z Prime
SEz(41) # Yoga
# Standard Error of Fishers z prime: 0.1622214
SEz(40) # Aerobic
# Standard Error of Fishers z prime: 0.164399

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to to Correlation Coefficient (r) (Malchow et al., 2015)
r_pos <- f_to_r(3.5, 2, 37) # PANSS Positive
# r = 0.398862
r_neg <- f_to_r(3.5, 2, 37) # PANSS Negative
# r = 0.398862
r_stm <- f_to_r(2.7, 4, 108) # STM Score
# r = 0.3015113
r_prospeed <- f_to_r(2.5, 4, 108) # Processing Speed
# r = 0.2911113

# Correlation Coefficient (r) to Fisher's Z (Malchow et al., 2015)
convert_r2z(0.398862) # PANSS Positive
# Fisher's Z: 0.4222949
convert_r2z(0.398862) # PANSS Negative
# Fisher's Z: 0.4222949
convert_r2z(0.3015113) # STM Score
# Fisher's Z: 0.3111812
convert_r2z(0.2911113) # Processing Speed
# Fisher's Z: 0.29978

# Calculating Standard Error of Fisher's Z Prime
SEz(66) 
# Standard Error of Fishers z prime: 0.1259882
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Wang et al.,2018)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)

d_to_r(-.09) # changes in positive symptoms
#Correlation Coefficient r = -0.04495451

d_to_r(-.97) # changes in negative symptoms
#Correlation Coefficient r = -0.4363839

d_to_r(-.64) # changes in general psychopathology between baseline and end
#Correlation Coefficient r = -0.3047757

##step 2: Correlation Coefficient (r) to Fisher's Z (z)

convert_r2z(-0.04495451) # changes in positive symptoms
# Fisher's Z: -0.04498483

convert_r2z(-0.4363839) # changes in negative symptoms
# Fisher's Z: -0.4677554

convert_r2z(-0.3047757) # changes in general psychopathology between baseline and end 
# Fisher's Z: -0.314776

##step 3: Calculating Standard Error of Fisher's Z Prime

SEz(62)
# Standard Error of Fishers z prime: 0.1301889

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Calculating Degrees of Freedom (Firth et al., 2016)(2)
# Formulas: df1 = t - 1, df2 = (n -1)(t-1)
df1 <- 3 - 1
df2 <- (19 -1)*(3-1)

# F-Value to Correlation Coefficient (r) (Firth et al., 2016)(2)
# Formula: r = √(F * df1)/(F * df1 + df2)
r_pos <- f_to_r(3.89, df1, df2) # Exercise x PANSS Positive
# r = 0.4215527
r_neg <- f_to_r(7.46, df1, df2) # Exercise x PANSS Negative
# r = 0.5413027
r_stm <- f_to_r(4.47, df1, df2) # Exercise x Verbal STM
# r = 0.4460178
r_prospeed_a <- f_to_r(4.57, df1, df2) # Exercise x Processing Speed (Trail Making Task A)
# r = 0.4499791
r_prospeed_b <- f_to_r(9.21, df1, df2) # Exercise x Processing Speed (Trail Making Task B)
# r = 0.5817891
r_ef <- f_to_r(3.08, df1, df2) # Exercise x EF (Stocking of Cambridge)
# r = 0.3822435


# Correlation Coefficient (r) to Fisher's Z (Firth et al., 2016)(2)
convert_r2z(0.4215527) # Exercise x PANSS Positive
# Fisher's Z: 0.4495788
convert_r2z(0.5413027) # Exercise x PANSS Negative
# Fisher's Z: 0.6059964
convert_r2z(0.4460178) # Exercise x Verbal STM
# Fisher's Z: 0.4797181
convert_r2z(0.4499791) # Exercise x Processing Speed (Trail Making Task A)
# Fisher's Z: 0.4846741
convert_r2z(0.5817891) # Exercise x Processing Speed (Trail Making Task B)
# Fisher's Z: 0.665163
convert_r2z(0.3822435) # Exercise x Executive Function (Stocking of Cambridge)
# Fisher's Z: 0.4026844

# Calculating Standard Error of Fisher's Z Prime
SEz(19) # PANSS
# Standard Error of Fishers z prime: 0.25
SEz(13) # Verbal STM
# Standard Error of Fishers z prime: 0.3162278
SEz(14) # Processing Speed
# Standard Error of Fishers z prime: 0.3015113
SEz(13) # Executive Function
# Standard Error of Fishers z prime: 0.3015113

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0
# Correlation Coefficient (r) to Fisher's Z (Hallgren et al., 2018)
convert_r2z(0.153) # Exercise x Processing Speed
# Fisher's Z: 0.1542109
convert_r2z(0.198) # Exercise x Working Memory
# Fisher's Z: 0.2006501

# Calculating Standard Error of Fisher's Z Prime
SEz(63)
# Standard Error of Fishers z prime: 0.1290994

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Calculating SDChange (O'Dea et al., 2022)
# Formula: SDChange = √SD^2(pre) + SD^2(post) - 2p(SDpre)(SDpost)
SDChange_pos = sqrt((7.39)^2 + (3.02)^2 - 2*(.05)*(7.39)*(3.02))
# SDChange = 7.842239
SDChange_neg = sqrt((5.16)^2 + (4.4)^2 - 2*(.10)*(5.16)*(4.4))
# SDChange = 6.437764

# Calculating Cohen's D from Mean Difference (O'Dea et al., 2022)
# Formula: d = Mean(post) - Mean(pre)
pos_d <- (11.25 - 15.5)/SDChange_pos
# Cohen's D: -0.541937
neg_d <- (12.08 - 15.5)/SDChange_neg
# Cohen's D: -0.5312404

# Standardized Mean Difference (d) to Correlation Coefficient (r) (O'Dea et al., 2022)
d_to_r(-0.541937) # Yoga x PANSS Positive
#Correlation Coefficient r = -0.261537
d_to_r(-0.5312404) # Yoga x PANSS Negative
#Correlation Coefficient r = -0.2567183

# Correlation Coefficient (r) to Fisher's Z (O'Dea et al., 2022)
convert_r2z(-0.261537) # Yoga x PANSS Positive
# Fisher's Z: -0.2677575
convert_r2z(-0.2567183) # Yoga x PANSS Negative
# Fisher's Z: -0.262592

# Calculating Standard Error of Fisher's Z Prime
SEz(12)
# Standard Error of Fishers z prime: 0.3333333
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Bang-Kittilsen et al., 2020)
d_to_r(0.32) # High Intensity Exercise x Neurocognition Composite
#Correlation Coefficient r = 0.1579905
d_to_r(0.37) # High Intensity Exercise x Working Memory
#Correlation Coefficient r = 0.1819132
d_to_r(0.38) # High Intensity Exercise x Processing Speed
#Correlation Coefficient r = 0.1866606
d_to_r(0.48) # High Intensity Exercise x Verbal Learning
#Correlation Coefficient r = 0.1866606

# Correlation Coefficient (r) to Fisher's Z (Bang-Kittilsen et al., 2020)
convert_r2z(0.1579905) # High Intensity Exercise x Neurocognition Composite
# Fisher's Z: 0.1593251
convert_r2z(0.1819132) # High Intensity Exercise x Working Memory
# Fisher's Z: 0.1839607
convert_r2z(0.1866606) # High Intensity Exercise x Processing Speed
# Fisher's Z: 0.188875
convert_r2z(0.233373) # High Intensity Exercise x Verbal Learning
# Fisher's Z: 0.2377538

# Calculating Standard Error of Fisher's Z Prime
SEz(43)
# Standard Error of Fishers z prime: 0.158113

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Don't love this paper, does not rlly split up MDD vs SCZ and stats are not fully provided

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Oertel-Knochel et al., 2014)
d_to_r(0.47) # Aerobic Exercise x Neurocognition Composite (min)
#Correlation Coefficient r = 0.228768
d_to_r(0.57) # Aerobic Exercise x Neurocognition Composite (max)
#Correlation Coefficient r = 0.274086
d_to_r(0.12) # Aerobic Exercise x SCZ Symptoms (min)
#Correlation Coefficient r = 0.05989229
d_to_r(0.24) # Aerobic Exercise x SCZ Symptoms (max)
#Correlation Coefficient r = 0.1191452

# Correlation Coefficient (r) to Fisher's Z (Oertel-Knochel et al., 2014)
convert_r2z(0.228768) # Aerobic Exercise x Neurocognition Composite (min)
# Fisher's Z: 0.232889
convert_r2z(0.274086) # Aerobic Exercise x Neurocognition Composite (max)
# Fisher's Z: 0.2812764
convert_r2z(0.05989229) # Aerobic Exercise x SCZ Symptoms (min)
# Fisher's Z: 0.05996406
convert_r2z(0.1191452) # Aerobic Exercise x SCZ Symptoms (max)
# Fisher's Z: 0.1197138

# Calculating Standard Error of Fisher's Z Prime
SEz(16) # Exercise Group
# Standard Error of Fishers z prime: 0.2773501
SEz(8) # SCZ Group
# Standard Error of Fishers z prime: 0.4472136

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Chen et al.,2016)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)

d_to_r(0.51) # processing speed
#Correlation Coefficient r = 0.2470929

d_to_r(0.35) # memory- range min
d_to_r(0.41) # memory- range max
#Correlation Coefficient min r = 0.1723803
#Correlation Coefficient max r = 0.2008236

d_to_r(0.51) # spontaneity and fluency aspects of executive function
#Correlation Coefficient r = 0.2470929

d_to_r(0.09) # RAVLT learning score
#Correlation Coefficient r = 0.04495451

##step 2: Correlation Coefficient (r) to Fisher's Z (z)

convert_r2z(0.2470929) # processing speed
# Fisher's Z: 0.2523143

convert_r2z(0.1723803) # memory- range min 
convert_r2z(0.2008236) # memory- range max 
# min Fisher's Z: 0.1741188
# max Fisher's Z: 0.2035906

convert_r2z(0.2470929) # spontaneity and fluency aspects of executive function 
# Fisher's Z: 0.2523143

convert_r2z(0.04495451) # RAVLT learning score
# Fisher's Z: 0.04495451


##step 3: Calculating Standard Error of Fisher's Z Prime

SEz(36)
# Standard Error of Fishers z prime: 0.1740777
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Kimhy et al.,2015)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)
d_to_r(0.93) # AE intervention
#Correlation Coefficient r = 0.4216441

##step 2: Correlation Coefficient (r) to Fisher's Z (z)
convert_r2z(0.4216441) # AE intervention 
# Fisher's Z: 0.4496899
convert_r2z(.54) # neurocognition enhancement 
# Fisher's Z: 0.6041556
convert_r2z(.48) # neurocognition - social cognition 
# Fisher's Z: 0.5229843
convert_r2z(.41) # neurocognition - visual learning 
# Fisher's Z: 0.4356112

##step 3: Calculating Standard Error of Fisher's Z Prime
SEz(26)
# Standard Error of Fishers z prime: 0.2085144
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Fisher et al., 2020)
d_to_r(-0.37, 7, 8) # Exercise x Human C-Reactive protein
#Correlation Coefficient r = -0.1944934
d_to_r(0.25, 7, 8) # Exercise x IL-6 
#Correlation Coefficient r = 0.1327865
d_to_r(0.13, 7, 8) # Exercise x TNF-α
#Correlation Coefficient r = 0.06949743
d_to_r(0.23, 7, 8) # Exercise x BDNF
#Correlation Coefficient r = 0.1223293

# Correlation Coefficient (r) to Fisher's Z (Fisher et al., 2020)
convert_r2z(-0.1944934) # Exercise x Human C-Reactive protein
# Fisher's Z: -0.197003
convert_r2z(0.1327865) # Exercise x IL-6 
# Fisher's Z: 0.1335753
convert_r2z(0.06949743) # Exercise x TNF-α
# Fisher's Z: 0.06960964
convert_r2z(0.1223293) # Exercise x BDNF
# Fisher's Z: 0.122945

# Calculating Standard Error of Fisher's Z Prime
SEz(15)
# Standard Error of Fishers z prime: 0.2886751

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Bang-Kittilsen et al., 2022)
d_to_r(-0.37) # # High Intensity Exercise x Negative Symptoms
#Correlation Coefficient r = -0.1819132

# Correlation Coefficient (r) to Fisher's Z (Bang-Kittilsen et al., 2022)
convert_r2z(-0.1819132) # High Intensity Exercise x Negative Symptoms
# Fisher's Z: 0.1839607

# Calculating Standard Error of Fisher's Z Prime
SEz(43)
# Standard Error of Fishers z prime: 0.1581139

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Romain et al., 2019)
d_to_r(-0.48) # High Intensity Exercise x  PANSS Negative 
#Correlation Coefficient r = -0.233373
d_to_r(-0.20) # High Intensity Exercise x  PANSS Positive 
#Correlation Coefficient r = -0.09950372

# Correlation Coefficient (r) to Fisher's Z (Romain et al., 2019)
convert_r2z(-0.233373) # High Intensity Exercise x PANSS Negative
# Fisher's Z: -0.2377538
convert_r2z(-0.09950372) # High Intensity Exercise x PANSS Positive
# Fisher's Z: -0.09983408

# Calculating Standard Error of Fisher's Z Prime
SEz(38)
# Standard Error of Fishers z prime: 0.1581139

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Calculating SDChange (Wu et al., 2015)
# Formula: SDChange = √SD^2(pre) + SD^2(post) - 2p(SDpre)(SDpost)
SDChange_pos = sqrt((2.27)^2 + (2.00)^2 - 2*(.729)*(2.27)*(2.00))
# SDChange = 1.591722
SDChange_neg = sqrt((2.16)^2 + (1.71)^2 - 2*(.001)*(2.16)*(1.71))
# SDChange = 2.7536

# Calculating Cohen's D from Mean Difference (Wu et al., 2015)
# Formula: d = Mean(post) - Mean(pre)
pos_d <- (12.33 - 12.28)/SDChange_pos
# Cohen's D: 0.03141251
neg_d <- (13.00 - 14.28)/SDChange_neg
# Cohen's D: -0.464846

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Wu et al., 2015)
d_to_r(0.03141251) # High Intensity Exercise x PANSS Positive
#Correlation Coefficient r = 0.01570432
d_to_r(-0.464846) # High Intensity Exercise x PANSS Negative
#Correlation Coefficient r = -0.2263886

# Correlation Coefficient (r) to Fisher's Z (Wu et al., 2015)
convert_r2z(0.01570432) # High Intensity Exercise x PANSS Positive
# Fisher's Z: 0.01570561
convert_r2z(-0.2263886) # High Intensity Exercise x PANSS Negative
# Fisher's Z: -0.2303797

# Calculating Standard Error of Fisher's Z Prime
SEz(18)
# Standard Error of Fishers z prime: 0.2581989

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Calculating SDChange (Verma et al., 2018)
# Formula: SDChange = √SD^2(pre) + SD^2(post) - 2p(SDpre)(SDpost)
SDChange_pos = sqrt((27.83)^2 + (30.20)^2 - 2*(.058)*(27.83)*(30.20))
# SDChange = 39.86295
SDChange_neg = sqrt((21.42)^2 + (15.15)^2 - 2*(.006)*(21.42)*(15.15))
# SDChange = 26.16189
SDChange_tmtA = sqrt((45.93)^2 + (21.63)^2 - 2*(.022)*(45.93)*(21.63))
# SDChange = 50.33596
SDChange_tmtB = sqrt((49.94)^2 + (53.78)^2 - 2*(.004)*(49.94)*(53.78))
# SDChange = 73.24483

# Calculating Cohen's D from Mean Difference (Verma et al., 2018)
# Formula: d = Mean(post) - Mean(pre)
pos_d <- (17.81 - 22.19)/SDChange_pos
# Cohen's D: -0.1098765
neg_d <- (19.93 - 31.61)/SDChange_neg
# Cohen's D: -0.4464508
tmtA_d <- (57.6 - 71.15)/SDChange_tmtA
# Cohen's D:  -0.2691912
tmtB_d <- (94.28 - 109.4)/SDChange_tmtB
# Cohen's D: -0.2064309

# Standardized Mean Difference (d) to Correlation Coefficient (r) (Verma et al., 2018)
d_to_r(-0.1098765) # Yoga x SAPS
#Correlation Coefficient r = -0.05485553
d_to_r(-0.4464508) # Yoga x SANS
#Correlation Coefficient r = -0.2178634
d_to_r(-0.2691912) # Yoga x TMT A
#Correlation Coefficient r = -0.1333928
d_to_r(-0.2064309) # Yoga x TMT B
#Correlation Coefficient r = -0.10267

# Correlation Coefficient (r) to Fisher's Z (Verma et al., 2018)
convert_r2z(-0.05485553) # Yoga x SAPS
# Fisher's Z: -0.05485553
convert_r2z(-0.2178634) # Yoga x SANS
# Fisher's Z: -0.2214119
convert_r2z(-0.1333928) # Yoga x TMT A
# Fisher's Z: 0.1341925
convert_r2z(-0.10267) # Yoga x TMT B
# Fisher's Z: 0.1030331

# Calculating Standard Error of Fisher's Z Prime
SEz(21)
# Standard Error of Fishers z prime: 0.2357023
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (Kern et al., 2020)
d_to_r(0.02, 36, 18) # Aerobic Exercise x Cognition
# Correlation Coefficient (r) = 0.009607246

# Correlation Coefficient (r) to Fisher's Z (Kern et al., 2020)
convert_r2z(0.009607246) # # Aerobic Exercise x Cognition
# Fisher's Z = 0.009607542

# T-Value to Correlation Coefficient (r) + Fisher's Z (Kern et al., 2020)
esc_t(
  t = 0.62,
  totaln = 54,
  es.type = "r") # Aerobic Exercise x Positive Symptoms 
# r: 0.0857
# Fisher's z: 0.0859

esc_t(
  t = 0.31,
  totaln = 54,
  es.type = "r") # Aerobic Exercise x Negative Symptoms
# r: 0.0429
# Fisher's z: 0.0430

# Calculating Standard Error of Fisher's Z Prime
SEz(54)
# Standard Error of Fishers z prime: 0.01488893
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to to Correlation Coefficient (r) (Fernández-Abascal et al., 2015)
r_neg <- f_to_r(5.81, 2, 92) # Aerobic Exercise x PANSS Negative
# r = 0.3348739
r_pos <- f_to_r(0.28, 2, 92) # Aerobic Exercise x PANSS Positive
# r = 0.07778258
r_prospeed <- f_to_r(0.24, 2, 92) # Aerobic Exercise x Processing Speed
# r = 0.07204382
r_wm <- f_to_r(1.21, 2, 92) # Aerobic Exercise x Working Memory
# r = 0.1600942
r_ef <- f_to_r(0.26, 2, 92) # Aerobic Exercise x Executive Function
# r = 0.07496937

# Correlation Coefficient (r) to Fisher's Z (Fernández-Abascal et al., 2015)
convert_r2z(0.3348739) # Aerobic Exercise x PANSS Negative
# Fisher's Z: 0.3483077
convert_r2z(0.07778258) # Aerobic Exercise x PANSS Positive
# Fisher's Z: 0.07794002
convert_r2z(0.07204382) # Aerobic Exercise x Processing Speed
# Fisher's Z: 0.07216885
convert_r2z(0.1600942) # Aerobic Exercise x Working Memory
# Fisher's Z: 0.1614834
convert_r2z(0.07496937) # Aerobic Exercise x Executive Function
# Fisher's Z: 0.0751103

# Calculating Standard Error of Fisher's Z Prime
SEz(48)
# Standard Error of Fishers z prime:  0.1490712

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# TBD PAPERS #






# TBD PAPERS #






# TBD PAPERS #

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0
# Standardized Mean Difference (d) to Correlation Coefficient (Leroux et al., 2024)
d_to_r(0.78, 15, 14) # Aerobic Exercise x Left Subiculum Volume
# Correlation Coefficient (r) = 0.3745427
d_to_r(-1.18, 16, 16) # Aerobic Exercise x Positive PANSS
# Correlation Coefficient (r) = -0.5203542
d_to_r(-0.43, 16, 16) # Aerobic Exercise x Negative PANSS
# Correlation Coefficient (r) = -0.2167712

# Correlation Coefficient (r) to Fisher's Z (Leroux et al., 2024)
convert_r2z(0.3745427) # Aerobic Exercise x Left Subiculum Volume
# Fisher's Z = 0.3936967
convert_r2z(-0.5203542) # Aerobic Exercise x Positive PANSS
# Fisher's Z = 0.5768253
convert_r2z(-0.2167712) #  Aerobic Exercise x Negative PANSS
# Fisher's Z =-0.2202656

# Calculating Standard Error of Fisher's Z Prime
SEz(29)
# Standard Error of Fishers z prime: 0.1961161

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Su et al., 2016)

# Calculate Cohen's D 
# Formula: d = (Mean2 - Mean1)/SD1
d_pos <- ((13.55) - (16.05))/6.48
# Cohen's D: -0.3858025
d_neg <- ((15.68) - (22.36))/9/65
# Cohen's D: -0.0114188

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)
d_to_r(0.36) # Aerobic x Processing Speed
#Correlation Coefficient r = 0.177153
d_to_r(0.26) # Aerobic x Working memory
#Correlation Coefficient r = 0.1289152
d_to_r(-0.3858025) # Aerobic x PANSS Positive
#Correlation Coefficient r = -0.1894094
d_to_r(-0.0114188) # Aerobic x PANSS Negative
#Correlation Coefficient r = -0.005709307

##step 2: Correlation Coefficient (r) to Fisher's Z (z)
convert_r2z(0.177153) # Aerobic x Processing Speed
# Fisher's Z: 0.1790419
convert_r2z(0.1289152) # Aerobic x Working memory
# Fisher's Z: 0.1296366
convert_r2z(-0.1894094) # Aerobic x PANSS Positive
# Fisher's Z: -0.1917245
convert_r2z(-0.005709307) # Aerobic x PANSS Negative
# Fisher's Z: -0.005709369

##step 3: Calculating Standard Error of Fisher's Z Prime

SEz(44)
# Standard Error of Fishers z prime: 0.1561738

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Korman et al., 2018)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)

d_to_r(0.8) # SANS score decrease
#Correlation Coefficient r = 0.3713907

d_to_r(0.29) # BPRS score decrease
#Correlation Coefficient r = 0.1434993


##step 2: Correlation Coefficient (r) to Fisher's Z (z)

convert_r2z(0.3713907) # SANS score decrease
# Fisher's Z: 0.3900353

convert_r2z(0.1434993) # BPRS score decrease
# Fisher's Z: 0.1444966


##step 3: Calculating Standard Error of Fisher's Z Prime

SEz(10)
# Standard Error of Fishers z prime: 0.3779645

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Bhatia et al., 2022)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)

d_to_r(-0.94) # Yoga x  PANSS positive
#Correlation Coefficient r = -0.4253611

d_to_r(-0.73) # Yoga x PANSS Negative
#Correlation Coefficient r = -0.3428742

d_to_r(0.10) # Yoga x Working Memory
#Correlation Coefficient r = 0.04993762


##step 2: Correlation Coefficient (r) to Fisher's Z (z)

convert_r2z(-0.4253611) # Yoga x PANSS positive
# Fisher's Z: -0.4542193

convert_r2z(-0.3428742) # Yoga x PANSS Negative
# Fisher's Z: -0.357346

convert_r2z(0.04993762) # Yoga x Working Memory
# Fisher's Z: 0.125669

##step 3: Calculating Standard Error of Fisher's Z Prime

SEz(50)
# Standard Error of Fishers z prime: 0.145865

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (Ho et al., 2016)
d_to_r(0.28, 51, 49) # Tai-chi x Positive PANSS
# Correlation Coefficient (r) = 0.1400005
d_to_r(0.13, 51, 49) # Tai-chi x Negative PANSS
# Correlation Coefficient (r) = 0.06550579
d_to_r(0.06, 51, 49) # Tai-chi x STM (Forward DS)
# Correlation Coefficient (r) = 0.03028462
d_to_r(0.46, 51, 49) # Tai-chi x STM (Backward DS)
# Correlation Coefficient (r) = 0.2262644
d_to_r(0.45, 51, 49) # Tai-chi x Mean Cortisol
# Correlation Coefficient (r) = 0.2215897
d_to_r(-0.33, 51, 49) # Tai-chi x Diurnal Cortisol Slope
# Correlation Coefficient (r) = -0.1643752
d_to_r(0.25, 51, 49) # Aerobic x Positive PANSS
# Correlation Coefficient (r) = 0.1252497
d_to_r(0.49, 51, 49) # Aerobic x Negative PANSS
# Correlation Coefficient (r) = 0.2401941
d_to_r(0.42, 51, 49) # Aerobic x STM (Forward DS)
# Correlation Coefficient (r) = 0.2074746
d_to_r(0.30, 51, 49) # Aerobic x STM (Backward DS)
# Correlation Coefficient (r) = 0.1497836
d_to_r(0.46, 51, 49) # Aerobic x Mean Cortisol
# Correlation Coefficient (r) = 0.2262644
d_to_r(0.15, 51, 49) # Aerobic x Diurnal Cortisol Slope
# Correlation Coefficient (r) = 0.07552992

# Correlation Coefficient (r) to Fisher's Z (Ho et al., 2016)
convert_r2z(0.1400005) # Tai-chi x Positive PANSS
# Fisher's Z = 0.1409261
convert_r2z(0.06550579) # Tai-chi x Negative PANSS
# Fisher's Z = 0.06559973
convert_r2z(0.03028462) # Tai-chi x STM (Forward DS)
# Fisher's Z = 0.03029388
convert_r2z(0.2262644) # Tai-chi x STM (Backward DS)
# Fisher's Z = 0.2302488
convert_r2z(0.2215897) # Tai-chi x Mean Cortisol
# Fisher's Z = 0.2253273
convert_r2z(-0.1643752) # Tai-chi x Diurnal Cortisol Slope
# Fisher's Z = -0.1658801
convert_r2z(0.1252497) # Aerobic x Positive PANSS
# Fisher's Z = 0.1259109
convert_r2z(0.2401941) # Aerobic x Negative PANSS
# Fisher's Z = 0.2449801
convert_r2z(0.2074746) # Aerobic x STM (Forward DS)
# Fisher's Z = 0.2105309
convert_r2z(0.1497836) # Aerobic x STM (Backwards DS)
# Fisher's Z = 0.1509191
convert_r2z(0.2262644) # Aerobic x Mean Cortisol
# Fisher's Z = 0.2302488
convert_r2z(0.07552992) # Aerobic x Diurnal Cortisol Slope
# Fisher's Z = 0.07567404

# Calculating Standard Error of Fisher's Z Prime
SEz(153)
# Standard Error of Fishers z prime: 0.08164966
# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to to Correlation Coefficient (r) (Dunleavy et al., 2024)
r_IL <- f_to_r(10.69, 1, 13) # Aerobic Exercise x IL-6
# r = 0.6717479
r_neg <- f_to_r(5.92, 1, 13) # Aerobic Exercise x PANSS Negative
# r = 0.5593714

# Correlation Coefficient (r) to Fisher's Z (Leroux et al., 2024)
convert_r2z(0.6717479) # Aerobic Exercise x IL-6
# Fisher's Z = 0.8139215
convert_r2z(0.5593714) # Aerobic Exercise x PANSS Negative
# Fisher's Z = 0.6319179

# Calculating Standard Error of Fisher's Z Prime
SEz(15)
# Standard Error of Fishers z prime: 0.2886751

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

#Paper: Browne et al., 2016; 16 total participants in analysis 

# Standardized Mean Difference (d) to Correlation Coefficient (Browne et al., 2016)
d_to_r(0.55) #PANSS Positive r = 0.2651565
d_to_r(0.61) #PANSS Negative r = 0.2917325
d_to_r(0.51) #PANSS Total Score r = 0.2470929


# Correlation Coefficient (r) to Fisher's Z (Browne et al., 2016)
convert_r2z(0.2651565) #PANSS Positive Fisher's Z = 0.2716468
convert_r2z(0.2917325) #PANSS Negative Fisher's Z = 0.3004589
convert_r2z(0.2470929) #PANSS Total Score Fisher's Z = 0.2523143


# Calculating Standard Error of Fisher's Z Prime (Browne et al., 2016)
SEz(16)

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

#Paper: Strassnig et al., 2015; 12 total participants in analysis

#For Cognitive Outcomes- Cohen's d to Fisher's Z 
# Standardized Mean Difference (d) to Correlation Coefficient (Strassnig et al., 2015)
d_to_r(0.46, 12) #Verbal Memory r = 0.228019
d_to_r(0.41, 12) #Verbal Fluency r = 0.20937
d_to_r(0.27, 12) # Digit Sequencing r = 0.1345933
d_to_r(0.48, 12) #Cognition Composite r = 0.2431489


# Correlation Coefficient (r) to Fisher's Z (Strassnig et al., 2015)
convert_r2z(0.228019) #Verbal Memory Fisher's Z = 0.2320988
convert_r2z(0.1345933) #Verbal Fluency Fisher's Z = 0.135415
convert_r2z(0.20937) #Digit Sequencing  Fisher's Z = 0.2125124
convert_r2z(0.2431489) #Cognition Composite Fisher's Z = 0.2481182

#For Psychosis Symptoms Outcomes- t-scores to Fisher's Z
#t-score value to Correlation Coefficient (r) + Fisher's Z (Strassnig et al., 2015)
esc_t(
  t = -.27,
  totaln = 12,
  es.type = "r") # PANSS Positive 
# r: -0.0851
#PANSS Positive Fisher's Z:-0.0853 

esc_t(
  t = 1.64,
  totaln = 12,
  es.type = "r") # PANSS Negative
# r: = 0.4604
#PANSS Negative Fisher's Z: 0.4978

esc_t(
  t = 2.5,
  totaln = 12,
  es.type = "r") # PANSS Total
# r: = 0.6202
#PANSS Total Fisher's Z: 0.7253

# Calculating Standard Error of Fisher's Z Prime (Strassnig et al., 2015)
SEz(12)

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

#Paper: Browne et al., 2021; 15 total participants in analysis 

# Standardized Mean Difference (d) to Correlation Coefficient (Browne et al., 2021)
d_to_r(0.883, 14) #PANSS Positive r = 0.4165291
d_to_r(0.561, 14) #PANSS Negative r = 0.2794885
d_to_r(-0.168, 14) #PANSS Total Score r = -0.0868416


# Correlation Coefficient (r) to Fisher's Z (Browne et al., 2021)
convert_r2z(0.4165291) #PANSS Positive Fisher's Z = 0.4434851
convert_r2z(0.2794885) #PANSS Negative Fisher's Z = 0.2871271
convert_r2z(-0.0868416) #PANSS Total Score Fisher's Z = -0.0870609


# Calculating Standard Error of Fisher's Z Prime (Browne et al., 2021)
SEz(14)
# Standard Error of Fishers z prime: 0.3015113

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

#Paper: Kurebayashi et al., 2022; 18 total participants in analysis 
#4 schizophrenia exercise, 14 schizophrenia control (treatment as usual, TAU); follow-up was 8 weeks after baseline

#Partial eta-squared --> Cohen's D --> Correlation coefficient (r) --> Fisher's Z

# Partial Eta-squared to Standardized Mean Difference (Kurebayashi et al., 2022)
pes_to_cohens_d = function(pes, n) sqrt( ((n-1)/n) * (pes/(1-pes))) # Creating function
pes_to_cohens_d(0.000, 18)        # PANSS Positive (Schizophrenia exercise vs. TAU)
pes_to_cohens_d(0.177, 18)       # PANSS Negative (Schizophrenia exercise vs. TAU)
pes_to_cohens_d(0.113, 18)      # PANSS Total (Schizophrenia exercise vs. TAU)
pes_to_cohens_d(0.399, 18)     # Neurocognitive index (Schizophrenia exercise vs. TAU) 
pes_to_cohens_d(0.407, 18)     # Processing speed (Schizophrenia exercise vs. TAU) 
pes_to_cohens_d(0.347, 18)     # Executive Function (Schizophrenia exercise vs. TAU) 
pes_to_cohens_d(0.009, 18)     # Composite Memory (Schizophrenia exercise vs. TAU) 


# Standardized Mean Difference (d) to Correlation Coefficient (r)
d_to_r(0.000, 18)        # PANSS Positive 
d_to_r(0.4506869, 18)       # PANSS Negative 
d_to_r(0.3468691, 18)      # PANSS Total 
d_to_r(0.79184, 18)     # Neurocognitive index 
d_to_r(0.8051153, 18)     # Processing speed
d_to_r(0.708429, 18)     # Executive Function
d_to_r(0.09261315, 18)     # Composite Memory 


# Correlation Coefficient to Fisher's Z (Kurebayashi et al., 2022)
convert_r2z(0.000)      # PANSS Positive Fisher's Z = 0.0000
convert_r2z(0.2258835)      # PANSS Negative Fisher's Z = 0.2298474 
convert_r2z(0.1756869)      # PANSS Total Fisher's Z = 0.1775287
convert_r2z(0.3772897)      # Neurocognitive index Fisher's Z = 0.3968957
convert_r2z(0.3826951)      # Processing speed Fisher's Z = 0.4032134
convert_r2z(0.3424461)      # Executive Function Fisher's Z = 0.356861
convert_r2z(0.04759507)      # Composite Memory Fisher's Z = 0.04763106


# Calculating Standard Error of Fisher's Z Prime (Kurebayashi et al., 2022)
SEz(18)

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

#Paper: Malchow et al., 2016

# F-Value to Standardized Mean Difference (d) (Malchow et al., 2016)
esc_f(f = 0.0, # PANSS positive at 3-months (Schizophrenia exercise vs. Schizophrenia table soccer)
      grp1n = 20, # Schizophrenia exercise 
      grp2n = 19, # Schizophrenia table soccer 
      es.type = "d")
# Standardized Mean Difference (d) = 0.0000

# Standardized Mean Difference (d) to Correlation Coefficient (Malchow et al., 2016)
d_to_r(0.0000, 20, 19)
# Correlation Coefficient (r) = 0.000

# Correlation Coefficient (r) to Fisher's Z (Malchow et al., 2016)
convert_r2z(0.0000)
# Fisher's Z = 0.0000


# F-Value to Standardized Mean Difference (d) (Malchow et al., 2016)
esc_f(f = 0.8, # PANSS negative at 3-months (Schizophrenia exercise vs. Schizophrenia table soccer)
      grp1n = 20, # Schizophrenia exercise 
      grp2n = 19, # Schizophrenia table soccer 
      es.type = "d")
# Standardized Mean Difference (d) = 0.2865

# Standardized Mean Difference (d) to Correlation Coefficient (Malchow et al., 2016)
d_to_r(0.2865, 20, 19)
# Correlation Coefficient (r) = 0.1454586

# Correlation Coefficient (r) to Fisher's Z (Malchow et al., 2016)
convert_r2z(0.1454586)
# Fisher's Z = 0.1464977


# F-Value to Standardized Mean Difference (d) (Malchow et al., 2016)
esc_f(f = 1.4, # PANSS total score at 3-months (Schizophrenia exercise vs. Schizophrenia table soccer)
      grp1n = 20, # Schizophrenia exercise 
      grp2n = 19, # Schizophrenia table soccer 
      es.type = "d")
# Standardized Mean Difference (d) = 0.3791

# Standardized Mean Difference (d) to Correlation Coefficient (Malchow et al., 2016)
d_to_r(0.3791, 20, 19)
# Correlation Coefficient (r) = 0.1909615

# Correlation Coefficient (r) to Fisher's Z (Malchow et al., 2016)
convert_r2z(0.1909615)
# Fisher's Z = 0.1933349

# Calculating Standard Error of Fisher's Z Prime
SEz(39)
# Standard Error of Fishers z prime: 0.1666667

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

###d to r to Z (Hahne et al., 2026)

##step 1: Standardized Mean Difference (d) to Correlation Coefficient (r)

d_to_r(-1.05) # YoGI PANSS total
#Correlation Coefficient r = -0.4648339

d_to_r(-1.00) # YoGI PANSS positive
#Correlation Coefficient r = -0.4472136

d_to_r(0.55) # YoGI PANSS negative
#Correlation Coefficient r = 0.2651565

d_to_r(-0.92) # YoGI PANSS general
#Correlation Coefficient r = -0.4179056

d_to_r(-0.69) # YoGI SSTICS- attention
#Correlation Coefficient r = -0.3261363

d_to_r(-0.42) # YoGI SSTICS- executive functioning
#Correlation Coefficient r = -0.2055172

d_to_r(-0.48) # YoGI SSTICS- language praxia
#Correlation Coefficient r = -0.233373


##step 2: Correlation Coefficient (r) to Fisher's Z (z)

convert_r2z(-0.4648339) # YoGI PANSS total 
# Fisher's Z: -0.50346

convert_r2z(-0.4472136) # YoGI PANSS positive 
# Fisher's Z: -0.4812118

convert_r2z(0.2651565) # YoGI PANSS negative 
# Fisher's Z: 0.2716468

convert_r2z(-0.4179056) # YoGI PANSS general 
# Fisher's Z: -0.4451517

convert_r2z(-0.3261363) # YoGI SSTICS- attention
# Fisher's Z: -0.3384985

convert_r2z(-0.2055172) # YoGI SSTICS- executive functioning
# Fisher's Z: -0.2084863

convert_r2z(-0.233373) # YoGI SSTICS- language praxia
# Fisher's Z: -0.2377538


##step 3: Calculating Standard Error of Fisher's Z Prime

SEz(50)
# Standard Error of Fishers z prime: 0.145865

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# F-Value to Standardized Mean Difference (d) (Nuechterlein et al., 2022)
esc_f(f = 3.33,  # Exercise x Cognition
      grp1n = 24, # Cog Training + Exercise
      grp2n = 23, # Cog Training Only
      es.type = "d")
# Cohen's D:  0.5325

# Standardized Mean Difference (d) to Correlation Coefficient (Nuechterlein et al., 2022)
d_to_r(0.5325, 24, 23) # Exercise x Cognition
# Correlation Coefficient (r) = 0.2625008

# Correlation Coefficient (r) to Fisher's Z (Nuechterlein et al., 2022)
convert_r2z(0.2625008) # Exercise x Cognition
# Fisher's Z: 0.2687924

# Calculating Standard Error of Fisher's Z Prime
SEz(47) 
# Standard Error of Fishers z prime: 0.1507557

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (McEwen et al., 2015)
d_to_r(0.94) # Physical Activity x Total Hippo Vol
# r = 0.4253611
d_to_r(0.95) # Physical Activity x Left Hippo Vol
# r = 0.4290568
d_to_r(0.84) # Physical Activity x Right Hippo Vol
# r = 0.3872325

# Correlation Coefficient (r) to Fisher's Z (McEwen et al., 2015)
convert_r2z(0.4253611) # Physical Activity x Total Hippo Vol
# Fisher's Z = 0.4542193
convert_r2z(0.4290568) # Physical Activity x Left Hippo Vol
# Fisher's Z = 0.4587401
convert_r2z(0.3872325) # Physical Activity x Right Hippo Vol 
# Fisher's Z = 0.4085402


# Calculating Standard Error of Fisher's Z Prime (McEwen et al., 2015)
SEz(14)
# SE: 0.3015113

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Standardized Mean Difference (d) to Correlation Coefficient (Wunderlich et al., 2025)
d_to_r(-1.34, 23) # Physical Exercise x hippocampal-prefrontal connection
# Correlation Coefficient (r) = -0.5651601
d_to_r(-2.19, 23) # Physical Exercise x hippocampal formation connection
# Correlation Coefficient (r) = -0.7458224
d_to_r(-2.19, 23) # Physical Exercise x DMN Connection
# Correlation Coefficient (r) = -0.7458224

# Correlation Coefficient (r) to Fisher's Z (Wunderlich et al., 2025)
convert_r2z(-0.5651601) # Physical Exercise x hippocampal-prefrontal connection
# Fisher's Z = -0.6403827
convert_r2z(-0.7458224) # Physical Exercise x hippocampal formation connection
# Fisher's Z = -0.9634739
convert_r2z(-0.7458224) # Physical Exercise x DMN Connection
# Fisher's Z = -0.9634739

# Calculating Standard Error of Fisher's Z Prime
SEz(23)
# Standard Error of Fishers z prime: 0.2236068

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Calculating SDChange
# Formula: SDChange = √SD^2(pre) + SD^2(post) - 2p(SDpre)(SDpost)
SDChange_wm = sqrt((14.75)^2 + (11.24)^2 - 2*(0.71)*(14.75)*(11.24))
# SDChange = 10.41529
SDChange_vl = sqrt((15.51)^2 + (10.91)^2 - 2*(.019)*(15.51)*(10.91))
# SDChange = 18.7925
SDChange_ps = sqrt((7.06)^2 + (8.69)^2 - 2*(.62)*(7.06)*(8.69))
# SDChange = 7.020254

# Calculating Cohen's D from Mean Difference
# Formula: d = Mean(post) - Mean(pre)
wm_d <- (33.31 - 29.56)/SDChange_wm
# Cohen's D: 0.3600475
vl_d <- (34.00 - 19.20)/SDChange_vl
# Cohen's D: 0.7875482
ps_d <- (24.68 - 21.37)/SDChange_ps
# Cohen's D: 0.4714929

# Standardized Mean Difference (d) to Correlation Coefficient (He et al., 2026)
d_to_r(0.3600475, 18, 14) # Dance x Working Memory (DS)
# Correlation Coefficient (r) = 0.1814087
d_to_r(0.7875482, 18, 14) # Dance x Verbal Learning (HVLT-R)
# Correlation Coefficient (r) = 0.3741857
d_to_r(0.4714929, 18, 14) # Dance x Processing Speed (TMT)
# Correlation Coefficient (r) = 0.2348141

# F-Value to Correlation Coefficient (r) (He et al., 2026)
# Formula: r = √(F * df1)/(F * df1 + df2)
r_occ <- f_to_r(4.46, 1, 25) # Dance x Cerebellar Motor x Occipital_Sup_L
# r = 0.3890909
r_front <- f_to_r(3.37, 1, 25) # Dance x Cerebellar Motor x Frontal_Sup_Medial_L
# r = 0.3446556
r_cuneus <- f_to_r(3.70, 1, 25) # Dance x Cerebellar Motor x Cuneus_R
# r = 0.3590541
r_cing <- f_to_r(4.31, 1, 25) # Dance x Cerebellar Cognitive x Cingulum_Mid_R
# r = 0.3834694
r_temp <- f_to_r(3.58, 1, 25) # Dance x Cerebellar Cognitive x Temporal_Inf_L
# r = 0.3539243

# Correlation Coefficient (r) to Fisher's Z (z)
convert_r2z(0.3890909) #  Dance x Cerebellar Motor x Occipital_Sup_L
# Fisher's Z: 0.4107283
convert_r2z(0.3446556) #  Dance x Cerebellar Motor x Frontal_Sup_Medial_L
# Fisher's Z: 0.3593662
convert_r2z(0.3590541) #  Dance x Cerebellar Motor x Cuneus_R
# Fisher's Z:  0.3757996
convert_r2z(0.3834694) # Dance x Cerebellar Cognitive x Cingulum_Mid_R
# Fisher's Z:  0.4041209
convert_r2z(0.3539243) #  Dance x Cerebellar Cognitive x Temporal_Inf_L
# Fisher's Z:  0.05996406
convert_r2z(0.1814087) #  Dance x Working Memory (DS)
# Fisher's Z:  0.1834389
convert_r2z(0.3741857) # Dance x Verbal Learning (HVLT-R)
# Fisher's Z:  0.3932815
convert_r2z(0.2348141) #  Dance x Processing Speed (TMT)
# Fisher's Z:  0.2392785

# Calculating Standard Error of Fisher's Z Prime
SEz(32) 
# Standard Error of Fishers z prime: 0.1856953

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Correlation Coefficient (r) to Fisher's Z (Roell et al., 2022)
convert_r2z(0.26) # Aerobic X Primary sensorimotor cortices and Transverse temporal gyri, primary auditory cortices Connectivity 
# Fisher's Z = 0.2661084
convert_r2z(0.25) #  Aerobic X Salience Network and Left Frontal-Parietal Network Connectivity
# Fisher's Z = 0.2554128
convert_r2z(-0.24) #  Aerobic X Basal Ganglia/Thalamus and Midbrain/Cerebellum Connectivity
# Fisher's Z = -0.2447741

# Calculating Standard Error of Fisher's Z Prime
SEz(58) 
# Standard Error of Fishers z prime: 0.13484

# 0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0

# Effects of aerobic exercise on hippocampal formation volume in people with schizophrenia (Roell et al., 2024)

# F-Value to Correlation Coefficient (r) (Roell et al., 2024)

# Formula: r = √(F * df1)/(F * df1 + df2)
# df2 approximated from N − groups
r_ca1 <- f_to_r(5.11, 1, 27) # Aerobic Exercise x CA1
# r = 0.3989241
r_ca2 <- f_to_r(7.62, 1, 27) # Aerobic Exercise x CA2/3
# r = 0.4691524
r_ca4 <- f_to_r(5.29, 1, 27) # Aerobic Exercise x CA4
# r = 0.4047565
r_dg <- f_to_r(5.67, 1, 27) # Aerobic Exercise x Dentate Gyrus
# r = 0.4165978

# Correlation Coefficient (r) to Fisher's Z (Roell et al., 2024)
convert_r2z(0.3989241) # Aerobic Exercise x CA1
# Fisher's Z = 0.4223688
convert_r2z(0.4691524) # Aerobic Exercise x CA2/3
# Fisher's Z = 0.508983
convert_r2z(0.4047565) # Aerobic Exercise x CA4
# Fisher's Z = 0.4293243
convert_r2z(0.4165978) # Aerobic Exercise x Dentate Gyrus
# Fisher's Z = 0.4435683


# Calculating Standard Error of Fisher's Z Prime
SEz(29)
# Standard Error of Fishers z prime: 0.1961161