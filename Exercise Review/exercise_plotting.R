##############################
# Exercise Plotting
# Author: Emma Lambert
# Date: 7/20/2026
##############################

# Import libraries 
library(tidyverse)
library(metaviz)

# Define path
pathway <- "/Users/eal200003/Downloads/"

# Read in .csv
ex_review <- read_csv(paste0(pathway, 'exercise_review.csv'))

# Creating seperate data frames
pos_symp <- ex_review %>%
     filter(Outcome == "Positive Symptoms")
neg_symp <- ex_review %>%
     filter(Outcome == "Negative Symptoms")
cognition <- ex_review %>%
     filter(Outcome == "Cognition")
neurodev <- ex_review %>%
  filter(!is.na(Neurodevelopmental_Function))
dyscon <- ex_review %>%
  filter(Outcome %in% c("Connectivity", "Dysconnectivity"))
cogdys <- ex_review %>%
  filter(!is.na(Cognitive_Function))

# Building numeric vector for viz_forest
posdata <-as.matrix(pos_symp[,c("Z", "SE")])
negdata <- as.matrix(neg_symp[, c("Z","SE")])
cogdata <- as.matrix(cognition[,c("Z", "SE")])
devdata <- as.matrix(neurodev[,c("Z", "SE")])
dysdata <- as.matrix(dyscon[,c("Z", "SE")])
cogdysdata <- as.matrix(cogdys[,c("Z", "SE")])


######### Positive Symptom Plotting #########
# Setting factor levels
exercise_type_pos <- factor(
  pos_symp$Exercise_Type,
  levels = c(
    "Aerobic",
    "Yoga",
    "Physical Activity Level",
    "Tai-chi",
    "Endurance",
    "Circuit Resistance Training"))

# Color-coordinating columns
exercise_cols <- c(
   "Aerobic" = "red",
    "Yoga" = "coral",
    "Physical Activity Level" = "orange",
    "Tai-chi" = "gold",
    "Endurance" = "yellow",
    "Circuit Resistance Training" = "pink1")

# Creating a Study Table to display Methods
positive_table <- data.frame(
  Author = pos_symp[, "Author_Name"],
  Exercise = pos_symp[, "Exercise_Type"],
  Diagnosis = pos_symp[, "Diagnosis"],
  Measure = pos_symp[, "Measure_Used"])

# Plotting all Positive Symptom data and grouping by Exercise
viz_forest(
   x = posdata,
   group = exercise_type_pos,
   study_labels = pos_symp$Author_Name,
   summary_label = levels(exercise_type_pos),
   table_headers = c("Author", "Type of Exercise", "Diagnosis", "Measure Used"),
   xlab = "Fisher's Z",
   study_table = positive_table,
   text_size = 4.5, 
   col = exercise_cols[as.character(exercise_type_pos)],
   summary_col = c("red","coral" ,"orange", "gold" ,"yellow", "pink1")) +
   ggtitle("Positive Symptom Benefit by Type of Exercise Intervention") +
   theme(plot.title = element_text(hjust = .5, face = "bold"))


######### Negative Symptom Plotting #########

# Setting factor levels
Exercise_Type_neg <- factor(
  neg_symp$Exercise_Type,
  levels = c(
    "Aerobic",
    "Yoga",
    "Physical Activity Level",
    "Tai-chi",
    "Endurance",
    "Circuit Resistance Training"))
   

# Color-coordinating columns
exercise_cols <- c(
  "Aerobic" = "darkgreen",
  "Yoga" = "green",
  "Physical Activity Level" = "turquoise",
  "Tai-chi" = "blue",
  "Endurance" = "violet",
  "Circuit Resistance Training" = "purple")

# Creating a Study Table to display Methods
neg_table <- data.frame(
  Author = neg_symp[, "Author_Name"],
  Exercise = neg_symp[, "Exercise_Type"],
  Diagnosis = neg_symp[, "Diagnosis"],
  Measure = neg_symp[, "Measure_Used"])

# Plotting all Negative Symptom data and grouping by Exercise
viz_forest(
  x = negdata,
  group = Exercise_Type_neg,
  study_labels = neg_symp$Author_Name,
  summary_label = levels(Exercise_Type_neg),
  table_headers = c("Author", "Type of Exercise", "Diagnosis", "Measure Used"),
  xlab = "Fisher's Z",
  study_table = neg_table,
  text_size = 4.5, 
  col = exercise_cols[as.character(Exercise_Type_neg)],
  summary_col = c("darkgreen","green", "turquoise", "blue", "violet", "purple")) +
  ggtitle("Negative Symptom Benefit by Type of Exercise Intervention") +
  theme(plot.title = element_text(hjust = .5, face = "bold"))


######### Cognitive Plotting #########

# Setting factor levels
measure_type_cog <- factor(
  cognition$Cognition_Domain,
  levels = c(
    "Composite",
    "Processing Speed",
    "Short-term Memory",
    "Working Memory",
    "Spatial Memory",
    "Executive Function",
    "Verbal Short-term Memory",
    "Verbal Learning"))

# Color-coordinating columns
cog_cols <- c(
  "Composite" = "red",
  "Processing Speed" = "orange",
  "Short-term Memory" = "yellow",
  "Working Memory" = "springgreen",
  "Executive Function" = "turquoise",
  "Verbal Short-term Memory" = "blue",
  "Verbal Learning" = "purple")

# Creating a Study Table to display Methods
cog_table_2 <- data.frame(
  Author = cognition[, "Author_Name"],
  Exercise = cognition[, "Exercise_Type"],
  Diagnosis = cognition[, "Diagnosis"],
  Measure = cognition[, "Measure_Used"],
  Domain = cognition[, "Cognition_Domain"])

# Plotting all Cognitive data and grouping by Domain
viz_forest(
  x = cogdata,
  group = measure_type_cog,
  study_labels = cognition$Author_Name,
  summary_label = levels(measure_type_cog),
  table_headers = c("Author", "Type of Exercise", "Diagnosis", "Measure Used", "Cognitive Domain"),
  xlab = "Fisher's Z",
  study_table = cog_table_2,
  text_size = 4.5, 
  col = cog_cols[as.character(measure_type_cog)],
  summary_col = c("red", "orange", "yellow", "springgreen", "turquoise", "blue", "purple")) +
  ggtitle("Cognitive Benefits by Domain of Cognition") +
  theme(plot.title = element_text(hjust = .5, face = "bold"))

######### Neurodevelopmental Model Plotting #########

# Setting factor levels
outcome_type <- factor(
  neurodev$Outcome,
  levels = c(
    "Volume",
    "Connectivity", 
    "Inflammation",
    "Cortisol"))

# Color-coordinating columns
outcome_cols <- c(
  "Volume" = "lightgreen",
  "Connectivity" = "lightgoldenrod1",
  "Inflammation" = "lightsalmon",
  "Cortisol" = "lightpink")

# Creating a Study Table to display Methods
dev_table <- data.frame(
  Author = neurodev[, "Author_Name"],
  Exercise = neurodev[, "Exercise_Type"],
  Diagnosis = neurodev[, "Diagnosis"],
  Outcome = neurodev[, "Outcome"],
  Function = neurodev[, "Neurodevelopmental_Function"])

# Plotting all Neurodevelopmemtal data and grouping by Function
viz_forest(
  x = devdata,
  group = outcome_type,
  study_labels = neurodev$Author_Name,
  summary_label = levels(outcome_type),
  table_headers = c("Author", "Type of Exercise", "Diagnosis", "Outcome", "Neurodevelopmental Function"),
  xlab = "Fisher's Z",
  study_table = dev_table,
  text_size = 4.5, 
  col = outcome_cols[as.character(outcome_type)],
  summary_col = c("lightgreen", "lightgoldenrod1", "lightsalmon", "lightpink")) +
  ggtitle("Neurodevelopmental Model") +
  theme(plot.title = element_text(hjust = .5, face = "bold"))

######### Dysconnectivity Model Plotting #########

# Setting factor levels
outcome_type <- factor(
  dyscon$Outcome,
  levels = c(
    "Connectivity", 
    "Dysconnectivity"))

# Color-coordinating columns
outcome_cols <- c(
  "Connectivity" = "blue",
  "Dysconnectivity" = "red")

# Creating a Study Table to display Methods
dys_table <- data.frame(
  Author = dyscon[, "Author_Name"],
  Exercise = dyscon[, "Exercise_Type"],
  Diagnosis = dyscon[, "Diagnosis"],
  Outcome = dyscon[, "Outcome"],
  Function = dyscon[, "Connectivity_Function"])

# Plotting all Dysconnectivity data and grouping by Function
viz_forest(
  x = dysdata,
  group = outcome_type,
  study_labels = dyscon$Author_Name,
  summary_label = levels(outcome_type),
  table_headers = c("Author", "Type of Exercise", "Diagnosis", "Outcome", "Connectivity Function"),
  xlab = "Fisher's Z",
  study_table = dys_table,
  text_size = 4.5, 
  col = outcome_cols[as.character(outcome_type)],
  summary_col = c("blue", "red")) +
  ggtitle("Dysconnectivity Model") +
  theme(plot.title = element_text(hjust = .5, face = "bold"))


# Ugly draft
viz_forest(
  x = dysdata,
  study_labels = dyscon$Author_Name,
  xlab = "Fisher's Z",
  study_table = dys_table) +
    ggtitle("Dysconnectivity Model") +
    theme(plot.title = element_text(hjust = .5, face = "bold"))

######### Cognitive Dysmetria Model Plotting #########

# Setting factor levels
outcome_type <- factor(
  cogdys$Outcome,
  levels = c(
    "Connectivity",
    "Cognition"))

# Color-coordinating columns
outcome_cols <- c(
  "Connectivity" = "magenta",
  "Cognition" = "purple")

# Creating a Study Table to display Methods
cogdys_table <- data.frame(
  Author = cogdys[, "Author_Name"],
  Exercise = cogdys[, "Exercise_Type"],
  Diagnosis = cogdys[, "Diagnosis"],
  Outcome = cogdys[, "Outcome"],
  Function = cogdys[, "Cognitive_Function"])

# Plotting all Cognitive Dysmetria data and grouping by Outcome
viz_forest(
  x = cogdysdata,
  group = outcome_type,
  study_labels = cogdys$Author_Name,
  summary_label = levels(outcome_type),
  table_headers = c("Author", "Type of Exercise", "Diagnosis", "Outcome", "Cognitive Dysmetria Function"),
  xlab = "Fisher's Z",
  study_table = cogdys_table,
  text_size = 4.5, 
  col = outcome_cols[as.character(outcome_type)],
  summary_col = c("magenta", "purple")) +
  ggtitle("Cognitive Dysmetria Model") +
  theme(plot.title = element_text(hjust = .5, face = "bold"))
