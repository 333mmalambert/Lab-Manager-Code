##############################
# Pubertal Plotting
# Author: Emma Lambert
# Date: 6/29/2026
##############################

# Import libraries 
library(tidyverse)
library(metaviz)

# Define path
pathway <- "/Users/eal200003/Downloads/"

# Read in .csv
firstsymp <- read_csv(paste0(pathway, "age_at_first_symptom.csv"))
sysreview <- read_csv(paste0(pathway, 'sys_review.csv'))
firsthosp <- read_csv(paste0(pathway, "age_at_first_hospitalization.csv"))

# Building numeric vector for viz_forest
alldata <-as.matrix(sysreview[,c("z", "se")])
symp <- as.matrix(firstsymp[, c("z", "se")])
symprisk <- as.matrix(firstsymp[, c("risk_effect")])
hosp <- as.matrix(firsthosp[, c("z", "se")])
hosprisk <- as.matrix(firsthosp[, c("risk_effect")])
outcome <- as.matrix(sysreview[,c("outcome")])
design <- as.matrix(sysreview[, c("design")])
risk <- as.matrix(sysreview[, c("risk_effect")])
population <- as.matrix(sysreview[, c("population")])
pub_metric <- as.matrix(sysreview[, c("puberty_metric")])
mena_range <- as.matrix(sysreview[, c("menarche_range")])

# Plotting data from Age @ First Symptom group
symp_cols <- c(
  "Late Pubertal Timing = Increased Risk" = "darkorchid",
  "Early Pubertal Timing = Increased Risk" = "deepskyblue3",
  "No Effect" = "darkseagreen3")  

viz_forest(
  x = symp,
  group = symprisk,
  study_labels = firstsymp$study_name,
  summary_label = c("Early Pubertal Timing = Increased Risk", "Late Pubertal Timing = Increased Risk", "No Effect"),
  xlab = "Fisher's Z",
  col = symp_cols[as.character(symprisk)],
  summary_col = c("deepskyblue3", "darkorchid", "darkseagreen3"))

# Creating a Study Table to display Methods
hosp_method <- data.frame(
  Author = firsthosp[, "study_name"],
  Method = firsthosp[, "method"],
  Risk_Effect = firsthosp[, "risk_effect"])

# Plotting data from Age @ First Hospitalization 
hosp_cols <- c(
  "Late Pubertal Timing = Increased Risk" = "darkorchid",
  "No Effect" = "darkseagreen3")

viz_forest(
  x = hosp,
  group = hosprisk,
  study_labels = firsthosp$study_name,
  summary_label = c("Late Pubertal Timing = Increased Risk", "No Effect"),
  xlab = "Fisher's Z",
  study_table = hosp_method,
  col = hosp_cols[as.character(hosprisk)],
  summary_col = c("darkorchid", "darkseagreen3"))


# Creating a Study Table to display Risk Effects
pubertal_onset <- data.frame(
  Author = sysreview[, "study_name"],
  Risk_Effect = sysreview[, "risk_effect"],
  Outcome = sysreview[, "outcome"])


# Plotting all (available) data and grouping by Outcome
outcome_cols <- c(
  "Age @ first hospitalization" = "red",
  "Age @ symptom onset" = "orange",
  "Age @ first odd behavior" = "springgreen2",
  "Psychotic-Like Experiences" = "yellow",
  "Age @ Illness Onset" =  "blue")

viz_forest(
  x = alldata,
  group = outcome,
  study_labels = sysreview$study_name,
  summary_label = c("Age @ first hospitalization","Age @ Illness Onset", "Age @ first odd behavior",
                    "Psychotic-Like Experiences", "Age @ symptom onset" ),
  xlab = "Fisher's Z",
  study_table = pubertal_onset,
  col = outcome_cols[as.character(outcome)],
  summary_col = c("red", "blue", "springgreen2", "yellow", "orange"))

# Plotting all (available) data and grouping by Risk Effect
risk_cols <- c(
  "Late Pubertal Timing = Increased Risk" = "darkorchid",
  "Early Pubertal Timing = Increased Risk" = "deepskyblue3",
  "No Effect" = "darkseagreen3")

viz_forest(
  x = alldata,
  group = risk,
  study_labels = sysreview$study_name,
  summary_label = c("Early Pubertal Timing = Increased Risk", "Late Pubertal Timing = Increased Risk", "No Effect" ),
  xlab = "Fisher's Z",
  annotate_CI = TRUE,
  col = risk_cols[as.character(risk)],
  summary_col = c("deepskyblue3", "darkorchid", "darkseagreen3"))


# Creating a Study Table to display Risk Effects/Design
design_table <- data.frame(
  Author = sysreview[, "study_name"],
  Risk_Effect = sysreview[, "risk_effect"],
  Design = sysreview[, "design"])


# Plotting all (available) data and grouping by study design
design_cols <- c(
  "Contemporaneous" = "darkgreen",
  "Retrospective" = "plum")

viz_forest(
  x = alldata,
  group = design,
  study_labels = sysreview$study_name,
  summary_label = c("Contemporaneous", "Retrospective"),
  xlab = "Fisher's Z",
  study_table = design_table,
  col = design_cols[as.character(design)],
  summary_col = c("darkgreen", "plum"))



# Creating a Study Table to display Risk Effects/Population
pop_table <- data.frame(
  Author = sysreview[, "study_name"],
  Risk_Effect = sysreview[, "risk_effect"],
  Population = sysreview[, "population"])


# Plotting all (available) data and grouping by Population
pop_cols <- c(
  "Diagnosed" = "cyan4",
  "CHR" = "maroon")

viz_forest(
  x = alldata,
  group = population,
  study_labels = sysreview$study_name,
  summary_label = c("Diagnosed", "CHR"),
  xlab = "Fisher's Z",
  study_table = pop_table,
  col = pop_cols[as.character(population)],
  summary_col = c("maroon", "cyan4"))


# Creating a Study Table to display Risk Effects/Puberty Metric
pub_table <- data.frame(
  Author = sysreview[, "study_name"],
  Risk_Effect = sysreview[, "risk_effect"],
  Puberty_Metric = sysreview[, "puberty_metric"])


# Plotting all (available) data and grouping by Population
pub_cols <- c(
  "Age @ Menarche" = "lightpink",
  "Age @ onset of secondary sex characteristics" = "orange",
  "Timing and Tempo" = "lightskyblue",
  "Menarche Status" = "gold")

viz_forest(
  x = alldata,
  group = pub_metric,
  study_labels = sysreview$study_name,
  summary_label = c("Age @ Menarche", "Age @ onset of secondary sex characteristics", "Timing and Tempo", "Menarche Status"),
  xlab = "Fisher's Z",
  study_table = pub_table,
  col = pub_cols[as.character(pub_metric)],
  summary_col = c("lightpink", "orange", "gold", "lightskyblue"))



# Creating a Study Table to display Risk Effects/Menarche Range
menarche_table <- data.frame(
  Author = sysreview[, "study_name"],
  Risk_Effect = sysreview[, "risk_effect"],
  Menarche_Range = sysreview[, "menarche_range"])

# Convert actual NA to character string "NA"
mena_range[is.na(mena_range)] <- "NA"

# Converting Menarche Range columns to a factor with defined leve;ls
mena_factor <- factor(mena_range, levels = c("11 to 14", "12 to 15", "10 to 14", "9 to 20", "10 to 15", "10 to 12", "9 to 12", "8 to 15", "10 to 17"))
mena_levels <- levels(mena_factor)

# Plotting all (available) data and grouping by Population
mena_cols <- c(
  "11 to 14" = "red",
  "12 to 15" = "orange",
  "10 to 14" = "yellow",
  "9 to 20" = "green",
  "10 to 15" = "springgreen4",
  "10 to 12" = "cyan",
  "9 to 12" = "blue",
  "8 to 15" = "violet",
  "10 to 17" = "purple")


# Plotting all (available) data and grouping by Menarche Range
viz_forest(
  x = alldata,
  group = mena_factor,
  study_labels = sysreview$study_name,
  summary_label = mena_levels,
  xlab = "Fisher's Z",
  study_table = menarche_table,
  col = mena_cols[as.character(mena_factor)],
  summary_col = unname(mena_cols[mena_levels]))
