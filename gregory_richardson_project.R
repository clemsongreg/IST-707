# IST 707 Project Work
# Gregory Richardson
# Analyzing Diabetic Patient Readmission Rates

# Using both Association Rules and Naive-Bayes techniques to analyze
# diabetic patient data set.

# ------  Package Inclusion -----------
library(arules)
library(ggplot2)
library(e1071)
library(arulesViz)

# ------ Data Loading and Prepocessing -------
loc <- "C:/Users/Greg/Documents/School/IST 707/Project/dataset_diabetes/diabetic_data.csv"

# Read in data file.  Converting any ? in the dataset to NA's
rawDf <- read.csv(loc, na.strings = "?")

summary(rawDf)
# From the summary, 97% of the data for Weight column is NA, so removing
# it.  Also removing payer_code and medical specialty - ~ 30% of data is 
# missing.

removeCols <- c("weight", "payer_code", "medical_specialty")
colnames(rawDf)
patDf <- rawDf[,-c(1,6,11,12)]

summary(patDf)

# Remove all rows that are incomplete
fullData <- complete.cases(patDf)
patDf <- patDf[fullData,]
summary(patDf)

# Remove observations where patient died
dead <- which(patDf$discharge_disposition_id %in% c(11,19,20,21))
patDf <- patDf[-dead,]

# Need to discretize variables that were misread as numeric.  This includes
# Patient Number, Admission Type, Discharge Disposition, and Admission 
# Source ID's.  
patDf$patient_nbr <- as.factor(patDf$patient_nbr)
patDf$admission_type_id <- as.factor(patDf$admission_type_id)
patDf$discharge_disposition_id <- as.factor(patDf$discharge_disposition_id)
patDf$admission_source_id <- as.factor(patDf$admission_source_id)

str(patDf)

# ------  Association Rules Mining ------
arDf <- patDf

# First step is to discretize any numeric variables. There are 8 variables
# to be discretized.
# Time In Hospital
range(arDf$time_in_hospital)
# Range of Time in Hospital is 1-14 days.  Discretizing into single day 
# units.
arDf$time_in_hospital <- as.factor(arDf$time_in_hospital)

# Num Lab Procedures
range(arDf$num_lab_procedures)
summary(arDf$num_lab_procedures)
# Discretizing into groups of 25
arDf$num_lab_procedures <- cut(arDf$num_lab_procedures, breaks=c(0,25,50,75,100,125,150))
summary(arDf$num_lab_procedures)

# Number of Procedures
summary(arDf$num_procedures)
# Ranges from 0-6.  Factorizing.
arDf$num_procedures <- as.factor(arDf$num_procedures)

# Number of Medications
summary(arDf$num_medications)
# Range from 1 to 81 with a median of 15.  Discretizing into groups of 5.
arDf$num_medications <- cut(arDf$num_medications, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85))
summary(arDf$num_medications)

# Number of Outpatient, Emergency, and Inpatient visits.  
summary(arDf$number_outpatient)
summary(arDf$number_emergency)
summary(arDf$number_inpatient)

arDf$number_outpatient <- cut(arDf$number_outpatient, breaks=c(0,1,5,10,15,20,25,30,35,40,45), right = FALSE)
arDf$number_emergency <- cut(arDf$number_emergency, breaks=c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80), right=FALSE)
arDf$number_inpatient <- cut(arDf$number_inpatient, breaks=c(0,1,5,10,15,20,25), right=FALSE)

# Number of Diagnoses
summary(arDf$number_diagnoses)
arDf$number_diagnoses <- cut(arDf$number_diagnoses, breaks = c(0,5,10,15,20))

str(arDf)

# Data is ready for evaluation with Association Rules.  First model is 
# looking for associations with the label - Readmitted
summary(arDf$readmitted)
arDf <- arDf[,-1]

names(arDf)
# Removing Admission Type, Discharge ID, Admission ID, and diagnosis codes
arDf <- arDf[,-c(4,5,6,14,15,16)]
# Removing information about specific medicines and dosage changes.  These
# were overcomplicating the model.
arDf <- arDf[,c(1:13, 37:39)]
#arDf <- arDf[,c(1:7,11:14)]
#arDf <- arDf[,c(1:8, 10:11)]

# Model 1 looks at Association Rules for patients readmitted after 1 month
# but still within the readmission window.  Support was set pretty low, at
# 0.0001, but only rules with a confidence of 0.9 or higher were included.
mod1 <- arules::apriori(arDf
                        , parameter = list(supp = 0.00005, conf=0.9, maxlen = 4, maxtime=0)
                        , appearance = list(rhs = c("readmitted=>30"))
                        )
mod1 <- sort(mod1, by="lift", decreasing= TRUE)
inspect(mod1[1:20])
mod1 <- sort(mod1, by="support", decreasing=TRUE)
inspect(mod1[1:10])
mod1 <- sort(mod1, by="confidence", decreasing=TRUE)
inspect(mod1[1:10])

# Model 2 looks at Association Rules for patients readmitted within a 1
# month window.  It uses the same Support and Confidence setup as the first
# model.
mod2 <- arules::apriori(arDf
                        , parameter = list(supp = 0.00003, conf = 0.95, maxlen = 4, maxtime = 0)
                        , appearance = list(rhs = c("readmitted=<30"))
                        )
mod2 <- sort(mod2, by="lift", decreasing = TRUE)
inspect(mod2[1:20])
mod2 <- sort(mod2, by="confidence", decreasing=TRUE)
inspect(mod2[1:10])


# Interactive plotting of the Association Rules for the first model
plot(mod1[1:10], method="graph", engine = "interactive")
plot(mod2[1:10], method="graph", engine = "interactive")

# ----- Plotting -----

# Creating a tile plot for various population segments, looking for which
# groups might be more at risk for readmittance.
g3 <- ggplot(data=arDf, aes(x=gender, y=age, fill=readmitted)) + geom_tile(color="white") + facet_wrap(vars(race), scales="free")
g3 <- g3 + theme_minimal()
g3 <- g3 + scale_fill_manual(name="Hospital Readmittance", values=c("#F66733", "#522D80", "#D4C99E"))
g3 <- g3 + labs(x="Gender", y="Age Bracket", title = "Readmittance By Population Segments ")
g3
