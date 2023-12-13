# import dataset
library(readr)
elaia2 <- read_csv("data/deidentified_elaia2_data_missing_NaN3.csv")


# scan for duplicates
library(janitor)
get_dupes(elaia2)


# remove outliers
library(tidyverse)
library(dplyr)

df <- subset(elaia2, (is.na(elaia2$time_to_rand) | elaia2$time_to_rand <= 2000) &
               (is.na(elaia2$wbcc_at_rand) | elaia2$wbcc_at_rand <= 900) &
               (is.na(elaia2$creat_at_rand) | elaia2$creat_at_rand <= 8) &
               (is.na(elaia2$temp_at_rand) | elaia2$temp_at_rand >= 80) &
               (is.na(elaia2$diastolic_at_rand) | elaia2$diastolic_at_rand >= 25) &
               (is.na(elaia2$pulse_at_rand) | elaia2$pulse_at_rand >= 20))


# remove rows with missing values
df <- df[complete.cases(df$maxcreat14_post), ]
df <- df[complete.cases(df$mincreat14_post), ]
df <- df[complete.cases(df$aki_stage14), ]
df <- df[complete.cases(df$hemoglobin_at_rand), ]
df <- df[complete.cases(df$plateletcount_at_rand), ]

view(df)

#library(table1)
#library(summarytools)

# Read the dfSummary, making sure not to treat the first row as headers
##dfSummary <- read.csv("Baseline Characteristics.csv", header = TRUE)

# Extract the second row for column names
#col_names <- dfSummary[1,]

# Remove the first two rows from the data
#dfSummary <- dfSummary[-c(1), ]

# Assign the extracted names as column names
#colnames(dfSummary) <- col_names

# Convert relevant columns to the appropriate data types
# For example, if 'age' should be numeric:
# Convert binary columns to factors
#dfSummary$`Sex (1 = female)` <- as.factor(dfSummary$`Sex (1 = female)`)
#dfSummary$`AKI progression within 14 days of Diagnosis` <- as.factor(dfSummary$`AKI progression within 14 days of Diagnosis`)
#dfSummary$`Death within 14 Days of AKI` <- as.factor(dfSummary$`Death within 14 Days of AKI`)
#dfSummary$`Dialysis within 14 days of AKI` <- as.factor(dfSummary$`Dialysis within 14 days of AKI`)
#dfSummary$`Readmission within 30 Days of Discharge` <- as.factor(dfSummary$`Readmission within 30 Days of Discharge`)
#dfSummary$`Admitted to a Medical Service` <- as.factor(dfSummary$`Admitted to a Medical Service`)
#dfSummary$`ICU at Randomization` <- as.factor(dfSummary$`ICU at Randomization`)
#dfSummary$`ER at Rand.` <- as.factor(dfSummary$`ER at Rand.`)
#dfSummary$`AKI stage after 14 days` <- as.factor(dfSummary$`AKI stage after 14 days`)

# Convert integer columns
#dfSummary$Age <- as.integer(dfSummary$Age)
#dfSummary$`Number of Medications at Rand.` <- as.integer(dfSummary$`Number of Medications at Rand.`)
#dfSummary$`Elixhauser Comorbidity Index` <- as.integer(dfSummary$`Elixhauser Comorbidity Index`)
#dfSummary$`Platelet Count at Rand. (nL)` <- as.integer(dfSummary$`Platelet Count at Rand. (nL)`)
#dfSummary$`Number of Providers that Accessed the Chart` <- as.integer(dfSummary$`Number of Providers that Accessed the Chart`)


# Convert floating-point columns
#dfSummary$`Hemoglobin at Rand. (g/dL)` <- as.numeric(dfSummary$`Hemoglobin at Rand. (g/dL)`)
#dfSummary$`Hours from AKI to Rand.` <- as.numeric(dfSummary$`Hours from AKI to Rand.`)
#dfSummary$`Duration of AKI (hours)` <- as.numeric(dfSummary$`Duration of AKI (hours)`)
#dfSummary$`Creatinine at Randomization (mg/dL)` <- as.numeric(dfSummary$`Creatinine at Randomization (mg/dL)`)
#dfSummary$`Min. Creatinine within 48 Hours Prior to Rand. (mg/dL)` <- as.numeric(dfSummary$`Min. Creatinine within 48 Hours Prior to Rand. (mg/dL)`)
#dfSummary$`Min. Creatinine within 7 Days Prior to Rand. (mg/dL)` <- as.numeric(dfSummary$`Min. Creatinine within 7 Days Prior to Rand. (mg/dL)`)
#dfSummary$`Creatinine at Admission (mg/dL)` <- as.numeric(dfSummary$`Creatinine at Admission (mg/dL)`)
#dfSummary$`Baseline Creatinine allowing for AKI Diagnosis (mg/dL)` <- as.numeric(dfSummary$`Baseline Creatinine allowing for AKI Diagnosis (mg/dL)`)
#dfSummary$`Systolic BP at Rand.` <- as.numeric(dfSummary$`Systolic BP at Rand.`)
#dfSummary$`Diastolic BP at Rand.` <- as.numeric(dfSummary$`Diastolic BP at Rand.`)
#dfSummary$`HR at Rand.` <- as.numeric(dfSummary$`HR at Rand.`)
#dfSummary$`Temp. at Rand. (°F)` <- as.numeric(dfSummary$`Temp. at Rand. (°F)`)
#dfSummary$`WBC Count at Rand. (/cL)` <- as.numeric(dfSummary$`WBC Count at Rand. (/cL)`)
# Repeat for other columns as needed

# Generate the summary table
#reportSummary <- table1(~ ., data = dfSummary)













