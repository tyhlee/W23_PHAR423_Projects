
# univariate level, standardized, uninteracted-----------------------------------------------

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
uni.norm.exp.coef.first.three <- function(uni.norm.model, uni.norm.model.name) {
  uni.norm.cf <- coef(summary(uni.norm.model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  uni.norm.cf <- uni.norm.cf[2:4, ] # assuming the first row is the Intercept; adjust if needed

  uni.norm.OR <- exp(uni.norm.cf[, "Estimate"])
  uni.norm.lower.CI <- exp(uni.norm.cf[, "Estimate"] - 1.96 * uni.norm.cf[, "Std. Error"])
  uni.norm.upper.CI <- exp(uni.norm.cf[, "Estimate"] + 1.96 * uni.norm.cf[, "Std. Error"])
  uni.norm.p.value <- uni.norm.cf[, "Pr(>|z|)"]

  uni.norm.result <- cbind(uni.norm.OR, uni.norm.lower.CI, uni.norm.upper.CI, uni.norm.p.value)
  rownames(uni.norm.result) <- names(coef(uni.norm.model))[2:4] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  uni.norm.result.df <- as.data.frame(uni.norm.result)
  uni.norm.result.df$uni.norm.model <- uni.norm.model.name

  return(uni.norm.result.df)
}

# Apply the function to each of your models
uni.norm.AKIprogression.results <- uni.norm.exp.coef.first.three(uni.norm.AKIprogressionGLM, "AKI Progression")
uni.norm.death.results <- uni.norm.exp.coef.first.three(uni.norm.deathGLM, "Death")
uni.norm.dialysis.results <- uni.norm.exp.coef.first.three(uni.norm.dialysisGLM, "Dialysis")
uni.norm.readmit.results <- uni.norm.exp.coef.first.three(uni.norm.readmitGLM, "Readmission")

# Combine all results into one data frame
uni.norm.results.table <- rbind(uni.norm.AKIprogression.results, uni.norm.death.results, uni.norm.dialysis.results, uni.norm.readmit.results)

# Add a covariate category column
uni.norm.num.rows <- nrow(uni.norm.results.table)
uni.norm.new.column <- rep(NA, uni.norm.num.rows)
uni.norm.new.column[c(1, 4, 7, 10)] <- "Pre-AKI Creatinine Variability"
uni.norm.new.column[c(2, 5, 8, 11)] <- "Proportional Change"
uni.norm.new.column[c(3, 6, 9, 12)] <- "Absolute Change"
uni.norm.results.table <- cbind(uni.norm.results.table, uni.norm.covariate = uni.norm.new.column)

print(uni.norm.results.table)


# standardized, multivariate, uninteracted--------------------------------------------

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
exp.coef.first.three <- function(model, model.name) {
  cf <- coef(summary(model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  cf <- cf[2:4, ] # assuming the first row is the Intercept; adjust if needed

  OR <- exp(cf[, "Estimate"])
  lower.CI <- exp(cf[, "Estimate"] - 1.96 * cf[, "Std. Error"])
  upper.CI <- exp(cf[, "Estimate"] + 1.96 * cf[, "Std. Error"])
  p.value <- cf[, "Pr(>|z|)"]

  result <- cbind(OR, lower.CI, upper.CI, p.value)
  rownames(result) <- names(coef(model))[2:4] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  result.df <- as.data.frame(result)
  result.df$model <- model.name

  return(result.df)
}

# Apply the function to each of your models
AKIprogression.results <- exp.coef.first.three(norm.AKIprogressionGLM, "AKI Progression")
death.results <- exp.coef.first.three(norm.deathGLM, "Death")
dialysis.results <- exp.coef.first.three(norm.dialysisGLM, "Dialysis")
readmit.results <- exp.coef.first.three(norm.readmitGLM, "Readmission")

# Combine all results into one data frame
results.table <- rbind(AKIprogression.results, death.results, dialysis.results, readmit.results)

# Add a covariate category column
num.rows <- nrow(results.table)
new.column <- rep(NA, num.rows)
new.column[c(1, 4, 7, 10)] <- "Pre-AKI Creatinine Variability"
new.column[c(2, 5, 8, 11)] <- "Proportional Change"
new.column[c(3, 6, 9, 12)] <- "Absolute Change"
results.table <- cbind(results.table, covariate = new.column)

print(results.table)



# interactions level, standardized, multivariate-------------------------------------

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
inter.exp.coef.first.three <- function(inter.model, inter.model.name) {
  inter.cf <- coef(summary(inter.model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  inter.cf <- inter.cf[7:9, ] # assuming the first row is the Intercept; adjust if needed

  inter.OR <- exp(inter.cf[, "Estimate"])
  inter.lower.CI <- exp(inter.cf[, "Estimate"] - 1.96 * inter.cf[, "Std. Error"])
  inter.upper.CI <- exp(inter.cf[, "Estimate"] + 1.96 * inter.cf[, "Std. Error"])
  inter.p.value <- inter.cf[, "Pr(>|z|)"]

  inter.result <- cbind(inter.OR, inter.lower.CI, inter.upper.CI, inter.p.value)
  rownames(inter.result) <- names(coef(inter.model))[7:9] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  inter.result.df <- as.data.frame(inter.result)
  inter.result.df$inter.model <- inter.model.name

  return(inter.result.df)
}

# Apply the function to each of your models
inter.AKIprogression.results <- inter.exp.coef.first.three(inter.norm.AKIprogressionGLM, "AKI Progression")
inter.death.results <- inter.exp.coef.first.three(inter.norm.deathGLM, "Death")
inter.dialysis.results <- inter.exp.coef.first.three(inter.norm.dialysisGLM, "Dialysis")
inter.readmit.results <- inter.exp.coef.first.three(inter.norm.readmitGLM, "Readmission")

# Combine all results into one data frame
inter.results.table <- rbind(inter.AKIprogression.results, inter.death.results, inter.dialysis.results, inter.readmit.results)

# Add a covariate category column
inter.num.rows <- nrow(inter.results.table)
inter.new.column <- rep(NA, inter.num.rows)
inter.new.column[c(1, 4, 7, 10)] <- "Pre-AKI Creatinine Variability"
inter.new.column[c(2, 5, 8, 11)] <- "Proportional Change"
inter.new.column[c(3, 6, 9, 12)] <- "Absolute Change"
inter.results.table <- cbind(inter.results.table, inter.covariate = inter.new.column)

print(inter.results.table)


# interactions results from the same regressions

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
inters.only.exp.coef.first.three <- function(inters.only.model, inters.only.model.name) {
  inters.only.cf <- coef(summary(inters.only.model))
  # Ensure it's a dataframe
  if (!is.data.frame(inters.only.cf)) {
    inters.only.cf <- as.data.frame(inters.only.cf)
  }

  # Select only the first three covariates (excluding the Intercept if it is there)
  inters.only.cf <- inters.only.cf[17:25, ] # assuming the first row is the Intercept; adjust if needed

  inters.only.p.value <- inters.only.cf[, "Pr(>|z|)"]

  # Convert p-values to a data frame
  inters.only.result.df <- as.data.frame(inters.only.p.value)
  rownames(inters.only.result.df) <- names(coef(inters.only.model))[17:25] # adjust the index if your covariates start from a different row

  # Add a column for the model name
  inters.only.result.df$inters.only.model <- inters.only.model.name

  return(inters.only.result.df)
}

# Apply the function to each of your models
inters.only.AKIprogression.results <- inters.only.exp.coef.first.three(inter.norm.AKIprogressionGLM, "AKI Progression")
inters.only.death.results <- inters.only.exp.coef.first.three(inter.norm.deathGLM, "Death")
inters.only.dialysis.results <- inters.only.exp.coef.first.three(inter.norm.dialysisGLM, "Dialysis")
inters.only.readmit.results <- inters.only.exp.coef.first.three(inter.norm.readmitGLM, "Readmission")

# Combine all results into one data frame
inters.only.results.table <- rbind(inters.only.AKIprogression.results, inters.only.death.results, inters.only.dialysis.results, inters.only.readmit.results)

# Add a covariate category column
inters.only.new.column <- c("Pre-AKI Creatinine Variability in ICU", "Pre-AKI Creatinine Variability in ER", "Pre-AKI Creatinine Variability in Medical Service",
                            "Proportional Change in ICU", "Proportional Change in ER", "Proportional Change in Medical Service",
                            "Absolute Change in ICU", "Absolute Change in ER", "Absolute Change in Medical Service")

# Repeat the categories for each model
inters.only.new.column <- rep(inters.only.new.column, times = length(unique(inters.only.results.table$inters.only.model)))

inters.only.results.table$covariate <- inters.only.new.column

print(inters.only.results.table)




# reportable, univariate, uninteracted, unstandardized------------------------------------------------

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
uni.exp.coef.first.three <- function(uni.model, uni.model.name) {
  uni.cf <- coef(summary(uni.model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  uni.cf <- uni.cf[2:4, ] # assuming the first row is the Intercept; adjust if needed

  uni.OR <- exp(uni.cf[, "Estimate"])
  uni.lower.CI <- exp(uni.cf[, "Estimate"] - 1.96 * uni.cf[, "Std. Error"])
  uni.upper.CI <- exp(uni.cf[, "Estimate"] + 1.96 * uni.cf[, "Std. Error"])
  uni.p.value <- uni.cf[, "Pr(>|z|)"]

  uni.result <- cbind(uni.OR, uni.lower.CI, uni.upper.CI, uni.p.value)
  rownames(uni.result) <- names(coef(uni.model))[2:4] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  uni.result.df <- as.data.frame(uni.result)
  uni.result.df$uni.model <- uni.model.name

  return(uni.result.df)
}

# Apply the function to each of your models
uni.AKIprogression.results <- uni.exp.coef.first.three(uni.AKIprogressionGLM, "AKI Progression")
uni.death.results <- uni.exp.coef.first.three(uni.deathGLM, "Death")
uni.dialysis.results <- uni.exp.coef.first.three(uni.dialysisGLM, "Dialysis")
uni.readmit.results <- uni.exp.coef.first.three(uni.readmitGLM, "Readmission")

# Combine all results into one data frame
uni.results.table <- rbind(uni.AKIprogression.results, uni.death.results, uni.dialysis.results, uni.readmit.results)

# Add a covariate category column
uni.num.rows <- nrow(uni.results.table)
uni.new.column <- rep(NA, uni.num.rows)
uni.new.column[c(1, 4, 7, 10)] <- "Pre-AKI Creatinine Variability"
uni.new.column[c(2, 5, 8, 11)] <- "Proportional Change"
uni.new.column[c(3, 6, 9, 12)] <- "Absolute Change"
uni.results.table <- cbind(uni.results.table, uni.covariate = uni.new.column)

print(uni.results.table)



# reportable adjusted, unstandarized, uninteracted, multivariate-----------------------------------------------

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
report.exp.coef.first.three <- function(report.model, report.model.name) {
  report.cf <- coef(summary(report.model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  report.cf <- report.cf[2:4, ] # assuming the first row is the Intercept; adjust if needed

  report.OR <- exp(report.cf[, "Estimate"])
  report.lower.CI <- exp(report.cf[, "Estimate"] - 1.96 * report.cf[, "Std. Error"])
  report.upper.CI <- exp(report.cf[, "Estimate"] + 1.96 * report.cf[, "Std. Error"])
  report.p.value <- report.cf[, "Pr(>|z|)"]

  report.result <- cbind(report.OR, report.lower.CI, report.upper.CI, report.p.value)
  rownames(report.result) <- names(coef(report.model))[2:4] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  report.result.df <- as.data.frame(report.result)
  report.result.df$report.model <- report.model.name

  return(report.result.df)
}

# Apply the function to each of your models
report.AKIprogression.results <- report.exp.coef.first.three(AKIprogressionGLM, "AKI Progression")
report.death.results <- report.exp.coef.first.three(deathGLM, "Death")
report.dialysis.results <- report.exp.coef.first.three(dialysisGLM, "Dialysis")
report.readmit.results <- report.exp.coef.first.three(readmitGLM, "Readmission")

# Combine all results into one data frame
report.results.table <- rbind(report.AKIprogression.results, report.death.results, report.dialysis.results, report.readmit.results)

# Add a covariate category column
report.num.rows <- nrow(report.results.table)
report.new.column <- rep(NA, report.num.rows)
report.new.column[c(1, 4, 7, 10)] <- "Pre-AKI Creatinine Variability"
report.new.column[c(2, 5, 8, 11)] <- "Proportional Change"
report.new.column[c(3, 6, 9, 12)] <- "Absolute Change"
report.results.table <- cbind(report.results.table, report.covariate = report.new.column)

print(report.results.table)



# reportable, interacted, unstandardized, multivariate-----------------------------------------------

# Function to calculate exponentiated coefficients, their 95% CIs, and p-values for the first three covariates
inter.report.exp.coef.first.three <- function(inter.report.model, inter.report.model.name) {
  inter.report.cf <- coef(summary(inter.report.model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  inter.report.cf <- inter.report.cf[7:9, ] # assuming the first row is the Intercept; adjust if needed

  inter.report.OR <- exp(inter.report.cf[, "Estimate"])
  inter.report.lower.CI <- exp(inter.report.cf[, "Estimate"] - 1.96 * inter.report.cf[, "Std. Error"])
  inter.report.upper.CI <- exp(inter.report.cf[, "Estimate"] + 1.96 * inter.report.cf[, "Std. Error"])
  inter.report.p.value <- inter.report.cf[, "Pr(>|z|)"]

  inter.report.result <- cbind(inter.report.OR, inter.report.lower.CI, inter.report.upper.CI, inter.report.p.value)
  rownames(inter.report.result) <- names(coef(inter.report.model))[7:9] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  inter.report.result.df <- as.data.frame(inter.report.result)
  inter.report.result.df$inter.report.model <- inter.report.model.name

  return(inter.report.result.df)
}

# Apply the function to each of your models
inter.report.AKIprogression.results <- inter.report.exp.coef.first.three(inter.AKIprogressionGLM, "AKI Progression")
inter.report.death.results <- inter.report.exp.coef.first.three(inter.deathGLM, "Death")
inter.report.dialysis.results <- inter.report.exp.coef.first.three(inter.dialysisGLM, "Dialysis")
inter.report.readmit.results <- inter.report.exp.coef.first.three(inter.readmitGLM, "Readmission")

# Combine all results into one data frame
inter.report.results.table <- rbind(inter.report.AKIprogression.results, inter.report.death.results, inter.report.dialysis.results, inter.report.readmit.results)

# Add a covariate category column
inter.report.num.rows <- nrow(inter.report.results.table)
inter.report.new.column <- rep(NA, inter.report.num.rows)
inter.report.new.column[c(1, 4, 7, 10)] <- "Pre-AKI Creatinine Variability"
inter.report.new.column[c(2, 5, 8, 11)] <- "Proportional Change"
inter.report.new.column[c(3, 6, 9, 12)] <- "Absolute Change"
inter.report.results.table <- cbind(inter.report.results.table, inter.report.covariate = inter.report.new.column)

print(inter.report.results.table)

# interactions only from the standardized regressions

print(inters.only.results.table)





# write a csv with all results----------------------------------------------------------
#standardized
#write.csv(uni.results.table, file = "uni_results_table.csv", row.names = TRUE)
#write.csv(report.results.table, file = "report_results_table.csv", row.names = TRUE)
#write.csv(inter.report.results.table, file = "inter_report_results_table.csv", row.names = TRUE)
#write.csv(inters.only.results.table, file = "inters_only_results_table.csv", row.names = TRUE)


#standardized for appendix
#write.csv(uni.norm.results.table, file = "uni_norm_results_table.csv", row.names = TRUE)
#write.csv(results.table, file = "results_table.csv", row.names = TRUE)
#write.csv(inter.results.table, file = "inter_results_table.csv", row.names = TRUE)
#write.csv(inters.only.results.table, file = "inters_only_results_table.csv", row.names = TRUE)



#library(dplyr)
#library(tidyr)

#analysisSummary <- read.csv("Test Report Results Table.csv", stringsAsFactors = FALSE)
