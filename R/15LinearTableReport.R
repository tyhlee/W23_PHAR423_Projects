# Function to calculate coefficients, and p-values for the first three covariates
linear.coef.first.three <- function(linear.model, linear.model.name) {
  linear.cf <- coef(summary(linear.model))
  # Select only the first three covariates (excluding the Intercept if it is there)
  linear.cf <-linear.cf[2:4, ] # assuming the first row is the Intercept; adjust if needed

  linear.beta <- linear.cf[, "Estimate"]
  linear.p.value <- linear.cf[, "Pr(>|t|)"]

  linear.result <- cbind(linear.beta, linear.p.value)
  rownames(linear.result) <- names(coef(linear.model))[2:4] # adjust the index if your covariates start from a different row

  # Convert to data frame and add a column for the model name
  linear.result.df <- as.data.frame(linear.result)
  linear.result.df$linear.model <- linear.model.name

  return(linear.result.df)
}

# Apply the function to each of your models
uni.linear.AKIduration.results <- linear.coef.first.three(uni.AKIdurationLm, "Univariate AKI Duration")
uni.linear.numProviders.results <- linear.coef.first.three(uni.numProvidersLm, "Univariate Number of Providers")
linear.AKIduration.results <- linear.coef.first.three(AKIdurationLm, "Multivariate AKI Duration")
linear.numProviders.results <- linear.coef.first.three(numProvidersLm, "Multivariate Number of Providers")
inter.linear.AKIduration.results <- linear.coef.first.three(inter.AKIdurationLm, "Interaction AKI Duration")
inter.linear.numProviders.results <- linear.coef.first.three(inter.numProvidersLm, "Interaction Number of Providers")


# Combine all results into one data frame
linear.results.table <- rbind(uni.linear.AKIduration.results, uni.linear.numProviders.results, linear.AKIduration.results, linear.numProviders.results, inter.linear.AKIduration.results, inter.linear.numProviders.results)

# Add a covariate category column
linear.num.rows <- nrow(linear.results.table)

# Initialize the new column with NA (or choose another default value)
linear.new.column <- rep(NA, linear.num.rows)

# Assign values to specific row positions
linear.new.column[c(1, 4, 7, 10, 13, 16 )] <- "Pre-AKI Creatinine Variability"
linear.new.column[c(2, 5, 8, 11, 14, 17)] <- "Proportional Change"
linear.new.column[c(3, 6, 9, 12, 15, 18)] <- "Absolute Change"

# Add the new column to your data frame
linear.results.table <- cbind(linear.results.table, linear.covariate = linear.new.column)

print(linear.results.table)
# write.csv(linear.results.table, file = "linear_results_table.csv", row.names = FALSE)





# interaction p-values only
# Function to calculate coeffiecents, and p-values for the first three covariates
inters.only.linear.coef.first.three <- function(inters.only.linear.model, inters.only.linear.model.name) {
  inters.only.linear.cf <- coef(summary(inters.only.linear.model))
  # Ensure it's a dataframe
  if (!is.data.frame(inters.only.linear.cf)) {
    inters.only.linear.cf <- as.data.frame(inters.only.linear.cf)
  }

  # Select only the first three covariates (excluding the Intercept if it is there)
  inters.only.linear.cf <- inters.only.linear.cf[17:25, ] # assuming the first row is the Intercept; adjust if needed

  inters.only.linear.p.value <- inters.only.linear.cf[, "Pr(>|t|)"]

  # Convert p-values to a data frame
  inters.only.linear.result.df <- as.data.frame(inters.only.linear.p.value)
  rownames(inters.only.linear.result.df) <- names(coef(inters.only.linear.model))[17:25] # adjust the index if your covariates start from a different row

  # Add a column for the model name
  inters.only.linear.result.df$inters.only.linear.model <- inters.only.linear.model.name

  return(inters.only.linear.result.df)
}

# Apply the function to each of your models
inters.only.linear.AKIduration.results <- inters.only.linear.coef.first.three(inter.AKIdurationLm, "Interaction AKI Duration")
inters.only.linear.numProviders.results <- inters.only.linear.coef.first.three(inter.numProvidersLm, "Interaction Number of Providers")


# Combine all results into one data frame
inters.only.linear.results.table <- rbind(inters.only.linear.numProviders.results, inters.only.linear.AKIduration.results)

# Add a covariate category column
inters.only.linear.new.column <- c("Pre-AKI Creatinine Variability in ICU", "Pre-AKI Creatinine Variability in ER", "Pre-AKI Creatinine Variability in Medical Service",
                                   "Proportional Change in ICU", "Proportional Change in ER", "Proportional Change in Medical Service",
                                   "Absolute Change in ICU", "Absolute Change in ER", "Absolute Change in Medical Service")

# Repeat the categories for each model
inters.only.linear.new.column <- rep(inters.only.linear.new.column, times = length(unique(inters.only.linear.results.table$inters.only.linear.model)))

inters.only.linear.results.table$covariate <- inters.only.linear.new.column

print(inters.only.linear.results.table)

# append
library(dplyr)
all.linear.results.table <- bind_rows(linear.results.table, inters.only.linear.results.table)

#write.csv(all.linear.results.table, file = "all.linear.results.table.csv", row.names = TRUE)

