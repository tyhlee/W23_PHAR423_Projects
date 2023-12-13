
library(ggplot2)

# Get the summary of the model
num_providerlm_summary <- summary(numProvidersLm)

# Extract the β coefficient and p-value for a chosen predictor
# Let's assume you want the coefficient for 'preAKIcreatVar'
beta_preAKIcreatVar <- num_providerlm_summary$coefficients["df$preAKIcreatVar", "Estimate"]
p_value_preAKIcreatVar <- num_providerlm_summary$coefficients["df$preAKIcreatVar", "Pr(>|t|)"]

# Create the scatter plot for 'preAKIcreatVar' against 'num_provider'
preAKIcreatVar.numProviderPlot <- ggplot(df, aes(x = preAKIcreatVar, y = num_provider)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
  labs(x = "Pre-AKI Creatinine Variability (SD)", y = "Number of Providers", title = "Multivariate Pre-AKI Creatinine Variability vs Number of Providers with Regression Fit") +
  theme_classic() +
  coord_cartesian(xlim = c(0, 1.5), ylim = c(0, 40)) + # Set the limits for x and y axes
  geom_text(aes(x = 1.3, y = 33, label = sprintf("(β = %.2f)", beta_preAKIcreatVar)),
            hjust = 1, vjust = 1, size = 4, parse = FALSE) +
  geom_text(aes(x = 1.3, y = 31, label = paste0("(p = ", formatC(p_value_preAKIcreatVar, format = "e", digits = 1), ")")),
            hjust = 1, vjust = 1, size = 4, parse = FALSE) +
  theme(plot.title = element_text(size = 12))
# Print the plot
print(preAKIcreatVar.numProviderPlot)





# Get the summary of the model
num_providerlm_summary <- summary(numProvidersLm)

# Extract the β coefficient and p-value for a chosen predictor
# Let's assume you want the coefficient for 'absoluteChange'
beta_absoluteChange <- num_providerlm_summary$coefficients["df$absoluteChange", "Estimate"]
p_value_absoluteChange <- num_providerlm_summary$coefficients["df$absoluteChange", "Pr(>|t|)"]



absoluteChange.numProviderPlot <- ggplot(df, aes(x = absoluteChange, y = num_provider)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
  labs(x = "Absolute Creatinine Change (mg/dL)", y = "Number of Providers", title = "Multivariate Absolute Creatinine Change vs Number of Providers with Regression Fit") +
  theme_classic() +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 40)) +
  geom_text(aes(x = 2, y = 32, label = sprintf("(β = %.2f)", beta_absoluteChange)),
            hjust = 1, vjust = 1, size = 4, parse = FALSE) +
  geom_text(aes(x = 2, y = 30, label = paste0("(p = ", formatC(p_value_absoluteChange, format = "e", digits = 1), ")")),
            hjust = 1, vjust = 1, size = 4, parse = FALSE) +
  theme(plot.title = element_text(size = 12))




# Print the plot
print(absoluteChange.numProviderPlot)

