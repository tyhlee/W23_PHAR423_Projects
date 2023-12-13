library(ggplot2)
library(dplyr)

#each section may need to be rerun to order the plot properly


# Univariate layer, uninteracted, standardized-----------------------------------------------
uni.norm.results.table$uni.norm.Study <- seq(nrow(uni.norm.results.table))
uni.norm.results.table <- uni.norm.results.table[order(uni.norm.results.table$uni.norm.OR), ]

# Create an ordered factor for the models based on their appearance order in the dataset
uni.norm.results.table$uni.norm.model_ordered <- factor(uni.norm.results.table$uni.norm.model, levels = unique(uni.norm.results.table$uni.norm.model))

# Assign plot positions based on the ordered factor and ensure each point within a model is offset
uni.norm.results.table <- uni.norm.results.table %>%
  group_by(uni.norm.model_ordered) %>%
  mutate(plot_position = as.numeric(uni.norm.model_ordered) + (row_number() - 1) * 0.1) %>%
  ungroup()

univariatePlot <- ggplot(uni.norm.results.table, aes(x = uni.norm.OR, y = uni.norm.Study, color = uni.norm.model)) +
  geom_point() +
  geom_errorbar(aes(xmin = uni.norm.lower.CI, xmax = uni.norm.upper.CI), width = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "orange") +
  labs(x = "Odds Ratio (log scale)", y = "Covariates", title = "Univariate Standardized Odds Ratios of Creatinine Change", color = "Model") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = uni.norm.results.table$uni.norm.Study, labels = uni.norm.results.table$uni.norm.covariate) +
  geom_text(aes(label = sprintf("p = %.3f", uni.norm.p.value)), hjust = 1.2, vjust = 0) +
  theme(panel.grid.major.x = element_line(color = "grey", size = 0.2),
        panel.grid.minor.x = element_line(color = "lightgrey", size = 0.2))

# Print the plot
print(univariatePlot)


#standarized, uninteracted, multivariate-----------------------------------------------

# Calculating the position for each point
results.table$Study <- seq(nrow(results.table))

# Ordering the data frame by OR in ascending order for plotting
results.table <- results.table[order(results.table$OR), ]

# Create an ordered factor for the models based on their appearance order in the dataset
results.table$model_ordered <- factor(results.table$model, levels = unique(results.table$model))

# Assign plot positions based on the ordered factor and ensure each point within a model is offset
results.table <- results.table %>%
  group_by(model_ordered) %>%
  mutate(plot_position = as.numeric(model_ordered) + (row_number() - 1) * 0.1) %>%
  ungroup()

# Define the breaks for the x-axis. You need to specify these based on the range of your data.
multivariatePlot <- ggplot(results.table, aes(x = OR, y = Study, color = model)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower.CI, xmax = upper.CI), width = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "orange") +
  labs(x = "Odds Ratio (log scale)", y = "Covariates", title = "Multivariate Standardized Odds Ratios of Creatinine Change", color = "Model") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = results.table$Study, labels = results.table$covariate) +
  geom_text(aes(label = sprintf("p = %.3f", p.value)), hjust = 1.2, vjust = 0) +
  theme(panel.grid.major.x = element_line(color = "grey", size = 0.2),
        panel.grid.minor.x = element_line(color = "lightgrey", size = 0.2))

# Print the plot
print(multivariatePlot)



# Interaction layer, multivariate, standardized-----------------------------------------------
inter.results.table$inter.Study <- seq(nrow(inter.results.table))
inter.results.table <- inter.results.table[order(inter.results.table$inter.OR), ]

# Create an ordered factor for the models based on their appearance order in the dataset
inter.results.table$inter.model_ordered <- factor(inter.results.table$inter.model, levels = unique(inter.results.table$inter.model))

# Assign plot positions based on the ordered factor and ensure each point within a model is offset
inter.results.table <- inter.results.table %>%
  group_by(inter.model_ordered) %>%
  mutate(plot_position = as.numeric(inter.model_ordered) + (row_number() - 1) * 0.1) %>%
  ungroup()

library(readr)
#Inters_Pvalues <- read_csv("Inters Pvalues.csv")
#View(Inters_Pvalues)

#inter.results.table <- inter.results.table[order(inter.results.table$inter.Study), ]
#inter.results.table$inter.pvalues <- Inters_Pvalues$inter.pvalues

# Now you can safely append using cbind or direct assignment as shown above


library(scales)  # For formatting

interactionsPlot <- ggplot(inter.results.table, aes(x = inter.OR, y = inter.Study, color = inter.model)) +
  geom_point() +
  geom_errorbar(aes(xmin = inter.lower.CI, xmax = inter.upper.CI), width = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "orange") +
  labs(x = "Odds Ratio (log scale)", y = "Covariates",
       title = "Multivariate Standardized Odds Ratios of Creatinine Change with Inpatient Unit Interaction Feature",
       color = "Model") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = inter.results.table$inter.Study, labels = inter.results.table$inter.covariate) +
  #geom_text(aes(label = sprintf(inter.pvalues)), hjust = 0.37, vjust = -.3, size = 2.8) +  # Size adjusted here
  theme(panel.grid.major.x = element_line(color = "grey", size = 0.2),
        panel.grid.minor.x = element_line(color = "lightgrey", size = 0.2),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),  # Adjusting axis text size
        axis.title = element_text(size = 12))  # Adjusting axis title size


# Print the plot
print(interactionsPlot)

# !!!! rerun to order
