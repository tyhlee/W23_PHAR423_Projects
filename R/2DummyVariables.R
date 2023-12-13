library(dplyr)

# Pre-AKI creatinine variability (sd)
df <- df %>%
  mutate(preAKIcreatVar = apply(.[, c("baseline_creat", "admit_creatinine","mincreat7_prior", "mincreat48_prior")], 1, sd))
# "creat_at_rand"

# 7-day pre-AKI proportional creatinine change
df <- df %>%
  mutate(proportionalChange = creat_at_rand / mincreat7_prior)


# 48-hour pre-AKI absolute creatinine change
df <- df %>%
  mutate(absoluteChange = creat_at_rand - mincreat48_prior)



# Baseline Creatinine Category (nominal)

df <- df %>%
  mutate(baselineCategory = case_when(
    baseline_creat < 0.4 ~ "low",
    baseline_creat > 1.0 ~ "high",
    baseline_creat >= 0.4 & baseline_creat <= 1.0 ~ "normal",
    TRUE ~ NA_character_
  ))
