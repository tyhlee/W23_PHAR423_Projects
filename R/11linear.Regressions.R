# multivariate

# AKI duration
AKIdurationLm <- lm(df$duration_of_aki~df$preAKIcreatVar+df$proportionalChange+df$absoluteChange +
                           df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                           df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med)
summary(AKIdurationLm)


# number of providers
numProvidersLm <- lm(df$num_provider~df$preAKIcreatVar+df$proportionalChange+df$absoluteChange +
                            df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                            df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med)
summary(numProvidersLm)
