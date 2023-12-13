# linear regressions with normalized independent variables for comparison of coefficients


# AKI duration
uni.AKIdurationLm <- lm(df$duration_of_aki ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange)
summary(uni.AKIdurationLm)


# number of providers
uni.numProvidersLm <- lm(df$num_provider ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange)
summary(uni.numProvidersLm)


summary(lm(df$num_provider~df$absoluteChange))
