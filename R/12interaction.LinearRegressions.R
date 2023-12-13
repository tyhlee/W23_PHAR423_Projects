# to test interactions of hospital admission location on previously significant and reported independant variables

  # AKI length of stay

inter.AKIdurationLm <- lm(df$duration_of_aki ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange + df$baselineCategory +
                        df$preAKIcreatVar*df$icu_at_rand + df$preAKIcreatVar*df$er_at_rand + df$preAKIcreatVar*df$admit_medical +
                        df$absoluteChange*df$icu_at_rand + df$absoluteChange*df$er_at_rand + df$absoluteChange*df$admit_medical +
                        df$proportionalChange*df$icu_at_rand + df$proportionalChange*df$er_at_rand + df$proportionalChange*df$admit_medical +
                        df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand + df$elx_score_pmhx + df$num_med)
summary(inter.AKIdurationLm)



  # number of providers
inter.numProvidersLm <- lm(df$num_provider ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange + df$baselineCategory +
                             df$preAKIcreatVar*df$icu_at_rand + df$preAKIcreatVar*df$er_at_rand + df$preAKIcreatVar*df$admit_medical +
                             df$absoluteChange*df$icu_at_rand + df$absoluteChange*df$er_at_rand + df$absoluteChange*df$admit_medical +
                             df$proportionalChange*df$icu_at_rand + df$proportionalChange*df$er_at_rand + df$proportionalChange*df$admit_medical +
                             df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand + df$elx_score_pmhx + df$num_med)
summary(inter.numProvidersLm)

