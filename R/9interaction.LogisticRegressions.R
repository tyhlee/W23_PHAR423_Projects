# to test interactions of hospital admission location on previously significant and reported independant variables


# primary endpoint: AKI progression
inter.AKIprogressionGLM <- glm(df$aki_progression14 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical + df$baselineCategory +
                                 df$preAKIcreatVar*df$icu_at_rand + df$preAKIcreatVar*df$er_at_rand + df$preAKIcreatVar*df$admit_medical +
                                 df$proportionalChange*df$icu_at_rand + df$proportionalChange*df$er_at_rand + df$proportionalChange*df$admit_medical +
                                 df$absoluteChange*df$icu_at_rand + df$absoluteChange*df$er_at_rand + df$absoluteChange*df$admit_medical +
                                 df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                                 df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.AKIprogressionGLM)


# death
inter.deathGLM <- glm(df$death14 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical + df$baselineCategory +
                        df$preAKIcreatVar*df$icu_at_rand + df$preAKIcreatVar*df$er_at_rand + df$preAKIcreatVar*df$admit_medical +
                        df$proportionalChange*df$icu_at_rand + df$proportionalChange*df$er_at_rand + df$proportionalChange*df$admit_medical +
                        df$absoluteChange*df$icu_at_rand + df$absoluteChange*df$er_at_rand + df$absoluteChange*df$admit_medical +
                        df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                        df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.deathGLM)


# dialylis
inter.dialysisGLM <- glm(df$dialysis14 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical + df$baselineCategory +
                           df$preAKIcreatVar*df$icu_at_rand + df$preAKIcreatVar*df$er_at_rand + df$preAKIcreatVar*df$admit_medical +
                           df$proportionalChange*df$icu_at_rand + df$proportionalChange*df$er_at_rand + df$proportionalChange*df$admit_medical +
                           df$absoluteChange*df$icu_at_rand + df$absoluteChange*df$er_at_rand + df$absoluteChange*df$admit_medical +
                           df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                           df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.dialysisGLM)


# readmission
inter.readmitGLM <- glm(df$readmit30 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical + df$baselineCategory +
                          df$preAKIcreatVar*df$icu_at_rand + df$preAKIcreatVar*df$er_at_rand + df$preAKIcreatVar*df$admit_medical +
                          df$proportionalChange*df$icu_at_rand + df$proportionalChange*df$er_at_rand + df$proportionalChange*df$admit_medical +
                          df$absoluteChange*df$icu_at_rand + df$absoluteChange*df$er_at_rand + df$absoluteChange*df$admit_medical +
                          df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                          df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.readmitGLM)
