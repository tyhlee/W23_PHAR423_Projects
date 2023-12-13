#interactions +

# logistic regressions with normalized independent variables for comparison of coefficients

# primary endpoint: AKI progression
inter.norm.AKIprogressionGLM <- glm(df$aki_progression14 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical +
                             df$baselineCategory + df$norm.preAKIcreatVar*df$icu_at_rand +
                             df$norm.preAKIcreatVar*df$er_at_rand + df$norm.preAKIcreatVar*df$admit_medical +
                             df$norm.proportionalChange*df$icu_at_rand + df$norm.proportionalChange*df$er_at_rand +
                             df$norm.proportionalChange*df$admit_medical + df$norm.absoluteChange*df$icu_at_rand+
                             df$norm.absoluteChange*df$er_at_rand + df$norm.absoluteChange*df$admit_medical +
                             df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                             df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.norm.AKIprogressionGLM)


# death
inter.norm.deathGLM <- glm(df$death14 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical +
                             df$baselineCategory + df$norm.preAKIcreatVar*df$icu_at_rand +
                             df$norm.preAKIcreatVar*df$er_at_rand + df$norm.preAKIcreatVar*df$admit_medical +
                             df$norm.proportionalChange*df$icu_at_rand + df$norm.proportionalChange*df$er_at_rand +
                             df$norm.proportionalChange*df$admit_medical + df$norm.absoluteChange*df$icu_at_rand+
                             df$norm.absoluteChange*df$er_at_rand + df$norm.absoluteChange*df$admit_medical +
                             df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                             df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.norm.deathGLM)


# dialylis
inter.norm.dialysisGLM <- glm(df$dialysis14 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical +
                                df$baselineCategory + df$norm.preAKIcreatVar*df$icu_at_rand +
                                df$norm.preAKIcreatVar*df$er_at_rand + df$norm.preAKIcreatVar*df$admit_medical +
                                df$norm.proportionalChange*df$icu_at_rand + df$norm.proportionalChange*df$er_at_rand +
                                df$norm.proportionalChange*df$admit_medical + df$norm.absoluteChange*df$icu_at_rand+
                                df$norm.absoluteChange*df$er_at_rand + df$norm.absoluteChange*df$admit_medical +
                                df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                                df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.norm.dialysisGLM)


# readmission
inter.norm.readmitGLM <- glm(df$readmit30 ~ df$icu_at_rand + df$er_at_rand + df$admit_medical +
                               df$baselineCategory + df$norm.preAKIcreatVar*df$icu_at_rand +
                               df$norm.preAKIcreatVar*df$er_at_rand + df$norm.preAKIcreatVar*df$admit_medical +
                               df$norm.proportionalChange*df$icu_at_rand + df$norm.proportionalChange*df$er_at_rand +
                               df$norm.proportionalChange*df$admit_medical + df$norm.absoluteChange*df$icu_at_rand+
                               df$norm.absoluteChange*df$er_at_rand + df$norm.absoluteChange*df$admit_medical +
                               df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand + df$plateletcount_at_rand +
                               df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(inter.norm.readmitGLM)
