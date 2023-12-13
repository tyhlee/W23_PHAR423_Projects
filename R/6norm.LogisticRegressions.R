# logistic regressions with normalized independent variables for comparison of coefficients

# primary endpoint: AKI progression
norm.AKIprogressionGLM <- glm(df$aki_progression14~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange +
                           df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                           df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(norm.AKIprogressionGLM)


# death
norm.deathGLM <- glm(df$death14~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange +
                       df$baselineCategory +  df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                       df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(norm.deathGLM)


# dialylis
norm.dialysisGLM <- glm(df$dialysis14~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange +
                     df$baselineCategory +  df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                     df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(norm.dialysisGLM)


# readmission
norm.readmitGLM <- glm(df$readmit30~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange +
                          df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                          df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(norm.readmitGLM)
