# logistic regressions with unscaled independent variables for interpretation of coefficients

# primary endpoint: AKI progression
AKIprogressionGLM <- glm(df$aki_progression14 ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange+
                           df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                           df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(AKIprogressionGLM)


# death
deathGLM <- glm(df$death14 ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange+
                  df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                  df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(deathGLM)


# dialylis
dialysisGLM <- glm(df$dialysis14 ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange+
                     df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                     df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(dialysisGLM)


# readmission
readmitGLM <- glm(df$readmit30 ~ df$preAKIcreatVar + df$proportionalChange + df$absoluteChange+
                    df$baselineCategory + df$age + df$sex + df$aki_to_rand + df$hemoglobin_at_rand +
                    df$plateletcount_at_rand + df$elx_score_pmhx + df$icu_at_rand+ df$num_med, family=binomial(link="logit"))
summary(readmitGLM)
