# logistic regressions with normalized independent variables for comparison of coefficients

# primary endpoint: AKI progression
uni.AKIprogressionGLM <- glm(df$aki_progression14~df$preAKIcreatVar+df$proportionalChange+df$absoluteChange
                                , family=binomial(link="logit"))
summary(uni.AKIprogressionGLM)


# death
uni.deathGLM <- glm(df$death14~df$preAKIcreatVar+df$proportionalChange+df$absoluteChange
                       , family=binomial(link="logit"))
summary(uni.deathGLM)


# dialylis
uni.dialysisGLM <- glm(df$dialysis14~df$preAKIcreatVar+df$proportionalChange+df$absoluteChange
                          , family=binomial(link="logit"))
summary(uni.dialysisGLM)


# readmission
uni.readmitGLM <- glm(df$readmit30~df$preAKIcreatVar+df$proportionalChange+df$absoluteChange
                        , family=binomial(link="logit"))
summary(uni.readmitGLM)
