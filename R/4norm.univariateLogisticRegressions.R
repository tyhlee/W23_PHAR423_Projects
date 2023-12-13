# logistic regressions with normalized independent variables for comparison of coefficients

# primary endpoint: AKI progression
uni.norm.AKIprogressionGLM <- glm(df$aki_progression14~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange
                                , family=binomial(link="logit"))
summary(uni.norm.AKIprogressionGLM)


# death
uni.norm.deathGLM <- glm(df$death14~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange
                       , family=binomial(link="logit"))
summary(uni.norm.deathGLM)


# dialylis
uni.norm.dialysisGLM <- glm(df$dialysis14~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange
                          , family=binomial(link="logit"))
summary(uni.norm.dialysisGLM)


# readmission
uni.norm.readmitGLM <- glm(df$readmit30~df$norm.preAKIcreatVar+df$norm.proportionalChange+df$norm.absoluteChange
                        , family=binomial(link="logit"))
summary(uni.norm.readmitGLM)



