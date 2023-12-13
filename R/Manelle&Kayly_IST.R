setwd("//Users/kaylychoy/Desktop/IST_2")

library(tidyverse)
library(janitor)
library(dplyr)
library(missForest)
library(survival)
library(ggplot2)
library(survminer)
library(broom)

#Trial data
df <- read_csv("ist_dataset.csv") 
  
  

#--------CREATE DATAFRAME--------
#Filter for DALIVE (only interested in patients that were discharged alive --> n=10322)
#Change to factors for imputation MCAR
df1 <- df %>% 
  filter(DALIVE == "Y") %>%
  mutate(sex.factor = as.factor(SEX)) %>% 
  mutate(fdead = as.factor(FDEAD))  %>% 
  mutate(stype = as.factor(STYPE)) %>% 
  mutate(dplace = as.factor(DPLACE)) %>% 
  mutate(dplace_b = ifelse(dplace == "B", 1, 0)) %>%                                          #dumbify variables w/ DPLACE A (home) as reference level
  mutate(dplace_c = ifelse(dplace == "C", 1, 0)) %>% 
  mutate(dplace_d = ifelse(dplace == "D", 1, 0)) %>% 
  mutate(dplace_e = ifelse(dplace == "E", 1, 0)) %>% 
  mutate(CNTRYNUM = as.factor(CNTRYNUM)) %>% 
  rename(age = AGE) %>% 
  rename(rsbp = RSBP) %>% 
  select(sex.factor, age, rsbp, stype, dplace, fdead, CNTRYNUM, TD, EXPDD, EXPD6, EXPD14,dplace_b, dplace_c, dplace_d, dplace_e)


#--------REMOVAL OF MISSING DATA--------

df1$num_missing <- apply(df1,1,FUN = function(each_row){sum(is.na(each_row))})

apply(df1,2,FUN = function(each_col){sum(is.na(each_col))})

df1_complete <- df1 %>% 
  filter(num_missing<=0) %>% 
  select(-num_missing) %>%                                                                    #Calculate number of people with missing covariate values
  filter(dplace == "A" | dplace == "B" | dplace == "C" | dplace == "D" | dplace == "E" ) %>%  #Calculate number of people with known specified discharge destinations
  filter(fdead == "N" | fdead == "Y") %>%                                                     #Calculate number of people with known 6month mortality status
  mutate(dplace_b = ifelse(dplace == "B", 1, 0)) %>%                                          #dumbify variables w/ DPLACE A (home) as reference level
  mutate(dplace_c = ifelse(dplace == "C", 1, 0)) %>% 
  mutate(dplace_d = ifelse(dplace == "D", 1, 0)) %>% 
  mutate(dplace_e = ifelse(dplace == "E", 1, 0)) %>%
  mutate(fdead = ifelse(fdead == "Y", 1, 0)) %>% 
  mutate(death = ifelse(fdead == 0, "Alive", "Dead")) %>% 
  mutate(sex = ifelse(sex.factor == "F", 1, 0))


#--------IMPUTATION--------
#Missing Data MCAR (Missing completely at random)
set.seed(1)
df1.imp <- missForest(as.data.frame(df1), variablewise = T)
df1.imp$OOBerror


#--------KM Plot--------

#Create fit
km0 <- survfit(Surv(TD, fdead)~dplace, data = df1_complete)

ggsurvplot(
  km0, 
  pval = T, 
  size = 0.5,
  censor.shape="*",
  xlab = "Days Elapsed",
  xlim = c(0, 200),
  ylab = "Survival Rate",
  break.time.by = 50,
  ggtheme = theme_light(),
  title = "Kaplan Meier Survival Curve",
  font.title = c(16, "bold"),
  legend = "bottom",
  risk.table = TRUE,
  risk.table.height = 0.3,
  risk.table.col = "strata",
  risk.table.y.text = FALSE,
  legend.labs =c("Home", "Relative's Home", "Residential Care", "Nursing Home", "Other Hospital Department")
)


#--------COX REGRESSION MODEL--------

#Cox Regression:
cr.unadjust <- coxph(Surv(TD, fdead)~age+rsbp+dplace_b+dplace_c+dplace_d+dplace_e+EXPD6+EXPDD+EXPD14, data = as.data.frame(df1_complete))
tbl_cr.unadj=tidy(cr.unadjust)
class(tbl_cr.unadj)
#Add columns: Proportional HR, 95% confidence interval
tbl_cr.unadj$HR <- c(exp(tbl_cr.unadj$estimate))
tbl_cr.unadj$lb <- c(tbl_cr.unadj$HR-1.96*tbl_cr.unadj$std.error)
tbl_cr.unadj$ub <- c(tbl_cr.unadj$HR+1.96*tbl_cr.unadj$std.error)
write.csv(tbl_cr.unadj, file = "ist_cox.cr.unadj.csv")

#Draw Forest Plot 1
forest1 <- ggforest(
  cr.unadjust,
  main = "Forest Plot of Cox Proportional Hazards Ratios - Unadjusted for Colinearity",
  noDigits = 3
)


#Check colinearity and remove highest collinearity value
cor.data <- cor(df1_complete[, c('EXPD14', 'EXPDD', 'EXPD6')])
write.csv(cor.data, file = "ist_correlation.csv")

#Adjusted for Colinearity Cox Regression: remove EXPD6
cr1 <- coxph(Surv(TD, fdead)~age+rsbp+dplace_b+dplace_c+dplace_d+dplace_e+EXPDD+EXPD14, data = as.data.frame(df1_complete))
tbl_cr1=tidy(cr1)
class(tbl_cr1)
#Add columns: Proportional HR, 95% confidence interval
tbl_cr1$HR <- c(exp(tbl_cr1$estimate))
tbl_cr1$lb <- c(tbl_cr1$HR-1.96*tbl_cr1$std.error)
tbl_cr1$ub <- c(tbl_cr1$HR+1.96*tbl_cr1$std.error)
write.csv(tbl_cr1, file = "ist_cox.cr1.csv")

#Draw Forest Plot 2
forest2 <- ggforest(
  cr1,
  main = "Forest Plot of Cox Proportional Hazards Ratios - Adjusted for Colinearity",
  noDigits = 3
  )

#Cox PH Test Assumptions (Schoenfeld Test)
test.ph <- cox.zph(cr1)
ggcoxzph(test.ph)

#--------ADJUSTED PH SURVIVAL CURVE--------
library (finalfit)
cr1.curve <- surv_adjustedcurves(cr1, data = df1_complete)

#Rename DPLACE Variables
df1_complete$dplace.tbl <- 
  factor(df1_complete$dplace, 
         levels=c("A", "B", "C", "D", "E"),
         labels=c("Home", 
                  "Relative's Home", 
                  "Residential Care",
                  "Nursing Home",
                  "Other Hospital Department"))

#Draw Adjusted PH  Survival Curve
ggadjustedcurves(cr1, 
                 pval=TRUE,
                 data = as.data.frame(df1_complete), 
                 method = "average", 
                 variable = "dplace.tbl",
                 xlab = "Days Elapsed",
                 ylab = "Survival Rate",
                 ggtheme = theme_light(),
                 title = "Adjusted Survival Curve From Cox Model",
                 font.title = c(16, "bold"),
                 legend = "bottom",
                 size = 0.5)

#--------VISUALIZATION WITH TREEMAP--------
#Treemap Packages
library(treemapify)
library(ggplot2)

#Proportion of Patients w/ 6 month Mortality - sort by discharge destimation
df1_dead <- df1_complete %>% 
  select(death,dplace.tbl) %>% 
  group_by(dplace.tbl, death) %>% 
  count()

#Generate Treemap
ggplot2::ggplot(df1_dead, ggplot2::aes(area = n, fill = dplace.tbl, label =paste("\n"))) +
  labs(title = "6 Month Mortality Based on Discharge Destination") +
  facet_wrap( ~ death) +
  theme(legend.position = "bottom") +
  geom_treemap()

-----#DESCRIPTIVE STATS---------

#Histogram of Age
hist_age <- ggplot(df1_complete, aes(x = AGE)) + 
  geom_histogram(binwidth = 1, colour="#231076", fill="#B8AAD8") +
  facet_grid(sex ~.) +
  xlab("Age (years)") + ylab("Number of Patients") +
  ggtitle("Sex Based Distribution of Age")
hist_age + theme(
  plot.title = element_text(size=20, face="bold"),
  axis.text=element_text(size=10),
  axis.title.x = element_text(size=14),
  axis.title.y = element_text(size=14)
)

#Patient Characteristics Table  

library(boot) 
library(table1)

df1_complete$sex.tbl <- 
  factor(df1_complete$sex.factor, levels=c("M", "F"),
         labels=c("Male",
                  "Female"))

label(df1_complete$sex.tbl)    <- "Sex"
label(df1_complete$age)        <- "Age"
label(df1_complete$rsbp)       <- "Systolic Blood Pressure"
label(df1_complete$stype)      <- "Stroke Subtype"
label(df1_complete$dplace.tbl) <- "Discharge Destination"
label(df1_complete$EXPDD)      <- "Probability of Death or Dependence at 6 Months"
label(df1_complete$EXPD14)     <- "Probability of Death at 14 Days"
label(df1_complete$EXPD6)      <- "Probability of Death at 6 Months"
label(df1_complete$age)        <- "Age"
label(df1_complete$death)      <- "6 Month Mortality Status"

units(df1_complete$age)        <- "years"
units(df1_complete$rsbp)       <- "mmHg"


descriptive.tbl <- table1(~ sex.tbl + age + rsbp + stype + death + EXPDD + EXPD14 + EXPD6 | dplace.tbl, data=df1_complete,
       overall=c(left="Total"), caption=caption)

write.csv(descriptive.tbl, file = "ist_descriptive.csv")
