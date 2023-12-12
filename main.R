library(tidyverse)
library(readxl)
library(ggplot2)
library(gtsummary)
theds <- read_xlsx("data/2017_6_30 bmjopen-2017-017430.R1 DATASET for DRYAD 2.xlsx",sheet=1)
view(theds)
#first I will make new data looking at patients that have hypertension
#note that we are setting the condition of hypertension if the SBP (systolic blood pressure) is
equal to or greater than 128 OR if the it is less than 90. The reason for this is that taking
hypertension medications may actually lead to hypo tension which would be classified as
overtreatment
#the problem is our hypertension column is in words (yes or no) so first we need to convert
"yes" and "no" to numbers
theds %>%
  mutate(HTN_coded = as.numeric(HTN=="HTN Dx")) -> tmp
tmp$HTN_coded %>% table()
htnpatients<- tmp%>%filter(HTN_coded!="0")
table(htnpatients$HTN_coded)
#now we will separate the data further so that the patients must be taking at least one
hypertension medication as some patients have hypertension but not taking medication
htn1<-htnpatients%>%mutate(HTN_treatment=ifelse(HTN_treatment>=1, 1, 0))
table(htn1$HTN_treatment)
htn2<-htn1%>%filter(HTN_treatment!="0")
table(htn2$HTN_treatment)
#now that we figured out who has hypertension we will now look at who is considered over
treated for hypertension meaning SBP is >=128 or <90mmHg
htnds<-htn2%>%mutate(SBP=ifelse(SBP>=128 | SBP<90, 1, 0))
table(htnds$SBP)
#now we will filter over treated and normally treated for hypertension
htnover<-htnds%>%filter(SBP!="0")
table(htnover$SBP)
view(htnover)
htnunder<-htnds%>%filter(SBP!="1")
table(htnunder$SBP)
#now we want to separate the groups of people on poly pharmacy treatments and people not on
poly pharmacy treatments
#poly pharmacy was defined as taking 9 or more medications
#note there are 3 columns from the excel spread sheet which show how many medications the
patients are taking. We will be looking at All_Reg_Meds which stands for regular medications
that patients are on
#first we will look at the non over treated group and see which are on poly pharmacy and which
are not
htnmedsunder<-htnunder%>%mutate(All_Reg_Meds=ifelse(All_Reg_Meds>=9, 1, 0))
table(htnmedsunder$All_Reg_Meds)
htnpolyunder<-htnmedsunder%>%filter(All_Reg_Meds!="0")
table(htnpolyunder$All_Reg_Meds)
htn_nonpolyunder<-htnmedsunder%>%filter(All_Reg_Meds!="1")
table(htn_nonpolyunder$All_Reg_Meds)
view(htn_nonpolyunder)
#now we will look at the over treated group and see who is on poly pharmacy and who is not
htnmedsover<-htnover%>%mutate(All_Reg_Meds=ifelse(All_Reg_Meds>=9, 1, 0))
table(htnmedsover$All_Reg_Meds)
htnpolyover<-htnmedsover%>%filter(All_Reg_Meds!="0")
table(htnpolyover$All_Reg_Meds)
htn_nonpolyover<-htnmedsover%>%filter(All_Reg_Meds!="1")
table(htn_nonpolyover$All_Reg_Meds)
view(htnpolyover)
#now we can run our 2x2 table
#method 1:
a=36
b=32
c=23
d=13
OR<- a*d/(b*c)
OR
log_OR<-log(OR)
log_OR
SE_log_OR<-sqrt(1/a + 1/b + 1/c + 1/d)
#now we construct a lower and upper bound
lb <- log_OR - 1.96*SE_log_OR #Using the 1.96 rule to construct the lower and upper bound of
95%CI
ub <- log_OR + 1.96*SE_log_OR
#above calculations were on the logarithmic scale so we need to convert back
lb2<-exp(lb)
ub2<-exp(ub)
or2<-exp(log_OR)
lb2 #= 0.277
ub2 #= 1.459
#the fact that the 95% confidence interval crosses 1 indicates there is a strong chance that
there is no statistical difference between exposure and non-exposure
#####@@@@@@@@ now for men ####@@@@@@@@
#first we will look at the non over treated group and see which are on poly pharmacy and which
are not
htnmedsunder<-htnunder%>%mutate(All_Reg_Meds=ifelse(All_Reg_Meds>=9, 1, 0))
table(htnmedsunder$All_Reg_Meds)
htnpolyunder<-htnmedsunder%>%filter(All_Reg_Meds!="0")
table(htnpolyunder$All_Reg_Meds)
htn_nonpolyunder<-htnmedsunder%>%filter(All_Reg_Meds!="1")
table(htn_nonpolyunder$All_Reg_Meds)
view(htn_nonpolyunder)
htnpolyunderm<- htnpolyunder%>%filter(SexMF!="0")
table(htnpolyunderm$SexMF)
htn_nonpolyunderm<- htn_nonpolyunder%>%filter(SexMF!="0")
table(htn_nonpolyunderm$SexMF)
#now we will look at the over treated group of men and see who is on poly pharmacy and who is
not
htnmedsover<-htnover%>%mutate(All_Reg_Meds=ifelse(All_Reg_Meds>=9, 1, 0))
table(htnmedsover$All_Reg_Meds)
htnpolyover<-htnmedsover%>%filter(All_Reg_Meds!="0")
table(htnpolyover$All_Reg_Meds)
htn_nonpolyover<-htnmedsover%>%filter(All_Reg_Meds!="1")
table(htn_nonpolyover$All_Reg_Meds)
htnpolyoverm<-htnpolyover%>%filter(SexMF!="0")
table(htnpolyoverm$SexMF)
htn_nonpolyoverm<-htn_nonpolyover%>%filter(SexMF!="0")
table(htn_nonpolyoverm$SexMF)
#now we will run our 2x2 calcualtions to get the odds ratio
am=25
bm=24
cm=17
dm=6
ORm<- am*dm/(bm*cm)
ORm
log_ORm<-log(ORm)
log_ORm
SE_log_ORm<-sqrt(1/am + 1/bm + 1/cm + 1/dm)
#now we construct a lower and upper bound
lbm <- log_ORm - 1.96*SE_log_ORm #Using the 1.96 rule to construct the lower and upper
bound of 95%CI
ubm <- log_ORm + 1.96*SE_log_ORm
#above calculations were on the logarithmic scale so we need to convert back
lb2m<-exp(lbm)
ub2m<-exp(ubm)
or2m<-exp(log_ORm)
lb2m
ub2m
or2m
###@@@@@@@for women@@@@@@@@@@@@@###########
#first we will look at the non over treated group and see which are on poly pharmacy and which
are not
htnmedsunder<-htnunder%>%mutate(All_Reg_Meds=ifelse(All_Reg_Meds>=9, 1, 0))
table(htnmedsunder$All_Reg_Meds)
htnpolyunder<-htnmedsunder%>%filter(All_Reg_Meds!="0")
table(htnpolyunder$All_Reg_Meds)
htn_nonpolyunder<-htnmedsunder%>%filter(All_Reg_Meds!="1")
table(htn_nonpolyunder$All_Reg_Meds)
htnpolyunderf<- htnpolyunder%>%filter(SexMF!="1")
table(htnpolyunderf$SexMF)
htn_nonpolyunderf<- htn_nonpolyunder%>%filter(SexMF!="1")
table(htn_nonpolyunderf$SexMF)
#now we will look at the over treated group of women and see who is on poly pharmacy and
who is not
htnmedsover<-htnover%>%mutate(All_Reg_Meds=ifelse(All_Reg_Meds>=9, 1, 0))
table(htnmedsover$All_Reg_Meds)
htnpolyover<-htnmedsover%>%filter(All_Reg_Meds!="0")
table(htnpolyover$All_Reg_Meds)
htn_nonpolyover<-htnmedsover%>%filter(All_Reg_Meds!="1")
table(htn_nonpolyover$All_Reg_Meds)
htnpolyoverf<-htnpolyover%>%filter(SexMF!="1")
table(htnpolyoverf$SexMF)
htn_nonpolyoverf<-htn_nonpolyover%>%filter(SexMF!="1")
table(htn_nonpolyoverf$SexMF)
#now for calculations for 2x2 table for women
af=11
bf=8
cf=6
df=7
ORf<- af*df/(bf*cf)
ORf
log_ORf<-log(ORf)
log_ORf
SE_log_ORf<-sqrt(1/af + 1/bf + 1/cf + 1/df)
#now we construct a lower and upper bound
lbf <- log_ORf - 1.96*SE_log_ORf #Using the 1.96 rule to construct the lower and upper bound
of 95%CI
ubf <- log_ORf + 1.96*SE_log_ORf
#above calculations were on the logarithmic scale so we need to convert back
lb2f<-exp(lbf)
ub2f<-exp(ubf)
or2f<-exp(log_ORf)
lb2f
ub2f
or2f
###########################################################################
#method #2 using a logistic regression model
# theds <- read.csv("data/ds2.csv")
head(theds)
htn2_transformed <- htn2 %>%
  mutate(
    sex=SexMF,
    polypharmacy=ifelse(All_Reg_Meds>=9, 1, 0),
    abnormal_bp=ifelse(SBP>=128 | SBP<90, 1, 0)
  ) %>%
  select(sex, polypharmacy, abnormal_bp)
head(htn2_transformed)
model_all <- glm(abnormal_bp ~ ., data = htn2_transformed, family = "binomial"(link="logit"))
model_all
# plot(model_all)
summary(model_all)
#this is code for plotting graph:
ggplot( data=htn2_transformed, mapping=aes(x=polypharmacy, y=abnormal_bp, color=sex))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, color="red")+
  labs(title="Logistic Regression Model showing Treatment of Hypertension vs Pharmacy
Treatment Plan",
       x="Polypharamcy or Non-polypharmacy Treatment",
       y="Treatment of Hypertension (overtreatment or not)")
#This is a box plot of this code:
ggplot( data=htn2_transformed, mapping=aes(x=polypharmacy, y=abnormal_bp,
                                           group=polypharmacy))+
  geom_boxplot(fill="lightblue")+
  labs(title="Boxplot of Predidcted Probabilities for Treatment of Hypertension vs. Pharmacy
Treatment Plan",
       x="Non-polypharmacy Treatment or Polypharmacy Treatment",
       y="Treatment of Hypertension (overtreatment or not)")
#this is using continuous variables (so we just use all values of All_Reg_Meds)
htn2_transformed <- htn2 %>%
  mutate(
    sex=SexMF,
    polypharmacy=All_Reg_Meds,
    abnormal_bp=ifelse(SBP>=128 | SBP<90, 1, 0)
  ) %>%
  select(sex, polypharmacy, abnormal_bp)
ggplot( data=htn2_transformed, mapping=aes(x=polypharmacy, y=abnormal_bp, color=sex))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, color="red")+
  labs(title="Logistic Regression Model showing Overtreatment of Hypertension vs Pharmacy
Treatment",
       x="Polypharamcy or Non-polypharmacy Treatment",
       y="Treatment of Hypertension (overtreatment or not)")
#now we do this if we want to extract coefficients to compare with our 2x2 table:
betas<-coefficients(model_all)
OR<-exp(betas)
print(OR)
ln_or<-betas[3]
summary(model_all)
se <- summary(model_all)$coefficients[3,2]
# 1.96 rule
cil <- ln_or - 1.96*se
ciu <- ln_or + 1.96*se
#IMPORTANT: everything we had done so far was on the log scale. We need to 'anti-log' i.e.,
exponentiate to get the results:
  exp(ln_or)
exp(cil)
exp(ciu)
#males
htn2_transformed_only_men <- htn2_transformed %>%
  filter(sex == 1) %>%
  select(-sex)
model_male <- glm(abnormal_bp ~ ., data = htn2_transformed_only_men, family =
                    "binomial"(link="logit"))
# plot(model_male)
summary(model_male)
plot(model_male)
ggplot( data=htn2_transformed_only_men, mapping=aes(x=polypharmacy, y=abnormal_bp))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, color="red")+
  labs(title="Logistic Regression Model showing Treatment of Hypertension vs Pharmacy
Tretment Plan for Males",
       x="Polypharamcy or Non-polypharmacy Treatment",
       y="Treatment of Hypertension (overtreatment or not)")
betas<-coefficients(model_male)
ORm<-exp(betas)
print(ORm)
ln_orm<-betas[2]
ln_orm
summary(model_male)
sem <- summary(model_male)$coefficients[2,2]
# 1.96 rule
cilm <- ln_orm - 1.96*sem
cium <- ln_orm + 1.96*sem
#IMPORTANT: everything we had done so far was on the log scale. We need to 'anti-log' i.e.,
exponentiate to get the results:
  exp(ln_orm)
exp(cilm)
exp(cium)
#for females
htn2_transformed_only_women <- htn2_transformed %>%
  filter(sex == 0) %>%
  select(-sex)
model_female <- glm(abnormal_bp ~ ., data = htn2_transformed_only_women, family =
                      "binomial"(link="logit"))
# plot(model_female)
summary(model_female)
plot(model_female)
betas<-coefficients(model_female)
ORf<-exp(betas)
print(ORf)
ln_orf<-betas[2]
ln_orf
summary(model_female)
sef <- summary(model_female)$coefficients[2,2]
# 1.96 rule
cilf <- ln_orf - 1.96*sef
ciuf <- ln_orf + 1.96*sef
#IMPORTANT: everything we had done so far was on the log scale. We need to 'anti-log' i.e.,
exponentiate to get the results:
  exp(ln_orf)
exp(cilf)
exp(ciuf)
plot(model_female)
#plot graph for female results
ggplot( data=htn2_transformed_only_women, mapping=aes(x=polypharmacy,
                                                      y=abnormal_bp))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, color="red")+
  labs(title="Logistic Regression Model showing Treatment of Hypertension vs Pharmacy
Tretment Plan for Females",
       x="Polypharamcy or Non-polypharmacy Treatment",
       y="Treatment of Hypertension (overtreatment or not)")
