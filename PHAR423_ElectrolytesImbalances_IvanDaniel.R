library(tidyverse)
library(ggplot2)
library(missForest)
library(dplyr)
library(readxl)
library(gtsummary)
setwd("D:/PHAR423")

#Reading the data set in R and replace the space bar in the column name with 
#underscore
df <- read_excel("data/Electrolyte_imbalances.xlsx") %>% 
  rename(Glucose_corrected_sodium = `Glucose corrected sodium`) %>% 
  rename(Albumin_corrected_calcium = `Albumin corrected calcium`) %>% 
  rename(Free_calcium = `Free calcium`) %>% 
  rename(Death_nr_days = `Death nr days`) %>% 
  rename(Readmission_nr_days = `Readmission nr days`) %>% 
  rename(Main_condition = `Main condition`) %>% 
  rename(Other_conditions = `Other conditions`)

#Finding the top disease among the data set and visualize the plot
mc_count_top5 <- count(df, Main_condition, sort = TRUE) %>% slice_max(
  n,n=5,with_ties = FALSE) %>% 
  mutate(Main_condition_name = case_when(
    Main_condition == "J159" ~ "Unspecified bacterial pneumonia",
    Main_condition == "R104" ~ "Other and unspecified abdominal pain",
    Main_condition == "R074" ~ "Chest pain, unspecified",
    Main_condition == "N390" ~ "Urinary tract infection",
    Main_condition == "I48" ~ "Atrial fibrillation and flutter",
  ))
ggplot(mc_count_top5, aes(x = Main_condition_name, y = n))+
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Main condition in the study", y = "Number of cases")+
  ggtitle("Top 5 main conditions in the study")+
  theme(axis.text.x = element_text(angle=90, size=5))

#Selecting the unspecified bacterial pneumonia patients for the study
df_pneu <- df %>% filter(Main_condition =="J159")

#Imputation to account for the missing value and combining thee imputed data 
#into the original data set
set.seed(2023)
Imp_df_pneu <- missForest(df_pneu %>% 
                            select(Age,Gender,SK, SNA, SGLU, SCA, SALB) %>% 
                            mutate(Gender = as.factor(Gender)) %>% 
                            as.data.frame())
Imp_df_pneu <- Imp_df_pneu[["ximp"]]
colnames(Imp_df_pneu) <- c("Age_2", "Gender_2", "SK_2", "SNA_2", 
                           "SGLU_2", "SCA_2", "SALB_2")
Imp_df_pneu <- mutate(Imp_df_pneu,GluNa=SNA_2+((SGLU_2-5.6)/5.6)*2.4,
                      AluCa=SCA_2+0.020*(40-SALB_2))
combined_df <- cbind(df_pneu, Imp_df_pneu)
head(combined_df)

#Generating patient characteristics data set and summary table
pat_df <- combined_df %>% 
  mutate("Serum potassium group" = case_when(
    SK < 3.6 ~ "Low serum potassium level",
    SK > 5 ~ "High serum potassium level",
    between(SK, 3.6, 5) ~ "Normal serum potassium level")) %>% 
  rename("Serum potassium level (mmol/L)" = `SK`) %>% 
  rename("Serum sodium level (mmol/L)" = `SNA`) %>% 
  rename("Glucose corrected sodium (mmol/L)" = `Glucose_corrected_sodium`) %>% 
  rename("Serum calcium level (mmol/L)" = `SCA`) %>% 
  rename("Serum albumin level (mmol/L)" = `SALB`) %>% 
  rename("Serum albumin corrected calcium level (mmol/L)" =
           `Albumin_corrected_calcium`) %>% 
  mutate(Gender = ifelse(Gender == "K", "F", Gender))

Pattable <-
  tbl_summary(
    pat_df,
    include = c(Age, "Serum potassium level (mmol/L)", "Serum potassium group", 
            "Serum sodium level (mmol/L)", "Glucose corrected sodium (mmol/L)", 
            "Serum calcium level (mmol/L)", "Serum albumin level (mmol/L)", 
            "Serum albumin corrected calcium level (mmol/L)"),
    by = Gender,
    missing = "no" 
  ) %>%
  add_n() %>% 
  add_p() %>% 
  modify_header(label = "**Variable of interest**") %>% 
  bold_labels()
Pattable

#Visualizing the number of patients in terms of biological sex
gender_count <- count(combined_df, Gender_2)
ggplot(gender_count, aes(x = Gender_2, y = n))+
  geom_bar(stat = "identity", fill="blue")+
  labs(x = "Sex", y = "Number of cases")+
  scale_x_discrete(labels=c("Female", 
                            "Male"))+
  ggtitle("Sex visit distribution in the study")+
  theme(axis.text.x = element_text(size=10))

#Filtering the sample by biological sex and providing summary of missing data
df_pneu_M <- filter(combined_df, Gender_2=="M") 
df_pneu_F <- filter(combined_df, Gender_2=="K")
ggplot(data=df_pneu, aes(x=Age, fill=Gender))+
  geom_histogram(binwidth = 1, position='identity', alpha=0.5) +
  labs(x="Age (Years)", y="Number of patients",
       title="Distribution of age by sex")+
  geom_vline(xintercept = 65, col='black', linetype = 'dotted') +
  scale_fill_discrete(labels=c('Female', 'Male'))
theme_classic()
MissValue <- as.data.frame(colSums(is.na(df_pneu)))

#Visualizing the distribution of serum potassium level in the whole sample in 
#box plot and histogram
ggplot(data=df_pneu,aes(y=SK,x=Gender)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Serum potassium level",
       title="Distribution of serum potassium level by sex")+
  scale_x_discrete(labels=c("Female", "Male"))

ggplot(data=df_pneu, aes(x=SK, fill=Gender))+
  geom_histogram(binwidth = 0.1, position='identity', alpha=0.5) +
  labs(x="Serum potassium level(mmol/L)", y="Number of patients",
       title="Distribution of serum potassium level by sex")+
  geom_vline(xintercept = 3.6,col='black',linetype='dotted')+
  geom_vline(xintercept = 5.0,col='black',linetype='dotted')+
  scale_fill_discrete(labels=c('Female', 'Male'))
theme_classic()

#Visualizing the distribution of electrolyte level including serum sodium and 
#albumin corrected calcium by biological sex
ggplot(data=df_pneu, aes(x=SNA, fill=Gender))+
  geom_histogram(binwidth = 1, position='identity', alpha=0.5) +
  labs(x="Serum sodium level(mmol/L)", y="Number of patients",
       title="Distribution of serum sodium level by sex")+
  geom_vline(xintercept = 136,col='black',linetype='dotted')+
  geom_vline(xintercept = 145,col='black',linetype='dotted')+
  scale_fill_discrete(labels=c('Female', 'Male'))
theme_classic()

ggplot(data=df_pneu, aes(x=Albumin_corrected_calcium, fill=Gender))+
  geom_histogram(binwidth = 0.01, position='identity', alpha=0.5) +
  labs(x="Albumin corrected calcium(mmol/L)", y="Number of patients",
       title="Distribution of Albumin corrected calcium by sex")+
  geom_vline(xintercept = 2.20,col='black',linetype='dotted')+
  geom_vline(xintercept = 2.60,col='black',linetype='dotted')+
  scale_fill_discrete(labels=c('Female', 'Male'))
theme_classic()

#Generating logistic data frame by the death status, electrolyte levels, 
#biological sex, and age group
log_df <- combined_df %>% 
  mutate(Death_nr_days = ifelse(is.na(Death_nr_days),999,Death_nr_days)) %>% 
  mutate(InhosDeath = as.numeric(Death_nr_days<=7)) %>% 
  mutate(Gender_3=ifelse(Gender=="M",1,0)) %>% 
  mutate(Age_group=ifelse(Age>=65,1,0)) %>% 
  mutate(LK = ifelse(SK_2 < 3.6, 1, 0), HK = ifelse(SK_2 > 5, 1, 0)) %>% 
  mutate(LNa = ifelse(SNA_2 < 136, 1, 0), HNa = ifelse(SNA_2 > 145, 1, 0)) %>% 
  mutate(LACa = ifelse(AluCa < 2.20, 1, 0), HACa = ifelse(AluCa > 2.60, 1, 0))

#Visualizing the 7-day mortality with respect to age and sex
ggplot(data = log_df, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5) +
  facet_wrap(~InhosDeath, labeller = labeller
             (InhosDeath = c('0' = 'Alive', '1' = 'Dead'))) +
  labs(x = "Age", y = "Number of patients",
       title = "7-day mortality separated by biological sex and age") +
  scale_fill_discrete(name = "Gender", labels = c('Female', 'Male')) +
  theme_classic()

#Calculating the portion of death
Death_count <- count(log_df, InhosDeath)
Surv <- Death_count[1,2]
Death <- Death_count[2,2]
Death/(Death+Surv)

#Calculating the portion of elderly
Age_count <- count(log_df, Age_group)
elder <- Age_count[2,2]
adult <- Age_count[1,2]
1-(elder/(elder+adult))

#Generating logistic regression model by serum potassium level 
regSK <- glm(InhosDeath~LK+HK, data=log_df, family=binomial(link="logit"))
summary(regSK)
results_logis_SK <- broom::tidy(regSK,exponentiate = T,conf.int = T)
results_logis_SK <- results_logis_SK[-1,] %>% 
  mutate(term = case_when(
    term == "LK" ~ "Low serum potassium level",
    term == "HK" ~ "High serum potassium level",
    TRUE ~ as.character(term)
  ))
ggplot(results_logis_SK,aes(y=term,x=estimate)) +
  geom_point() +
  labs(x="Estimated odd ratios", y="Variables of interest", 
       title="Odd ratios of abnormal serum potassium level (unadjusted)") +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high)) +
  xlim(0,NA) +
  geom_vline(xintercept = 1,col='black',linetype='dotted') +
  theme_minimal()

ORtable_SK <- tbl_regression(regSK, exponentiate = TRUE,
                             label = list(LK~"Low serum potassium level",
                                          HK~"High serum potassium level"
                             ))
ORtable_SK

#Generating logistic regression model by biological sex and serum potassium 
#level
regGen <- glm(InhosDeath~Gender_3+LK+HK, 
              data=log_df, family=binomial(link="logit"))
summary(regGen)
results_logis_SKGen <- broom::tidy(regGen,exponentiate = T,conf.int = T)
results_logis_SKGen <- results_logis_SKGen[-1,] %>% 
  mutate(term = case_when(
    term == "LK" ~ "Low serum potassium level",
    term == "HK" ~ "High serum potassium level",
    term == "Gender_3" ~"Male",
    TRUE ~ as.character(term)
  ))
ggplot(results_logis_SKGen,aes(y=term,x=estimate)) +
  geom_point() +
  labs(x="Estimated odd ratios", y="Variables of interest", 
  title="Odd ratios of abnormal serum potassium level and Sex (unadjusted)") +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high)) +
  xlim(0,NA) +
  geom_vline(xintercept = 1,col='black',linetype='dotted') +
  theme_minimal()

ORtable_SKGen <- tbl_regression(regGen, exponentiate = TRUE,
                                label = list(LK~"Low serum potassium level",
                                             HK~"High serum potassium level",
                                             Gender_3~"Male"
                                ))
ORtable_SKGen

#Generating logistic regression model by age groups
regAge <- glm(InhosDeath~Age_group, data=log_df, family=binomial(link="logit"))
summary(regAge)

#Adjusting the variables
logis_adj <- glm(InhosDeath~Gender_3+LK+HK+Age_group+LNa+HNa+LACa+HACa, 
                 data=log_df, family=binomial(link="logit"))
summary(logis_adj)
results_logis_adj <- broom::tidy(logis_adj,exponentiate = T,conf.int = T)
results_logis_adj <- results_logis_adj[-1,] %>% 
  mutate(term = case_when(
    term == "Gender_3" ~ "Male",
    term == "LK" ~ "Low serum potassium level",
    term == "HK" ~ "High serum potassium level",
    term == "Age_group" ~ "Age above 65",
    term == "LNa" ~ "Low serum sodium level",
    term == "HNa" ~ "High serum sodium level",
    term == "LACa" ~ "Low albumin corrected calcium",
    term == "HACa" ~ "High albumin corrected calcium",
    TRUE ~ as.character(term)
  ))

ggplot(data=results_logis_adj,aes(y=term,x=estimate)) +
  geom_point() +
  labs(x="Estimated odd ratios", y="Variables of interest", 
       title="Summary table of odd ratios") +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high)) +
  xlim(0,NA) +
  geom_vline(xintercept = 1,col='black',linetype='dotted') +
  theme_minimal()

ORtable <- tbl_regression(logis_adj, exponentiate = TRUE,
                          label = list(Gender_3~"Male",
                                       LK ~ "Low serum potassium level",
                                       HK ~ "High serum potassium level",
                                       Age_group ~ "Age above 65",
                                       LNa ~ "Low serum sodium level",
                                       HNa ~ "High serum sodium level",
                                       LACa ~ "Low albumin corrected calcium",
                                       HACa ~ "High albumin corrected calcium"))
ORtable

#Creating the flow chart of filtering process
library(DiagrammeR)
Totalcount <- as.data.frame(count(df))
total <- Totalcount[1,1]
dfcount <- as.data.frame(count(df_pneu))
dfpat <- dfcount[1,1]
SKcount <- pat_df %>%
  group_by(`Serum potassium group`) %>%
  summarise(count = n())
SKcount <- as.data.frame(SKcount)
low <- SKcount[2,2]
high <- SKcount[1,2]
normal <- SKcount[3,2]
data <- list(a=total, b=dfpat, c=low, d=normal, e=high)

DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 1, fillcolor = Biege]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']
e [label = '@@5']

a -> b
b -> c
b -> d 
b -> e 
}

[1]: paste0('All ED Visit to Diakonhjemmet Hospital (n = ', data$a, ')')
[2]: paste0('Patients with main ICD-10 discharge condition of unspecified pneumonia(n = ', data$b, ')')
[3]: paste0('Low Potassium (n = ', data$c, ')')
[4]: paste0('Normal Potassium (n = ', data$d, ')')
[5]: paste0('High Potassium (n = ', data$e, ')')
")