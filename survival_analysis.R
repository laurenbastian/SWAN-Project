##SET WORKING DIRECTORY TO SOURCE FILE LOCATION

##LIBRARIES
#install.packages("survival")
#install.pacakges("ggplot2)
#install.packages("survminer")
#install.packages("rms")
library(survival)
library(ggplot2)
library(survminer)
library(rms)


##LOAD DATA
source("build_survival_dataset_updated.R")

surv.df$race = as.factor(surv.df$race)
surv.df$race = relevel(surv.df$race, ref = "(4) Caucasian/White Non-Hispanic")
surv.df$income = as.factor(surv.df$income)
surv.df$income = relevel(surv.df$income, ref = ">100")
surv.df$age_group = as.factor(surv.df$age_group)
surv.df$insured = as.factor(surv.df$insured)
surv.df$event = as.numeric(surv.df$event)

######################################

##DESCRIPTIVE STATISTICS


##MEAN AGE AT t = 0
print("Mean Age at t = 0")
mean(surv.df$t0_age) #46.92 years at t = 0 (visit 1)

##AGE GROUP DISTRIBUTION
print("Age Group Distribution")
table(surv.df$age_group)

##INSURANCE STATUS
#6.65% of participants report no insurance during screening study
print("Insurance Status")
table(surv.df$insured)
nrow(surv.df[surv.df$insured == "No",]) / nrow(surv.df) 

##RACE
print("Race Distribution")
table(surv.df$race)

##HOUSEHOLD INCOME
print("Household Income Distribution")
table(surv.df$income)

##EVENT INCIDENCE
table(surv.df$event)


########################################

##EXPLORATORY

##FACTOR CROSS TABS
ftable(xtabs(~race + income + insured, data = surv.df))

##EVENT INCIDENCE BY RACE
xtabs(~race + event, data = surv.df)
xtabs(~race + event, data = surv.df)[,2] / 
  xtabs(~race + event, data = surv.df)[,1]
#incidence by end of study appears proportionally higher 
#for black and hispanic participants

##EVENT INCIDENCE BY INSURANCE STATUS
xtabs(~insured + event, data = surv.df)
xtabs(~insured + event, data = surv.df)[,2] / 
  xtabs(~insured + event, data = surv.df)[,1]
#incidence by end of study appears proportionally higher for uninsured

##EVENT INCIDENCE BY HOUSEHOLD INCOME
xtabs(~income + event, data = surv.df)
xtabs(~income + event, data = surv.df)[,2] / 
  xtabs(~income + event, data = surv.df)[,1]
#incidence by end of study appears proportionally higher for lowest income


######################################

##COX MODELS

surv.df$surv_obj = with(surv.df, Surv(t2event, event))

##MODEL WITH ONLY INSURANCE STRATIFIED BY AGE GROUP
dbts.mod.ins= coxph(surv_obj ~ insured + strata(age_group), 
                    data = surv.df)
dbts.mod.ins

##Assess PH Assumption
#Assumption met
cox.zph(dbts.mod.ins)
plot(cox.zph(dbts.mod.ins), 
     main = "Scaled Schoenfeld Residuals (~insured + strata(age_group)")

##BUILD MODEL WITH ALL FACTORS OF INTEREST
dbts.mod.all = coxph(surv_obj ~ insured + race + income + strata(age_group), data = surv.df)
dbts.mod.all

##Assess PH Assumption
cox.zph(dbts.mod.all)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.all))




#stratified CPH with race and income (no insurance)
dbts.mod.noinsure = coxph(surv_obj ~ race + income + strata(age_group), data = surv.df)
dbts.mod.noinsure

#LRT Chi-Sq test for models
anova(dbts.mod.noinsure, dbts.mod.all)

#assess PH assumption
cox.zph(dbts.mod.noinsure)
par(mfrow = c(1,2))
plot(cox.zph(dbts.mod.noinsure))

