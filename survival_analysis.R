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

surv.comp = surv.df[complete.cases(surv.df),]

##MEAN AGE AT t = 0
print("Mean Age at t = 0")
mean(surv.comp$t0_age) #46.92 years at t = 0 (visit 1)

##AGE GROUP DISTRIBUTION
print("Age Group Distribution")
table(surv.comp$age_group)

##INSURANCE STATUS
#6.65% of participants report no insurance during screening study
print("Insurance Status")
table(surv.comp$insured)
nrow(surv.comp[surv.comp$insured == "No",]) / nrow(surv.comp) 

##RACE
print("Race Distribution")
table(surv.comp$race)

##HOUSEHOLD INCOME
print("Household Income Distribution")
table(surv.comp$income)

##OVERALL EVENT INCIDENCE DURING STUDY
table(surv.comp$event)

##FACTOR CROSS TABS
ftable(xtabs(~race + income + insured, data = surv.comp))

##EVENT INCIDENCE BY RACE
xtabs(~race + event, data = surv.comp)
xtabs(~race + event, data = surv.comp)[,2] / 
  xtabs(~race + event, data = surv.comp)[,1]

##EVENT INCIDENCE BY INSURANCE STATUS
xtabs(~insured + event, data = surv.comp)
xtabs(~insured + event, data = surv.comp)[,2] / 
  xtabs(~insured + event, data = surv.comp)[,1]

##EVENT INCIDENCE BY HOUSEHOLD INCOME
xtabs(~income + event, data = surv.comp)
xtabs(~income + event, data = surv.comp)[,2] / 
  xtabs(~income + event, data = surv.comp)[,1]

surv.comp$surv_obj = with(surv.comp, Surv(t2event, event))

######################################

##STRATIFIED COX MODEL WITH INSURANCE STRATIFIED BY AGE GROUP
#BUILD MODEL
dbts.mod.ins= coxph(surv_obj ~ insured + strata(age_group), 
                    data = surv.comp)
dbts.mod.ins

##PH ASSUMPTION
cox.zph(dbts.mod.ins)
plot(cox.zph(dbts.mod.ins), 
     main = "Scaled Schoenfeld Residuals (~insured + strata(age_group)")

##NO INTERACTION ASSUMPTION
#The coefficients do not appear very different
coxph(surv_obj ~ insured, 
      data = surv.comp[surv.comp$age_group == "42_45",])
coxph(surv_obj ~ insured, 
      data = surv.comp[surv.comp$age_group == "46_49",])
coxph(surv_obj ~ insured, 
      data = surv.comp[surv.comp$age_group == "50_54",])

#stratified interaction model w/o main effect of age group
dbts.mod.interaction = coxph(surv_obj ~ insured*age_group - age_group + strata(age_group),
      data = surv.comp)
dbts.mod.interaction

#interaction not significant
anova(dbts.mod.interaction, dbts.mod.ins)

#################################

##STRATIFIED COX MODEL WITH INSURANCE, RACE, AND INCOME, STRATIFIED BY AGE
#BUILD MODEL
dbts.mod.all = coxph(surv_obj ~ insured + race + income + strata(age_group), data = surv.comp)
dbts.mod.all

#PH ASSUMPTION
cox.zph(dbts.mod.all)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.all))

#NO INTERACTION ASSUMPTION
coxph(surv_obj ~ insured + race + income, data = surv.comp[surv.comp$age_group == "42_45",])
coxph(surv_obj ~ insured + race + income, data = surv.comp[surv.comp$age_group == "46_49",])
coxph(surv_obj ~ insured + race + income, data = surv.comp[surv.comp$age_group == "50_54",])

dbts.mod.all.interaction = coxph(surv_obj ~ (insured + race + income)*age_group - age_group + strata(age_group), data = surv.comp)
dbts.mod.all.interaction

#chi-squared test for significance of interaction
anova(dbts.mod.all.interaction, dbts.mod.all)


#######################################

##STRATIFIED COX MODEL WITH RACE AND INCOME, AND STRATIFIED BY AGE
#BUILD MODEL
dbts.mod.noinsure = coxph(surv_obj ~ race + income + strata(age_group), data = surv.comp)
dbts.mod.noinsure

#LRT Chi-Sq test for models
anova(dbts.mod.noinsure, dbts.mod.all)

#PH ASSUMPTION
cox.zph(dbts.mod.noinsure)
par(mfrow = c(1,2))
plot(cox.zph(dbts.mod.noinsure))

#NO INTERACTION ASSUMPTION
coxph(surv_obj ~ race + income, data = surv.comp[surv.comp$age_group == "42_45",])
coxph(surv_obj ~ race + income, data = surv.comp[surv.comp$age_group == "46_49",])
coxph(surv_obj ~ race + income, data = surv.comp[surv.comp$age_group == "50_54",])

dbts.mod.noinsure.interaction = coxph(surv_obj ~ (race + income)*age_group - age_group + strata(age_group), data = surv.comp)
dbts.mod.noinsure.interaction

#Chi-Squared test of deviance
anova(dbts.mod.noinsure.interaction, dbts.mod.noinsure)

#MODEL SUMMARY
dbts.mod.noinsure

