## ----setup, include=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries and dataset, include = FALSE, fig.show = "hide", results = "hide"----------
##SET WORKING DIRECTORY TO SOURCE FILE LOCATION

##LIBRARIES
#install.packages("survival")
#install.pacakges("ggplot2)
#install.packages("survminer")
#install.packages("rms")
#install.packages("gmodels")
library(survival)
library(ggplot2)
library(survminer)
library(rms)
library(gmodels)

##LOAD DATA
source("build_survival_dataset_updated.R")

surv.df$race = as.factor(surv.df$race)
levels(surv.df$race) = c("Black/African American",
                         "Chinese/Chinese American",
                         "Japanese/Japanese American",
                         "Non-Hispanic White/Caucausian",
                         "Hispanic")

surv.df$income = as.factor(surv.df$income)
surv.df$income = factor(surv.df$income, 
                        levels = c("<20","20-49","50-99","100+"))

surv.df$age_group = as.factor(surv.df$age_group)

surv.df$bmi_group = as.factor(surv.df$bmi_group)

surv.df$insured = as.factor(surv.df$insured)

surv.df$event = as.numeric(surv.df$event)



## ----Create surv object in surv.df , echo = FALSE-----------------------------------------
surv.df$surv_obj = with(surv.df, Surv(t2event, event))


## ----descriptives, echo = FALSE-----------------------------------------------------------
##MEAN AGE AT t = 0
mean(surv.df$t0_age) 


## ---- echo = FALSE------------------------------------------------------------------------
##AGE GROUP DISTRIBUTION
table(surv.df$age_group)


## ---- echo = FALSE------------------------------------------------------------------------
##INSURANCE STATUS
#6.65% of participants report no insurance during screening study
table(surv.df$insured)


## ---- echo = FALSE------------------------------------------------------------------------
nrow(surv.df[surv.df$insured == "No",]) / nrow(surv.df) 


## ---- echo = FALSE------------------------------------------------------------------------
##RACE
table(surv.df$race)


## ---- echo = FALSE------------------------------------------------------------------------
##HOUSEHOLD INCOME
table(surv.df$income)


## ---- echo = FALSE------------------------------------------------------------------------
hist(surv.df$t0_bmi, main = "Baseline BMI (Continuous)", xlab = "BMI")


## ---- echo = FALSE------------------------------------------------------------------------
table(surv.df$bmi_group)


## ---- echo = FALSE------------------------------------------------------------------------
##OVERALL EVENT INCIDENCE DURING STUDY
table(surv.df$event)
274/2412


## ---- echo = FALSE------------------------------------------------------------------------
##FACTOR CROSS TABS
ftable(xtabs(~race + income, data = surv.df))

CrossTable(surv.df$race, surv.df$income,
           prop.chisq = FALSE,
           format = "SPSS",
           digits = 2)


## ---- echo = FALSE------------------------------------------------------------------------
##FACTOR CROSS TABS
ftable(xtabs(~race + insured, data = surv.df))

CrossTable(surv.df$race, surv.df$insured,
           prop.chisq = FALSE,
           format = "SPSS",
           digits = 2)


## ---- echo = FALSE------------------------------------------------------------------------
ftable(xtabs(~race + bmi_group, data = surv.df))

CrossTable(surv.df$race, surv.df$bmi_group,
           prop.chisq = FALSE,
           format = "SPSS",
           digits = 2)


## ---- echo = FALSE------------------------------------------------------------------------
##EVENT INCIDENCE BY RACE
xtabs(~race + event, data = surv.df)


## ---- echo = FALSE------------------------------------------------------------------------
xtabs(~race + event, data = surv.df)[,2] / 
  xtabs(~race + event, data = surv.df)[,1]


## ---- echo = FALSE------------------------------------------------------------------------
##EVENT INCIDENCE BY INSURANCE STATUS
xtabs(~insured + event, data = surv.df)


## ---- echo = FALSE------------------------------------------------------------------------
xtabs(~insured + event, data = surv.df)[,2] / 
  xtabs(~insured + event, data = surv.df)[,1]


## ---- echo = FALSE------------------------------------------------------------------------
##EVENT INCIDENCE BY HOUSEHOLD INCOME
xtabs(~income + event, data = surv.df)


## ---- echo = FALSE------------------------------------------------------------------------
xtabs(~income + event, data = surv.df)[,2] / 
  xtabs(~income + event, data = surv.df)[,1]


## ---- echo =  FALSE-----------------------------------------------------------------------
xtabs(~bmi_group + event, data = surv.df)


## ---- echo = FALSE------------------------------------------------------------------------
xtabs(~bmi_group + event, data = surv.df)[,2] / 
  xtabs(~bmi_group + event, data = surv.df)[,1]


## ---- echo = FALSE------------------------------------------------------------------------
surv.df$insured = relevel(surv.df$insured, ref = "No")
levels(surv.df$insured)

surv.df$race = relevel(surv.df$race, ref = "Non-Hispanic White/Caucausian")
levels(surv.df$race)

surv.df$bmi_group = relevel(surv.df$bmi_group, ref = "<30")
levels(surv.df$bmi_group)

surv.df$income = relevel(surv.df$income, ref = "50-99")
levels(surv.df$income)


## ----insurance only, echo = FALSE---------------------------------------------------------
##STRATIFIED COX MODEL WITH INSURANCE STRATIFIED BY AGE GROUP

#BUILD MODEL
dbts.mod.ins= coxph(surv_obj ~ insured + strata(age_group), 
                    data = surv.df)
dbts.mod.ins



## ---- echo = FALSE------------------------------------------------------------------------

##PH ASSUMPTION
cox.zph(dbts.mod.ins)
plot(cox.zph(dbts.mod.ins), 
     main = "Scaled Schoenfeld Residuals (~insured + strata(age_group)")


## ---- echo = FALSE------------------------------------------------------------------------
##NO INTERACTION ASSUMPTION
#The coefficients do not appear very different
coef(coxph(surv_obj ~ insured, 
      data = surv.df[surv.df$age_group == "42_45",]))
coef(coxph(surv_obj ~ insured, 
      data = surv.df[surv.df$age_group == "46_49",]))
coef(coxph(surv_obj ~ insured, 
      data = surv.df[surv.df$age_group == "50_53",]))

#stratified interaction model w/o main effect of age group
dbts.mod.interaction = coxph(surv_obj ~ insured*age_group - age_group + strata(age_group),
      data = surv.df)
dbts.mod.interaction

#interaction not significant
anova(dbts.mod.interaction, dbts.mod.ins)



## ----all factors, echo = FALSE------------------------------------------------------------
##STRATIFIED COX MODEL WITH INSURANCE, RACE, INCOME, and BMI group STRATIFIED BY AGE
#BUILD MODEL
dbts.mod.all = coxph(surv_obj ~ insured + race + income + bmi_group + strata(age_group), data = surv.df)
dbts.mod.all


## ---- echo = FALSE------------------------------------------------------------------------
#PH ASSUMPTION
cox.zph(dbts.mod.all)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.all))


## ---- echo = FALSE------------------------------------------------------------------------
#NO INTERACTION ASSUMPTION
coef(coxph(surv_obj ~ insured + race + income + bmi_group, 
      data =surv.df[surv.df$age_group == "42_45",]))
coef(coxph(surv_obj ~ insured + race + income + bmi_group, 
      data = surv.df[surv.df$age_group == "46_49",]))
coef(coxph(surv_obj ~ insured + race + income + bmi_group, 
      data = surv.df[surv.df$age_group == "50_53",]))

dbts.mod.all.interaction = coxph(surv_obj ~ (insured + race + income + bmi_group)*age_group - age_group + strata(age_group), data = surv.df)

#chi-squared test for significance of interaction
anova(dbts.mod.all.interaction, dbts.mod.all)


## ----reduced model, echo = FALSE----------------------------------------------------------
##STRATIFIED COX MODEL WITH RACE, BMI, AND INCOME, AND STRATIFIED BY AGE
#BUILD MODEL
dbts.mod.noinsure = coxph(surv_obj ~ race + income + bmi_group + strata(age_group), data = surv.df)
dbts.mod.noinsure

#LRT Chi-Sq test for models
anova(dbts.mod.noinsure, dbts.mod.all)


## ---- echo = FALSE------------------------------------------------------------------------
#PH ASSUMPTION
cox.zph(dbts.mod.noinsure)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.noinsure))


## ---- echo = FALSE------------------------------------------------------------------------
#NO INTERACTION ASSUMPTION
coef(coxph(surv_obj ~ race + income + bmi_group, data = surv.df[surv.df$age_group == "42_45",]))
coef(coxph(surv_obj ~ race + income + bmi_group, data = surv.df[surv.df$age_group == "46_49",]))
coef(coxph(surv_obj ~ race + income + bmi_group, data = surv.df[surv.df$age_group == "50_53",]))

dbts.mod.noinsure.interaction = coxph(surv_obj ~ (race + income + bmi_group)*age_group - age_group + strata(age_group), data = surv.df)

#Chi-Squared test of deviance
anova(dbts.mod.noinsure.interaction, dbts.mod.noinsure)



## ----selected model summary, echo = FALSE-------------------------------------------------
#MODEL SUMMARY
dbts.mod.noinsure


## ---- echo = FALSE------------------------------------------------------------------------
coxph(surv_obj ~ insured + race*income + bmi_group + strata(age_group), data = surv.df)


## ----42_45, echo = FALSE------------------------------------------------------------------
km42_45 = survfit(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="42_45",])
ggsurvplot(km42_45, 
           data = surv.df[surv.df$age_group=="42_45",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Diabetes Survival (42-45 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3)

survdiff(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="42_45",])


## ----46_49, echo = FALSE------------------------------------------------------------------
km46_49 = survfit(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="46_49",])
ggsurvplot(km46_49, 
           data = surv.df[surv.df$age_group=="46_49",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Diabetes Survival (46-49 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3)
survdiff(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="46_49",])


## ----50_53, echo = FALSE------------------------------------------------------------------
km50_53 = survfit(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="50_53",])
ggsurvplot(km50_53, 
           data = surv.df[surv.df$age_group=="50_53",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Diabetes Survival (50-53 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3)

survdiff(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="50_53",])

