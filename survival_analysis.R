## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----setup images, include = FALSE----------------------------------
knitr::opts_chunk$set(dev = "png",
                      dpi = 300,
                      echo = FALSE,
                      cache = TRUE)


## ----libraries and dataset, include = FALSE, fig.show = "hide", results = "hide"----
##SET WORKING DIRECTORY TO SOURCE FILE LOCATION

##LIBRARIES
#install.packages("survival")
#install.pacakges("ggplot2)
#install.packages("survminer")
#install.packages("rms")
#install.packages("gmodels")
#install.packages("lsr")
library(survival)
library(ggplot2)
library(survminer)
library(rms)
library(gmodels)
library(lsr)

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

##Create surv object in surv.df
surv.df$surv_obj = with(surv.df, Surv(t2event, event))

attach(surv.df)



## ----Hist_t2event_IncidentDiabetes, echo = FALSE--------------------
windowsFonts(Times=windowsFont("TT Times New Roman"))

event.hist = ggplot(data = surv.df[surv.df$censored == 0,], aes(x = t2event)) +
  geom_histogram(binwidth = 1, fill = '#00BFC4', color = 'white') +
  theme_bw() +
  labs(title = "Time to Event for Individuals w/ Incident Diabetes") +
  xlab("Time to Event") +
  ylab("Counts") +
  geom_vline(xintercept = mean(surv.df[surv.df$censored == 0,]$t2event), size = 1, linetype = "longdash") +
  theme(text = element_text(family = "Times"))

event.hist

## -------------------------------------------------------------------
mean(surv.df[surv.df$censored == 0,]$t2event)
sd(surv.df[surv.df$censored == 0,]$t2event)


## ----Hist_Age_IncidentDiabetes, echo = FALSE------------------------
windowsFonts(Times=windowsFont("TT Times New Roman"))

event.hist = ggplot(data = surv.df[surv.df$censored == 0,], aes(x = t0_age + t2event)) +
  geom_histogram(binwidth = 1, fill = '#00BFC4', color = 'white') +
  theme_bw() +
  labs(title = "Age of Individuals w/ Incident Diabetes") +
  xlab("Age") +
  ylab("Counts") +
  geom_vline(xintercept = mean(surv.df[surv.df$censored == 0,]$t2event + surv.df[surv.df$censored == 0,]$t0_age), size = 1, linetype = "longdash") +
  theme(text = element_text(family = "Times"))

event.hist


## -------------------------------------------------------------------
mean(surv.df[surv.df$censored == 0,]$t0_age + surv.df[surv.df$censored == 0,]$t2event)
sd(surv.df[surv.df$censored == 0,]$t0_age + surv.df[surv.df$censored == 0,]$t2event)


## ----Hist_t2event_allsubjects, echo = FALSE-------------------------
all.subjects = surv.df$t2event
event.subjects = surv.df$t2event[surv.df$censored == 0]
all.subjects = data.frame("t2event" = all.subjects, "subset" = rep("All Subjects", 2686))
event.subjects = data.frame("t2event" = event.subjects, "subset" = rep("Event Occured", 274))
hist.data = rbind(all.subjects, event.subjects)


ggplot(data = hist.data, aes(x = t2event, fill = subset)) +
  geom_histogram(position = 'identity', binwidth = 1, alpha = 1, color = 'white') +
  theme_bw() +
  labs(title = "Time to Incident Diabetes for Censored versus All Individuals") +
  xlab("Time to Event") +
  ylab("Counts") +
  theme(text = element_text(family = "Times"), legend.title=element_blank()) +
  scale_fill_manual(values=c("gray85", "#00BFC4"),
                       labels=c("All Individuals", "Uncensored"))



## ----descriptives, echo = FALSE-------------------------------------
##MEAN AGE AT t = 0
mean(surv.df$t0_age)


## ---- echo = FALSE--------------------------------------------------
##AGE GROUP DISTRIBUTION
table(surv.df$age_group)


## ---- echo = FALSE--------------------------------------------------
##INSURANCE STATUS
#6.65% of participants report no insurance during screening study
table(surv.df$insured)


## ---- echo = FALSE--------------------------------------------------
nrow(surv.df[surv.df$insured == "No",]) / nrow(surv.df) 


## ---- echo = FALSE--------------------------------------------------
##RACE
table(surv.df$race)


## ---- echo = FALSE--------------------------------------------------
##HOUSEHOLD INCOME
table(surv.df$income)


## ----Hist_BaselineBMI, echo = FALSE---------------------------------
cutoffs = c(30, 40)

ggplot(surv.df, aes(x = t0_bmi)) +
  geom_histogram(binwidth = 1, fill = '#00BFC4', color = 'white') +
  theme_bw() +
  labs(title = "Baseline BMI") +
  xlab("BMI") +
  ylab("Counts") +
  geom_vline(xintercept = cutoffs, size = 1, linetype = "longdash") +
  theme(text = element_text(family = "Times"))



## ---- echo = FALSE--------------------------------------------------
table(surv.df$bmi_group)


## ---- echo = FALSE--------------------------------------------------
##OVERALL EVENT INCIDENCE DURING STUDY
table(surv.df$event)
274/2412


## ---- echo = FALSE--------------------------------------------------
##FACTOR CROSS TABS
ftable(xtabs(~race + income, data = surv.df))

CrossTable(surv.df$race, surv.df$income,
           prop.chisq = FALSE,
           format = "SPSS",
           digits = 2)


## ---- echo = FALSE--------------------------------------------------
##FACTOR CROSS TABS
ftable(xtabs(~race + insured, data = surv.df))

CrossTable(surv.df$race, surv.df$insured,
           prop.chisq = FALSE,
           format = "SPSS",
           digits = 2)


## ---- echo = FALSE--------------------------------------------------
ftable(xtabs(~race + bmi_group, data = surv.df))

CrossTable(surv.df$race, surv.df$bmi_group,
           prop.chisq = FALSE,
           format = "SPSS",
           digits = 2)


## ---- echo = FALSE--------------------------------------------------
#Incident Diabetes BY RACE
xtabs(~race + event, data = surv.df)


## ---- echo = FALSE--------------------------------------------------
xtabs(~race + event, data = surv.df)[,2] / 
  xtabs(~race + event, data = surv.df)[,1]


## ---- echo = FALSE--------------------------------------------------
##EVENT INCIDENCE BY INSURANCE STATUS
xtabs(~insured + event, data = surv.df)


## ---- echo = FALSE--------------------------------------------------
xtabs(~insured + event, data = surv.df)[,2] / 
  xtabs(~insured + event, data = surv.df)[,1]


## ---- echo = FALSE--------------------------------------------------
##EVENT INCIDENCE BY HOUSEHOLD INCOME
xtabs(~income + event, data = surv.df)


## ---- echo = FALSE--------------------------------------------------
xtabs(~income + event, data = surv.df)[,2] / 
  xtabs(~income + event, data = surv.df)[,1]


## ---- echo =  FALSE-------------------------------------------------
xtabs(~bmi_group + event, data = surv.df)


## ---- echo = FALSE--------------------------------------------------
xtabs(~bmi_group + event, data = surv.df)[,2] / 
  xtabs(~bmi_group + event, data = surv.df)[,1]


## ---- echo = FALSE--------------------------------------------------
surv.df$insured = relevel(surv.df$insured, ref = "No")
levels(surv.df$insured)

surv.df$race = relevel(surv.df$race, ref = "Non-Hispanic White/Caucausian")
levels(surv.df$race)

surv.df$bmi_group = relevel(surv.df$bmi_group, ref = "<30")
levels(surv.df$bmi_group)

surv.df$income = relevel(surv.df$income, ref = "50-99")
levels(surv.df$income)


## -------------------------------------------------------------------
table(surv.df$race, surv.df$insured)
chisq.test(table(surv.df$race, surv.df$insured))

## -------------------------------------------------------------------
cramersV(table(surv.df$race, surv.df$insured))


## -------------------------------------------------------------------
table(surv.df$race, surv.df$income)
chisq.test(table(surv.df$race, surv.df$income))

## -------------------------------------------------------------------
cramersV(table(surv.df$race, surv.df$income))


## -------------------------------------------------------------------
table(surv.df$race, surv.df$bmi_group)
chisq.test(table(surv.df$race, surv.df$bmi_group))

## -------------------------------------------------------------------
cramersV(table(surv.df$race, surv.df$bmi_group))


## -------------------------------------------------------------------
table(surv.df$insured, surv.df$income)
chisq.test(table(surv.df$insured, surv.df$income))


## -------------------------------------------------------------------
cramersV(table(surv.df$insured, surv.df$income))


## -------------------------------------------------------------------
table(surv.df$insured, surv.df$bmi_group)
chisq.test(table(surv.df$insured, surv.df$bmi_group))


## -------------------------------------------------------------------
cramersV(table(surv.df$insured, surv.df$bmi_group))


## -------------------------------------------------------------------
table(surv.df$income, surv.df$bmi_group)
chisq.test(table(surv.df$income, surv.df$bmi_group))

## -------------------------------------------------------------------
cramersV(table(surv.df$income, surv.df$bmi_group))


## ----insurance only, echo = FALSE-----------------------------------
##STRATIFIED COX MODEL WITH INSURANCE STRATIFIED BY AGE GROUP

#BUILD MODEL
dbts.mod.ins= coxph(surv_obj ~ insured + strata(age_group), 
                    data = surv.df)
dbts.mod.ins



## ----SchoenResid_insured, echo = FALSE------------------------------
##PH ASSUMPTION
cox.zph(dbts.mod.ins)
plot(cox.zph(dbts.mod.ins), 
     main = "Scaled Schoenfeld Residuals (~insured + strata(age_group)")


## ---- echo = FALSE--------------------------------------------------
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



## ----all factors, echo = FALSE--------------------------------------
##STRATIFIED COX MODEL WITH INSURANCE, RACE, INCOME, and BMI group STRATIFIED BY AGE
#BUILD MODEL
dbts.mod.all = coxph(surv_obj ~ insured + race + income + bmi_group + strata(age_group), data = surv.df)
dbts.mod.all


## ----SchoenResid_insured_race_income_bmi, echo = FALSE--------------
#PH ASSUMPTION
cox.zph(dbts.mod.all)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.all))


## ---- echo = FALSE--------------------------------------------------
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


## ----reduced model, echo = FALSE------------------------------------
##STRATIFIED COX MODEL WITH RACE, BMI, AND INCOME, AND STRATIFIED BY AGE
#BUILD MODEL
dbts.mod.noinsure = coxph(surv_obj ~ race + income + bmi_group + strata(age_group), data = surv.df)
dbts.mod.noinsure

#LRT Chi-Sq test for models
anova(dbts.mod.noinsure, dbts.mod.all)


## ----ShoenResid_race_income_bmi, echo = FALSE-----------------------
#PH ASSUMPTION
cox.zph(dbts.mod.noinsure)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.noinsure))


## ---- echo = FALSE--------------------------------------------------
#NO INTERACTION ASSUMPTION
coef(coxph(surv_obj ~ race + income + bmi_group, data = surv.df[surv.df$age_group == "42_45",]))
coef(coxph(surv_obj ~ race + income + bmi_group, data = surv.df[surv.df$age_group == "46_49",]))
coef(coxph(surv_obj ~ race + income + bmi_group, data = surv.df[surv.df$age_group == "50_53",]))

dbts.mod.noinsure.interaction = coxph(surv_obj ~ (race + income + bmi_group)*age_group - age_group + strata(age_group), data = surv.df)

#Chi-Squared test of deviance
anova(dbts.mod.noinsure.interaction, dbts.mod.noinsure)



## ----selected model summary, echo = FALSE---------------------------
#MODEL SUMMARY
dbts.mod.noinsure


## ---- echo = FALSE--------------------------------------------------
coxph(surv_obj ~ insured + race + strata(age_group), data = surv.df)

cox.zph(coxph(surv_obj ~ insured + race + strata(age_group), data = surv.df))


## ---- echo = FALSE--------------------------------------------------
coxph(surv_obj ~ insured + income + strata(age_group), data = surv.df)
cox.zph(coxph(surv_obj ~ insured + income + strata(age_group), data = surv.df))


## ---- echo = FALSE--------------------------------------------------
coxph(surv_obj ~ insured + strata(income) + strata(age_group), data = surv.df)
cox.zph(coxph(surv_obj ~ insured + strata(income) + strata(age_group), data = surv.df))



## ---- echo = FALSE--------------------------------------------------
coxph(surv_obj ~ insured + bmi_group + strata(age_group), data = surv.df)
cox.zph(coxph(surv_obj ~ insured + bmi_group + strata(age_group), data = surv.df))


## ---- echo = FALSE--------------------------------------------------
dbts.mod.noincome = coxph(surv_obj ~ insured + bmi_group + race + strata(age_group), data = surv.df)

summary(dbts.mod.noincome)


## ----ShoenResid_insured_bmi_race, echo = FALSE----------------------
#PH ASSUMPTION
cox.zph(dbts.mod.noincome)
par(mfrow = c(2,2))
plot(cox.zph(dbts.mod.noincome))


## ---- echo = FALSE--------------------------------------------------
#NO INTERACTION ASSUMPTION
dbts.mod.noincome.interaction = coxph(surv_obj ~ (insured + race + bmi_group)*age_group - age_group + strata(age_group), data = surv.df)

#Chi-Squared test of deviance
anova(dbts.mod.noincome.interaction, dbts.mod.noincome)


## ----SurvCurve_42_45_insurance, echo = FALSE------------------------
km42_45 = survfit(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="42_45",])
ggsurvplot(km42_45, 
           data = surv.df[surv.df$age_group=="42_45",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (42-45 Age Group)",
           ylim = c(0.6, 1.0),
           legend.title = "Insurance Status",
           legend.labs = c("Uninsured", "Insured"),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           ggtheme = theme_classic2())

survdiff(surv_obj ~ insured, 
         data = surv.df[surv.df$age_group=="42_45",])




## ----SurvCurve_46_49_insurance, echo = FALSE------------------------
km46_49 = survfit(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="46_49",])
ggsurvplot(km46_49, 
           data = surv.df[surv.df$age_group=="46_49",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (46-49 Age Group)",
           ylim = c(0.6, 1.0),
           legend.title = "Insurance Status",
           legend.labs = c("Uninsured", "Insured"),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           ggtheme = theme_classic2(base_family = "Times"))

survdiff(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="46_49",])


## ----SurvCurve_50_53_insurance, echo = FALSE------------------------
km50_53 = survfit(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="50_53",])
ggsurvplot(km50_53, 
           data = surv.df[surv.df$age_group=="50_53",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (50-53 Age Group)",
           ylim = c(0.6, 1.0),
           legend.title = "Insurance Status",
           legend.labs = c("Uninsured", "Insured"),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           ggtheme = theme_classic2())

survdiff(surv_obj ~ insured, 
                  data = surv.df[surv.df$age_group=="50_53",])


## ----SurvCurve_42_45_race, echo = FALSE-----------------------------
km42_45 = survfit(surv_obj ~ race, 
                  data = surv.df[surv.df$age_group=="42_45",])
ggsurvplot(km42_45, 
           data = surv.df[surv.df$age_group=="42_45",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (42-45 Age Group)",
           ylim = c(0.6, 1.0),
           legend.title = "Race",
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.labs = c("White", "Black/African American", "Chinese/Chinese American", "Japanese/Japanese American", "Hispanic"),
           ggtheme = theme_classic2())



## -------------------------------------------------------------------
survdiff(surv_obj ~ race, 
         data = surv.df[surv.df$age_group=="42_45",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ race, 
         data = surv.df[surv.df$age_group=="42_45",])


## ----SurvCurve_46_49_race, echo = FALSE-----------------------------
km46_49 = survfit(surv_obj ~ race, 
                  data = surv.df[surv.df$age_group=="46_49",])
ggsurvplot(km46_49, 
           data = surv.df[surv.df$age_group=="46_49",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (46-49 Age Group)",
           ylim = c(0.6, 1.0),
           legend.title = "Race",
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.labs = c("White", "Black/African American", "Chinese/Chinese American", "Japanese/Japanese American", "Hispanic"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ race, 
         data = surv.df[surv.df$age_group=="46_49",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ race, 
         data = surv.df[surv.df$age_group=="46_49",])


## ----SurvCurve_50_53_race, echo = FALSE-----------------------------
km50_53 = survfit(surv_obj ~ race, 
                  data = surv.df[surv.df$age_group=="50_53",])
ggsurvplot(km50_53, 
           data = surv.df[surv.df$age_group=="50_53",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (50-53 Age Group)",
           ylim = c(0.6, 1.0),
           legend.title = "Race",
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.labs = c("White", "Black/African American", "Chinese/Chinese American", "Japanese/Japanese American", "Hispanic"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ race, 
         data = surv.df[surv.df$age_group=="50_53",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ race, 
         data = surv.df[surv.df$age_group=="50_53",])


## ----SurvCurve_42_45_income, echo = FALSE---------------------------

##relevel for plotting
surv.df$income = factor(surv.df$income, levels = c("<20", "20-49", "50-99", "100+"))

km42_45 = survfit(surv_obj ~ income, 
                  data = surv.df[surv.df$age_group=="42_45",])
ggsurvplot(km42_45, 
           data = surv.df[surv.df$age_group=="42_45",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (42-45 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.title = "Income in thousands of dollars",
           legend.labs = c("<20", "20-49", "50-99", "100+"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ income, 
         data = surv.df[surv.df$age_group=="42_45",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ income, 
         data = surv.df[surv.df$age_group=="42_45",])


## ----SurvCurve_46_49_income, echo = FALSE---------------------------
km46_49 = survfit(surv_obj ~ income, 
                  data = surv.df[surv.df$age_group=="46_49",])
ggsurvplot(km46_49, 
           data = surv.df[surv.df$age_group=="46_49",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (46-49 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.title = "Income in thousands of dollars",
           legend.labs = c("<20", "20-49", "50-99", "100+"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ income, 
         data = surv.df[surv.df$age_group=="46_49",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ income, 
         data = surv.df[surv.df$age_group=="46_49",])


## ----SurvCurve_50_53_income, echo = FALSE---------------------------
km50_53 = survfit(surv_obj ~ income, 
                  data = surv.df[surv.df$age_group=="50_53",])
ggsurvplot(km50_53, 
           data = surv.df[surv.df$age_group=="50_53",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (50-53 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.title = "Income in thousands of dollars",
           legend.labs = c("<20", "20-49", "50-99", "100+"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ income, 
         data = surv.df[surv.df$age_group=="50_53",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ income, 
         data = surv.df[surv.df$age_group=="50_53",],
         p.adjust.method = "bonferroni")


## ----SurvCurve_42_45_bmi, echo = FALSE------------------------------

km42_45 = survfit(surv_obj ~ bmi_group, 
                  data = surv.df[surv.df$age_group=="42_45",])
ggsurvplot(km42_45, 
           data = surv.df[surv.df$age_group=="42_45",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (42-45 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.title = "BMI Group",
           legend.labs = c("<30", "30_40", "40+"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ bmi_group, 
         data = surv.df[surv.df$age_group=="42_45",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ bmi_group, 
         data = surv.df[surv.df$age_group=="42_45",])


## ----SurvCurve_46_49_bmi, echo = FALSE------------------------------
km46_49 = survfit(surv_obj ~ bmi_group, 
                  data = surv.df[surv.df$age_group=="46_49",])
ggsurvplot(km46_49, 
           data = surv.df[surv.df$age_group=="46_49",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (46-49 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.title = "BMI Group",
           legend.labs = c("<30", "30_40", "40+"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ bmi_group, 
         data = surv.df[surv.df$age_group=="46_49",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ bmi_group, 
         data = surv.df[surv.df$age_group=="46_49",])


## ----SurvCurve_50_53_bmi, echo = FALSE------------------------------
km50_53 = survfit(surv_obj ~ bmi_group, 
                  data = surv.df[surv.df$age_group=="50_53",])
ggsurvplot(km50_53, 
           data = surv.df[surv.df$age_group=="50_53",], 
           conf.int=FALSE, 
           xlab = "Time (t)", 
           ylab = "Survival Probability", 
           title = "Survival Function for Time to Diabetes (50-53 Age Group)",
           ylim = c(0.6, 1.0),
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 1,
           fontsize = 3,
           legend = "top",
           legend.title = "BMI Group",
           legend.labs = c("<30", "30_40", "40+"),
           ggtheme = theme_classic2())


## -------------------------------------------------------------------
survdiff(surv_obj ~ bmi_group, 
         data = surv.df[surv.df$age_group=="50_53",])


## -------------------------------------------------------------------
pairwise_survdiff(surv_obj ~ bmi_group, 
         data = surv.df[surv.df$age_group=="50_53",])

