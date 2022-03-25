##SET WORKING DIRECTORY TO SOURCE FILE LOCATION

##LIBRARIES
library(survival)
library(survminer)

##LOAD DATA
source("build_survival_dataset.R")

##KM CURVES
##Right Censor Data to Age 50
surv.df$event = as.numeric(surv.df$event)
surv.df.rc = surv.df[surv.df$start <=50 & surv.df$end > 50,]
surv.df.rc$start = 50

windowsFonts(A = windowsFont("Times New Roman"))

surv.obj = Surv(time = surv.df.rc$start, 
                time2 = surv.df.rc$end, 
                event = surv.df.rc$event == 1)

#KM overall
surv.fit.full = survfit(surv.obj ~ 1)
plot(surv.fit.full, 
     xlab = "Age", 
     ylab = "Survival Probability", 
     main = "Diabetes Incidence Survival versus Age",
     xlim = c(50,60))
ggsurvplot(surv.fit.full, 
           data = surv.df.rc, 
           conf.int=FALSE, 
           xlab = "Age", 
           ylab = "Survival Probability", 
           main = "Diabetes Incidence Survival versus Age", 
           xlim = c(50,60),
           legend = "none",
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 2,
           fontsize = 3)

#KM by race
surv.fit.race = survfit(surv.obj ~ surv.df.rc$race)
summary(surv.fit.race)
plot(surv.fit.race, 
     xlab = "Age", 
     ylab = "Survival Probability", 
     main = "Diabetes Incidence Survival by Race", 
     xlim = c(50,60))
plot.race = ggsurvplot(surv.fit.race, 
           data = surv.df.rc, 
           xlab = "Age", 
           ylab = "Survival Probability", 
           title = "Diabetes Incidence Survival by Race", 
           xlim = c(50,60),
           ylim = c(0,1),
           palette = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7"),
           linetype = 1, 
           legend = 'right',
           legend.labs = c("Black/African-American", "Chinese/Chinese-American", 
                           "Japanese/Japanese-American", "Caucasian/White-Non-Hispanic", 
                           "Hispanic"),
           legend.title = "Race/Ethnicity",
           ggtheme = theme_classic2(base_size=12, base_family = "A"),
           font.family = "A",
           font.legend = 9,
           risk.table = TRUE,
           risk.table.height = .5,
           break.x.by = 2, 
           fontsize = 3)
print(plot.race)

#KM by insurance
surv.fit.ins = survfit(surv.obj ~ surv.df.rc$insured)
summary(surv.fit.ins)
plot(surv.fit.ins, 
     xlab = "Age", 
     ylab = "Survival Probability", 
     main = "Diabetes Incidence Survival by Insurance", 
     xlim = c(50,60))
plot.ins = ggsurvplot(surv.fit.ins, 
                       data = surv.df.rc, 
                       xlab = "Age", 
                       ylab = "Survival Probability", 
                       title = "Diabetes Incidence Survival by Health Insurance Status", 
                       xlim = c(50,60),
                       ylim = c(0,1),
                       palette = c("#E69F00", "#56B4E9"),
                       linetype = 1, 
                       legend = 'right',
                       legend.labs = c("Uninsured", "Insured"),
                       legend.title = "Health Insurance Status",
                       ggtheme = theme_classic2(base_size=12, base_family = "A"),
                       font.family = "A",
                       font.legend = 9,
                       risk.table = TRUE,
                       risk.table.height = .5,
                       break.x.by = 2, 
                       fontsize = 3)
print(plot.ins)





