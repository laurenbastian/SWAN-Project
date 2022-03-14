##LIBRARIES
library(survival)
library(survminer)

##LOAD DATA
source("build_survival_dataset.R")

##KM CURVES
windowsFonts(A = windowsFont("Times New Roman"))

surv.obj = Surv(time = surv.df.censored$start, 
                time2 = surv.df.censored$end, 
                event = surv.df.censored$event == "1")
summary(surv.obj)


surv.fit.full = survfit(surv.obj ~ 1)
plot(surv.fit.full, 
     xlab = "Age", 
     ylab = "Survival Probability", 
     main = "Diabetes Incidence Survival versus Age",
     xlim = c(40,60))
ggsurvplot(surv.fit.full, 
           data = surv.df.censored, 
           conf.int=FALSE, 
           xlab = "Age", 
           ylab = "Survival Probability", 
           main = "Diabetes Incidence Survival versus Age", 
           xlim = c(40,60),
           legend = "none")

surv.fit.race = survfit(surv.obj ~ surv.df.censored$race)
summary(surv.fit.race)
plot(surv.fit.race, 
     xlab = "Age", 
     ylab = "Survival Probability", 
     main = "Diabetes Incidence Survival by Race", 
     xlim = c(40,60))
plot.race = ggsurvplot(surv.fit.race, 
           data = surv.df.censored, 
           xlab = "Age", 
           ylab = "Survival Probability", 
           title = "Diabetes Incidence Survival by Race", 
           xlim = c(40,60), 
           palette = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7"),
           linetype = 1, 
           legend = 'right',
           legend.labs = c("Black/African American", "Chinese/Chinese American", 
                           "Japanese/Japanese American", "Caucasian/White Non-Hispanic", 
                           "Hispanic"),
           legend.title = "Race/Ethnicity",
           #risk.table = TRUE,
           #risk.table.poss = 'out',
           #tables.height = 0.4,
           #tables.theme = clean_table_theme(),
           ggtheme = theme_classic2(base_size=12, base_family = "A"),
           font.family = "A",
           font.legend = 9,
           break.time.by = 5)
print(plot.race)

ggsurvtable(surv.fit.race, 
            data = surv.df.censored,
            break.time.by = 5)

