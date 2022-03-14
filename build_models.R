##LIBRARIES
#install.packages("dplyr")
library(dplyr)
library(geepack)

#######################
#LOAD DATA
source("build_dataset.R")
#swan.df: wide dataset of the variables of interest for visits 1-10
#swan.df.wide: wide dataset of variables of interest with NA values for visits 2-10
#swan.df.long: long dataset of variables of interest with NA values for visits 2-10
#swan.df.wide.complete: wide dataset of variables of interest without values for visits 2-10
#swan.df.long.complete: long dataset of variables of interest without values for visits 2-10


########################
##EXPLORATORY DATA ANALYSIS

#Font
windowsFonts(A = windowsFont("Times New Roman"))

#mean age
colMeans(swan.df.wide.complete[, 39:47])
#mean bmi
colMeans(swan.df.wide.complete[, 30:38])

swan.df.wide.complete$ENDO2 = as.numeric(swan.df.wide.complete$ENDO2)
swan.df.wide.complete$ENDO3 = as.numeric(swan.df.wide.complete$ENDO3)
swan.df.wide.complete$ENDO4 = as.numeric(swan.df.wide.complete$ENDO4)
swan.df.wide.complete$ENDO5 = as.numeric(swan.df.wide.complete$ENDO5)
swan.df.wide.complete$ENDO6 = as.numeric(swan.df.wide.complete$ENDO6)
swan.df.wide.complete$ENDO7 = as.numeric(swan.df.wide.complete$ENDO7)
swan.df.wide.complete$ENDO8 = as.numeric(swan.df.wide.complete$ENDO8)
swan.df.wide.complete$ENDO9 = as.numeric(swan.df.wide.complete$ENDO9)
swan.df.wide.complete$ENDO10 = as.numeric(swan.df.wide.complete$ENDO10)

endo.p = colMeans(swan.df.wide.complete[, 21:29])
endo.p
xtabs(~endo + visit, swan.df.long.complete)

#estrogen % and counts
estrogen.p = xtabs(~estrogen + visit, data = swan.df.long.complete)[2,] / 1105
estrogen.p
xtabs(~estrogen + visit, data = swan.df.long.complete)

#racial distribution
race = c("Black/African American", "Chinese/Chinese American", 
         "Japanese/Japanese American", "Caucasian/Non Hispanic White")
counts = table(swan.df.wide.complete$RACE)
race.df = data.frame(race, counts)
ggplot(race.df, aes(x="", y=counts, fill=race)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(counts/1105, 2), "%")), position = position_stack(vjust=0.5), color = "white") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Set2") +
  theme(text = element_text(family = "A"))

##Endometriosis vs Estrogen
ggplot(swan.df.long.complete) +
  geom_col(aes(x = visit, y = endo, fill = estrogen)) +
  theme_light() + 
  scale_fill_brewer(palette = "Set2", "Estrogen") +
  xlab("Visit") +
  ylab("Endometriosis Cases") +
  theme(text = element_text(family = "A"))

##Endometriosis by Status
ggplot(swan.df.long.complete) +
  geom_col(aes(x = visit, y = endo, fill = status)) +
  theme_light() + 
  scale_fill_brewer(palette = "Set2", "Menopausal Status",labels=c('Post by BSO', 'Natural Post', "Late Perimenopause", 
                               "Early Perimenopause", "Premenopausal", "Pregnant/Breastfeeding",
                               "Unknown due to hormone therapy", "Unknown due to hysterectomy")) +
  xlab("Visit") +
  ylab("Endometriosis Cases") +
  theme(text = element_text(family = "A"))
  

#########################
##GENERALIZED ESTIMATING EQUATIONS
#geepack library 
#(https://faculty.washington.edu/heagerty/Courses/b571/homework/geepack-paper.pdf)


##GEE WITHOUT MISSING VALUES (swan.df.long)
##different correlation structures
geemod1 = geeglm(endo ~ 1 + estrogen + status + bmi, 
                 id = SWANID,
                 data = swan.df.long.complete, 
                 family = "binomial", 
                 corstr = "independence")
summary(geemod1)

geemod2 = geeglm(endo ~ 1 + estrogen + status + bmi, 
                 id = SWANID,
                 data = swan.df.long.complete, 
                 family = "binomial", 
                 corstr = "ar1")
summary(geemod2)

##remove race
geemod3 = geeglm(endo ~ 1 + status, 
                 id = SWANID,
                 data = swan.df.long, 
                 family = "binomial", 
                 corstr = "ar1")
summary(geemod3)
coef(geemod3)

######################














