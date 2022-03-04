##LIBRARIES
#install.packages("dplyr")
#install.packages("geepack")
#install.packages("lme4")
library(dplyr)
library(geepack)
library(lme4)


#######################
#LOAD DATA
source("build_dataset.R")
#swan.df: wide dataset of the variables of interest for visits 1-10
#swan.df.wide: wide dataset of variables of interest with NA values for visits 2-10
#swan.df.long: long dataset of variables of interest with NA values for visits 2-10
#swan.df.wide.complete: wide dataset of variables of interest without values for visits 2-10
#swan.df.long.complete: long dataset of variables of interest without values for visits 2-10


#########################
##GENERALIZED ESTIMATING EQUATIONS
#geepack library 
#(https://faculty.washington.edu/heagerty/Courses/b571/homework/geepack-paper.pdf)


###########################
##GEE WITHOUT MISSING VALUES (swan.df.long)
##different correlation structures
geemod1 = geeglm(endo ~ 1 + estrogen + status + RACE + bmi, 
                 id = SWANID,
                 data = swan.df.long.complete, 
                 family = "binomial", 
                 corstr = "independence")
summary(geemod1)

geemod2 = geeglm(endo ~ 1 + estrogen + status + RACE + bmi, 
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
##GEE WITH MISSING VALUES (swan.df.long.na)
geemod1.na = geeglm(endo ~ 1 + estrogen + status + RACE, 
                 id = SWANID,
                 data = swan.df.long.na, 
                 family = "binomial", 
                 corstr = "independence")
summary(geemod1.na)





######################
##MIXED EFFECTS MODEL
#install.packages("lme4")
#library(lme4)
#lmermod1 = glmer(endo ~ visit + estrogen + status + RACE + (1|SWANID), data = swan.df.long[], family = "binomial")












