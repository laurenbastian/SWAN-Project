##LIBRARIES
#install.packages("dplyr")
library(dplyr)

##############################

##LOAD DATA
#base: a dataframe of all visits
#visit_: individual dataframes for each of the 10 follow-up visits
#swan.df: a dataframe of all individuals with observations for all data collection times
source("load_swan_data.R")

##Get all IDs for which there are values at every time point
##if SWANID is in all datasets, add it to the list

swan.df = merge(base, visit1, by = "SWANID")
swan.df = merge(swan.df, visit2, by = "SWANID")
swan.df = merge(swan.df, visit3, by = "SWANID")
swan.df = merge(swan.df, visit4, by = "SWANID")
swan.df = merge(swan.df, visit5, by = "SWANID")
swan.df = merge(swan.df, visit6, by = "SWANID")
swan.df = merge(swan.df, visit7, by = "SWANID")
swan.df = merge(swan.df, visit8, by = "SWANID")
swan.df = merge(swan.df, visit9, by = "SWANID")
swan.df = merge(swan.df, visit10, by = "SWANID")


####################################

##SELECT VARIABLES OF INTEREST
#ESTLSTV were you taking estrogen/progestin medications at previous visit
#ESTROG taking estrogen since last visit
##endometriosis ENDO
##start of menopause STATUS
##compare for HRT groups ESTROG and other
##SWANID


swan.df = dplyr::select(swan.df, SWANID, RACE, 
                     STATUS2, STATUS3, STATUS4, STATUS5, STATUS6, 
                     STATUS7, STATUS8, STATUS9, STATUS10,
                     ESTROG12, ESTROG13, ESTROG14, ESTROG15,
                     ESTROG16, ESTROG17, ESTROG18, ESTROG19, ESTROG110,
                     ENDO2, ENDO3, ENDO4, ENDO5, ENDO6, ENDO7, ENDO8, ENDO9, ENDO10,
                     BMI2, BMI3, BMI4, BMI5, BMI6, BMI7, BMI8, BMI9, BMI10)

#remove visits to save memory
rm(base, visit1, visit2, visit3, visit4, visit5, visit6, visit7, visit8, visit9, visit10)

###############################

##CLEANING DATA WIDE FORMAT

swan.df.wide = swan.df

#menopausal status

status1.codes = c("(1) Post by Bilateral Salpingo Oophorectomy", "(1) Post by BSO", "(1) Hysterectomy/both ovaries removed", 
                  "(1) 1: Post by bilateral salpingo oophorectomy (BSO)", "(1) Post by BSO (Bilateral Salpingo Oophorectomy)",
                  "(1) 1: Post by BSO", "(1) Hysterectomy/both ovaries removed")
status2.codes = c("(2) Natural post", "(2) Natural Post", "(2) Post-menopausal", "(2) 2: Natural post", "(2) 2: Natural Post")
status3.codes = c("(3) Late perimenopause", "(3) Late peri", "(3) Late Peri", "(3) 3: Late peri", "(3) 3: Late Peri")
status4.codes = c("(4) Early perimenopause", "(4) Early peri", "(4) Early Peri", "(4) 4: Early peri", "(4) 4: Early Peri")
status5.codes = c("(5) Pre-menopausal", '(5) 5: Pre-menopausal')
status6.codes = c("(6) Pregnant/breastfeeding", "(6) 6: Pregnant/breastfeeding")
status7.codes = c("(7) Unknown due to hormone therapy use", "(7) Unknown due to HT use", 
                  "(7) 7: Unknown due to hormone therapy (HT) use", "(7) Unknown due to hormone therapy (HT) use", 
                  "(7) 7: Unknown due to HT use", "(7) Unknown due to hormones (HT) use")
status8.codes = c("(8) Unknown due to hysterectomy", "(8) 8: Unknown due to hysterectomy")


for (i in 3:11)
{
  swan.df.wide[,i] = as.character(swan.df.wide[,i])
  swan.df.wide[,i][swan.df.wide[,i] %in% status1.codes] = "1"
  swan.df.wide[,i][swan.df.wide[,i] %in% status2.codes] = "2"
  swan.df.wide[,i][swan.df.wide[,i] %in% status3.codes] = "3"
  swan.df.wide[,i][swan.df.wide[,i] %in% status4.codes] = "4"
  swan.df.wide[,i][swan.df.wide[,i] %in% status5.codes] = "5"
  swan.df.wide[,i][swan.df.wide[,i] %in% status6.codes] = "6"
  swan.df.wide[,i][swan.df.wide[,i] %in% status7.codes] = "7"
  swan.df.wide[,i][swan.df.wide[,i] %in% status8.codes] = "8"
}

##estrogen
for (i in 12:20)
{
  swan.df.wide[,i] = as.character(swan.df.wide[,i])
  swan.df.wide[,i][swan.df.wide[,i] == "(2) Yes" | swan.df.wide[,i] == "(2) 2: Yes"] = "Yes"
  swan.df.wide[,i][swan.df.wide[,i] == "(1) No" | swan.df.wide[,i] == "(1) 1: No"] = "No"
  
}

##endometriosis
for (i in 21:29)
{
  swan.df.wide[,i] = as.character(swan.df.wide[,i])
  swan.df.wide[,i][swan.df.wide[,i] == "(2) Yes" | swan.df.wide[,i] == "(2) 2: Yes"] = 1
  swan.df.wide[,i][swan.df.wide[,i] == "(1) No" | swan.df.wide[,i] == "(1) 1: No"] = 0
}

#########################################
##LONG FORMAT (complete) swan.df.long.complete

#removing observations with NA
cols = as.vector(colnames(swan.df.wide))
swan.df.wide.complete = swan.df.wide[complete.cases(swan.df.wide[cols]), cols]

#reshape to long
swan.df.long.complete = reshape(data = swan.df.wide.complete, 
                       idvar = c("SWANID", "RACE"), 
                       varying = list(c(3:11), c(12:20), c(21:29), c(30:38)), 
                       v.names = c("status", "estrogen", "endo", "bmi"), 
                       timevar = "visit",
                       times = c(2,3,4,5,6,7,8,9,10), 
                       direction = "long")

swan.df.long.complete$visit = as.factor(swan.df.long.complete$visit)
swan.df.long.complete$estrogen = as.factor(swan.df.long.complete$estrogen)
swan.df.long.complete$status = as.factor(swan.df.long.complete$status)
swan.df.long.complete$endo = as.numeric(swan.df.long.complete$endo)
swan.df.long.complete$RACE = as.factor(swan.df.long.complete$RACE)
swan.df.long.complete$SWANID = as.factor(swan.df.long.complete$SWANID)


##LONG FORMAT (incomplete) swan.df.long

#reshape to long
swan.df.long = reshape(data = swan.df.wide, 
                       idvar = c("SWANID", "RACE"), 
                       varying = list(c(3:11), c(12:20), c(21:29), c(30:38)), 
                       v.names = c("status", "estrogen", "endo", "bmi"), 
                       timevar = "visit",
                       times = c(2,3,4,5,6,7,8,9,10), 
                       direction = "long")

swan.df.long$visit = as.factor(swan.df.long$visit)
swan.df.long$estrogen = as.factor(swan.df.long$estrogen)
swan.df.long$status = as.factor(swan.df.long$status)
swan.df.long$endo = as.numeric(swan.df.long$endo)
swan.df.long$RACE = as.factor(swan.df.long$RACE)
swan.df.long$SWANID = as.factor(swan.df.long$SWANID)


##remove unnecessary objects
rm(cols, i, status1.codes, status2.codes, status3.codes, status4.codes,
   status5.codes, status6.codes, status7.codes, status8.codes)

