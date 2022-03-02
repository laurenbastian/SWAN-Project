##LIBRARIES
#install.packages("dplyr")
#install.packages("gee")
library(dplyr)
library(gee)

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


swan.use.df = select(swan.df, SWANID, RACE, 
                     STATUS1, STATUS2, STATUS3, STATUS4, STATUS5, STATUS6, 
                     STATUS7, STATUS8, STATUS9, STATUS10,
                     ESTROG11, ESTROG12, ESTROG13, ESTROG14, ESTROG15,
                     ESTROG16, ESTROG17, ESTROG18, ESTROG19, ESTROG110,
                     ENDO2, ENDO3, ENDO4, ENDO5, ENDO6, ENDO7, ENDO8, ENDO9, ENDO10)

#remove visits to save memory
rm(base, visit1, visit2, visit3, visit4, visit5, visit6, visit7, visit8, visit9, visit10)

#########################################

##CLEANING DATA

##Get data in long format
newcol = data.frame("ENDO1" = rep(NA, nrow(swan.use.df)))
swan.use.newcol = cbind(swan.use.df, newcol)
swan.use.newcol = swan.use.newcol[ ,c(1:22, 32, 23:31)]

swan.df.long = reshape(data = swan.use.newcol, 
                       idvar = c("SWANID", "RACE"), 
                       varying = list(c(3:12), c(13:22), c(23:32)), 
                       v.names = c("status", "estrogen", "endo"), 
                       timevar = "visit",
                       times = c(1,2,3,4,5,6,7,8,9,10), 
                       direction = "long")

#Cleaning endometriosis status
swan.df.long$endo = as.character(swan.df.long$endo)
swan.df.long$endo[swan.df.long$endo == "(2) Yes" | swan.df.long$endo == "(2) 2: Yes"] = "Yes"
swan.df.long$endo[swan.df.long$endo == "(1) No" | swan.df.long$endo == "(1) 1: No"] = "No"

#Cleaning estrogen use
swan.df.long$estrogen = as.character(swan.df.long$estrogen)
swan.df.long$estrogen[swan.df.long$estrogen == "(2) Yes" | swan.df.long$estrogen == "(2) 2: Yes"] = "Yes"
swan.df.long$estrogen[swan.df.long$estrogen == "(1) No" | swan.df.long$estrogen == "(1) 1: No"] = "No"

#Cleaning menopausal status
status1.codes = c("(1) Post by Bilateral Salpingo Oophorectomy", "(1) Post by BSO", "(1) Hysterectomy/both ovaries removed", 
                  "(1) 1: Post by bilateral salpingo oophorectomy (BSO)", "(1) Post by BSO (Bilateral Salpingo Oophorectomy)",
                  "(1) 1: Post by BSO", "(1) Hysterectomy/both ovaries removed")
status2.codes = c("(2) Natural post", "(2) Post-menopausal", "(2) 2: Natural post", "(2) 2: Natural Post")
status3.codes = c("(3) Late perimenopause", "(3) Late peri", "(3) Late Peri", "(3) 3: Late peri", "(3) 3: Late Peri")
status4.codes = c("(4) Early perimenopause", "(4) Early peri", "(4) Early Peri", "(4) 4: Early peri", "(4) 4: Early Peri")
status5.codes = c("(5) Pre-menopausal", '(5) 5: Pre-menopausal')
status6.codes = c("(6) Pregnant/breastfeeding", "(6) 6: Pregnant/breastfeeding")
status7.codes = c("(7) Unknown due to hormone therapy use", "(7) Unknown due to HT use", 
                  "(7) 7: Unknown due to hormone therapy (HT) use", "(7) Unknown due to hormone therapy (HT) use", 
                  "(7) 7: Unknown due to HT use", "(7) Unknown due to hormones (HT) use")
status8.codes = c("(8) Unknown due to hysterectomy", "(8) 8: Unknown due to hysterectomy")

swan.df.long$status = as.character(swan.df.long$status)
swan.df.long$status[swan.df.long$status %in% status1.codes] = "1"
swan.df.long$status[swan.df.long$status %in% status2.codes] = "2"
swan.df.long$status[swan.df.long$status %in% status3.codes] = "3"
swan.df.long$status[swan.df.long$status %in% status4.codes] = "4"
swan.df.long$status[swan.df.long$status %in% status5.codes] = "5"
swan.df.long$status[swan.df.long$status %in% status6.codes] = "6"
swan.df.long$status[swan.df.long$status %in% status7.codes] = "7"
swan.df.long$status[swan.df.long$status %in% status8.codes] = "8"
