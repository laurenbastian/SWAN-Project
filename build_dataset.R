##LIBRARIES
#install.packages("dplyr")
#install.packages("gee")
#install.packages("reshape2")
setwd("./")
library(dplyr)
library(gee)
library(reshape2)

##LOAD DATA
#visit.list: a list of all visits
#base: a dataframe of all visits
#visit_: individual dataframes for each of the 10 follow-up visits
#swan.df: a dataframe of all individuals with observations for all data collection times
source("load_swan_data.R")

##SELECT VARIABLES OF INTEREST

#ESTLSTV were you taking estrogen/progestin medications at previous visit
#ESTROG taking estrogen since last visit
##Fibroids FIBROID, FIBRUTR
##endometriosis ENDO
##breast cancer CANCERS
##cervical cancer SITESPE
##start of menopause STATUS
##compare for HRT groups ESTROG and other
##SWANID


swan.use.df = select(swan.df, SWANID, RACE, 
                     STATUS1, STATUS2, STATUS3, STATUS4, STATUS5, STATUS6, 
                     STATUS7, STATUS8, STATUS9, STATUS10,
                     ESTROG11, ESTROG12, ESTROG13, ESTROG14, ESTROG15,
                     ESTROG16, ESTROG17, ESTROG18, ESTROG19, ESTROG110,
                     ENDO2, ENDO3, ENDO4, ENDO5, ENDO6, ENDO7, ENDO8, ENDO9, ENDO10)

#########################################

##CLEANING DATA
#CLEANING MENOPAUSAL STATUS
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

status.list = list(swan.use.df$STATUS1, swan.use.df$STATUS2, swan.use.df$STATUS3, 
               swan.use.df$STATUS4, swan.use.df$STATUS5, swan.use.df$STATUS6, 
               swan.use.df$STATUS7, swan.use.df$STATUS8, swan.use.df$STATUS9, 
               swan.use.df$STATUS10)


for (i in 1:length(status.list))
{
  status = unlist(status.list[[i]])
  status = ifelse(status %in% status1.codes, 1, status)
  status = ifelse(status %in% status2.codes, 2, status)
  status = ifelse(status %in% status3.codes, 3, status)
  status = ifelse(status %in% status4.codes, 4, status)
  status = ifelse(status %in% status5.codes, 5, status)
  status = ifelse(status %in% status6.codes, 6, status)
  status = ifelse(status %in% status7.codes, 7, status)
  status = ifelse(status %in% status8.codes, 8, status)
  
  if (i == 1)
  {
    swan.use.df$STATUS1 = status
  }
  else if (i == 2)
  {
    swan.use.df$STATUS2 = status
  }
  else if (i == 3)
  {
    swan.use.df$STATUS3 = status
  }
  else if (i == 4)
  {
    swan.use.df$STATUS4 = status
  }
  else if (i == 5)
  {
    swan.use.df$STATUS5 = status
  }
  else if (i == 6)
  {
    swan.use.df$STATUS6 = status
  }
  else if (i == 7)
  {
    swan.use.df$STATUS7 = status
  }
  else if (i == 8)
  {
    swan.use.df$STATUS8 = status
  }
  else if (i == 9)
  {
    swan.use.df$STATUS9 = status
  }
  else if (i == 10)
  {
    swan.use.df$STATUS10 = status
  }
}



#CLEANING ENDOMETRIOSIS DIOGNOSIS

yes.endo.codes = c("(2) Yes", "(2) 2: Yes")
no.endo.codes = c("(1) No", "(1) 1: No")

#endo.list is a list of all ENDO factors, ENDO1 is unobserved, so give and NA placeholder
endo.list = list(NA, swan.use.df$ENDO2, swan.use.df$ENDO3, swan.use.df$ENDO4,
                 swan.use.df$ENDO5, swan.use.df$ENDO6, swan.use.df$ENDO7,
                 swan.use.df$ENDO8, swan.use.df$ENDO9, swan.use.df$ENDO10)

#replaces with 1 if no endometriosis and 2 if endometriosis, NA values retained
for (i in 2:length(endo.list))
{
  endo = unlist(endo.list[[i]])
  endo = ifelse(status %in% yes.endo.codes, 1, endo)
  endo = ifelse(status %in% no.endo.codes, 0, endo)
  

  if (i == 2)
  {
    swan.use.df$ENDO2 = endo
  }
  else if (i == 3)
  {
    swan.use.df$ENDO3 = endo
  }
  else if (i == 4)
  {
    swan.use.df$ENDO4 = endo
  }
  else if (i == 5)
  {
    swan.use.df$ENDO5 = endo
  }
  else if (i == 6)
  {
    swan.use.df$ENDO6 = endo
  }
  else if (i == 7)
  {
    swan.use.df$ENDO7 = endo
  }
  else if (i == 8)
  {
    swan.use.df$ENDO8 = endo
  }
  else if (i == 9)
  {
    swan.use.df$ENDO9 = endo
  }
  else if (i == 10)
  {
    swan.use.df$ENDO10 = endo
  }
}





