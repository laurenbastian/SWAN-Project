##LOAD DATA
#visit.list: a list of all visits
#base: a dataframe of all visits
#visit_: individual dataframes for each of the 10 follow-up visits
#swan.df: a dataframe of all individuals with observations for all data collection times
source("load_swan_data.R")

#ESTLSTV were you taking estrogen/progestin medications at previous visit
#ESTROG taking estrogen since last visit
##Fibroids FIBROID, FIBRUTR
##endometriosis ENDO
##breast cancer CANCERS
##cervical cancer SITESPE
##start of menopause STATUS
##compare for HRT groups ESTROG and other
##SWANID

library(dplyr)
swan.use.df = select(swan.df, SWANID, RACE, STATUS1, STATUS2, STATUS3, STATUS4, 
                     STATUS5, ESTROG11, ESTROG12, ESTROG13, ESTROG14, ESTROG15, FIBROID1,
                     FIBROID2,FIBROID3, FIBRUTR4, FIBRUTR5, ENDO2, ENDO3, ENDO4, ENDO5)





