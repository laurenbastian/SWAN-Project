##BUILD DATASET FOR SURVIVAL ANALYSIS
##############################################
##LIBRARIES
library(dplyr)

##LOAD DATA
source("load_swan_data.R")

##GET VARIABLES OF INTEREST
df.base = base[,c("SWANID", "AGE0", "RACE", "STATUS0", "INCOME0", 
                  "PREPAID0", "OTHRPRI0", "MEDICAR0", "MEDICAI0", 
                  "MILITAR0", "NOINSUR0", "OTHINSU0")]
df.1 = visit1[,c("SWANID",  "AGE1", "STATUS1", 
                 "INCOME1", "DIABETE1")]
df.2 = visit2[,c("SWANID", "AGE2", "STATUS2", 
                 "INCOME2", "DIABETE2")]
df.3 = visit3[,c("SWANID", "AGE3", "STATUS3", 
                 "INCOME3", "DIABETE3")]
df.4 = visit4[,c("SWANID", "AGE4", "STATUS4", 
                 "INCOME4", "DIABETE4")]
df.5 = visit5[,c("SWANID", "AGE5", "STATUS5", 
                "INCOME5", "DIABETE5")]
df.6 = visit6[,c("SWANID", "AGE6", "STATUS6", 
                 "INCOME6", "DIABETE6")]
df.7 = visit7[,c("SWANID", "AGE7", "STATUS7", 
                 "INCOME7", "DIABETE7")]
df.8 = visit8[,c("SWANID","AGE8","STATUS8", 
                 "INCOME8", "DIABETE8")]
df.9 = visit9[,c("SWANID", "AGE9", "STATUS9", 
                 "INCOME9", "DIABETE9")]
df.10 = visit10[,c("SWANID", "AGE10", "STATUS10", 
                 "INCOME10", "DIABETE10")]
merge.df = merge(df.base, df.1, by = "SWANID")
merge.df = merge(merge.df, df.2, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.3, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.4, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.5, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.6, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.7, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.8, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.9, by = "SWANID", all.x = "TRUE")
merge.df = merge(merge.df, df.10, by = "SWANID", all.x = "TRUE")

#reorder columns
merge.df = merge.df[,c (1:5, 13:52, 6:12)]
swan.df.wide = merge.df[, c(1,
                            3,
                            2,6,10,14,18,22,26,30,34,38,42,
                            4,7,11,15,19,23,27,31,35,39,43,
                            5,8,12,16,20,24,28,32,36,40,44,
                            9,13,17,21,25,29,33,37,41,45,
                            46:52)]

######################################

##CLEAR SPACE IN MEMORY
rm(merge.df, df.base, df.1, df.2, df.3, df.4, df.5, df.6, df.7, df.8, df.9, df.10, 
   base, visit1, visit2, visit3, visit4, visit5, 
   visit6, visit7, visit8, visit9, visit10)

############################

##CLEAN DATA

#Clean income data
income1.codes = c("(1) Less Than $19,999", "(1) 1: Less than $19,999")
income2.codes = c("(2) $20,000 to $49,999", "(2) 2: $20,000 to $49,999")
income3.codes = c("(3) $50,000 to $99,999", "(3) 3: $50,000 to $99,999")
income4.codes = c("(4) $100,000 or More", "(4) 4: $100,000 or More", 
                  "(4) $100,000 or more", "(4) 4: $100,000 or more")

for (i in 25:35)
{
  swan.df.wide[,i] = as.character(swan.df.wide[,i])
  swan.df.wide[,i][swan.df.wide[,i] %in% income1.codes] = "<20"
  swan.df.wide[,i][swan.df.wide[,i] %in% income2.codes] = "20-49"
  swan.df.wide[,i][swan.df.wide[,i] %in% income3.codes] = "50-99"
  swan.df.wide[,i][swan.df.wide[,i] %in% income4.codes] = ">100"
}

#Clean diabetes data
diabetes.no.codes = c("(1) No", "(1) 1: No")
diabetes.yes.codes = c("(2) Yes", "(2) 2: Yes")

for (i in 36:45)
{
  swan.df.wide[,i] = as.character(swan.df.wide[,i])
  swan.df.wide[,i][swan.df.wide[,i] %in% diabetes.no.codes] = "0"
  swan.df.wide[,i][swan.df.wide[,i] %in% diabetes.yes.codes] = "1"
}

#Clean menopausal status data
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

for (i in 14:24)
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


#Clean and dichotomize insurance data
insure.no.codes = c("(1) No")
insure.yes.codes = c("(2) Yes")

for (i in 46:52)
{
  swan.df.wide[,i] = as.character(swan.df.wide[,i])
  swan.df.wide[,i][swan.df.wide[,i] %in% insure.no.codes] = "0"
  swan.df.wide[,i][swan.df.wide[,i] %in% insure.yes.codes] = "1"
}

#dichotomize
swan.df.wide$insured = ifelse(swan.df.wide$NOINSUR0 == "1", "No", "Yes")


##RESHAPE TO LONG
#diabetes not recorded at baseline 
swan.df.long = reshape(data = swan.df.wide, 
                                idvar = c("SWANID"), 
                                varying = list(c(4:13), c(15:24), c(26:35), c(36:45)), 
                                v.names = c("age", "status", "income", "diabetes"), 
                                timevar = "visit",
                                times = c(1,2,3,4,5,6,7,8,9,10), 
                                direction = "long")

#reorder long
swan.df.long = arrange(swan.df.long, SWANID)
swan.df.long = swan.df.long[, -(6:12)] ##keep only dichotomized insurance
rownames(swan.df.long) = c(1:nrow(swan.df.long))
colnames(swan.df.long) = c("SWANID", "race", "base_age", "base_status", 
                          "base_income", "insured", "visit", "age", "status", "income", "diabetes")

##FIND TIME TO EVENT
##need:
# ID
# RACE
# start time, age at baseline
# end time/event time, age at event OR end of study
# event status
surv.df = data.frame("SWANID" = c(), 
                     "base_status" = c(), 
                     "base_income" = c(), 
                     "insured" = c(), 
                     "race" = c(), 
                     "start" = c(), 
                     "end" = c(), 
                     "event" = c())
for (i in 1:nrow(swan.df.long))
{
  id.curr = swan.df.long$SWANID[i]
  j = i-1
  id.prev = 0
  
  if(j != 0)
  {
    id.prev = swan.df.long$SWANID[j]
  }
  
  if (id.curr != id.prev) #add new row to the survival data frame
  {
    surv.df = rbind(surv.df, data.frame("SWANID" = id.curr,
                                        "base_status" = swan.df.long$base_status[i],
                                        "base_income" = swan.df.long$base_income[i],
                                        "insured" = swan.df.long$insured[i],
                                        "race" = swan.df.long$race[i],
                                        "start" = swan.df.long$age[i],
                                        "end" = swan.df.long$age[i],
                                        "event" = swan.df.long$diabetes[i]))
  }
  else #get row of the survival data frame
  {
    
    #if the event has already occurred, do not update event and age, next iteration
    if (swan.df.long$diabetes[j] == 1 & !is.na(swan.df.long$diabetes[j]))
    {
      next
    }
    
    #if event has not already occurred update
    if (swan.df.long$diabetes[j] == 0 & !is.na(swan.df.long$diabetes[j]))
    {
      #if there has been a change, update event, end, and censoring
      if (swan.df.long$diabetes[i] == 1 & !is.na(swan.df.long$diabetes[i]))
      {
        surv.df$end[surv.df$SWANID == id.curr] = swan.df.long$age[i]
        surv.df$event[surv.df$SWANID == id.curr] = 1
      }
      #if there has not been a change, update age and check whether to censor
      else if (swan.df.long$diabetes[i] == 0 & !is.na(swan.df.long$diabetes[i]))
      {
        surv.df$end[surv.df$SWANID == id.curr] = swan.df.long$age[i]
      }
    }
  }
}

#remove NA values
surv.df = surv.df[!is.na(surv.df$event),]
#remove values where no time passed
surv.df = surv.df[surv.df$start != surv.df$end,]

##censor data
censored = rep(0, nrow(surv.df))
for (i in 1:nrow(surv.df))
{
  if(is.na(surv.df$event[i]) | surv.df$event[i] == 0)
  {
    censored[i] = 1
  }
}

surv.df.censored = cbind(surv.df, censored)

rm(income1.codes, income2.codes, income3.codes, income4.codes,
   i, j, id.curr, id.prev, status1.codes, status2.codes, status3.codes,
   status4.codes, status5.codes, status6.codes, status7.codes,
   status8.codes, diabetes.no.codes, diabetes.yes.codes, censored,
   insure.no.codes, insure.yes.codes)
     
