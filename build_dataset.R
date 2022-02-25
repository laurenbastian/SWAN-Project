##LOAD DATA
#visit.list: a list of all visits
#base: a dataframe of all visits
#visit_: individual dataframes for each of the 10 follow-up visits
#ID.vec: a vector of all IDs for individuals who have observations for all data collection times
source("load_swan_data.R")

table(base$STATUS0)
table(visit1$STATUS1)

#ESTLSTV were you taking estrogen/progestin medications at previous visit
#ESTROG taking estrogen since last visit

##MENOPAUSAL STATUSES
stat0 = base$STATUS0[match(ID.vec, base$SWANID)]
stat1 = visit1$STATUS1[match(ID.vec, visit1$SWANID)]
stat2 = visit2$STATUS1[match(ID.vec, visit2$SWANID)]
stat3 = visit3$STATUS1[match(ID.vec, visit3$SWANID)]
stat4 = visit4$STATUS1[match(ID.vec, visit4$SWANID)]
stat5 = visit5$STATUS1[match(ID.vec, visit5$SWANID)]
stat6 = visit6$STATUS1[match(ID.vec, visit6$SWANID)]
stat7 = visit7$STATUS1[match(ID.vec, visit7$SWANID)]
stat8 = visit8$STATUS1[match(ID.vec, visit8$SWANID)]
stat9 = visit9$STATUS1[match(ID.vec, visit9$SWANID)]
stat10 = visit10$STATUS1[match(ID.vec, visit10$SWANID)]


