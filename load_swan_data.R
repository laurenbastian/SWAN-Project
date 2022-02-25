##Load All Datasets
load("./swan_data/swan_base.rda")
base = da28762.0001
rm(da28762.0001)

load("./swan_data/swan_visit1.rda")
visit1 = da29221.0001
rm(da29221.0001)

load("./swan_data/swan_visit2.rda")
visit2 = da29401.0001
rm(da29401.0001)

load("./swan_data/swan_visit3.rda")
visit3 = da29701.0001
rm(da29701.0001)

load("./swan_data/swan_visit4.rda")
visit4 = da30142.0001
rm(da30142.0001)

load("./swan_data/swan_visit5.rda")
visit5 = da30501.0001
rm(da30501.0001)

load("./swan_data/swan_visit6.rda")
visit6 = da31181.0001
rm(da31181.0001)

load("./swan_data/swan_visit7.rda")
visit7 = da31901.0001
rm(da31901.0001)

load("./swan_data/swan_visit8.rda")
visit8 = da32122.0001
rm(da32122.0001)

load("./swan_data/swan_visit9.rda")
visit9 = da32721.0001
rm(da32721.0001)

load("./swan_data/swan_visit10.rda")
visit10 = da32961.0001
rm(da32961.0001)

##Get all IDs for which there are values at every time point
##if SWANID is in all datasets, add it to the list
visit.list = list(visit1, visit2, visit3, visit4, visit5, visit6, visit7, visit8, visit9, visit10)

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










