
# Author: tim
###############################################################################

# TODO:
# 1) sex integer 1,2
# 2) harmonize location codes?
# 3) single ages 0-100+

me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)

LT <- local(get(load(file.path("Data","Inputs","WPP","WPPLT.Rdata"))))
LT <- LT[LT$SexID!=3];gc()
LT[,c("VarID","Variant","AgeGrp","Sex"):=NULL]
setnames(LT,c("AgeGrpStart","AgeGrpSpan","SexID"),c("Age","AgeInt","Sex"))