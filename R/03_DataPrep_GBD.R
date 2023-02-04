
source(here("R","00_Install_Packages.R"))

gbd.folder <- here("Data","Inputs","GBD")

dir.create(here("Data","Grouped","GBD"), showWarnings = FALSE, recursive = TRUE)

GBD     <- readRDS(file.path(gbd.folder,"GBD.rds"))
head(GBD)
# remove Percent, not needed now.
GBD <- GBD[metric!= "Percent"]; gc()  

# recode age
rmages <- c("Under 5","Early Neonatal","Late Neonatal","Post Neonatal",
		"15-49 years","80 plus","All Ages","5-14 years","15-49 years",
		"50-69 years","70+ years","<20 years","10 to 24","10 to 54","Age-standardized","<70 years")

GBD <- GBD[!age%in%rmages];gc()    
recvec <- c("<1 year" = 0, "1 to 4" = 1, "5 to 9" = 5, "10 to 14" = 10, 
		"15 to 19" = 15, "20 to 24" = 20, "25 to 29" = 25, "30 to 34" = 30, 
		"35 to 39" = 35, "40 to 44" = 40, "45 to 49" = 45, "50 to 54" = 50, 
		"55 to 59" = 55, "60 to 64" = 60, "65 to 69" = 65, "70 to 74" = 70, 
		"75 to 79" = 75, "80 to 84" = 80, "85 to 89" = 85, "90 to 94" = 90, 
		"95 plus" = 95)
GBD$age <- recvec[GBD$age]

# More reshaping to match GHE: 3 files with MID, LOW, UPP.
# Can use Rate and Number to recuperate exposure.

# group state violence and terrorism
recvec <- c("All causes" = "a", "Conflict and terrorism" = "w", "Interpersonal violence" = "h", 
		"Executions and police conflict" = "w")
GBD$cause <- recvec[GBD$cause]   

# reset names for metric:
recvec = c("Number" = "D", "Rate" = "M")
GBD$metric <- recvec[GBD$metric]
# now group causes
GBD <- GBD[, .(val = sum(val), upper = sum(upper) ,lower = sum(lower)),
		by = .(metric, location, year, sex, cause, age)];gc()



# Countrycodes into ISO3
GBD$ISO3 <-countrycode(GBD$location, origin="country.name", destination="iso3c")


# shape, save, rm

MID <- dcast(GBD, ISO3 + location + year + sex + age ~ metric + cause, 
             value.var = "val")
MID <- as.data.table(MID)
MID <- MID[,.(M_a = M_a/1e5, M_h = M_h/1e5, M_w = M_w/1e5, D_a = D_a, D_h = D_h, D_w = D_w), by=.(ISO3,location,year,sex,age)]
setnames(MID, colnames(MID), new = gsub(pattern = "_", replace = "", colnames(MID)))
saveRDS(MID, file = here("Data","Grouped","GBD","GBDmid.rds")) ; rm(MID) ; gc()


UPP <- dcast(GBD, ISO3 + location + year + sex + age ~ metric + cause, value.var = "upper")
UPP <- as.data.table(UPP)
UPP <- UPP[,.(M_a = M_a/1e5, M_h = M_h/1e5, M_w = M_w/1e5, D_a = D_a, D_h = D_h, D_w = D_w),by=.(ISO3,location,year,sex,age)]
setnames(UPP, colnames(UPP), new = gsub(pattern = "_", replace = "", colnames(UPP)))
saveRDS(UPP, file = here("Data","Grouped","GBD","GBDupp.rds")) ; rm(UPP) ; gc()


LOW <- dcast(GBD, ISO3 + location + year + sex + age ~ metric + cause, value.var = "lower")
LOW<-as.data.table(LOW)
LOW <- LOW[,.(M_a = M_a/1e5, M_h = M_h/1e5, M_w = M_w/1e5, D_a = D_a, D_h = D_h, D_w = D_w),by=.(ISO3,location,year,sex,age)] 
setnames(LOW, colnames(LOW), new = gsub(pattern = "_", replace = "", colnames(LOW)))
saveRDS(LOW, file = here("Data","Grouped","GBD","GBDlow.rds")) ; rm(LOW) ; gc()


# Next step: 04_DataPrep_Graduate.R
