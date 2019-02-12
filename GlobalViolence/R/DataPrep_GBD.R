
# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)
gbd.folder <- file.path("Data","Inputs","GBD")

GBD <- local(get(load(file.path(gbd.folder,"GBD.Rdata"))))
GBD$Sex <- ifelse(GBD$sex == "Male",1,2)
GBD[,c("sex"):=NULL];gc()

# remove Percent, not needed now.
GBD <- GBD[metric != "Percent"];gc()

GBD[location=="Estonia" & year == 1990 & Sex == 1 & cause == "All causes" & metric == "Number"]$age
rmages <- c("Under 5","Early Neonatal","Late Neonatal","Post Neonatal",
		"15-49 years","80 plus","All Ages","5-14 years","15-49 years",
		"50-69 years","70+ years","<20 years","10 to 24","10 to 54","Age-standardized")

GBD <- GBD[!age%in%rmages];gc()



ages <- sort(unique(GBD$age))
recvec <- c(0,1,seq(5,95,by=5))
names(recvec) <- ages[c(4,3,13,1,2,5:12,14:21)]
recvec
GBD$Age <- recvec[GBD$age]
GBD[,c("age"):=NULL];gc()

# More reshaping to match GHE: 3 files with MID, LOW, UPP.
# Can use Rate and Number to recuperate exposure.