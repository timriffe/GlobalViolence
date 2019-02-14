
# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)
gbd.folder <- file.path("Data","Inputs","GBD")
dir.create(file.path("Data","Grouped","GBD"), showWarnings = FALSE, recursive = TRUE)


GBD <- local(get(load(file.path(gbd.folder,"GBD.Rdata"))))
GBD$Sex <- ifelse(GBD$sex == "Male",1,2)
GBD[,c("sex","measure"):=NULL];gc()

# remove Percent, not needed now.
GBD <- GBD[metric != "Percent"];gc()

GBD[location=="Estonia" & year == 1990 & Sex == 1 & cause == "All causes" & metric == "Number"]$age
rmages <- c("Under 5","Early Neonatal","Late Neonatal","Post Neonatal",
		"15-49 years","80 plus","All Ages","5-14 years","15-49 years",
		"50-69 years","70+ years","<20 years","10 to 24","10 to 54","Age-standardized")

GBD <- GBD[!age%in%rmages];gc()



#ages <- sort(unique(GBD$age))
#recvec <- c(0,1,seq(5,95,by=5))
#names(recvec) <- ages[c(4,3,13,1,2,5:12,14:21)]
#dput(recvec)
recvec <- c(`<1 year` = 0, `1 to 4` = 1, `5 to 9` = 5, `10 to 14` = 10, 
		`15 to 19` = 15, `20 to 24` = 20, `25 to 29` = 25, `30 to 34` = 30, 
		`35 to 39` = 35, `40 to 44` = 40, `45 to 49` = 45, `50 to 54` = 50, 
		`55 to 59` = 55, `60 to 64` = 60, `65 to 69` = 65, `70 to 74` = 70, 
		`75 to 79` = 75, `80 to 84` = 80, `85 to 89` = 85, `90 to 94` = 90, 
		`95 plus` = 95)

GBD$Age <- recvec[GBD$age]
GBD[,c("age"):=NULL];gc()

# More reshaping to match GHE: 3 files with MID, LOW, UPP.
# Can use Rate and Number to recuperate exposure.

# group state violence and terrorism
#recvec <- c("a","w","h","w")
#names(recvec)<- unique(GBD$cause)
#dput(recvec)
recvec <- c(`All causes` = "a", `Conflict and terrorism` = "w", `Interpersonal violence` = "h", 
		`Executions and police conflict` = "w")
GBD$cause <- recvec[GBD$cause]


# now group causes
GBD <- GBD[,.(val=sum(val),upper=sum(upper),lower=sum(lower)),
		by=.(metric,location,year,Sex,cause,Age)];gc()

table(GBD$cause)
# set order for clean chunking
setorder(GBD,metric,location,year,Sex,cause,Age)

NN  <- GBD[metric == "Number"]
GBD <- GBD[metric == "Rate"];gc()
dim(GBD);dim(NN)

# rates split
M   	<- GBD[cause == "a"]
GBD 	<- GBD[cause != "a"]
Mw  	<- GBD[cause == "w"]
Mh 		<- GBD[cause != "w"];gc() 
rm(GBD);gc()
# counts split
D   	<- NN[cause == "a"]
NN   	<- NN[cause != "a"]
Dw  	<- NN[cause == "w"]
Dh   	<- NN[cause != "w"];gc() # Dh
rm(NN);gc()
# -----------------------
# MID, LOW, UPP
MID 	<- copy(D)
MID[,c("upper","lower","metric"):=NULL ];gc()
setnames(MID,"val","D")
MID$Dh 	<- Dh$val
MID$Dw 	<- Dw$val
MID$M 	<- M$val / 1e5
MID$Mh 	<- Mh$val / 1e5
MID$Mw 	<- Mw$val / 1e5

Dh[,c("val"):=NULL];D[,c("val"):=NULL];Dw[,c("val"):=NULL]
Mh[,c("val"):=NULL];M[,c("val"):=NULL];Mw[,c("val"):=NULL]
gc()

#DemoTools::LTabr(nMx=MID$M[1:21]/1e5,Age=c(0,1,seq(5,95,by=5)))

save(MID, file = file.path("Data","Grouped","GBD","GBDmid.Rdata"))
rm(MID);gc()
#-----
# LOW
LOW 	<- copy(D)
LOW[,c("upper","metric"):=NULL ];gc()
setnames(LOW,"lower","D")
LOW$Dh 	<- Dh$lower
LOW$Dw 	<- Dw$lower
LOW$M 	<- M$lower / 1e5
LOW$Mh 	<- Mh$lower / 1e5
LOW$Mw 	<- Mw$lower / 1e5

Dh[,c("lower"):=NULL];D[,c("lower"):=NULL];Dw[,c("lower"):=NULL]
Mh[,c("lower"):=NULL];M[,c("lower"):=NULL];Mw[,c("lower"):=NULL]
gc()
save(LOW, file = file.path("Data","Grouped","GBD","GBDlow.Rdata"))
rm(LOW);gc()
#-----
# LOW
UPP 	<- copy(D)
UPP[,c("metric"):=NULL ];gc()
setnames(UPP,"upper","D")
UPP$Dh 	<- Dh$upper
UPP$Dw 	<- Dw$upper
UPP$M 	<- M$upper / 1e5
UPP$Mh 	<- Mh$upper / 1e5
UPP$Mw 	<- Mw$upper / 1e5
save(UPP, file = file.path("Data","Grouped","GBD","GBDupp.Rdata"))
rm(UPP);gc()
rm(Dh,D,Dw,Mh,M,Mw);gc()

# end (still prefer single ages tho)
# -------
#
#LOW <- local(get(load(file.path("Data","Grouped","GBD","GBDlow.Rdata"))))
#DemoTools::LTabr(nMx=LOW$M[1:21],Age=c(0,1,seq(5,95,by=5)))
#
#UPP <- local(get(load(file.path("Data","Grouped","GBD","GBDupp.Rdata"))))
#DemoTools::LTabr(nMx=UPP$M[1:21],Age=c(0,1,seq(5,95,by=5)))
# exposure check too.
#LOW$Dw[100]/LOW$Mw[100]
#UPP$Dw[100]/UPP$Mw[100]
