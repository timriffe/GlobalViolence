# Author: tim

# WARNING, this script in progress. May crash your memory
###############################################################################

# step 1, for each age format, get an Age, AgeInterval column made.
# group infant deaths if necessary (not sure).
me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)
who.folder <- file.path("Data","Inputs","WHO")
# output direwctory for grouped data
dir.create(file.path("Data","Grouped","WHO"), showWarnings = FALSE, recursive = TRUE)



readWHO_1 <- function(){
	WHO <- local(get(load(file.path(who.folder,"WHO.Rdata"))))
	setnames(WHO, paste0("Deaths",1:26),as.character(c(9999,0:5,seq(10,95,by=5),999)))
# 9999 for total, and 999 for unk Age
	WHO[,c("IM_Deaths1","IM_Deaths2","IM_Deaths3","IM_Deaths4","IM_Frmat","Frmat"):=NULL];gc()
	
	# Brasil filter
	ind1 <- !is.na(WHO$Admin1) & WHO$Admin1 == "901" & WHO$Country == "2070" ;gc()
	ind2 <- !is.na(WHO$Admin1) & WHO$Admin1 == "902" & WHO$Country == "2070" ;gc()
	
# so, we can remove ind1 and ind2
	keep        <- !(ind1 | ind2)
	WHO         <- WHO[keep,];gc()
	
	WHO
}

WHO_2_Long <- function(WHOchunk){
	WHOL<- melt(WHOchunk, id.vars = c("Country", "Year", "List", "Cause", "Sex"), 
			measure.vars = as.character(c(9999,0:5,seq(10,95,by=5),999)),
			variable.name = "Age",
			value.name = "Deaths");gc()
	WHOL[,Age := as.character(Age)]
	WHOL[,Age := as.integer(Age)]
	WHOL
}


# There are 3 WHO files we need to deal with

# here the first one, in several chunks, here chunk 1

# TO BE USED FOR 3-DIGIT CODES, 4-DIGIT CODES TO BE REDUCED TO 3
# strict homicide: x85-y09
h3 <- c(paste0("X", sprintf("%02d", 85:99)), paste0("Y", sprintf("%02d", 0:9)))
# suspicious external
y3 <- paste0("Y", sprintf("%02d", 20:30))
# police & war
w3 <- paste0("Y", sprintf("%02d", 35:36))

grouph3 <- function(.SD,h3,w3,y3){
	data.frame(D = sum(.SD$Deaths),
			Dh = sum(.SD$Deaths[.SD$Cause %in% h3]),
			Dw = sum(.SD$Deaths[.SD$Cause %in% w3]),
			Dy = sum(.SD$Deaths[.SD$Cause %in% y3]))
}
# -------------------------------------------------------


# 1)
WHO <- readWHO_1()
# list 104, Males, years 1988 to 2005
WHO_1 <- WHO[List == "104" & Sex == 1 & Year < 2006];rm(WHO);gc()
# remove unneeded columns for this chunk
WHO_1[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_1 <- WHO_2_Long(WHO_1)
# cut to first 3 characters:
WHO_1[,Cause := substr(Cause, 1, 3)];gc()
# regroup deaths
WHO_1[,
		Deaths := sum(Deaths), 
		by = .(Country, Year, Cause, Sex, Age)];gc()
# and create new group columns
WHO_1 <- WHO_1[,grouph3(.SD,h3,w3,y3),
				by = .(Country, Year, Sex, Age)];gc()

save(WHO_1, file=file.path("Data","Grouped","WHO","WHO_1.Rdata"))
rm(WHO_1);gc()

# ----------------#
# WHO chunk 2:    #
# ----------------#

WHO    <- readWHO_1()
WHO_2  <- WHO[List == "104" & Sex == 1 & Year >= 2006];rm(WHO);gc()
WHO_2[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_2  <- WHO_2_Long(WHO_2)
WHO_2[,Cause := substr(Cause, 1, 3)];gc()
WHO_2[,
		Deaths := sum(Deaths), 
		by = .(Country, Year, Cause, Sex, Age)];gc()
WHO_2  <- WHO_2[,grouph3(.SD,h3,w3,y3),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_2, file=file.path("Data","Grouped","WHO","WHO_2.Rdata"))
rm(WHO_2);gc()

# ----------------#
# WHO chunk 3:    #
# ----------------#

WHO    <- readWHO_1()
WHO_3  <- WHO[List == "104" & Sex == 2 & Year < 2006];rm(WHO);gc()
WHO_3[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_3  <- WHO_2_Long(WHO_3)
WHO_3[,Cause := substr(Cause, 1, 3)];gc()
WHO_3[,
		Deaths := sum(Deaths), 
		by = .(Country, Year, Cause, Sex, Age)];gc()
WHO_3  <- WHO_3[,grouph3(.SD,h3,w3,y3),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_3, file=file.path("Data","Grouped","WHO","WHO_3.Rdata"))
rm(WHO_3);gc()

# ----------------#
# WHO chunk 3:    #
# ----------------#

WHO    <- readWHO_1()
WHO_4  <- WHO[List == "104" & Sex == 2 & Year >= 2006];rm(WHO);gc()
WHO_4[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_4  <- WHO_2_Long(WHO_4)
WHO_4[,Cause := substr(Cause, 1, 3)];gc()
WHO_4[,
		Deaths := sum(Deaths), 
		by = .(Country, Year, Cause, Sex, Age)];gc()
WHO_4  <- WHO_4[,grouph3(.SD,h3,w3,y3),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_4, file=file.path("Data","Grouped","WHO","WHO_4.Rdata"))
rm(WHO_4);gc()

# -----------------------------------------
# Now the chunks that already come in 3 digit codes

WHO    <- readWHO_1()
WHO_5  <- WHO[List == "103" & Sex == 1];rm(WHO);gc()
# need to spot check, seems ok
WHO_5[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_5  <- WHO_2_Long(WHO_5)
WHO_5  <- WHO_5[,grouph3(.SD,h3,w3,y3),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_5, file=file.path("Data","Grouped","WHO","WHO_5.Rdata"))
rm(WHO_5);gc()
# again for females
WHO    <- readWHO_1()
WHO_6  <- WHO[List == "103" & Sex == 2];rm(WHO);gc()
# need to spot check, seems ok
WHO_6[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_6  <- WHO_2_Long(WHO_6)
WHO_6  <- WHO_6[,grouph3(.SD,h3,w3,y3),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_6, file=file.path("Data","Grouped","WHO","WHO_6.Rdata"))
rm(WHO_6);gc()
# -------------------------------------
# Portugal special years
WHO    <- readWHO_1()
WHO_7  <- WHO[List == "UE1"];rm(WHO);gc()
"UE64" # is h3
"UE65" # is y3 approx(Y10-Y34) instead of our Y20-Y30
# w3 is 0s
WHO_7[,c("Admin1","SubDiv"):=NULL];gc()
WHO_7  <- WHO_2_Long(WHO_7)

groupUE1 <- function(.SD){
	data.frame(D = sum(.SD$Deaths),
			Dh = sum(.SD$Deaths[.SD$Cause %in% "UE64"]),
			Dw = 0,
			Dy = sum(.SD$Deaths[.SD$Cause %in% "UE65"]))
}
WHO_7  <- WHO_7[,groupUE1(.SD),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_7, file=file.path("Data","Grouped","WHO","WHO_7.Rdata"))
rm(WHO_7);gc()

# -------------------------------------
WHO    <- readWHO_1()
WHO_8 <- WHO[List == "101"];rm(WHO);gc()


# D  "1000" use because there are redundant groupings
# Dh "1102"
# 1103 is also larger than Dy + Dw....
# Dy "1103", but much too inclusive set to NA
# Dw set to NA
WHO_8[,c("Admin1","SubDiv"):=NULL];gc()
WHO_8  <- WHO_2_Long(WHO_8)
group101 <- function(.SD){
	data.frame(D = sum(.SD$Deaths[.SD$Cause %in% "1000"]),
			Dh = sum(.SD$Deaths[.SD$Cause %in% "1102"]),
			Dw = NA,
			Dy = NA)
}
WHO_8  <- WHO_8[,group101(.SD),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_8, file=file.path("Data","Grouped","WHO","WHO_8.Rdata"))
rm(WHO_8);gc()

# --------------------------
# Mixed codes can be reduced to 3. These are mutually exclusive and therefore sum.
# so we can treat as if they were 4 digits
WHO <- readWHO_1()
WHO_9 <- WHO[List == "10M"];rm(WHO);gc()

WHO_9[,c("Admin1","SubDiv"):=NULL];gc()
WHO_9  <- WHO_2_Long(WHO_9)

# Codes are mutually exclusive, so can collapse to 3
WHO_9[,Cause := substr(Cause, 1, 3)];gc()
WHO_9[,
		Deaths := sum(Deaths), 
		by = .(Country, Year, Cause, Sex, Age)];gc()
WHO_9  <- WHO_9[,grouph3(.SD,h3,w3,y3),
		by = .(Country, Year, Sex, Age)];gc()	
save(WHO_9, file=file.path("Data","Grouped","WHO","WHO_9.Rdata"))
rm(WHO_9);gc()
 
# -------------------------

files <- paste0("WHO_",1:9,".Rdata")

WHO <- do.call("rbind",lapply(files,function(x){
					local(get(load(file.path("Data","Grouped","WHO",x))))
				}))

save(WHO,file=file.path("Data","Grouped","WHO","WHO1_Combined.Rdata"))

# some cleaning
rm(files,group101,grouph3,groupUE1,readWHO_1,WHO_2_Long)

# ------------------------------------------------------------
# now, what's GHE?

GHE <- local(get(load(file.path(who.folder,"WHO_GHE.Rdata"))))


GHE$sex <- ifelse(GHE$sex == "FMLE",2,1)
GHE <- GHE[GHE$causename %in% c("All Causes","Intentional injuries","Interpersonal violence")]   

GHE <- reshape(GHE, direction='long', 
		varying=c(paste0('dths',2000:2016), paste0('low',2000:2016), paste0('upp',2000:2016)), 
		timevar='Year',
		times=c(2000:2016),
		v.names=c('dths','low',"upp"))

# standardize ages
# age 1 -> 0
# age 2 -> 1
ind1          <- GHE$age == 1
GHE$age[ind1] <- 0
ind2          <- GHE$age == 2
GHE$age[ind2] <- 1
rm(ind1,ind2);gc()

GHE <- GHE[,.(dths=sum(dths),low=sum(low),upp=sum(upp)), by = .(iso3,causename,sex,age,Year)]

D   <- GHE[causename == "All Causes"]
GHE <- GHE[causename != "All Causes"];gc()
Dh  <- GHE[causename == "Interpersonal violence"]
GHE <- GHE[causename != "Interpersonal violence"];gc()
Dwy <- GHE[causename == "Intentional injuries"]
rm(GHE);gc()

# Make 3 datsets

MID <- copy(D)   # seems to only make reference
MID[,c("low","upp"):=NULL]
setnames(MID, "dths","D")
MID$Dh  <- Dh$dths
MID$Dwy <- Dwy$dths

save(MID, file=file.path("Data","Grouped","WHO","GHEmid.Rdata"))
rm(MID);gc()

# lower bound
LOW <- copy(D)   # seems to only make reference
LOW[,c("dths","upp"):=NULL]
setnames(LOW, "low","D")
LOW$Dh  <- Dh$low
LOW$Dwy <- Dwy$low
save(LOW, file=file.path("Data","Grouped","WHO","GHElow.Rdata"))
rm(LOW);gc()

# upper bound
UPP <- copy(D)   # seems to only make reference
UPP[,c("dths","low"):=NULL]
setnames(UPP, "upp","D")
UPP$Dh  <- Dh$upp
UPP$Dwy <- Dwy$upp
save(UPP, file=file.path("Data","Grouped","WHO","GHEupp.Rdata"))
rm(UPP);gc()
rm(Dh,D,Dwy);gc()
# --------------------------------------#
# Take a look at Population Data        #
# --------------------------------------#

POP <- local(get(load(file.path(who.folder,"WHO_POP.Rdata"))))

setnames(POP, paste0("Pop",1:26),as.character(c(9999,0:5,seq(10,95,by=5),999)))
# 9999 for total, and 999 for unk Age
POP[,c("Lb"):=NULL];gc()

# affects Brasil, Panama, and Israel
ind1 <- !is.na(POP$Admin1) & (POP$Admin1 == "901" | POP$Admin1 == "902") ;gc()
POP <- POP[!ind1]
rm(ind1);gc()
POP[,c("Admin1","SubDiv"):=NULL];gc()
# get Age to long
POP <- melt(POP, id.vars = c("Country", "Year", "Sex"), 
		measure.vars = as.character(c(9999,0:5,seq(10,95,by=5),999)),
		variable.name = "Age",
		value.name = "Pop");gc()
POP[,Age := as.character(Age)]
POP[,Age := as.integer(Age)]
# save out
save(POP, file=file.path("Data","Grouped","WHO","WHO_POP.Rdata"))

# --------------------------------------------------------------- #
# Done with WHO for now. Still prefer single ages 0-100+ though.  #
# --------------------------------------------------------------- #

