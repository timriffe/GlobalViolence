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
			measure.vars = as.character(c(0:5,seq(10,95,by=5),999)),
			variable.name = "Age",
			value.name = "Deaths");gc()
	WHOL[,Age := as.character(Age)]
	WHOL[,Age := as.integer(Age)]
	WHOL
}


# There are 3 WHO files we need to deal with

# here the first one, in at least 4 chunks, here chunk 1
# 1)
WHO <- readWHO_1()

# try processing in chunks by List.

# list 104, Males
WHO_1 <- WHO[WHO$List == "104" & WHO$Sex == 1]
rm(WHO);gc()

# remove unneeded columns for this chunk
WHO_1[,c("Admin1","SubDiv"):=NULL];gc()
# now to long
WHO_1 <- WHO_2_Long(WHO_1)

# cut to first 3 characters:
WHO_1[,Cause := substr(Cause, 1, 3)];gc()

# group deaths
WHO_1[,
		Deaths := sum(Deaths), 
		by = .(Country, Year, Cause, Sex, Age)];gc()


#0       1       2       3       4       6       7       9 
#3220899  314182  156397    2567    5300     324    2985    2242 

#WHO <- melt(WHO, id.vars = c("Country", "Admin1", "SubDiv", "Year", "List", "Cause", "Sex", "Frmat"), 
#		measure.vars = as.character(c(0:5,seq(10,95,by=5),999)),
#		variable.name = "age",
#		value.name = "Deaths")
homicideCodes <- list(UE1 = c("UE64","UE65"),'101'=c("1102"))
sort(unique(WHO$Cause[WHO$List=="UE1"]))
# oooooooh, so CH00  

sort(unique(WHO$Cause[WHO$List == "101"]))

keepUE1 <- WHO$List != "UE1" | (WHO$List == "UE1" & WHO$Cause %in% c("CH00","UE64","UE65"))

"1000" # All Cause!
"1102" # assault, rest can be removed
keep101 <- WHO$List != "101" | (WHO$List == "101" & WHO$Cause %in% c("1000" , "1102"))

WHO <- WHO[keepUE1 & keep101]

# separate out list 104, truncate to 3
ind      <- WHO$List == "104" | WHO$List == "10M"
WHO$Cause[ind] <- substr(WHO$Cause[ind], 1, 3)

WHOstep1 <- WHO[ind]
WHOstep1 <- melt(WHOstep1, id.vars = c("Country", "Admin1", "SubDiv", "Year", "List", "Cause", "Sex", "Frmat"), 
		measure.vars = as.character(c(0:5,seq(10,95,by=5),999)),
		variable.name = "Age",
		value.name = "Deaths")
gc()
WHO[,Deaths:=sum(Deaths),by=list(Country,Admin1,SubDiv,Year,Sex)]


sort(unique(WHO$Cause[WHO$List == "103"]))
table(nchar(sort(unique(WHO$Cause[WHO$List == "104"]))))
sort(unique(WHO$Cause[WHO$List == "10M"]))

# smaller
sum(WHO$List == "104")

Full4 <- c(t(outer(LETTERS[-26],sprintf("%03d", 0:999),paste0)))
Full3 <- c(t(outer(LETTERS[-26],sprintf("%02d", 0:99),paste0)))



PRT <- WHO[WHO$Country == "4240" & WHO$Year == 2007 & WHO$Sex == 2]

d <- PRT[grepl(PRT$Cause,pattern="CH")]$'-1'
sum(d[-1])


gc()
rm(hm)
Frmt <- list()
paste0("Deaths",1:26)
colnames(WHO)[10:35]
table(WHO$Frmat)
WHO <- melt(WHO)
WHO <- WHO[!is.na(value)]

# at least columns follow regular schema for lower age bounds,
# meaning interval can be detected.


melt(WHO[Frmat == 0])
rm(WHO)
gc()




