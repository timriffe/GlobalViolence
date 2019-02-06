# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)

# ------------------------------------ #
# README
# ------------------------------------ #
# this script will require user intervention
# in a few places, so please step through it
# sequentially, reading annotations as you go
# for any questions, contact tim riffe
# ------------------------------------- #




# base folders whose names we recycle throughout
# file.path() used throughout under presumption that 
# OS differences in file seperators taken into account
who.folder <- file.path("Data","Inputs","WHO")
gbd.folder <- file.path("Data","Inputs","GBD")
wpp.folder <- file.path("Data","Inputs","WPP")
ihme.folder <- file.path("Data","Inputs","IHME")
# make inout directories:
dir.create(who.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(gbd.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(wpp.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(ihme.folder, showWarnings = FALSE, recursive = TRUE)
# ----------------------------------------------------------------- #
# WHO                                                               #
# ----------------------------------------------------------------- #
# Two ICD10 COD file locations, not sure how they relate, so download both
# One Population file for denominators, can be compared with others too.

# 1)
# https://www.who.int/healthinfo/statistics/mortality_rawdata/en/
# two files:
# 1) Mortality, ICD-10 (part 1/2)
# 2) Mortality, ICD-10 (part 2/2)
# automatic download, unpack, save as R data, and cleanup:

# download
download.file(url = "https://www.who.int/healthinfo/statistics/Morticd10_part1.zip?ua=1",
		destfile = file.path(who.folder,"Morticd10_part1.zip"))
download.file(url = "https://www.who.int/healthinfo/statistics/Morticd10_part2.zip?ua=1",
		destfile = file.path(who.folder,"Morticd10_part2.zip"))

# unzip
unzip(file.path(who.folder,"Morticd10_part1.zip"), exdir = who.folder)
unzip(file.path(who.folder,"Morticd10_part2.zip"), exdir = who.folder)

# read in fast as data.table
WHO1 <- fread(file.path(who.folder, "Morticd10_part1"))
WHO2 <- fread(file.path(who.folder, "Morticd10_part2"))
WHO  <- rbind(WHO1,WHO2)

# save as single R fil
save(WHO, file=file.path(who.folder,"WHO.Rdata"))

# remove objects from memory for now:
rm(WHO1, WHO2, WHO)
# take out the trash
gc()

# WHO COD downloaded, yay, but still more work with this later.
# -------------------------------------------------------------
# now let's do WHO all-cause, as a check:
# 2 files, one for each sex:
# download:
download.file(url = "http://terrance.who.int/mediacentre/data/ghe/ghe2016_deaths_country_mle.zip",
		destfile = file.path(who.folder,"ghe2016_deaths_country_mle.zip"))
download.file(url = "http://terrance.who.int/mediacentre/data/ghe/ghe2016_deaths_country_fmle.zip",
		destfile = file.path(who.folder,"ghe2016_deaths_country_fmle.zip"))

# unzip
unzip(file.path(who.folder,"ghe2016_deaths_country_mle.zip"),exdir=who.folder)
unzip(file.path(who.folder,"ghe2016_deaths_country_fmle.zip"),exdir=who.folder)

# read in:
WHOF <- fread(file.path(who.folder,"ghe2016_deaths_country_fmle.csv"))
WHOM <- fread(file.path(who.folder,"ghe2016_deaths_country_mle.csv"))

# save as single R file
WHO  <- rbind(WHOF, WHOM)

save(WHO, file=file.path(who.folder,"WHO_GHE.Rdata"))

# remove objects from memory for now:
rm(WHOF, WHOM, WHO)
# take out the trash
gc()

# ----------------------------------------------------
# and how about WHO denominators, at least as a check:
download.file(url = "https://www.who.int/healthinfo/Pop.zip",
		destfile = file.path(who.folder,"Pop.zip"))

unzip(file.path(who.folder,"Pop.zip"),exdir=who.folder)
WHOPOP <- fread(file.path(who.folder,"pop"))
save(WHOPOP, file=file.path(who.folder,"WHO_POP.Rdata"))

# ----------------------------------------------------
# remove the stuff we don't need anymore
fls  <- list.files(who.folder)
fls  <- fls[!fls%in% c("WHO.Rdata","WHO_GHE.Rdata","ghe2016_deaths_codebook.xlsx","WHO_POP.Rdata")]
file.remove(file.path(who.folder,fls))

# ----------------------------------------------------------------- #
# GBD                                                               #
# ----------------------------------------------------------------- #

# in results tool, I selected:
# Base: Single
# Location: "seklect only countries and territories"
# Year "select all"
# Context "Cause"
# Age "select all"
# Metric - check each Number, percent, rate, probability of death
# Measure "Deaths"
# Sex check both "Male" and "Female"
# Cause check 4: "Total" "C.3.2" "C.3.3" "C.3.4"

# select "download csv"
# You'll be asked Include IDS or Names, select Names
# enter email addy-- will be multiple files

# you'll receive confirmation email immediately,
# but files will take time to generate

# The data will be made available in 9 different zip files
# get a url by right clicking the link and select "copy url" or similar,
# then paste as character strings, like below. YOUR link will be different
# than this one, which will likely expire.
links <- c(
"http://s3.healthdata.org/gbd-api-2017-public/7937bbf8510f8dd0f9d6d453f6d3cc71_files/IHME-GBD_2017_DATA-7937bbf8-1.zip",
"http://s3.healthdata.org/gbd-api-2017-public/7937bbf8510f8dd0f9d6d453f6d3cc71_files/IHME-GBD_2017_DATA-7937bbf8-2.zip"
# etc etc etc for 9 urls
)
# EASIER:
# or notice that the links are the same except the last digit, find the pattern
# YOU NEED TO CHANGE THIS PATH 
links <- paste0("http://s3.healthdata.org/gbd-api-2017-public/7937bbf8510f8dd0f9d6d453f6d3cc71_files/IHME-GBD_2017_DATA-7937bbf8-",
		1:9,".zip")

# now do bulk download like so
for (i in 1:9){
	this.name <- file.path(gbd.folder,paste0("GBD",i,".zip"))
	download.file(url = links[i],
			# simplify names of zip files...
			destfile = this.name)
	# and unpack them
	unzip(file.path(gbd.folder,this.name),exdir=gbd.folder)
}

# get csv names
gbdcsvs <- list.files(gbd.folder,pattern=".csv")

# read and rbind in one go
GBD <- do.call("rbind", lapply(file.path(gbd.folder,gbdcsvs), fread))

# save as Rdata:
save(GBD, file = file.path(gbd.folder,"GBD.Rdata"))

# remove redundant files:
fls  <- list.files(gbd.folder)
fls  <- fls[!fls%in% c("GBD.Rdata","citation.txt")]
file.remove(file.path(gbd.folder,fls))

# remove from memory
rm(GBD)
gc()

# ----------------------------------------------------------------- #
# IHME lifetables                                                   #
# ----------------------------------------------------------------- #

# all located here: https://cloud.ihme.washington.edu/index.php/s/2JLHyPXCnZQyd9Q?path=%2FLife%20Tables

# copy and paste one of the links as a character string here:
"https://cloud.ihme.washington.edu/index.php/s/2JLHyPXCnZQyd9Q/download?path=%2FLife%20Tables&files=IHME_GBD_2017_LIFE_TABLES_1950_FEMALE_Y2018M11D08.zip"
# The pattern is 1 file per sex per year.
# 1950 to 2017
yrs <- 1950:2017
sx  <- c("MALE","FEMALE")
# this part same for everyone:
(YRSX <- c(outer(yrs,sx,paste,sep="_")))

# this part u should verify, as urls may change:
# just make sure to chop it and paste together like so:
links <- paste0("https://cloud.ihme.washington.edu/index.php/s/2JLHyPXCnZQyd9Q/download?path=%2FLife%20Tables&files=IHME_GBD_2017_LIFE_TABLES_",
		YRSX,
		"_Y2018M11D08.zip"   # if they do a version change then this would change I guess
		)

# now do bulk download like so
for (i in 1:length(links)){
			this.name <- file.path(ihme.folder,paste0("IHME",i,".zip"))
			download.file(url = links[i],
					# simplify names of zip files...
					destfile = this.name)
#			# and unpack them
			unzip(this.name,exdir=ihme.folder)
}		
		
# and read into single file like so:

# get csv names
ihmecsvs <- list.files(ihme.folder,pattern=".CSV")

# read and rbind in one go

# let's do this in 8 bite-sized chunks so we don't risk memory overload
# FYI this will not work as intended if the total number of CSVs isn't 136
nint  <- length(ihmecsvs)/8
toi   <- cumsum(rep(nint,8))
fromi <- toi - rep(nint,8)+1

# this may take several minutes to execute
for (b in 1:8){
	IHMEb  <- do.call("rbind", lapply(file.path(ihme.folder,ihmecsvs[fromi[b]:toi[b]]), fread))
	qx <- IHMEb[metric_id == 8]
	ex <- IHMEb[metric_id == 5]
	save(qx,file=file.path(ihme.folder,paste0("IHMEqx",b,".Rdata")))
	save(ex,file=file.path(ihme.folder,paste0("IHMEex",b,".Rdata")))
	rm(IHMEb,qx,ex)
	gc()
}

# then one at a time, we rbind the 1-8 files, but separately for qx and ex
IHMEqx  <- do.call("rbind", 
		lapply(file.path(ihme.folder,paste0("IHMEqx",1:8,".Rdata")), 
				# anonymous function
				function(x){
					# need local instance of object in here only to rbind
							local(get(load(x)))
						})
		)
# save out
save(IHMEqx,file=file.path(ihme.folder,"IHMEqx.Rdata"))
rm(IHMEqx)
gc()

# may as well take time to do the same for IHME ex as a check
IHMEex  <- do.call("rbind", 
		lapply(file.path(ihme.folder,paste0("IHMEex",1:8,".Rdata")), 
				# anonymous function
				function(x){
					# need local instance of object in here only to rbind
					local(get(load(x)))
				})
)
save(IHMEex,file=file.path(ihme.folder,"IHMEex.Rdata"))
rm(IHMEex)
gc()

# now cut down on file chaff in IHME folder:
fls <- list.files(ihme.folder)
fls <- fls[!fls%in%c("IHMEex.Rdata","IHMEqx.Rdata")]
file.remove(file.path(ihme.folder,fls))

# ----------------------------------------------------------------- #
# WPP lifetables                                                    #
# ----------------------------------------------------------------- #
"https://population.un.org/wpp/Download/Standard/Mortality/"
# Suggested citation: United Nations, Department of Economic and Social Affairs, Population Division (2017). 
# World Population Prospects: The 2017 Revision, DVD Edition.


wpp.url <- "https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_LifeTable.csv"

download.file(url = wpp.url,
		# simplify names of zip files...
		destfile = file.path(wpp.folder,"WPPLT.csv"))

WPPLT <- read.csv(file.path(wpp.folder,"WPPLT.csv"))
WPPLT <- data.table(WPPLT)
save(WPPLT, file = file.path(wpp.folder,"WPPLT.Rdata"))
rm(WPPLT);gc()
# repeat for WPP denoms, medium variant (no need for projections)
# populations in 1000s
wpp.url <- "https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_PopulationByAgeSex_Medium.csv"
download.file(url = wpp.url,
		# simplify names of zip files...
		destfile = file.path(wpp.folder,"WPPpop.csv"))

WPPpop <- read.csv(file.path(wpp.folder,"WPPpop.csv"))
WPPpop <- data.table(WPPpop)
save(WPPpop, file = file.path(wpp.folder,"WPPpop.Rdata"))
rm(WPPpop);gc()
#
file.remove(file.path(wpp.folder,c("WPPpop.csv","WPPLT.csv")))
# so far that's it, might still want WPP denominators.