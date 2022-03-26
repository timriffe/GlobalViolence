
source(here("R","00_Install_Packages.R"))

# ------------------------------------ #
# README
# ------------------------------------ #
# this script will require user intervention
# in a few places, so please step through it
# sequentially, reading annotations as you go
# for any questions, contact _masked for now_
# ------------------------------------ #

# ------------------------------------ #
# Note: substantial portions of this code are 
# not required, and are commented out.
# ------------------------------------ #

# base folders whose names we recycle throughout
# file.path() used throughout under presumption that 
# OS differences in file seperators taken into account
# who.folder  <- here("Data","Inputs","WHO")
gbd.folder  <- here("Data","Inputs","GBD")
# wpp.folder  <- here("Data","Inputs","WPP")
# ihme.folder <- here("Data","Inputs","IHME")
# make inout directories:
# dir.create(who.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(gbd.folder, showWarnings = FALSE, recursive = TRUE)
# dir.create(wpp.folder, showWarnings = FALSE, recursive = TRUE)
# dir.create(ihme.folder, showWarnings = FALSE, recursive = TRUE)
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
# download.file(url = "https://www.who.int/healthinfo/statistics/Morticd10_part1.zip?ua=1",
# 		destfile = file.path(who.folder, "Morticd10_part1.zip"))
# download.file(url = "https://www.who.int/healthinfo/statistics/Morticd10_part2.zip?ua=1",
# 		destfile = file.path(who.folder, "Morticd10_part2.zip"))
# 
# # unzip
# unzip(file.path(who.folder,"Morticd10_part1.zip"), exdir = who.folder)
# unzip(file.path(who.folder,"Morticd10_part2.zip"), exdir = who.folder)
# 
# # read in fast as data.table
# WHO1 <- fread(file.path(who.folder, "Morticd10_part1"))
# WHO2 <- fread(file.path(who.folder, "Morticd10_part2"))
# WHO  <- rbind(WHO1, WHO2)
# 
# # save as single R fil
# saveRDS(WHO, file = file.path(who.folder, "WHO.rds"))
# 
# # remove objects from memory for now:
# rm(WHO1, WHO2, WHO)
# # take out the trash
# gc()

# WHO COD downloaded, yay, but still more work with this later.
# -------------------------------------------------------------
# now let's do WHO all-cause, as a check:
# 2 files, one for each sex:
# download:
# download.file(url = "http://terrance.who.int/mediacentre/data/ghe/ghe2016_deaths_country_mle.zip",
# 		destfile = file.path(who.folder,"ghe2016_deaths_country_mle.zip"))
# download.file(url = "http://terrance.who.int/mediacentre/data/ghe/ghe2016_deaths_country_fmle.zip",
# 		destfile = file.path(who.folder,"ghe2016_deaths_country_fmle.zip"))
# 
# # unzip
# unzip(file.path(who.folder,"ghe2016_deaths_country_mle.zip"),exdir=who.folder)
# unzip(file.path(who.folder,"ghe2016_deaths_country_fmle.zip"),exdir=who.folder)
# 
# # read in:
# WHOF <- fread(file.path(who.folder,"ghe2016_deaths_country_fmle.csv"))
# WHOM <- fread(file.path(who.folder,"ghe2016_deaths_country_mle.csv"))
# 
# # save as single R file
# WHO  <- rbind(WHOF, WHOM)
# 
# saveRDS(WHO, file = file.path(who.folder, "WHO_GHE.rds"))
# 
# # remove objects from memory for now:
# rm(WHOF, WHOM, WHO)
# # take out the trash
# gc()

# ----------------------------------------------------
# # and how about WHO denominators, at least as a check:
# download.file(url = "https://www.who.int/healthinfo/Pop.zip",
# 		destfile = file.path(who.folder, "Pop.zip"))
# 
# unzip(file.path(who.folder,"Pop.zip"), exdir= who.folder)
# WHOPOP <- fread(file.path(who.folder, "pop"))
# saveRDS(WHOPOP, file = file.path(who.folder, "WHO_POP.rds"))

# ----------------------------------------------------
# remove the stuff we don't need anymore
# fls  <- list.files(who.folder)
# fls  <- fls[!fls%in% c("WHO.rds","WHO_GHE.rds","ghe2016_deaths_codebook.xlsx","WHO_POP.rds")]
# file.remove(file.path(who.folder, fls))

# ----------------------------------------------------------------- #
# GBD                                                               #
# ----------------------------------------------------------------- #

# in browser, go to:
# This one selects years 2000+
# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/6be4e7149bf6b4f32ce65adc9651420f

# This one selects years 1990+ and downloads now fail
# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2017-permalink/e39887527afdc6b5e1cc14a5463befde
# select 'get permalink'
# select "download csv"
# You'll be asked Include IDS or Names, select Names
# enter email addy-- will be multiple files
# you'll receive confirmation email immediately,
# but files will take time to generate

# The data will be made available in 10 different zip files
# get a url by right clicking the link and select "copy url" or similar,
# then paste as character strings, like below. YOUR link will be different
# than this one, which will likely expire.


# in results tool, I selected:
# Base: Single
# Location: "select only countries and territories"
# Year "select all"
# Context "Cause"
# Age "select all"
# Metric - check each Number, percent, rate, probability of death
# Measure "Deaths"
# Sex check both "Male" and "Female"
# Cause check 4: "Total" "C.3.2" "C.3.3" "C.3.4"


# EASIER:
# or notice that the links are the same except the last digit, find the pattern
# YOU NEED TO CHANGE THIS PATH 
# On my last
HowManyFilesAreThere <- 6
links <- paste0("https://s3.healthdata.org/gbd-api-2019-public/cb201b3abc797778e2105e627c23f983_files/IHME-GBD_2019_DATA-cb201b3a-",
  		1:HowManyFilesAreThere,".zip")
  
  # now do bulk download like so
  for (i in 1:length(links)){
  	this.name <- file.path(gbd.folder, paste0("GBD", i, ".zip"))
  	 download.file(url = links[i],
  	 		# simplify names of zip files...
  	 		destfile = this.name)
  	# and unpack them
  	unzip(this.name, exdir = gbd.folder)
  }
  
  # get csv names
  gbdcsvs <- list.files(gbd.folder, pattern = ".csv")
  gbdcsvs <- gbdcsvs[!gbdcsvs %in% c("GBD_ISO3.csv", "GBD_Countries.csv")]
  # read and rbind in one go
  GBD <- do.call("rbind", lapply(file.path(gbd.folder,gbdcsvs), fread))
  
  # save as Rdata:
  saveRDS(GBD, file = file.path(gbd.folder,"GBD.rds"))
  
  # remove redundant files:
  fls  <- list.files(gbd.folder)
  fls  <- fls[!fls%in% c("GBD.rds","citation.txt","GBD_ISO3.csv", "GBD_Countries.csv")]
  file.remove(file.path(gbd.folder,fls))
  
  # remove from memory
  rm(GBD)
  gc()

# ----------------------------------------------------------------- #
# IHME lifetables                                                   #
# ----------------------------------------------------------------- #

# all located here: https://cloud.ihme.washington.edu/index.php/s/2JLHyPXCnZQyd9Q?path=%2FLife%20Tables

# copy and paste one of the links as a character string here:
# "https://cloud.ihme.washington.edu/index.php/s/2JLHyPXCnZQyd9Q/download?path=%2FLife%20Tables&files=IHME_GBD_2017_LIFE_TABLES_1950_FEMALE_Y2018M11D08.zip"
# # The pattern is 1 file per sex per year.
# # 1950 to 2017
# yrs <- 1950:2017
# sx  <- c("MALE","FEMALE")
# # this part same for everyone:
# (YRSX <- c(outer(yrs,sx,paste,sep="_")))
# 
# # this part u should verify, as urls may change:
# # just make sure to chop it and paste together like so:
# links <- paste0("https://cloud.ihme.washington.edu/s/2JLHyPXCnZQyd9Q/download?path=%2FLife%20Tables&files=IHME_GBD_2017_LIFE_TABLES_",
# 		YRSX,
# 		"_Y2018M11D08.zip"   # if they do a version change then this would change I guess
# 		)
# getOption("timeout")
# options(timeout=100)
# # now do bulk download like so
# for (i in 1:length(links)){
# 			this.name <- file.path(ihme.folder,paste0("IHME",i,".zip"))
# 			download.file(url = links[i],
# 					# simplify names of zip files...
# 					destfile = this.name)
# #			# and unpack them
# 			unzip(this.name, exdir = ihme.folder)
# 			Sys.sleep(1)
# }		
# 		
# # and read into single file like so:
# 
# # get csv names
# ihmecsvs <- list.files(ihme.folder, pattern = ".CSV")
# length(ihmecsvs)
# # read and rbind in one go
# 
# # let's do this in 8 bite-sized chunks so we don't risk memory overload
# # FYI this will not work as intended if the total number of CSVs isn't 136
# nint  <- length(ihmecsvs)/8
# toi   <- cumsum(rep(nint,8))
# fromi <- toi - rep(nint,8)+1
# 
# # this may take several minutes to execute
# for (b in 1:8){
# 	IHMEb  <- do.call("rbind", lapply(file.path(ihme.folder,ihmecsvs[fromi[b]:toi[b]]), fread))
# 	qx <- IHMEb[metric_id == 8]
# 	ex <- IHMEb[metric_id == 5]
# 	saveRDS(qx, file = file.path(ihme.folder, paste0("IHMEqx", b, ".rds")))
# 	saveRDS(ex, file = file.path(ihme.folder, paste0("IHMEex", b, ".rds")))
# 	rm(IHMEb,qx,ex)
# 	gc()
# }
# 
# # then one at a time, we rbind the 1-8 files, but separately for qx and ex
# IHMEqx  <- do.call("rbind", 
# 		lapply(file.path(ihme.folder, paste0("IHMEqx", 1:8, ".rds")), 
# 				# anonymous function
# 				function(x){
# 					# need local instance of object in here only to rbind
# 							readRDS(x)
# 						})
# 		)
# # save out
# saveRDS(IHMEqx, file = file.path(ihme.folder, "IHMEqx.rds"))
# rm(IHMEqx)
# gc()
# 
# # may as well take time to do the same for IHME ex as a check
# IHMEex  <- do.call("rbind", 
# 		lapply(file.path(ihme.folder, paste0("IHMEex", 1:8, ".rds")), 
# 				# anonymous function
# 				function(x){
# 					# need local instance of object in here only to rbind
# 					readRDS(x)
# 				})
# )
# save(IHMEex, file = file.path(ihme.folder, "IHMEex.rds"))
# rm(IHMEex)
# gc()
# 
# # now cut down on file chaff in IHME folder:
# fls <- list.files(ihme.folder)
# fls <- fls[!fls%in%c("IHMEex.rds","IHMEqx.rds")]
# file.remove(file.path(ihme.folder,fls))

# ----------------------------------------------------------------- #
# WPP lifetables                                                    #
# ----------------------------------------------------------------- #
# "https://population.un.org/wpp/Download/Standard/Mortality/"
# # Suggested citation: United Nations, Department of Economic and Social Affairs, Population Division (2019). 
# # World Population Prospects: The 2019 Revision, DVD Edition.
# 
# 
# # wpp.url <- "https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_LifeTable.csv"
# wpp.url <-  "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table.csv"
# download.file(url = wpp.url,
# 		# simplify names of zip files...
# 		destfile = file.path(wpp.folder,"WPPLT.csv"))
# 
# WPPLT <- read.csv(file.path(wpp.folder,"WPPLT.csv"))
# WPPLT <- data.table(WPPLT)
# saveRDS(WPPLT, file = file.path(wpp.folder,"WPPLT.rds"))
# rm(WPPLT);gc()
# # repeat for WPP denoms, medium variant (no need for projections)
# # populations in 1000s
# #wpp.url <- "https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_PopulationByAgeSex_Medium.csv"
# wpp.url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv"
# download.file(url = wpp.url,
# 		# simplify names of zip files...
# 		destfile = file.path(wpp.folder,"WPPpop.csv"))
# 
# WPPpop <- readr::read_csv(file.path(wpp.folder,"WPPpop.csv"))
# WPPpop <- data.table(WPPpop)
# saveRDS(WPPpop, file = file.path(wpp.folder,"WPPpop.rds"))
# rm(WPPpop);gc()
# #
# file.remove(file.path(wpp.folder,c("WPPpop.csv","WPPLT.csv")))
# 
# end
# so far that's it, might still want WPP denominators.
