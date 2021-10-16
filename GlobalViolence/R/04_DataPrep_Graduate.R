
# Author: tim
###############################################################################

# In the first place, this script will be written for the GBD
# data (MID, LOW, UPP), but later it'll be expanded as needed 
# for the comparison datasets.

library(here)
#install.packages('ungroup')
library(data.table)
library(ungroup)
library(DemoTools)
library(MortalitySmooth)
library(reshape2)
library(magrittr)
library(parallel)
library(dplyr)
 
if(.Platform$OS.type == "windows"){
  library(parallelsugar)
} 

source(here("GlobalViolence","R","01_Functions.R"))

dir.create(here("GlobalViolence","Data","Single","GBD"), showWarnings = FALSE, recursive = TRUE)

gbd.folder <- here("GlobalViolence","Data","Grouped","GBD")

variants <- c("low","mid","upp")

# takes a long time to run, approaches 12Gb memory a bit. If you don't have >= 12Gb mem,
# you can change this:
# mc.cores = (detectCores() - 1)
# to use fewer cores. It'll work even if mc.cores is 1 or 2.

for (i in 1:length(variants)){
  GBDi <- readRDS(file.path(gbd.folder, paste0("GBD",variants[i],".rds"))) 
  GBDi %>% 
    split(list(GBDi$location,GBDi$sex,GBDi$year), drop = TRUE) %>% 
    mclapply(GBD.chunk,mc.cores = (detectCores() - 1)) %>% 
	  rbindlist() %>% 
	  saveRDS(file = here("GlobalViolence","Data","Single","GBD",paste0("GBD",variants[i],".rds")))
	gc()
}


# these are not 'finalized' still, as the pclm closeout isn't demographically informed, 
# and can go haywire. Next step DataPrep_Closeout.R
dir.create(file.path("Figures","GBD","Closeout","pclm"), showWarnings = FALSE, recursive = TRUE)

locs<- readRDS(file.path(gbd.folder, paste0("GBD",variants[i],".rds"))) %>%
  pull(location) %>% unique()
# diagnostic flipbooks
for (i in 1:3){
	GBDi <- readRDS(here("GlobalViolence","Data","Single","GBD",paste0("GBD",variants[i],".rds")))
	GBDi <- GBDi %>% 
	  mutate(sex = as.character(sex),
	         location = as.character(location))
	
	pdf(here("GlobalViolence","Figures","GBD","Closeout","pclm",paste0("Diagnostic_GBD",variants[i],"males_pclm.pdf")))
	for (l in 1:length(locs)){
		M <- acast(filter(GBDi,
		                  sex == "Male",
		                  location == locs[l]), age~year, value.var = "Ma")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	dev.off()
	
	pdf(here("GlobalViolence","Figures","GBD","Closeout","pclm",paste0("Diagnostic_GBD",variants[i],"_females_pclm.pdf")))
	for (l in 1:length(locs)){
		M <- acast(filter(GBDi,
		                  sex == "Female",
		                  location == locs[l]), age~year, value.var = "Ma")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	dev.off()
	
	rm(GBDi);gc()
}


# end
# ---------------------------

# deprecated:
#graduateSmall <- function(x,y,off1,nlast){
#	AgeInt <- age2int(x,OAvalue=nlast)
#	if (sum(y) < 100){
#		Mi <- splitUniform(Value = y,AgeInt = AgeInt)
#		return(Mi / off1)
#	} else {
#		
#		Mi     <- splitMono(Value = y,AgeInt = AgeInt)
#		Mii    <- beers(Mi,Age=0:110)
#		ind <- Mii < 0
#		if (any(ind)){
#			Mii[ind] <- Mi[ind]
#		}
#		
#		return(rescaleAgeGroups(Mii,rep(1,111),y,AgeInt,recursive=FALSE,splitfun=splitUniform) / off1)
#	}
#}
#ungroup.GBD <- function(.SD,omega=110){
#	x      <- .SD$Age[-1]
#	y      <- .SD$D[-1]
#	m      <- .SD$M[-1]
#	# just retain IMR, no problem
#	m0     <- .SD$M[1]
#	nlast  <- omega - max(x) + 1
#	offset <- y / m
#	off1   <- pclm(x=x,y=offset,nlast=nlast,control=list(lambda=1/1e6))$fitted
#	fac <- ifelse(sum(.SD$D) > 2e6,10,1)
#	M  <- pclm(x=x,y=y/fac,nlast=nlast,offset=off1/fac,control=list(lambda=1/1e6))$fitted
#	M[is.nan(M)] <- 0
#	Mh <- c(graduateSmall(x,.SD$Dh,off1,nlast))
#	Mw <- c(graduateSmall(x,.SD$Dw,off1,nlast))
#	data.table(data.frame(
#					location=rep(.SD$location[1],111),
#					year=rep(.SD$year[1],111),
#					Sex =rep(.SD$Sex[1],111),
#					Age=0:110,
#					M=M,Mh=Mh,Mw=Mw))
#}