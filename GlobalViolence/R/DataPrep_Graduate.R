
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

# use this for all-cause mortality graduation.
GBD.pclm <- function(.SD, omega = 110, mort = "a"){
	
	# determine which variables to use
	Dx     <- paste0("D",mort)
	Mx     <- paste0("M",mort)
	
	# siphon off vectors to use, throw out IMR
	x      <- .SD$age[-1]
	y      <- .SD[[Dx]][-1]
	m      <- .SD[[Mx]][-1]
	
	n      <- nrow(.SD)
	
	if (all(is.na(m))){
	  return(rep(NA,111))
	}
	# just retain IMR, no problem
	m0     <- .SD[[Mx]][1]
	
	if (any(is.na(y)) | any(is.na(m))){
	  rmind <- is.na(y) | is.na(x)
	}
	
	
	# how wide open interval, for practical purposes
	nlast  <- omega - max(x) + 1
	
	# back out exposure
	offset <- y / m
	
	# split exposure using pclm...
	off1   <- pclm(x = x, y = offset, nlast = nlast, control = list(lambda = 1 / 1e6))$fitted
	
	# possibly deflate counts & exposures for splitting- this was breaking
	# in some very large populations. Odd.
	fac    <- ifelse(sum(y) > 2e6,10,1)
	
	# split counts using split exposure as offset returns rates
	M      <- pclm(x = x, 
			           y = y / fac, 
				         nlast = nlast, 
				         offset = off1 / fac, 
				         control = list(lambda = 1))$fitted
	M[is.nan(M)] <- 0
	
	# append to saved IMR
	M      <- c(m0, M)  
	M
}

# use this for homicide and 'other violence' graduation
GBD.mono.ms <- function(.SD, omega=110,lambda=.1,mort="h"){
	
  # determine which variables to use
  Dx     <- paste0("D",mort)
  Mx     <- paste0("M",mort)
	
	# siphen off vectors
	x      <- .SD$age
	y      <- .SD[[Dx]]
	m      <- .SD[[Mx]]
	
	# backstop in case no counts
	if (all(is.na(m))){
	  return(rep(NA,111))
	}
	
	if (all(m == 0)){
		return(rep(0,111))
	}

	
	# don't touch infant mort. Derivative too high, messes up splines
	# of all kinds. Until Carl Schmertmann boxes up his nifty solution.
	m0     <- .SD[[Mx]][1]
	
	# over how many ages do we want to distribute the open age group?
	# Here no one lives beyond 110
	nlast  <- omega - max(x) + 1
	
	# back out exposure from rates & counts. Sometimes it's hideous.
	offset <- y / m
	
	# split it to single ages using monotonic spline over cumulative exposure.
	# it's fast and sufficient, since we only use this as an offset.
	# TR: this way we let nlast do the work
	off1   <- graduate_mono(offset, 
	                        AgeInt = age2int(Age = x, OAvalue = nlast), 
	                        Age = x,
	                        OAG = FALSE)[-1] 
	off1[off1<0] <- 0
	
	# spread rates over single ages within intervals
	m5     <- rep(m, times = age2int(Age = x, OAvalue = nlast))

	# assign 0 weights to pathological cases
	w      <- rep(1,110)
	ind    <- off1 == 0 | is.na(off1) | is.infinite(off1)
	w[ind] <- 0
	
	# first step, 'blocky' hypothetical counts that we'll smooth
	d      <- m5[-1] * off1
	d[ind] <- 0
	
	# if rates are too low things break ¯\_(ツ)_/¯
    # so we keep the 'blocky' rates
	if (floor(sum(d)) < 10){
		return(m5)
	} 
	
	# But there are some cases where we can make the smoother 
    # work by inflating counts+rates. So the trick is to inflate,
    # then smooth, then deflate by sae factor
	if (sum(d) < 100){
		fac <- 100
	} else {
		fac <- 1
	}
	d      <- d * fac
	
	# The smoother function. Sometimes it complains even though
    # it finds an acceptible solution.
	mod    <- suppressWarnings(Mort1Dsmooth(
			    x = 1:110, 
			    y = d, 
			    offset = log(off1), 
			    w = w, 
			    lambda = lambda, # gives as arg
			    method = 3))
    
    # append smoothed rates (deflated) to saved infant mort
	mx     <- c(m0, exp(mod$logmortality) / fac)
	mx
}

# combines the prior two, operating on and returning a chunk
GBD.chunk <- function(.SD,omega=110){
	
	# graduate the 3 mortality vectors.
	# I really wish I knew of a way to do a compositional graduation
	# of all 3 at once...
	M  <- suppressWarnings(GBD.pclm(.SD, omega = 110,mort = "a"))
	Mh <- GBD.mono.ms(.SD, omega = 110, mort = "h")
	Mw <- GBD.mono.ms(.SD, omega = 110, mort = "w")
	# Mh + Mw are here not guaranteed to be < M,
	# so will have to check for that. Unlikely as
	# they are most often orders of magnitude different
	
	data.table(data.frame(
	        ISO3 = rep(.SD$ISO3[1],111),
					location = rep(.SD$location[1], 111),
					year = rep(.SD$year[1], 111),
					sex = rep(.SD$sex[1], 111),
					age = 0:110,
					Ma = M, Mh = Mh, Mw = Mw))
}

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