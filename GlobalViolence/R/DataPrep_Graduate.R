
# Author: tim
###############################################################################

# In the first place, this script will be written for the GBD
# data (MID, LOW, UPP), but later it'll be expanded as needed 
# for the comparison datasets.

me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}
if (me == "sam\\jmaburto"){
  setwd("C:/Users/jmaburto/Documents/GitHub/GlobalViolence/GlobalViolence/")
}

#install.packages('ungroup')
library(data.table)
library(ungroup)
library(DemoTools)
library(MortalitySmooth)
ifelse(me == "tim", library(parallel),library(parallelsugar))


# use this for all-cause mortality graduation.
GBD.pclm <- function(.SD, omega = 110, mort = "all"){
	
	# determine which variables to use
	mtype  <- ifelse(mort == "all","",mort)
	Dx     <- paste0("D",mtype)
	Mx     <- paste0("M",mtype)
	
	# siphen off vectors to use, throw out IMR
	x      <- .SD$Age[-1]
	y      <- .SD[[Dx]][-1]
	m      <- .SD[[Mx]][-1]
	
	# just retain IMR, no problem
	m0     <- .SD[[Mx]][1]
	
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
	
	# form relevant variable names
	mtype  <- ifelse(mort == "all","",mort)
	Dx     <- paste0("D",mtype)
	Mx     <- paste0("M",mtype)
	
	# siphen off vectors
	x      <- .SD$Age
	y      <- .SD[[Dx]]
	m      <- .SD[[Mx]]
	
	# backstop in case no counts
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
	off1   <- splitMono(offset, AgeInt = age2int(Age = x, OAvalue = nlast))[-1]
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
	M  <- suppressWarnings(GBD.pclm(.SD, omega = 110,mort = "all"))
	Mh <- GBD.mono.ms(.SD, omega = 110, mort = "h")
	Mw <- GBD.mono.ms(.SD, omega = 110, mort = "w")
	# Mh + Mw are here not guaranteed to be < M,
	# so will have to check for that. Unlikely as
	# they are most often orders of magnitude different
	
	data.table(data.frame(
					location = rep(.SD$location[1], 111),
					year = rep(.SD$year[1], 111),
					Sex = rep(.SD$Sex[1], 111),
					Age = 0:110,
					M = M, Mh = Mh, Mw = Mw))
}


dir.create(file.path("Data","Single","GBD"), showWarnings = FALSE, recursive = TRUE)

gbd.folder <- file.path("Data","Grouped","GBD")

variants <- c("low","mid","upp")

# takes a long time to run
for (i in 1:length(variants)){
	GBDi <- local(get(load(file.path("Data","Grouped","GBD",paste0("GBD",variants[i],".Rdata")))))
	sdl  <- split(GBDi,list(GBDi$location,GBDi$Sex,GBDi$year))
	sdL  <- mclapply(sdl, GBD.chunk, mc.cores = 3)
	GBDi <- rbindlist(sdL)
	rm(sdl);gc()
	save(GBDi,file=file.path("Data","Single","GBD",paste0("GBD",variants[i],".Rdata")))
	rm(GBDi);gc()
}

# these are not 'finalized' still, as the pclm closeout isn't demographically informed, 
# and can go haywire. Next step DataPrep_Closeout.R

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