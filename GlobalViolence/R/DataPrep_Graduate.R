
# Author: tim
###############################################################################

# In the first place, this script will be written for the GBD
# data (MID, LOW, UPP), but later it'll be expanded as needed 
# for the comparison datasets.

me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)
library(ungroup)
library(DemoTools)
library(parallel)

graduateSmall <- function(x,y,off1,nlast){
	AgeInt <- age2int(x,OAvalue=nlast)
	if (sum(y) < 100){
		Mi <- splitUniform(Value = y,AgeInt = AgeInt)
		return(Mi / off1)
	} else {
		
		Mi     <- splitMono(Value = y,AgeInt = AgeInt)
		Mii    <- beers(Mi,Age=0:110)
		ind <- Mii < 0
		if (any(ind)){
			Mii[ind] <- Mi[ind]
		}
		
		return(rescaleAgeGroups(Mii,rep(1,111),y,AgeInt,recursive=FALSE,splitfun=splitUniform) / off1)
	}
}
ungroup.GBD <- function(.SD,omega=110){
	x      <- .SD$Age
	nlast  <- omega - max(x) + 1
	offset <- .SD$D / .SD$M
	off1   <- pclm(x=x,y=offset,nlast=nlast,control=list(lambda=1/1e6))$fitted
	fac <- ifelse(sum(.SD$D) > 2e6,10,1)
	M  <- pclm(x=x,y=.SD$D/fac,nlast=nlast,offset=off1/fac,control=list(lambda=1/1e6))$fitted
	M[is.nan(M)] <- 0
	Mh <- c(graduateSmall(x,.SD$Dh,off1,nlast))
	Mw <- c(graduateSmall(x,.SD$Dw,off1,nlast))
	data.table(data.frame(
					location=rep(.SD$location[1],111),
					year=rep(.SD$year[1],111),
					Sex =rep(.SD$Sex[1],111),
					Age=0:110,
					M=M,Mh=Mh,Mw=Mw))
}

dir.create(file.path("Data","Single","GBD"), showWarnings = FALSE, recursive = TRUE)

gbd.folder <- file.path("Data","Grouped","GBD")

variants <- c("low","mid","upp")

# takes a long time to run
for (i in 1:length(variants)){
	GBDi <- local(get(load(file.path("Data","Grouped","GBD",paste0("GBD",variants[i],".Rdata")))))
	sdl  <- split(GBDi,list(GBDi$location,GBDi$Sex,GBDi$year))
	sdl  <- mclapply(sdl, ungroup.GBD, mc.cores = 3)
	GBDi <- rbindlist(sdl)
	rm(sdl);gc()
	save(GBDi,file=file.path("Data","Single","GBD",paste0("GBD",variants[i],".Rdata")))
	rm(GBDi);gc()
}

# end
# ---------------------------

