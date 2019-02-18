
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

dir.create(file.path("Data","Single","GBD"), showWarnings = FALSE, recursive = TRUE)

gbd.folder <- file.path("Data","Grouped","GBD")

variants <- c("low","mid","upp")

sdl <- split(GBDi,list(GBDi$location,GBDi$year,GBDi$Sex))
sdl[[1]]
tests <- sample(1:length(sdl),size=100,replace=FALSE)
pclm.lamda <- function(.SD,omega=110){
	x      <- .SD$Age
	nlast  <- omega - max(x) + 1
	offset <- .SD$D / .SD$M
	l1 <- pclm(x=x,y=.SD$D,nlast=nlast,offset=offset)$smoothPar["lambda"]
	l2 <- pclm(x=x,y=.SD$Dh,nlast=nlast,offset=offset)$smoothPar["lambda"]
	l3 <- pclm(x=x,y=.SD$Dw,nlast=nlast,offset=offset)$smoothPar["lambda"]
	c(l1,l2,l3)
}
logit <- function(x){
	log(x/(1-x))
}
expit <- function(x){
	1/(1+exp(-x))
}
expit(logit(.1))
ungroup.GBD <- function(.SD,omega=110){
	x      <- .SD$Age
	nlast  <- omega - max(x) + 1
	offset <- .SD$D / .SD$M
	
	l1     <- pclm(x=x,y=.SD$D,nlast=nlast,offset=offset,control=list(lambda=1/1e6))$fitted
	plot(splitMono(Value = .SD$Dh,AgeInt = age2int(x,OAvalue=nlast))/
			splitMono(Value = offset,AgeInt = age2int(x,OAvalue=nlast)))
lines(a,)
	
}
?parallel::mclapply
lambdas <- do.call("rbind",parallel::mclapply(sdl[tests],pclm.lamda,mc.cores=3))

for (i in 1:length(variants)){
GBDi <- local(get(load(file.path(gbd.folder,paste0("GBD",variants[i],".Rdata")))))

# takes a very long time to run! Not memory intensive though.
# Go enjoy a coffee. Take your time. Try that new cafe around
# the corner. 
GBDi <- GBDi[,pclm.SD(.SD),.(location,year,Sex)]
# Data  

save(GBDi,file=file.path("Data","Single","GBD",paste0("GBD",variants[i],".Rdata")))
}
