
# Author: tim
###############################################################################
me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

devtools::load_all("/home/tim/git/DistributionTTD/DistributionTTD/R/DistributionTTD")
library(DemoTools)
library(data.table)
dir.create(file.path("Data","Results","GBD"), showWarnings = FALSE, recursive = TRUE)

mx2dx <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx2dx(qx2lx(qx))
}

mx2sd <- function(mx){
	dx <- mx2dx(mx)
	sqrt(momentN(dx, n = 2, ax = c(.1,rep(.5,110))))
}

mx2edagger <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx <- qx2lx(qx)
	dx <- lx2dx(lx)
	Lx <- lxdxax2Lx(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- Lx2Tx(Lx)
	ex <- Tx / lx
	fya <- da2fya(dx)
	colSums(fya * ex)
}


# for now just GBD
variants <- c("low","mid","upp")
for (i in 1:length(variants)){
	GBDi <- local(get(load(file.path("Data","Single","GBD",paste0("GBD",variants[i],".Rdata")))))
	GBDi[,sdx:=mx2sd(M),.(location,year,Sex)]
	GBDi[,edx:=mx2edagger(M),.(location,year,Sex)]
	save(GBDi, file = file.path("Data","Results","GBD",paste0("GBD",variants[i],".Rdata")))
	rm(GBDi);gc()
}


