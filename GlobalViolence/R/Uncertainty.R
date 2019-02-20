
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
	lx2dx(qx2lx(qx,radix=1))
}

mx2sd <- function(mx){
	dx  <- mx2dx(mx)
	vx  <- momentN(dx, n = 2, ax = c(.1,rep(.5,110)))
	sdx <- suppressWarnings(sqrt(vx))
	sdx
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
mx2ex <- function(mx){
	ax <- c(.1,rep(.5,110))
	qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
	lx <- qx2lx(qx)
	dx <- lx2dx(lx)
	Lx <- lxdxax2Lx(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- Lx2Tx(Lx)
	Tx / lx
}

# for now just GBD
variants <- c("low","mid","upp")
for (i in 1:length(variants)){
	GBDi <- local(get(load(file.path("Data","Single","GBD",paste0("GBD",variants[i],".Rdata")))))
	GBDi[,sdx:=mx2sd(M),.(location,year,Sex)]
	GBDi[,sdx_no_h:=mx2sd(M-Mh),.(location,year,Sex)]
	GBDi[,sdx_no_hw:=mx2sd(M-Mh-Mw),.(location,year,Sex)]
	GBDi[,edx:=mx2edagger(M),.(location,year,Sex)]
	GBDi[,edx_no_h:=mx2edagger(M-Mh),.(location,year,Sex)]
	GBDi[,edx_no_hw:=mx2edagger(M-Mh-Mw),.(location,year,Sex)]
	GBDi[,ex:=mx2ex(M),.(location,year,Sex)]
	GBDi[,ex_no_h:=mx2ex(M-Mh),.(location,year,Sex)]
	GBDi[,ex_no_hw:=mx2ex(M-Mh-Mw),.(location,year,Sex)]
	save(GBDi, file = file.path("Data","Results","GBD",paste0("GBD",variants[i],".Rdata")))
	rm(GBDi);gc()
}


GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",variants[2],".Rdata")))))

plot(GBDi[Age == 15]$ex,GBDi[Age == 15]$edx, pch = 16, col = "#00000050",cex=.7)
ex <- mx2ex(GBDi[year==2000&location=="United States"&Sex==2]$M)
dx <- mx2dx(GBDi[year==2000&location=="United States"&Sex==2]$M)
plot(dx)