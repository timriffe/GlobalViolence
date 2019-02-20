
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
	lx <- qx2lx(qx,radix=1)
	dx <- lx2dx(lx)
	Lx <- lxdxax2Lx(lx = lx, ndx = dx, nax = ax, AgeInt=rep(1,111))
	Tx <- Lx2Tx(Lx)
	ex <- Tx / lx
	DX <- matrix(dx,nrow=111,ncol=111)
	DX[upper.tri(DX)] <- 0
	colSums(sweep(DX,2,colSums(DX),`/`) * ex)
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

# ISO Codes for mapping. This should be earlier in processing, move at some point.

ISO <- read.csv(file.path("Data","Inputs","GBD","GBD_ISO3.csv"),stringsAsFactors=FALSE)
recvec <- ISO[,2]
names(recvec) <- ISO[,1]

for (i in 1:length(variants)){
	GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",variants[i],".Rdata")))))
	GBDi$ISO3 <- recvec[GBDi$location]
	save(GBDi, file = file.path("Data","Results","GBD",paste0("GBD",variants[i],".Rdata")))
	rm(GBDi);gc()
}

setnames(GBDi,"ISO","ISO3")
GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",variants[i],".Rdata")))))
library(rworldmap)
mapped_data <- joinCountryData2Map(GBDi[Age==0&Sex==1], joinCode = "ISO3", 
		nameJoinColumn = "location")
GBDi$edx[is.nan(GBDi$edx)] <- NA
quantile(GBDi[Age==0]$edx,na.rm=TRUE)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot = "edx")


#GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",variants[2],".Rdata")))))
#plot(GBDi[Age == 0]$ex,GBDi[Age == 0]$edx, pch = 16, col = "#00000050",cex=.7)
#








