
# Author: tim
###############################################################################
me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}
source("R/Functions.R")
# install if necessary
#library(devtools)
#install_github("timriffe/DistributionTTD/DistributionTTD/R/DistributionTTD")
#install_github("timriffe/DemoTools")
library(DistributionTTD)
library(DemoTools)
library(data.table)


dir.create(file.path("Data","Results","GBD"), showWarnings = FALSE, recursive = TRUE)


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




#GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",variants[2],".Rdata")))))
#plot(GBDi[Age == 0]$ex,GBDi[Age == 0]$edx, pch = 16, col = "#00000050",cex=.7)
#








