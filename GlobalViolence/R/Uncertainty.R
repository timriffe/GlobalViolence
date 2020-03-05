# Note: now we use data after DataPrep_Closeout.R
# Author: tim
###############################################################################

source(here("GlobalViolence","R","Functions.R"))
# install if necessary
library(devtools)
#install_github("timriffe/DistributionTTD/DistributionTTD/R/DistributionTTD")
#install_github("timriffe/DemoTools")
library(here)
library(DistributionTTD)
library(DemoTools)
library(data.table)




dir.create(here("GlobalViolence","Data","Results","GBD"), showWarnings = FALSE, recursive = TRUE)

# for now just GBD
variants <- c("low","mid","upp")
for (i in 1:length(variants)){
	cat(i,"\n")
	# choose an explicit closeout file
  # don´t know why but for me "sex" was saved without capital "s"
  # also now you add and "a" before all causes of death (check sequence from DataPrep)..
  
	GBDi <- readRDS(here("GlobalViolence","Data","Closeout","GBD",
									paste0("GBD",variants[i],"_ggompertz_65_90_65.rds")))
	GBDi[,sdx:=mx2sd(Ma),.(location,year,sex)]
	GBDi[,sdx_no_h:=mx2sd(Ma-Mh),.(location,year,sex)]
	GBDi[,sdx_no_hw:=mx2sd(Ma-Mh-Mw),.(location,year,sex)]
	GBDi[,edx:=mx2edagger(Ma),.(location,year,sex)]
	GBDi[,edx_no_h:=mx2edagger(Ma-Mh),.(location,year,sex)]
	GBDi[,edx_no_hw:=mx2edagger(Ma-Mh-Mw),.(location,year,sex)]
	GBDi[,ex:=mx2ex(Ma),.(location,year,sex)]
	GBDi[,ex_no_h:=mx2ex(Ma-Mh),.(location,year,sex)]
	GBDi[,ex_no_hw:=mx2ex(Ma-Mh-Mw),.(location,year,sex)]
	saveRDS(GBDi, file = here("GlobalViolence","Data","Results","GBD",paste0("GBD",variants[i],".rds")))
	rm(GBDi);gc()
}
A <- readRDS(file = here("GlobalViolence","Data","Results","GBD",paste0("GBD",variants[i],".rds")))


# Next file, for example Comparison.R or Relationships.R
# end
# --------------------------------

#GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",variants[2],".Rdata")))))
#plot(GBDi[Age == 0]$ex,GBDi[Age == 0]$edx, pch = 16, col = "#00000050",cex=.7)
#

# ------------------------------
# Sex ratio check
#GBDi <- local(get(load(file.path("Data","Results","GBD",paste0("GBDmid.Rdata")))))
#
#mx2lx <- function(mx){
#ax <- c(.1,rep(.5,110))
#qx <- mxax2qx(nMx=mx, nax=ax, AgeInt=rep(1,111), closeout = TRUE,IMR=NA)
#lx <- qx2lx(qx,radix=1)
#}
#asdrlt <- function(mx,wx){
#	lx <- mx2lx(mx)
#	wmean(wx,lx)
#}
#
#HM <- GBDi[,.(asdrMw = asdrlt(M,Mh)),by=.(ISO,Sex,year)]
#SRW <- function(X){
#	X$asdrMw[X$Sex==1] / X$asdrMw[X$Sex==2]
#}
#SR <- HM[,.(SRw = SRW(.SD)),by=.(ISO,year)]
#
#mean(SR$SRw)
#SRy <- SR[,.(SRw = mean(SRw)),by=.(year)]
#plot(SRy)

