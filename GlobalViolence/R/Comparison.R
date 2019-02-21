
# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)
library(DemoTools)
devtools::load_all("/home/tim/git/DistributionTTD/DistributionTTD/R/DistributionTTD")
source("R/Functions.R")
library(DemoDecomp)

vnt  <- "mid"
yr   <- 2017


GPI      <- read.csv(file.path("Data","Inputs","GPI","GPI_ISO3.csv"),stringsAsFactors=FALSE)
setnames(GPI,"ISO3c","ISO3")
GPI      <- data.table(GPI)

# we calculate our own rank based on score- 
# there were NAs for some reason in original
GPI      <- GPI[type == "score"]

library(reshape2)
acast(GPI, ISO3~year, value.var = "value")


GPI[,rank := rank(value),by=list(year)]
setorder(GPI, year, rank)

# use table because 'times' used as weights.
# e.g. Iceland gets a weight of 11, AUS a weight of 1
GPILOW   <- table(GPI[rank <= 10]$ISO3)

# N highest ranking (25 now)
maxr     <- max(GPI[year==yr]$rank)
GPIHIGH  <- GPI[year==yr & rank > (maxr - 25)]$ISO3

GBD  <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",vnt,".Rdata")))))
setnames(GBD,"ISO","ISO3")
# merge in GPI
GBD  <- merge(GBD,GPI[,c(2,4,5,7)])
setnames(GBD,"value","GPI")

# use table to set weights
GBD$wt <- GPILOW[GBD$ISO3]
GBD$wt[is.na(GBD$wt)] <- 0


wmean <- function(x,w){
	sum(x*w)/sum(w)
}
LOWVIO <- GBD[year == yr, 
		.(M = wmean(M,wt), Mh = wmean(Mh, wt), Mw = wmean(Mw, wt)), 
		by = list(Sex,Age)]

# Mh, Mw, Mo will go into decomp
LOWVIO[,Mo:=M-Mh-Mw]
LOWVIO[,ex:=mx2ex(M),by=.(Sex)]
LOWVIO[,edx:=mx2edagger(M),by=.(Sex)]
LOWVIO[,sdx:=mx2sd(M),by=.(Sex)]

rn <-c("M","Mh","Mw","Mo","ex","edx","sdx")
setnames(LOWVIO,rn,paste0("low_",rn))

HIGHVIO <- GBD[year == yr & ISO3 %in% GPIHIGH]
HIGHVIO[,Mo:=M-Mh-Mw]
HIGHVIO <- merge(HIGHVIO, LOWVIO, by=c("Sex","Age"))

setorder(HIGHVIO,ISO3,Sex,Age)
.SD <- HIGHVIO[ISO3 == "AFG" & Sex == 1]


mx2sd_mat <- function(mxmat){
	mx2sd(rowSums(mxmat))
}

mx2sd_vec <- function(mxcvec,n=3,Age=10){
	dim(mxcvec) <- c(length(mxcvec)/3,3)
	mx2sd_mat(mxcvec)[Age+1]
}
decomp_sd <- function(.SD,Age=10){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	mx2sd_mat(pars1)[Age+1]
	mx2sd_mat(pars2)[Age+1]
	
	comp <- horiuchi(mx2sd_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}

DEC <- HIGHVIO[,decomp_sd(.SD),by=list(ISO3,Sex)]


