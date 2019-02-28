# TODO: rerun this after replace mx in old ages with a MortalityLaws extrapolation.
# ggompertz, Beard, MakehamKannisto all options. The cause-specific partition in old 
# ages is inconsequential, so no need to extrapolate it in some clever way.
# TR 28-2-2019: see new script DataPrep_Closeout.R under construction

# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}
if (me == "sam\\jmaburto"){
  setwd("C:/Users/jmaburto/Documents/GitHub/GlobalViolence/GlobalViolence/")
}
library(data.table)
# install if necessary
#library(devtools)
#install_github("timriffe/DistributionTTD/DistributionTTD/R/DistributionTTD")
#install_github("timriffe/DemoTools")
library(DemoTools)
library(DistributionTTD)
library(DemoDecomp)
source("R/Functions.R")

# variant
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

# where X is of dimension (111,3),
# in order Mo, Mh, Mw
plot_testing <- function(X,x,sx="Males",ylim=c(-.1,1.7)){
	
	Adag              <- which(abs(diff(sign(rowSums(X))[40:90]))==2)[1] + 40
	X[(Adag+1):111,]  <- NA
	X[1:10,]          <- NA
	
	p            <- colSums(X,na.rm=TRUE) 
	P            <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P <- 100}
	
	X[61:111,]   <- NA
	X5           <- apply(X,2,groupAges)
	rownames(X5) <- NULL
	
	NEG   <- POS <- X5
	NEG[NEG > 0] <- 0
	POS[POS < 0] <- 0
	NEG[is.na(NEG)] <- 0
	POS[is.na(POS)] <- 0
	
	barplot(-t(NEG),width=5,space=0,border=NA,
			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
			ylim=ylim)
	barplot(-t(POS),width=5,space=0,border=NA,add=TRUE,
			col=c("#AAAAAA","#bc3905","#87021a"),axes=FALSE)
	axis(1)
	title(paste(x,sx,P,"%"))
}


DECsd  <- HIGHVIO[,decomp_sd(.SD),by=list(ISO3,Sex)]
DECed  <- HIGHVIO[,decomp_edagger(.SD),by=list(ISO3,Sex)]
# (i.e. tempoirary edagger 10-60)
#DECedt <- HIGHVIO[,decomp_edagger_temp(.SD,n=50),by=list(ISO3,Sex)]

# plot sd decomp results in flipbooks
pdf(file.path("Figures","GBD","Decomp_sd_Males_FlipBook.pdf"))
for (x in unique(DECsd$ISO3)){
	
	X  <- as.matrix(DECsd[ISO3==x & Sex == 1,c(4:6)])
	plot_testing(X,x,sx="Males",ylim=c(-.1,2.5))
}
dev.off()

pdf(file.path("Figures","GBD","Decomp_sd_Females_FlipBook.pdf"))
for (x in unique(DECsd$ISO3)){
	
	X  <- as.matrix(DECsd[ISO3==x & Sex == 2,c(4:6)])
	plot_testing(X,x,sx="Females",ylim=c(-.1,1.5))
}
dev.off()

# plot edagger decomp results in flipbooks
pdf(file.path("Figures","GBD","Decomp_Edagger_Males_FlipBook.pdf"))
for (x in unique(DECed$ISO3)){
	
	X  <- as.matrix(DECed[ISO3==x & Sex == 1,c(4:6)])
	plot_testing(X,x,sx="Males",ylim=c(-.1,1.7))
}
dev.off()


pdf(file.path("Figures","GBD","Decomp_Edagger_Females_FlipBook.pdf"))
for (x in unique(DECed$ISO3)){
	
	X  <- as.matrix(DECed[ISO3==x & Sex == 2,c(4:6)])
	plot_testing(X,x,sx="Females",ylim=c(-.1,1))
}
dev.off()






#pdf(file.path("Figures","GBD","Decomp_EdaggerT_Males_FlipBook.pdf"))
#for (x in unique(DECedt$ISO3)){
#	
#	X  <- as.matrix(DECedt[ISO3==x & Sex == 1,c(4:6)])
#	
#	p <- colSums(X,na.rm=TRUE) 
#	P <- round(sum(p[2:3])/sum(p)*100,1)
#	
#	X[61:111,] <- NA
#	X5 <- apply(X,2,groupAges)
#	rownames(X5) <- NULL
#	
#	p <- colSums(X,na.rm=TRUE) 
#	P <- round(sum(p[2:3])/sum(p)*100,1)
#	if (P > 100){P <- 100}
#	barplot(-t(X5),width=5,space=0,border=NA,
#			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
#			ylim=c(0,1.5))
#	axis(1)
#	title(paste(x,"Males",P,"%"))
#}
#dev.off()



