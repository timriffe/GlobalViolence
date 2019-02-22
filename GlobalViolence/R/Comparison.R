# TODO: rerun this after replace mx in old ages with a MortalityLaws extrapolation.
# ggompertz, Beard, MakehamKannisto all options. The cause-specific partition in old 
# ages is inconsequential, so no need to extrapolate it in some clever way.

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
	
	
	
	comp <- horiuchi(mx2sd_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}




DEC <- HIGHVIO[,decomp_sd(.SD),by=list(ISO3,Sex)]

X <- DEC[ISO3=="AFG"&Sex]

#mx2sd_mat(pars1)[Age+1]
#mx2sd_mat(pars2)[Age+1]
sum(X$Mo)
sum(X$Mh)
sum(X$Mw)

pdf(file.path("Figures","GBD","Decomp_sd_Males_FlipBook.pdf"))
for (x in unique(DEC$ISO3)){
	
	X  <- as.matrix(DEC[ISO3==x & Sex == 1,c(4:6)])
	
	Adag <- which(abs(diff(sign(rowSums(X))[40:90]))==2)[1]+40
	X[(Adag+1):111,] <- 0
	X5 <- apply(X,2,groupAges)
	rownames(X5) <- NULL
	
	
	
	p <- colSums(X) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P <- 100}
	barplot(-t(X5),width=5,space=0,border=NA,
			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
			ylim=c(0,2.5))
	axis(1)
	title(paste(x,"Males",P,"%"))
}
dev.off()


pdf(file.path("Figures","GBD","Decomp_sd_Females_FlipBook.pdf"))
for (x in unique(DEC$ISO3)){
	
	X  <- as.matrix(DEC[ISO3==x & Sex == 2,c(4:6)])
	
	Adag <- which(abs(diff(sign(rowSums(X))[40:90]))==2)[1]+40
	X[(Adag+1):111,] <- 0
	X5 <- apply(X,2,groupAges)
	rownames(X5) <- NULL
	
	
	
	p <- colSums(X) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P <- 100}
	barplot(-t(X5),width=5,space=0,border=NA,
			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
			ylim=c(0,1.5))
	axis(1)
	title(paste(x,"Females",P,"%"))
}
dev.off()


# --------------------------------------------------
# better to decompose edagger. better to separate at adagger

mx2edagger_mat <- function(mxmat){
	mx2edagger(rowSums(mxmat))
}

mx2edagger_vec <- function(mxcvec,n=3,Age=10){
	dim(mxcvec) <- c(length(mxcvec)/3,3)
	mx2edagger_mat(mxcvec)[Age+1]
}
decomp_edagger <- function(.SD,Age=10){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	comp <- horiuchi(mx2edagger_vec,c(pars1),c(pars2),N=20,Age=Age)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}

DECed <- HIGHVIO[,decomp_edagger(.SD),by=list(ISO3,Sex)]

# figure out ordering




adaglist <- list()
pdf(file.path("Figures","GBD","Decomp_Edagger_Males_FlipBook.pdf"))
for (x in unique(DECed$ISO3)){
	
	X  <- as.matrix(DECed[ISO3==x & Sex == 1,c(4:6)])
	
	Adag <- which(abs(diff(sign(rowSums(X))[40:90]))==2)[1]+40
	adaglist[[x]] <- Adag
	X[(Adag+1):111,] <- NA
	X[1:10,]          <- NA
	
	p <- colSums(X,na.rm=TRUE) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P <- 100}
	
	X[61:111,] <- NA
	X5 <- apply(X,2,groupAges)
	rownames(X5) <- NULL
	
	NEG <- POS <- X5
	NEG[NEG > 0] <- 0
	POS[POS < 0] <- 0
	NEG[is.na(NEG)] <- 0
	POS[is.na(POS)] <- 0

	barplot(-t(NEG),width=5,space=0,border=NA,
			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
			ylim=c(-.2,1.7))
	barplot(-t(POS),width=5,space=0,border=NA,add=TRUE,
			col=c("#AAAAAA","#bc3905","#87021a"),axes=FALSE)
	axis(1)
	title(paste(x,"Males",P,"%"))
}
dev.off()


pdf(file.path("Figures","GBD","Decomp_Edagger_Females_FlipBook.pdf"))
for (x in unique(DECed$ISO3)){
	
	X  <- as.matrix(DECed[ISO3==x & Sex == 2,c(4:6)])
	
	Adag <- which(abs(diff(sign(rowSums(X))[40:90]))==2)[1]+40
	adaglist[[x]] <- Adag
	X[(Adag+1):111,] <- NA
	X[1:10]          <- NA
	
	p <- colSums(X,na.rm=TRUE) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	
	X[61:111,] <- NA
	X5 <- apply(X,2,groupAges)
	rownames(X5) <- NULL
	
	p <- colSums(X,na.rm=TRUE) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P <- 100}
	barplot(-t(X5),width=5,space=0,border=NA,
			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
			ylim=c(0,1.5))
	axis(1)
	title(paste(x,"Females",P,"%"))
}
dev.off()


# fast temp
edaggTemp <- function(mxcvec,.N=3,a=10,n=60){
	dim(mxcvec) <- c(111,.N)
	mx          <- rowSums(mxcvec)
	n           <- n + 1
	mx          <- mx[-(1:a)][1:n]
	ax          <- rep(.5,n)
	qx          <- mx / (1 + (1 - ax) * mx) 
	lx          <- cumprod(c(1, 1 - qx[-n]))
	Lx          <- (lx[-n] + lx[-1])/2
	Tx          <- Lx2Tx(Lx)
	ex          <- Tx / lx[-n]
	dxx         <- -diff(lx)
	wmean(ex,dxx)
}
decomp_edagger_temp <- function(.SD,Age=10,n=60){
	pars1 <- as.matrix(.SD[,c("Mo","Mh","Mw")])
	pars2 <- as.matrix(.SD[,c("low_Mo","low_Mh","low_Mw")])
	
	comp <- horiuchi(edaggTemp,c(pars1),c(pars2),N=20,a=Age,n=n)
	dim(comp)      <- dim(pars1)
	dimnames(comp) <- dimnames(pars1)
	data.table(data.frame(Age=0:110,comp))
}

DECedt <- HIGHVIO[,decomp_edagger_temp(.SD,n=50),by=list(ISO3,Sex)]


pdf(file.path("Figures","GBD","Decomp_EdaggerT_Males_FlipBook.pdf"))
for (x in unique(DECedt$ISO3)){
	
	X  <- as.matrix(DECedt[ISO3==x & Sex == 1,c(4:6)])
	
	p <- colSums(X,na.rm=TRUE) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	
	X[61:111,] <- NA
	X5 <- apply(X,2,groupAges)
	rownames(X5) <- NULL
	
	p <- colSums(X,na.rm=TRUE) 
	P <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P <- 100}
	barplot(-t(X5),width=5,space=0,border=NA,
			xlim=c(10,60),las=1,col=c("#AAAAAA","#bc3905","#87021a"),
			ylim=c(0,1.5))
	axis(1)
	title(paste(x,"Males",P,"%"))
}
dev.off()



