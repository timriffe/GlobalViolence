# Author: tim
###############################################################################

library(here)
library(data.table)
# install if necessary
#library(devtools)
#install_github("timriffe/DistributionTTD/DistributionTTD/R/DistributionTTD")
#install_github("timriffe/DemoTools")
library(DemoTools)
library(DistributionTTD)
library(DemoDecomp)
library(readr)
library(tidyverse)
source(here("GlobalViolence","R","Functions.R"))

dir.create(here("GlobalViolence","Figures","GBD","Decomp"), showWarnings = FALSE, recursive = TRUE)
# variant
vnt  <- "mid"
yr   <- 2017

# TR 17-2-2020: I didn't have this file :-/ 
# Code from here down not rerun
GPI      <- read_csv(here("GlobalViolence","Data","Inputs","GPI","GPI_ISO3.csv"))
setnames(GPI,"ISO3c","ISO3")
GPI      <- data.table(GPI)

# we calculate our own rank based on score- 
# there were NAs for some reason in original
# GPI      <- GPI[type == "score"]

# library(reshape2)
# acast(GPI, ISO3~year, value.var = "value")
# GPI

GPI[,rank := rank(score),by=list(year)]
setorder(GPI, year, rank)

# use table because 'times' used as weights.
# e.g. Iceland gets a weight of 11, AUS a weight of 1
GPILOW   <- table(GPI[rank <= 10]$ISO3)
ISOLOW <- names(GPILOW)
GPILOW <- as.vector(GPILOW)
names(GPILOW) <- ISOLOW
# N highest ranking (25 now)
maxr     <- max(GPI[year==yr]$rank)
GPIHIGH  <- GPI[year==yr & rank > (maxr - 25)]$ISO3


GBD  <- readRDS(here("GlobalViolence","Data","Results","GBD",paste0("GBD",vnt,".rds")))
GBD$sex <- as.character(GBD$sex)
#setnames(GBD,"ISO","ISO3")
# merge in GPI
GPI <- GPI %>% select(ISO3,year, rank,score)
GBD <- GBD %>% left_join(GPI)

setnames(GBD,"score","GPI")

# use table to set weights

GBD$wt <- GPILOW[GBD$ISO3]

LOWVIO <- GBD[year == yr & !is.na(wt)]

LOWVIO <- LOWVIO[, 
              .(Ma = wmean(Ma,wt), Mh = wmean(Mh, wt), Mw = wmean(Mw, wt)), 
	          	by = .(sex,age)]

LOWVIO %>% 
  ggplot(aes(x=age,y=Ma,color=sex)) +
  geom_line() + 
  scale_y_log10()

# Mh, Mw, Mo will go into decomp
LOWVIO[,Mo:=Ma-Mh-Mw]
LOWVIO[,ex:=mx2ex(Ma),by=.(sex)]
LOWVIO[,edx:=mx2edagger(Ma),by=.(sex)]
LOWVIO[,sdx:=mx2sd(Ma),by=.(sex)]

rn <-c("Ma","Mh","Mw","Mo","ex","edx","sdx")
setnames(LOWVIO,rn,paste0("low_",rn))

HIGHVIO <- GBD[year == yr & ISO3 %in% GPIHIGH]
HIGHVIO[,Mo:=Ma-Mh-Mw]
HIGHVIO <- merge(HIGHVIO, LOWVIO, by=c("sex","age"))

setorder(HIGHVIO,ISO3,sex,age)

# per email request:
GPIHIGH2 <- c("SYR", "IRQ", "SLV", "ZAF", "VEN", "GTM", "THA", "CAF", "SSD", "COL", "ZWE", "MEX", "GNB", "RUS", "NGA", "ERI", "CMR", "SOM", "CIV", "KEN", "TCD", "YEM", "LBY", "COG", "UKR", "BDI", "MLI", "NER", "AFG")

HIGHVIO2 <- GBD[year == yr & ISO3 %in% GPIHIGH2]
HIGHVIO2[,Mo:=Ma-Mh-Mw]
HIGHVIO2 <- merge(HIGHVIO2, LOWVIO, by=c("sex","age"))
setorder(HIGHVIO2,ISO3,sex,age)
# where X is of dimension (111,3),
# in order Mo, Mh, Mw
plot_testing <- function(X,x,sx="Males",ylim=c(-.1,1.7)){
	
	Adag              <- which(abs(diff(sign(rowSums(X))[40:90]))==2)[1] + 40
	X[(Adag+1):111,]  <- NA
	X[1:10,]          <- NA
	
	p               <- colSums(X,na.rm=TRUE) 
	P               <- round(sum(p[2:3])/sum(p)*100,1)
	if (P > 100){P  <- 100}
	
	X[61:111,]      <- NA
	X5              <- apply(X,2,groupAges)
	rownames(X5)    <- NULL
	
	NEG   <- POS    <- X5
	NEG[NEG > 0]    <- 0
	POS[POS < 0]    <- 0
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

# this is the decomposition code, can take a long time.
DECsd  <- HIGHVIO[,decomp_sd(.SD),by=list(ISO3,sex)]
save(DECsd,file=here("GlobalViolence","Data","Results","GBD","DECsd10_HIGHVIO.rds"))

DECsd2  <- HIGHVIO2[,decomp_sd(.SD),by=list(ISO3,sex)]
save(DECsd2,file=here("GlobalViolence","Data","Results","GBD","DECsd10_HIGHVIO2.rds"))


DECed  <- HIGHVIO[,decomp_edagger(.SD),by=list(ISO3,sex)]
save(DECed,file=here("GlobalViolence","Data","Results","GBD","DECed10_HIGHVIO.rds"))

DECed2  <- HIGHVIO2[,decomp_edagger(.SD),by=list(ISO3,sex)]
save(DECed2,file=here("GlobalViolence","Data","Results","GBD","DECed10_HIGHVIO2.rds"))

# (i.e. tempoirary edagger 10-60)
#DECedt <- HIGHVIO[,decomp_edagger_temp(.SD,n=50),by=list(ISO3,Sex)]

# plot sd decomp results in flipbooks
pdf(here("GlobalViolence","Figures","GBD","Decomp","Decomp_sd_Males_FlipBook.pdf"))
for (x in unique(DECsd$ISO3)){
	
	X  <- as.matrix(DECsd[ISO3==x & sex == "Male",c(4:6)])
	plot_testing(X,x,sx="Males",ylim=c(-.1,2.5))
}
dev.off()

pdf(here("GlobalViolence","Figures","GBD","Decomp","Decomp_sd_Females_FlipBook.pdf"))
for (x in unique(DECsd$ISO3)){
	
	X  <- as.matrix(DECsd[ISO3==x & Sex == "Female",c(4:6)])
	plot_testing(X,x,sx="Females",ylim=c(-.1,1.5))
}
dev.off()

# plot edagger decomp results in flipbooks
pdf(here("GlobalViolence","Figures","GBD","Decomp","Decomp_Edagger_Males_FlipBook.pdf"))
for (x in unique(DECed$ISO3)){
	
	X  <- as.matrix(DECed[ISO3==x & Sex == 1,c(4:6)])
	plot_testing(X,x,sx="Males",ylim=c(-.1,1.7))
}
dev.off()


pdf(here("GlobalViolence","Figures","GBD","Decomp","Decomp_Edagger_Females_FlipBook.pdf"))
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



