
# Author: tim
###############################################################################

# Before working on other sources, need to check how GBD rates closed out using the pclm method.
# older ages may need to be overwritten using MortalityLaws if they are awful.

# This script *follows* DataPrep_Graduate.R for now.

me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}
if (me == "sam\\jmaburto"){
	setwd("C:/Users/jmaburto/Documents/GitHub/GlobalViolence/GlobalViolence/")
}


library(data.table)
library(MortalityLaws)
library(DemoTools)
library(ungroup)
library(reshape2)


GBD.closeout <- function(.SD,
		fit_low = 65, 
		fit_up = 90, 
		extrap_low = 65, 
		omega = 110, 
		law = "kannisto_makeham"){
	
	# this comes from DemoTools, calling MortalityLaws...
	extra_mortality(.SD$M, 
			.SD$Age, 
			x_fit = fit_low:fit_up, 
			x_extr = extrap_low:omega,
			law = law)$values
}




dir.create(file.path("Data","Closeout","GBD"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path("Figures","GBD","Closeout","ggompertz"),recursive=TRUE,showWarnings=FALSE)

variants <- c("low","mid","upp")

# add anything here you like, such as "kannisto_makeham", "gompertz", "ggompertz", blablabla.
laws          <- c("ggompertz")
fit_low       <- c(60,65,70)
fit_upper     <- c(85,90,95)
extrap_low    <- c(60,70,80,90)

#
#.SD <- GBDi[location=="Tajikistan" & Sex == 1 & year == 2002]
#mod <- extra_mortality(.SD$M, .SD$Age, x_fit = 65:90, x_extr = 65:110,law="ggompertz")
#plot(.SD$Age, .SD$M, log = 'y')
#lines(0:110,mod$values)

for (i in 1:3){
	GBDi          <- local(get(load(file.path("Data","Single","GBD",paste0("GBD",variants[i],".Rdata")))))
	GBDi$location <- as.character(GBDi$location)
	locs          <- unique(GBDi$location)
	
# this simply overwrites M
	GBDi[,M:=GBD.closeout(.SD,
					fit_low = 65, 
					fit_up = 90, 
					extrap_low = 65, 
					omega = 110, 
					law = "ggompertz"),
			by = .(location, Sex, year)]
	save(GBDi,file = file.path("Data","Closeout","GBD",
					paste0("GBD",variants[i],"_ggompertz_65_90_65.Rdata")))
	rm(GBDi);gc()
}

# diagnostic flipbooks
for (i in 1:3){
	GBDi <-  local(get(load(
							file.path("Data","Closeout","GBD",
									paste0("GBD",variants[i],"_ggompertz_65_90_65.Rdata")))))
	
	pdf(file.path("Figures","GBD","Closeout","ggompertz",paste0("Diagnostic_GBD",variants[i],"_males_ggompertz_65_90_65.pdf")))
	for (l in 1:length(locs)){
		M <- acast(GBDi[Sex == 1 & location == locs[l]], Age~year, value.var = "M")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	dev.off()
	
	pdf(file.path("Figures","GBD","Closeout","ggompertz",paste0("Diagnostic_GBD",variants[i],"_females_ggompertz_65_90_65.pdf")))
	for (l in 1:length(locs)){
		M <- acast(GBDi[Sex == 2 & location == locs[l]], Age~year, value.var = "M")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	
	
	dev.off()
	rm(GBDi);gc()
}

# make flip book, one page per country,
# one book per sex.


# this plot makes it clear that something needs to change at the pclm location.

pdf(file.path(file.path("Figures","GBD","Closeout","Diagnostic_pclm_males.pdf")))
for (l in 1:length(locs)){
M <- acast(GBDi[Sex==1 & location == locs[l]],Age~year,value.var = "M")
matplot(0:110,M,ylim=c(1e-6,1.5),log='y',type='l',lty=1,col="#00000088",
		main = locs[l])
}
dev.off()


GBDi <- local(get(load(file.path("Data","Grouped","GBD",paste0("GBD",variants[2],".Rdata")))))

loc <- "Tajikistan"

chunk <- GBDi[year == 1995 & location == loc & Sex == 1]
plot(0:110,ungroup.GBD.test(chunk,110,.1)$M,log='y',type='n',ylim=c(1e-7,1.5))
for (y in 1990:2017){
	chunk <- GBDi[year == y & location == loc & Sex == 1]
	lines(0:110,ungroup.GBD.test(chunk,110,1)$M)
}

library(MortalitySmooth)

#ungroup.GBD.ms <- function(.SD,omega=110,lambda=1/1e6){
#	x      <- .SD$Age
#	y      <- .SD$D
#	offset <- y / .SD$M
#	w      <- rep(1,length(offset))
#	w[is.infinite(offset) | offset == 0 |is.nan(offset)] <- 0
#	# ignore infants- keep GBD estimate?
#	w[1]   <- 0
#    mod    <- Mort1Dsmooth(x, y = y, offset = log(offset), w = w, lambda= lambda,method=3)
#	mx     <- exp(predict(mod, newdata=0:110))
#	mx[1] <- .SD$M[1]
#	mx
#}
#
#GBD.pclm <- function(.SD,omega=110,mort = "all"){
#	
#	mtype <- ifelse(mort == "all","",mort)
#	Dx <- paste0("D",mtype)
#	Mx <- paste0("M",mtype)
#	x      <- .SD$Age[-1]
#	y      <- .SD[[Dx]][-1]
#	m      <- .SD[[Mx]][-1]
#	# just retain IMR, no problem
#	m0     <- .SD[[Mx]][1]
#	nlast  <- omega - max(x) + 1
#	offset <- y / m
#	off1   <- pclm(x=x,y=offset,nlast=nlast,control=list(lambda=1/1e6))$fitted
#	fac    <- ifelse(sum(y) > 2e6,10,1)
#	M      <- pclm(x=x,y=y/fac,nlast=nlast,offset=off1/fac,control=list(lambda=1))$fitted
#	M[is.nan(M)] <- 0
#	M      <- c(m0,M)  
#	M
#}


for (y in 1990:2017){
	chunk <- GBDi[year == y & location == "Tajikistan" & Sex == 1]
	plot(c(0,1,seq(5,95,by=5)),chunk$M,log='y',type='s',ylim=c(1e-7,1.5))
	lines(0:110,ungroup.GBD.test(chunk,110,1)$M)
	lines(0:110,GBD.pclm(chunk,110,"all"),col="red")
	locator(1)
}


for (y in 1990:2017){
	chunk <- GBDi[year == y & location == "Tajikistan" & Sex == 1]
	plot(c(0,1,seq(5,95,by=5)),chunk$Mh,log='y',type='s',ylim=c(1e-7,1.5))
	lines(0:110,ungroup.GBD.test(chunk,110,1)$Mh)
	#lines(0:110,GBD.pclm(chunk,110,"h"),col="red")
	lines(0:110,ungroup.GBD.ms(chunk,110,1/1e4,"h"),col="blue")
	lines(0:110,GBD.mono.ms(chunk,110,1/1e4,"h"),col="magenta")
	locator(1)
}

#ungroup.GBD.ms <- function(.SD,omega=110,lambda=1/1e6,mort="all"){
#	
#	mtype <- ifelse(mort == "all","",mort)
#	Dx <- paste0("D",mtype)
#	Mx <- paste0("M",mtype)
#	x      <- .SD$Age[-1]
#	y      <- .SD[[Dx]][-1]
#	m      <- .SD[[Mx]][-1]
#	m0     <- .SD[[Mx]][1]
#	
#	offset <- y / m
#	w      <- rep(1,length(offset))
#	w[is.infinite(offset) | offset == 0 | is.nan(offset)] <- 0
#	# ignore infants- keep GBD estimate?
#	mod    <- Mort1Dsmooth(x, y = y, offset = log(offset), w = w, lambda= lambda,method=3)
#	mx     <- exp(predict(mod, newdata=1:110))
#	mx     <- c(m0,mx)
#	mx
#}
#
#GBD.mono.ms <- function(.SD, omega=110,lambda=.1,mort="all"){
#	mtype <- ifelse(mort == "all","",mort)
#	Dx <- paste0("D",mtype)
#	Mx <- paste0("M",mtype)
#	x      <- .SD$Age
#	y      <- .SD[[Dx]]
#	m      <- .SD[[Mx]]
#	m0     <- .SD[[Mx]][1]
#
#	nlast  <- omega - max(x) + 1
#	offset <- y / m
#	off1   <- splitMono(offset,AgeInt=age2int(Age=x,OAvalue=nlast))
#	m5     <- rep(m,times=age2int(Age=x,OAvalue=nlast))
#	mod    <- Mort1Dsmooth(x=1:110, y = m5[-1]*off1[-1], offset = log(off1[-1]), lambda= lambda,method=3)
#	mx     <- c(m0,exp(predict(mod, newdata=1:110)))
#	mx
#}


#GBDi <- local(get(load(file.path("Data","Grouped","GBD",paste0("GBD",variants[2],".Rdata")))))
#
#M <- acast(GBDi[Sex==1 & location == "Sweden" ],Age~year,value.var = "M")
#a <- c(0,1,seq(5,95,by=5))
#matplot(a,M,ylim=c(1e-7,1.5),log='y',type='l',lty=1,col="#00000088",
#		main = "Sweden")
#library(HMDHFDplus)
#mlt <- readHMDweb("SWE","mltper_5x1",us,pw)
#hmd <- acast(mlt[mlt$Year >=1990,],Age~Year,value.var="mx")
#matplot(c(0,1,seq(5,110,by=5)),hmd,type='l',col="#FF000050",lty=1,add=TRUE)
#
#hmd <- hmd[rownames(M),colnames(M)]
#brks <- pretty(range(t(log(M)-log(hmd))),n=20)
#m <- max(abs(brks))
#brks <- seq(-m,m,by=.05)
#ramp <- colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"),space="Lab")
#
#pdf(file.path("Figures","GBD","Closeout","SwedenMalesGBDvsHMD.pdf"))
#image(1990:2017,a,t(log(M)-log(hmd)),breaks=brks,col=ramp(length(brks)-1),xlab="Year",ylab="Age")
#contour(1990:2017,a,t(log(M)-log(hmd)),breaks=seq(-m,m,by=.4),add=TRUE)
#dev.off()
#
#library(DemoTools)
#
#apply(M,2,LTabr,Age=a,axmethod="un")
