# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)
# get the GBD data
GBD  <- local(get(load(file.path("Data","Results","GBD","GBDmid.Rdata"))))
setnames(GBD,"ISO","ISO3")

# get the GPI data
GPI  <- read.csv(file.path("Data","Inputs","GPI","GPI_ISO3.csv"),stringsAsFactors=FALSE)
setnames(GPI,"ISO3c","ISO3")
GPI  <- data.table(GPI)

# merge, keep only score, don't care about rank per se
GPIi <- GPI[type == "score"]
GBD  <- merge(GBD,GPIi[,c(2,4,5)])
setnames(GBD,"value","GPI")

# change as needed. These are just to get rolling
a <- 10
s <- 2

GBD0 <- GBD0[!is.na(GPI)]
GBD0 <- GBD0[!is.na(edx)]

mod <- lm(sdx~GPI,data=GBD0)
png("/home/tim/git/GlobalViolence/GlobalViolence/Figures/GPIvssd10females.png")
plot(GBD0$GPI, GBD0$sdx, main = paste("slope=",round(mod$coef[2],3), ", cor=",round(cor(GBD0$sdx,GBD0$GPI),3)))
abline(lm(sdx~GPI,data=GBD0))
dev.off()


mod <- lm(sdx~ex,data=GBD0)
png("/home/tim/git/GlobalViolence/GlobalViolence/Figures/exvssd10females.png")
plot(GBD0$ex, GBD0$sdx, main = paste("slope=",round(mod$coef[2],3), ", cor=",round(cor(GBD0$sdx,GBD0$ex),3)))
abline(lm(sdx~ex,data=GBD0))
dev.off()