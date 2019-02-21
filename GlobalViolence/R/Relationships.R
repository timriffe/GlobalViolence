# Author: tim
###############################################################################

me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(data.table)


# get the GPI data
GPI      <- read.csv(file.path("Data","Inputs","GPI","GPI_ISO3.csv"),stringsAsFactors=FALSE)
setnames(GPI,"ISO3c","ISO3")
GPI      <- data.table(GPI)
GPIi <- GPI[type == "score"]


# loop over variable combinations
ages     <- c(0,10,15)
variants <- c("low","mid","upp")
sexes    <- c(1,2)
disps    <- c("sdx","edx")



dir.create(file.path("Figures","GBD"), showWarnings = FALSE,recursive=TRUE,)
# vnt <- "low"; a <- 0; s <- 1; d <- "edx"
for (vnt in variants){ # low, mid, upp
	
	# get the GBD data
	GBD  <- local(get(load(file.path("Data","Results","GBD",paste0("GBD",vnt,".Rdata")))))
	setnames(GBD,"ISO","ISO3")
	# merge in GPI
	GBD  <- merge(GBD,GPIi[,c(2,4,5)])
	setnames(GBD,"value","GPI")
	
	
	for (a in ages){      # 0, 10, 15
		for (s in sexes){    # 1, 2
			sx  <- ifelse(s==1,"males","females")
			
			
			
			for (d in disps){    # sdx, edx
				
				# subset for sex and age, fresh
				GBD0 <- GBD[Sex==s & Age == a]
						
				# change name of disparity measure to "y", for ease
				setnames(GBD0,d,"y")
				
				# remove NAs for correlations
				GBD0 <- GBD0[!is.na(GPI)]
				GBD0 <- GBD0[!is.na(y)]
				
				# ---------------------
				namei <- paste0("GPIx",d,"_",a,sx,"_",vnt,".pdf")
				path  <- file.path("Figures","GBD",namei)
				
				mod <- lm(y~GPI,data=GBD0)
				
				pdf(path)
				plot(GBD0$GPI, GBD0$y, 
						main = paste("slope=",round(mod$coef[2],3), ", cor=",round(cor(GBD0$y,GBD0$GPI),3)),
						xlab = "GPI", ylab = paste0("sd(",a,")"),pch = 16, col = "#AAAAAA55", las = 1,ylim=c(5,30))
				abline(mod)
				dev.off()
				
				# ---------------------
				
				namei <- paste0("GPIxex",a,sx,"_",vnt,".pdf")
				path  <- file.path("Figures","GBD",namei)
				
				mod <- lm(ex~GPI,data=GBD0)
				
				pdf(path)
				plot(GBD0$GPI, GBD0$ex, 
						main = paste("slope=",round(mod$coef[2],3), ", cor=",round(cor(GBD0$ex,GBD0$GPI),3)),
						xlab = "GPI", ylab = paste0("e(",a,")"),pch = 16, col = "#AAAAAA55", las = 1,
						ylim=c(10,90))
				abline(mod)
				dev.off()
				
				# ---------------------
				namei <- paste0(d,"xex",a,sx,"_",vnt,".pdf")
				path  <- file.path("Figures","GBD",namei)
				
				mod <- lm(y~ex,data=GBD0)
				pdf(path)
				plot(GBD0$ex, GBD0$y,
						main = paste("slope=",round(mod$coef[2],3), ", cor=",round(cor(GBD0$y,GBD0$ex),3)),
						xlab = paste0("e(",a,")"), ylab = paste0(d,"(",a,")"),pch = 16, col = "#AAAAAA55", las = 1,
						xlim=c(10,90),ylim=c(5,30))
				abline(mod)
				dev.off()
				
				# ----------------------
                namei <- paste0("exx",d,a,sx,"_highlowGPI_",vnt,".pdf")
                path  <- file.path("Figures","GBD",namei)

				
# break down for extremes
				ind <- GBD0$GPI < 1.6
				mod <- lm(y~ex,data=GBD0[ind])
				pdf(path)
# ex ~ sd split
				plot(GBD0$ex, GBD0$y, main = "",
						pch = 16, col = "#AAAAAA55",xlab = paste0("e(",a,")"), ylab = paste0(d,"(",a,")"),
						xlim=c(10,90),ylim=c(5,30))
				
				points(GBD0$ex[ind], GBD0$y[ind], pch=16,col="#2ab6f7A1", las = 1)
				abline(mod,col="#2ab6f7")
				text(30,25,paste("GPI < 1.6\nslope=",round(mod$coef[2],3), "\ncor=",round(cor(GBD0$y[ind],GBD0$ex[ind]),3)),pos=4)
				
				ind <- GBD0$GPI > 2.5
				mod <- lm(y~ex,data=GBD0[ind])
				points(GBD0$ex[ind], GBD0$y[ind], pch=16,col="#dd4706A1")
				abline(mod,col="#dd4706")
				text(30,28,paste("GPI > 2.5\nslope=",round(mod$coef[2],3), 
								"\ncor=",round(cor(GBD0$y[ind],GBD0$ex[ind]),3)),pos=4)
				dev.off()
			}
		}
	}
} # end