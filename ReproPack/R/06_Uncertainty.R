
source(here("R","00_Install_Packages.R"))
source(here:here("R","01_Functions.R"))
dir.create(here("Data","Results","GBD"), showWarnings = FALSE, recursive = TRUE)

# for now just GBD
variants <- c("low","mid","upp")
for (i in 1:length(variants)){
	cat(i,"\n")

	GBDi <- readRDS(here("Data","Closeout","GBD",
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
	saveRDS(GBDi, file = here("Data","Results","GBD",paste0("GBD",variants[i],".rds")))
	rm(GBDi);gc()
}


