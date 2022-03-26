# In the first place, this script is written for the GBD
# data (MID, LOW, UPP), but later it'll be expanded as needed 
# for potential comparison datasets.

source(here("R","00_Install_Packages.R"))
 
if(.Platform$OS.type == "windows"){
  library(parallelsugar)
} 

source(here("R","01_Functions.R"))

dir.create(here("Data","Single","GBD"), showWarnings = FALSE, recursive = TRUE)

gbd.folder <- here("Data","Grouped","GBD")

variants <- c("low","mid","upp")

# takes a long time to run, approaches 12Gb memory a bit. If you don't have >= 12Gb mem,
# you can change this:
# mc.cores = (detectCores() - 1)
# to use fewer cores. It'll work even if mc.cores is 1 or 2.

for (i in 1:length(variants)){
  GBDi <- readRDS(file.path(gbd.folder, paste0("GBD",variants[i],".rds"))) 
  GBDi %>% 
    split(list(GBDi$location,GBDi$sex,GBDi$year), drop = TRUE) %>% 
    mclapply(GBD.chunk,mc.cores = (detectCores() - 1)) %>% 
	  rbindlist() %>% 
	  saveRDS(file = here("Data","Single","GBD",paste0("GBD",variants[i],".rds")))
	gc()
}


# these are not 'finalized' still, as the pclm closeout isn't demographically informed, 
# and can go haywire. Next step DataPrep_Closeout.R
make_flipbooks <- FALSE
if (make_flipbooks){
  dir.create(file.path("Figures","GBD","Closeout","pclm"), showWarnings = FALSE, recursive = TRUE)
  
locs<- readRDS(file.path(gbd.folder, paste0("GBD",variants[i],".rds"))) %>%
  pull(location) %>% unique()
# diagnostic flipbooks
for (i in 1:3){
	GBDi <- readRDS(here("Data","Single","GBD",paste0("GBD",variants[i],".rds")))
	GBDi <- GBDi %>% 
	  mutate(sex = as.character(sex),
	         location = as.character(location))
	
	pdf(here("Figures","GBD","Closeout","pclm",paste0("Diagnostic_GBD",variants[i],"males_pclm.pdf")))
	for (l in 1:length(locs)){
		M <- acast(filter(GBDi,
		                  sex == "Male",
		                  location == locs[l]), age~year, value.var = "Ma")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	dev.off()
	
	pdf(here("Figures","GBD","Closeout","pclm",paste0("Diagnostic_GBD",variants[i],"_females_pclm.pdf")))
	for (l in 1:length(locs)){
		M <- acast(filter(GBDi,
		                  sex == "Female",
		                  location == locs[l]), age~year, value.var = "Ma")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	dev.off()
	
	rm(GBDi);gc()
}
}

# next step 05_DataPrep_Closeout.R
# ---------------------------
