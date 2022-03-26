
# Author: tim
###############################################################################

# Before working on other sources, need to check how GBD rates closed out using the pclm method.
# older ages may need to be overwritten using MortalityLaws if they are awful.

# This script *follows* DataPrep_Graduate.R for now.
source(here("R","00_Install_Packages.R"))

if(.Platform$OS.type == "windows"){
  library(parallelsugar)
} 


GBD.closeout <- function(.SD,
		fit_low = 65, 
		fit_up = 90, 
		extrap_low = 65, 
		omega = 110, 
		law = "kannisto_makeham"){
	.SD <- arrange(.SD, age)
	# this comes from DemoTools, calling MortalityLaws...
	.SD$Ma <-	lt_rule_m_extrapolate(.SD$Ma, 
			.SD$age, 
			x_fit = fit_low:fit_up, 
			x_extr = extrap_low:omega,
			law = law)$values
	.SD
}

dir.create(here("Data","Closeout","GBD"),recursive=TRUE,showWarnings=FALSE)
dir.create(here("Figures","GBD","Closeout","ggompertz"),recursive=TRUE,showWarnings=FALSE)

variants <- c("low","mid","upp")

# add anything here you like, such as "kannisto_makeham", "gompertz", "ggompertz", blablabla.
laws          <- c("ggompertz")
fit_low       <- c(60,65,70)
fit_upper     <- c(85,90,95)
extrap_low    <- c(60,70,80,90)

# takes 5-10 min, uses 10 or Gb. Reduce mc.cores to reduce memory requirements.
for (i in 1:3){
	GBDi          <- readRDS(here("Data","Single","GBD",paste0("GBD",variants[i],".rds")))
	GBDi$location <- as.character(GBDi$location)
	GBDi$ISO3     <- as.character(GBDi$ISO3)

	GBDi %>% split(list(GBDi$location, GBDi$sex, GBDi$year), drop = TRUE) %>% 
	  mclapply(GBD.closeout, mc.cores = (detectCores() - 1),
	           fit_low = 65, fit_up = 90, extrap_low = 65,
	           omega = 110, law = "ggompertz") %>% 
	  rbindlist() %>% 
		saveRDS(file = here("Data","Closeout","GBD",
					paste0("GBD",variants[i],"_ggompertz_65_90_65.rds")))
	gc()
}

# diagnostic flipbooks
make_flipbooks <- FALSE
if (make_flipbooks){
  # make flip book, one page per country,
  # one book per sex.
  
for (i in 1:3){
	GBDi <-  readRDS(
							here("Data","Closeout","GBD",
									paste0("GBD",variants[i],"_ggompertz_65_90_65.rds")))
	locs <- GBDi %>% pull(location) %>% unique()
	pdf(here("Figures","GBD","Closeout","ggompertz",
	         paste0("Diagnostic_GBD",variants[i],"_males_ggompertz_65_90_65.pdf")))
	for (l in 1:length(locs)){
		M <- acast(GBDi[sex == "Male" & location == locs[l]], age~year, value.var = "Ma")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}
	dev.off()
	
	pdf(here("Figures","GBD","Closeout","ggompertz",
	         paste0("Diagnostic_GBD",variants[i],"_females_ggompertz_65_90_65.pdf")))
	for (l in 1:length(locs)){
		M <- acast(GBDi[sex == "Female" & location == locs[l]], age~year, value.var = "Ma")
		matplot(0:110, M, ylim = c(1e-6, 1.5), log = 'y', type = 'l', lty = 1, col = "#00000088",
				main = locs[l])
	}

	dev.off()
	rm(GBDi);gc()
}
}

# next step 06_Uncertainty.R
