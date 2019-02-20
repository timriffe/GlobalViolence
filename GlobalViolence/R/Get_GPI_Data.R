# prepared by VdL and JM, modified by TR
#Downloading data from wikipedia
# A table that has all GPIs


me <- system("whoami",intern=TRUE)

# augment this as needed
if (me == "tim"){
	setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

library(htmltab)
library(data.table)
library(countrycode)
library(dplyr)

GPI <- data.table(htmltab("https://en.wikipedia.org/wiki/Global_Peace_Index",2))

GPI.dt         <- melt.data.table(data = GPI,id.vars = 1)

GPI.dt$country <- ifelse(GPI.dt$Country == sort(unique(GPI.dt$Country))[1], substr(GPI.dt$Country,7,nchar(GPI.dt$Country)),
                         ifelse(GPI.dt$Country == sort(unique(GPI.dt$Country))[2],substr(GPI.dt$Country,5,nchar(GPI.dt$Country)),
                                substr(GPI.dt$Country,3,nchar(GPI.dt$Country))))
GPI.dt$year    <- as.numeric(substr(GPI.dt$variable,1,4))
GPI.dt$type    <- substr(GPI.dt$variable,6,nchar(as.character(GPI.dt$variable)))
GPI.dt$value   <- as.numeric(GPI.dt$value)
GPI.dt         <- GPI.dt[order(year,country), c('country','year','type','value')]
GPI.dt

# transforming country names into ISO3 codes

GPI_ISO3<-GPI.dt %>% 
  mutate(ISO3c=countrycode(GPI.dt$country, "country.name", "iso3c")) %>% 
  mutate(ISO3n=countrycode(GPI.dt$country, "country.name", "iso3n")) 
#View(GPI_ISO3)

# TR modified:
dir.create(file.path("Data","Inputs","GPI"), showWarnings = FALSE, recursive = TRUE)
write.table(GPI_ISO3,file.path("Data","Inputs","GPI","GPI_ISO3.csv"), sep = ",", row.names = FALSE)






