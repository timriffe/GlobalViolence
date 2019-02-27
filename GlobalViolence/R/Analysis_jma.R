#Some ANALYSIS

# augment this as needed
if (me == "tim"){
  setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}
if (me == "sam\\jmaburto"){
  setwd("C:/Users/jmaburto/Documents/GitHub/GlobalViolence/GlobalViolence/")
}

library(data.table)

load('Data/Results/GBD/GBDmid.Rdata')

# get the GPI data
GPI      <- read.csv(file.path("Data","Inputs","GPI","GPI_ISO3.csv"),stringsAsFactors=FALSE)
setnames(GPI,"ISO3c","ISO3")
GPI      <- data.table(GPI)
GPIi <- GPI[type == "score"]

GBD  <- merge(GBDi,GPIi[,c(2,4,5)])
setnames(GBD,"value","GPI")


#Data in maps
map.dt <- GBD[year == 2017 & Age == 10]

gdata:: keep(map.dt, sure = T)
# 1 males 
# 2 females

map.dt <- map.dt[order(-sdx),]
map.dt.males <- map.dt[Sex == 1]
map.dt.males[1:25,]
