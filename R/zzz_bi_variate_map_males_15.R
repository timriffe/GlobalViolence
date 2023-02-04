###############################################################################
me <- system("whoami",intern=TRUE)

# change this as needed
if (me == "tim"){
  setwd("/home/tim/git/GlobalViolence/GlobalViolence")
}

if (me == "desktop-0jp28lo\\vdile"){
  setwd("C:/Users/vdile/Documents/Git/Violence")
}

library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)
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
View(GPI.dt)

GPI_ISO3<-GPI.dt %>%
  mutate(ISO3c=countrycode(GPI.dt$country, "country.name", "iso3c")) %>%
  mutate(ISO3n=countrycode(GPI.dt$country, "country.name", "iso3n"))

View(GPI_ISO3)

# TR modified:
dir.create(file.path("Data","Inputs","GPI"), showWarnings = FALSE, recursive = TRUE)
write.table(GPI_ISO3,file.path("Data","Inputs","GPI","GPI_ISO3.csv"), sep = ",", row.names = FALSE)

View(GPI_ISO3)

# loading data: using GBDmid created by Tim

load("C:/Users/vdile/Documents/Git/Violence/GBDmid.Rdata")
GPI_ISO3 <- read.csv("C:/Users/vdile/Documents/Git/Violence/GPI_ISO3.csv", header=T)
GPI<-GPI_ISO3 %>% filter(type=="score")
GPI_year<-GPI %>% filter(year==2017)
View(GPI_year)

# selecting only the latest years for the GPI
GBD_year<-GBDi %>% filter(year=="2017")

# ages 15 only
GBD_age_year<-GBD_year %>% filter(Age==15)
View(GBD_age_year)

# ages 15 only, males
GBD_age_year_male<-GBD_age_year %>% filter(Sex==1)
View(GBD_age_year_male)

# just saw that Azerbaijan has NaN edx values.
# Substitute for 0 so  it is not left out later
GBD_age_year_male[is.na(GBD_age_year_male)] <- 0

# matching GBD file with GPI file

library(data.table)
setnames(GPI_year, "ISO3c","ISO3")
GBD_GPI_male<-full_join(GBD_age_year_male, GPI_year, by="ISO3")
View(GBD_GPI_male)

# selecting na´s to check who is in the GBD data but do not have GPI estimates
GBD_GPI_na <-GBD_GPI_male[!complete.cases(GBD_GPI_male), ]

View(GBD_GPI_na)

# 34 countries that have info from GBD but not from GPI, so taking them out.
# Actually later we have to check this. In the end I take them out but then cannot create
# color scheme for missing value in the map.

GBD_GPI_male2 <-GBD_GPI_male[complete.cases(GBD_GPI_male), ]


#install.packages("tmap")
#install.packages("tmaptools")
# World shape files have a bunch of options.
#I tested with all those below. In the end chose the tmap package shapefile

library(tmaptools)
library(tmap)
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
#tmap_mode("view")
# getting world shape file
#data("World")
#View(World)

library(rnaturalearth)
library(sp)

# Cartography Library
# I kept this one for the analysis
library(cartography)
library(rgdal)
library(tmap)

# grabing world data
data(World)

# checking coordinate system
st_crs(World)

#changing to Robinson system; Tim´s request, hope this is what he expected
world_rob<-st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#checkin again coordinates
st_crs(world_rob)   # good to go

# joining data with shape file
setnames(world_rob, "iso_a3", "ISO3")
GBD_GPI_male_full<-left_join(GBD_GPI_male2, world_rob, by="ISO3")
class(GBD_GPI_male_full)

# turning into st_file format for maping
st_geometry(GBD_GPI_male_full) <- GBD_GPI_male_full$geometry
class(GBD_GPI_male_full)

# using Timo Grossenbaucher theme for maping. It really enhances the outlook

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#22211d"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4e4d47",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      plot.caption = element_text(size = 9,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

# making bivariate map
# first creating the classes
# used quantiles here to make the breaks

data_map_men <- bi_class(GBD_GPI_male_full, x = sdx, y = value, style = "quantile", dim = 3)
View(data_map_men)
class(data_map_men)

# here we can see how the classes have been created and how sdx is binned into quantiles
# however there are other options for this..jenks optimizer and also equal bins.
table(data_map_men$bi_class)

map <- ggplot()+
  geom_sf(data = data_map_men,  mapping = aes(fill = bi_class),
          color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3)+
  theme_map()

# legend
legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Lifetime \nUncertainty",
                    ylab = "Level \nof Violence",
                    size = 8)


# final plot
X11(width=15,height=9)
finalPlot_men <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1)+
  draw_plot(legend, 0.1, .1, 0.2, 0.2) # use this only if want legend together

# In the end I just cropped the legend and added the labels separately using
# power point and a bit of inkscape due to our paa deadline. But I am working
# on better developing this only using R.

finalPlot_men

## scatter plot: this is the scatter plot JM asked for. Because I wanted to take
# advantage of the biscale color scheme but used the package, I took out their
# color pallete and built it manually.

data_map_men_plot2<-as.data.frame(data_map_men)
class(data_map_men_plot2)
View(data_map_men_plot2)

# checking if the classes are working here
scatter_plot <- ggplot(data_map_men_plot2, aes(x = sdx, y = value))+
  geom_point(aes(fill=bi_class))    # it works!

# creating the color scheme for scatter plot manually using the same pallete for the
# classes as for the map. The package in the map does this already, but here for
# scatter plot I had to do it manually...

data_map_men_plot2$bi_class<-as.factor(data_map_men_plot2$bi_class)
levels(data_map_men_plot2$bi_class)

library(grDevices)
library(ggthemes)
library(ggpubr)

palette<-c("#CABED0","#89A1C8" ,"#4885C1" ,
           "#BC7C8F","#806A8A", "#435786" ,
           "#AE3A4E", "#77324C" ,"#3F2949")

scatter_plot <- ggplot(data_map_men_plot2, aes(x = sdx, y = value))+
  geom_point(aes(color=factor(bi_class)),size=2)+
  scale_color_manual(values=palette)+
  xlab("Lifetime Uncertainty")+
  ylab("Level of Violence") +
  theme_classic()+
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black", size=0.3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_cor(method = "pearson",
            label.x = 13, label.y = 4)

X11()
scatter_plot_m <- ggdraw() +
  draw_plot(scatter_plot, 0, 0, 1, 1)+
  draw_plot(legend, 0.7, .1, 0.2, 0.2)
scatter_plot_m

# Just checking Calculating Pearson's product-moment correlation: all good
cor.test(data_map_men_plot2$sdx, data_map_men_plot2$value, method = "pearson", conf.level = 0.95)


