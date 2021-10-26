#-----------------------------------------------------------------------------------------------#
# Bivariate maps for women and men, at age 10, year 2017. In addition, bivariate scatterplots
# for women and men, at age 10, year 2017, with Pearson's correlation coefficient printed.
# author: Vanessa di Lego
#-----------------------------------------------------------------------------------------------#

library(tidyverse) 
library(magrittr) 
library(lintr) 
library(sf) 
library(cowplot) 
library(data.table)
library(countrycode)
library(dplyr)
library(here)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(biscale)
library(cowplot)
library(sp)
library(cartography)
library(tmap)
library(raster)

# creating new file directories for bivariate map and bivariate scatter figures

dir.create(here("GlobalViolence","Figures","BivariateMaps"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("GlobalViolence","Figures","BivariateScatterPlots"), showWarnings = FALSE, recursive = TRUE)

# loading data for internal Peace and selecting only latest year and age 10 for analysis

GPI_int<- readRDS(here("GlobalViolence","Data","Results","GPI","GBD_GPI_int.rds"))%>% 
  filter(year==2017 & age==10)

  
# grabbing world data

data(World)

# checking coordinate system

st_crs(World)

#changing to Robinson system; Tim?s request, hope this is what he expected

world_rob<-st_transform(World, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#checking again coordinates

st_crs(world_rob)   

# joining data with shape file

setnames(world_rob, "iso_a3", "ISO3")
GBD_GPI_full<-left_join(GPI_int, world_rob, by="ISO3")

# turning into st_file format for mapping

st_geometry(GBD_GPI_full) <- GBD_GPI_full$geometry


# Making bivariate map - first creating the classes using quantiles to make the breaks.

data_map_men_10 <- bi_class(GBD_GPI_full %>% 
                              filter(year==2017 & Type=="Internal Peace" & 
                                       sex=="Male" & age=="10"), 
                              #group_by(sex, age),
                            x = sdx, 
                            y = score_domain, 
                            style = "quantile", dim = 3)

# here we can see how the classes have been created and how sdx is binned into quantiles
# however there are other options for this..jenks optimizer and also equal bins.

table(data_map_men_10$bi_class)

# extracting coordinates for annotation - we use this later for annotating the map
# bbox_list <- lapply(st_geometry(data_map_men_10), st_bbox) 

# bbox<-data.frame(matrix(unlist(bbox_list), nrow=162, byrow=T),stringsAsFactors=FALSE)
# colnames(bbox)<-c("xmin","ymin","xmax","ymax")
# Final shape
# data_map_men_10_2=bind_cols(data_map_men_10,bbox)
# head(data_map_men_10_2,3)

# finding the coordinates of the places we want to add the annotations
# ann_1<-data_map_men_10_2 %>% 
# filter(location=="China")

map_m <- ggplot()+
  geom_sf(data = data_map_men_10 ,  
          mapping = aes(fill = bi_class),
          color = "grey50", size = 0.1, show.legend = F) +
  bi_scale_fill(pal = "DkViolet", dim = 3)+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank())+
  geom_curve(aes(x = -5867671, 
                 y = 6395418, 
                 xend = -3067671, 
                 yend =5000000 ), 
             curvature = -0.3,
             arrow = arrow(length = unit(0.04, "npc")), 
             size=1.6, color="#CABED0") +
  annotate(geom="text", 
           x=-3567671, 
           y=3700000, 
           label="Light violet:\n low level of violence \nand low lifetime uncertainty",
           size=4.1,
           fontface = "bold")+
  geom_curve(aes(x = 11922086, 
                 y =6398580, 
                 xend = 12922086, 
                 yend =8590494 ),
             curvature = 0.5,
             arrow = arrow(length = unit(0.04, "npc")), 
             size=1.6, color="#3F2949") +
  annotate(geom="text", 
           x=12922086, 
           y=9190494, 
           label="Dark violet: high level of violence \nand high lifetime uncertainty", 
           size=4.1,
           fontface = "bold")+
  geom_curve(aes(x =1936824, 
                 y =-2606450, 
                 xend =903769 , 
                 yend =-5106450),
             curvature = 0.3,
             arrow = arrow(length = unit(0.04, "npc")), 
             size=1.6, 
             color="#AE3A4E") +
  annotate(geom="text", 
           x=903769, 
           y=-6006450, 
           label="Bright red: low level of violence \nand high lifetime uncertainty", 
           size=4.1,
           fontface = "bold")+
  geom_curve(aes(x =3031144, 
                 y =4082025, 
                 xend =6831144, 
                 yend =-386450),
             curvature = -0.3,
             arrow = arrow(length = unit(0.04, "npc")), 
             size=1.6,color="#4885C1") +
  annotate(geom="text", 
           x=7031144, 
           y=-1206450, 
           label="Bright blue:\n high level of violence \nand low lifetime uncertainty", 
           size=4.1,
           fontface = "bold")+
  ggtitle("Global lifetime uncertainty (standard deviation) and level of violence\n (GPI Internal Peace) for men and women, 2017")+
  labs(subtitle = "Men, age 10")+ 
  #  caption = "Source: Own elaboration based on Institute for Health Metrics and Evaluation (IHME). 
  #Findings from the Global Burden of Disease Study 2017. Seattle, WA: IHME, 2018;
  #Institute for Economics & Peace. Global Peace Index 2017: Measuring Peace in a Complex World, Sydney, June 2017")+
  theme(plot.title = element_text(size = 26, face = "bold"),
        plot.subtitle = element_text(face = "italic", size=20))

# legend
legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = " \nLifetime \nUncertainty",
                    ylab = " \nLevel \nof Violence",
                    size = 16)+
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# final plot  -uncomment here to create the plot for men separately

#pdf(file = here("GlobalViolence","Maps","men_bimap.pdf"), width = 18,height = 10) 
finalPlot_men <- ggdraw() +
  draw_plot(map_m, 0, 0, 1, 1)+
  draw_plot(legend, 0.1, .1, 0.2, 0.2) # use this only if want legend together
finalPlot_men
#dev.off()

# Creating the color scheme for scatter plot manually using the same palette for the
# classes as for the map. The package in the map does this already, but here for
# scatter plot I did it manually. 

palette<-c("#CABED0","#89A1C8" ,"#4885C1" ,
           "#BC7C8F","#806A8A", "#435786" ,
           "#AE3A4E", "#77324C" ,"#3F2949")

scatter_plot <- ggplot(data_map_men_10, 
                       aes(x = sdx, y = score_domain ))+
  geom_point_rast(shape = 21,colour = "black",alpha=1/10, size=5.5,show.legend = FALSE)+ 
  geom_point_rast(aes(color=factor(bi_class)),size=5, alpha=1/15)+
  scale_color_manual(values=palette)+
  xlab("Lifetime Uncertainty")+
  ylab("Level of Violence") +
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black", size=0.3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
 # facet_grid(.~Region)+
  stat_cor(method = "pearson",
           label.x = 12, label.y = 4,size = 8)



pdf(file = here("GlobalViolence","Figures","BivariateScatterPlots","men_biscatter_2017.pdf"), width = 10,height = 10) 
scatter_plot_m <- ggdraw() +
  draw_plot(scatter_plot, 0, 0, 1, 1)+
  draw_plot(legend, 0.7, .1, 0.2, 0.2)
scatter_plot_m
dev.off()


# for women

data_map_f_10 <- bi_class(GBD_GPI_full %>% 
                              filter(year==2017 & Type=="Internal Peace" & 
                                        sex=="Female" & age=="10"), 
                            #group_by(sex, age),
                            x = sdx, 
                            y = score_domain, 
                            style = "quantile", dim = 3)

# here we can see how the classes have been created and how sdx is binned into quantiles
# however there are other options for this..jenks optimizer and also equal bins.
table(data_map_f_10$bi_class)

map_f <- ggplot()+
  geom_sf(data = data_map_f_10 ,  
          mapping = aes(fill = bi_class),
          color = "grey50", size = 0.1, show.legend = F) +
  bi_scale_fill(pal = "DkViolet", dim = 3)+
  theme_minimal()+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank())+
#  ggtitle("Global Violence (Internal Peace) and Lifespan Inequality")+
  labs(subtitle = "Women,age 10", 
       caption = "Source: Own elaboration based on Institute for Health Metrics and Evaluation (IHME). 
  Findings from the Global Burden of Disease Study 2017. Seattle, WA: IHME, 2018;
  Institute for Economics & Peace. Global Peace Index 2017: Measuring Peace in a Complex World, Sydney, June 2017")+
  theme(#plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(face = "italic", size=20))#+
# theme_map()

# legend
legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Lifetime \nUncertainty",
                    ylab = "Level \nof Violence",
                    size = 16)+
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

# final plot
#pdf(file = here("GlobalViolence","Maps","women_bimap.pdf"), width = 18,height = 10) 
#finalPlot_f <- ggdraw() +
#  draw_plot(map_f, 0, 0, 1, 1)+
#  draw_plot(legend, 0.1, .1, 0.2, 0.2) # use this only if want legend together


#X11(width = 30, height = 25)

# joining women and men for a composite map

pdf(file = here("GlobalViolence","Figures","BivariateMaps","women_men_bimap.pdf"), width = 15,height = 20) 
ggarrange(finalPlot_men, map_f, ncol = 1)
dev.off()


# Creating the color scheme for scatter plot manually using the same palette for the
# classes as for the map. The package in the map does this already, but here for
# scatter plot I did it manually. 


palette<-c("#CABED0","#89A1C8" ,"#4885C1" ,
           "#BC7C8F","#806A8A", "#435786" ,
           "#AE3A4E", "#77324C" ,"#3F2949")

scatter_plot_f <- ggplot(data_map_f_10, 
                       aes(x = sdx, y =score_domain))+
  geom_point_rast(shape = 21,colour = "black",alpha=1/10, size=5.5,show.legend = FALSE)+ 
  geom_point_rast(aes(color=factor(bi_class)),size=5, alpha=1/15)+
  scale_color_manual(values=palette)+
  xlab("Lifetime Uncertainty")+
  ylab("Level of Violence") +
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  geom_smooth(method="lm", se=F, col="black", size=0.3)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_cor(method = "pearson",
           label.x = 11, label.y = 4,size = 8)
 # facet_grid(.~Region)

pdf(file = here("GlobalViolence","Figures","BivariateScatterPlots","women_biscatter_2017.pdf"), width = 10,height = 10) 
scatter_plot_f <- ggdraw() +
  draw_plot(scatter_plot_f, 0, 0, 1, 1)+
  draw_plot(legend, 0.7, .1, 0.2, 0.2)
scatter_plot_f

dev.off()

# extracting coordinates for annotation
#bbox_list <- lapply(st_geometry(data_map_men_10), st_bbox) 

#bbox<-data.frame(matrix(unlist(bbox_list), nrow=162, byrow=T),stringsAsFactors=FALSE)
#colnames(bbox)<-c("xmin","ymin","xmax","ymax")
#Final shape
#data_map_men_10_2=bind_cols(data_map_men_10,bbox)
#head(data_map_men_10_2,3)

# finding the coordinates of the places we want to add the annotations
# ann_1<-data_map_men_10_2 %>% 
# filter(location=="China")


# Theme from Timo Grossenbaucher for maping. It really enhances the outlook, but usually more interesting when the 
# maps has relief.

#theme_map <- function(...) {
#  theme_minimal() +
#    theme(
#      text = element_text(family = "sans", color = "#22211d"),
#      # remove all axes
#      axis.line = element_blank(),
#      axis.text.x = element_blank(),
#      axis.text.y = element_blank(),
#      axis.ticks = element_blank(),
#      axis.title.x = element_blank(),
#      axis.title.y = element_blank(),
#      # add a subtle grid
#      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
#      panel.grid.minor = element_blank(),
#      plot.background = element_rect(fill = "#f5f5f2", color = NA),
#      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
#      panel.border = element_blank(),
#      panel.background = element_rect(fill = "#f5f5f2", color = NA),
#      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
#      legend.background = element_rect(fill = "#f5f5f2", color = NA),
#      legend.title = element_text(size = 13),
#      legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47"),
#      plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47"),
#      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4e4d47",
#                                   margin = margin(b = -0.1,
#                                                   t = -0.1,
#                                                   l = 2,
#                                                   unit = "cm"),
#                                   debug = F),
#      plot.caption = element_text(size = 9,
#                                  hjust = .5,
#                                  margin = margin(t = 0.2,
#                                                  b = 0,
#                                                  unit = "cm"),
#                                  color = "#939184"),
#      ...
#    )
#}



