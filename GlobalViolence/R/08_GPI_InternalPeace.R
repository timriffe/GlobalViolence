#---------------------------------------------------------------------------------------------------#
# GPI Internal Peace Indicator: Weighted Average Construction 
# author: Vanessa di Lego
#
# Rebuilding the GPI indicator by internal and external peace domain, in order to construct an 
# indicator for internal and external peace separately. The weights provided for each one of the
# 23 indicators that compose the internal and external domains of the GPI were averaged and summed 
# to yield a new indicator. As a robustness check, a comparison between the original GPI scores 
# and the one obtained with weighted average was performed.
# Two new datasets are then saved, one containing the GPI only for internal peace and another
# with internal and external peace. Many additional robustness checks were performed, deprecated.
#
# Each GPI indicator has a specific weight. The weights are available at the report produced
# by the Institute for Economics and peace - Visions of Humanity, and available here:
# https://www.visionofhumanity.org/wp-content/uploads/2020/10/GPI17-Report-1.pdf
#----------------------------------------------------------------------------------------------------#

library(readxl)
library(openxlsx)
library(data.table)
library(countrycode)
library(here)
library(tidyverse)
library(rio)
library(purrr)
library(magrittr)
library(viridis)
library(RColorBrewer)
library(ggpubr)
library(ggrastr)
library(readr)

# creating new file directories for figures and storing data with the new GPI internal peace indicator created

dir.create(here("GlobalViolence","Data","Results","GPI"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("GlobalViolence","Figures","GPI","Diagnostic"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("GlobalViolence","Figures","SuppMaterial"), showWarnings = FALSE, recursive = TRUE)


# Loading GBD data with all estimates and inequality measures

GBD_est <- readRDS(here("GlobalViolence","Data",
                        "Results","GBD",
                        "GBDmid.rds")) %>% 
            mutate(ISO3=countrycode(location, 
                                    origin="country.name",
                                    destination="iso3c"),
                   year=as.numeric(year))

# Read in GPI score values for all years and score type (overall score and individual domain scores).
# Bulk read the scores

path <- here("GlobalViolence","Data",
              "Inputs","GPI","GPI_scores_ind.xls")
Year <- path %>%
  excel_sheets() %>%
  purrr::set_names()
ranges <- list("A4:Z167")
GPI_ind_scores <- map2_df(Year,
                          ranges,
                          ~ read_excel(path, sheet = .x, range = .y),
                          .id = "Year")  %>% 
  pivot_longer(cols=-(1:4),
               names_to = "Indicator", 
               values_to = "Scores") %>%
  mutate(Indicator=as.factor(Indicator)) %>%
  as.data.frame(GPI_ind_scores)
  

# reading the weights

weights_gpi<-fread(here("GlobalViolence","Data","Inputs","GPI","GPI_weights.csv"))%>%
  mutate(Indicator=as.factor(Indicator),
         Type=as.factor(Type))

# Joining both datasets and dropping na's because of Kosovo. Kosovo has data for the GPI 
# but not for the GBD, so we exclude it from the analysis.

gpi_ind<-left_join(GPI_ind_scores,weights_gpi, by="Indicator") %>% 
  mutate(ISO3=countrycode(Country, "country.name", "iso3c"))%>% 
  drop_na()

gpi_test<- gpi_ind %>% 
  group_by(Year,Country) %>% 
  mutate(score_mean=round(weighted.mean(Scores,Weight),3)) %>% 
  arrange(Year,Rank,Type)%>% 
  ungroup()%>% 
  group_by(Year,Country, Type) %>% 
  mutate(score_domain=round(weighted.mean(Scores,Weight),3)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))

# testing whether the indicator we created with the weighted average yields a different
# ranking, to check its stability and whether it´s consistent.

gpi.test.rank<-gpi_test %>% 
  select(c(1:4,10)) %>%
  distinct() %>% 
  group_by(Year) %>% 
  mutate(my_rank = order(order(`Overall Score`, decreasing=F)),
         diff_rank=Rank-my_rank)

# diagnostic plot

pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIdiagnostic.pdf"), width = 10, height = 10) 
ggplot(gpi.test.rank, aes(Rank,my_rank))+
  geom_point(size=1.5,alpha=1/20)+
  theme_bw(base_size = 16)+
  labs(title="Testing for Rank differences between GPI\n construction and Weighted Average", 
       caption="Data source: Institute for Economics & Peace.Global Peace Index 2018:\n Measuring Peace in a Complex World")+
  theme(plot.title = element_text(size = 16),   
    plot.caption = element_text(hjust = 0, face = "italic", size=12))
dev.off()

# are any of the observed differences statistically significant? No.
gpi.test.rank<-gpi.test.rank %>% 
  mutate(t.score=t.test(`Overall Score`, score_mean, 
       alternative = "two.sided", var.equal = FALSE)$p.value,
       t.rank=t.test(Rank, my_rank, 
                     alternative = "two.sided", var.equal = FALSE)$p.value)

# both the differences in rank and in scores as constructed with our approach of weighted average are
# not statistically significant (p-value>0.05) from the ones estimated by the GPI team. We have robust 
# indicator for internal and external peace.

# Joining everything 

gpi_all_dim<-inner_join(GBD_est, gpi_test, by=c("ISO3","year"="Year")) 

# Estimating the correlations by sex, year and age for overall score of GPI,
# i.e., internal and external peace together to see how external peace can be affecting our results

GBD_GPI_cor_all_dim<-gpi_all_dim %>% 
  filter(age%in% c(10,15,20,25,30)) %>% 
  group_by(year,sex, age) %>% 
  mutate(corr_sdx= cor.test(sdx,`Overall Score`, method = "pearson", conf.level = 0.95)$estimate,
         corr_ex= cor.test(ex, `Overall Score`, method = "pearson", conf.level = 0.95)$estimate,
         corr_sdx_test=cor.test(sdx, `Overall Score`, method = "pearson", conf.level = 0.95)$p.value,
         corr_ex_test=cor.test(ex, `Overall Score`, method = "pearson", conf.level = 0.95)$p.value)

# Tracking the differences between specific indicators, scores and internal vs external peace for
# each region as robustness checks

gpi_ind<-gpi_ind %>% 
  mutate(Region=countrycode(Country, "country.name","region")) %>% 
  drop_na()

# all regions
pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIdiagnostic_Regions.pdf"), 
    width = 20, height = 15) 
ggplot(gpi_ind, aes(Region, Indicator, fill=Scores))+
  geom_tile(aes(width=.95, height=.95))+
  geom_point(aes(shape=Type), size=2)+
  scale_fill_viridis_c()+
  theme_classic(base_size = 16)
dev.off()

# East Asia & Pacific

pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIdiagnostic_Asia.pdf"), 
    width = 30, height = 15) 
ggplot(gpi_ind %>% filter(Region=="East Asia & Pacific"), aes(Country, Indicator, fill=Scores))+
  geom_tile(aes(width=.95, height=.95))+
  geom_point(aes(shape=Type), size=2)+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_classic(base_size = 16)
dev.off()

# Latin America & Caribbean
pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIdiagnostic_LAC.pdf"), 
    width = 30, height = 15) 
ggplot(gpi_ind %>% filter(Region=="Latin America & Caribbean"), aes(Country, Indicator, fill=Scores))+
  geom_tile(aes(width=.95, height=.95))+
  geom_point(aes(shape=Type), size=2)+
  scale_fill_viridis_c()+
  theme_classic(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Sub-Saharan Africa
pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIdiagnostic_Africa.pdf"), 
    width = 30, height = 15) 
ggplot(gpi_ind %>% filter(Region=="Sub-Saharan Africa"), aes(Country, Indicator, fill=Scores))+
  geom_tile(aes(width=.95, height=.95))+
  geom_point(aes(shape=Type), size=2)+
  scale_fill_viridis_c()+
  theme_classic(base_size = 16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# North America
pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIdiagnostic_US_CAN.pdf"), 
    width = 20, height = 15) 
ggplot(gpi_ind %>% filter(Region=="North America"), aes(Country, Indicator, fill=Scores))+
  geom_tile(aes(width=.95, height=.95))+
  geom_point(aes(shape=Type), size=2)+
  scale_fill_viridis_c()
dev.off()

#----------------------------------------------------------------------------------------------# 
# External peace indicators are affecting the overall score in ways that confound our results;
# constructing an indicator without external scores by weighted averages - the ranks are not 
# affected when we build the score like this, so it is a simple way to keep the score meanings
# ---------------------------------------------------------------------------------------------# 

gpi_test<- gpi_ind %>% 
  group_by(Year,Country) %>% 
  mutate(score_mean=round(weighted.mean(Scores,Weight),3)) %>% 
  arrange(Year,Rank,Type)%>% 
  ungroup()%>% 
  group_by(Year,Country, Type) %>% 
  mutate(score_domain=round(weighted.mean(Scores,Weight),3))

GBD_est$year<-as.numeric(GBD_est$year)
gpi_test$Year<-as.numeric(gpi_test$Year)

gpi_int_peace<-inner_join(GBD_est, gpi_test, by=c("ISO3","year"="Year")) 

# Estimating the correlations by sex, year, age and indicator type

gpi_domain_score<-gpi_int_peace %>% 
  filter(age%in% c(10,15,20,25,30)) %>% 
  group_by(year,sex, age, Type) %>% 
  mutate(corr_sdx= cor.test(sdx,score_domain, method = "pearson", conf.level = 0.95)$estimate,
         corr_ex= cor.test(ex, score_domain, method = "pearson", conf.level = 0.95)$estimate,
         corr_sdx_test=cor.test(sdx, score_domain, method = "pearson", conf.level = 0.95)$p.value,
         corr_ex_test=cor.test(ex, score_domain, method = "pearson", conf.level = 0.95)$p.value)


# graphing these results for the latest year
# for women:

pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIDomain_Women.pdf"), width = 20,height = 5) 

labels_sdx_type<-gpi_domain_score %>%    
  filter(sex=="Female" & year==2017)%>% 
  group_by(age,Type, corr_sdx) %>% 
  dplyr::summarise()

labels_sdx_type$corr_sdx<-sprintf("italic(r) == %.3f", labels_sdx_type$corr_sdx)

ggplot(gpi_domain_score %>% filter(sex=="Female" & year==2017), 
       aes(sdx,score_domain, group=Type, color=Type))+ 
  geom_point_rast(aes(colour=Type, fill=Type),shape = 21,colour = "black",alpha=1/10, size=4,show.legend = FALSE)+ 
  geom_point_rast(alpha=1/25, size=3.5)+facet_grid(.~age)+  
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Standard Deviation",limits=c(10,22))+
  geom_text(x = 19.5, y = 3.9, aes(label = corr_sdx),size=4.5, parse=T,data = labels_sdx_type %>% filter(Type=="External Peace"),show.legend = F)+
  geom_text(x = 19.5, y = 3.6, aes(label = corr_sdx), size=4.5,parse=T,data = labels_sdx_type %>% filter(Type=="Internal Peace"),show.legend = F)+
  scale_color_manual(values = c('#882255','#009988')) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill=NA),
        strip.text = element_text(size=15))
dev.off()

# for men:

pdf(file = here("GlobalViolence","Figures","GPI","Diagnostic","GPIDomain_Men.pdf"), width = 20,height = 5) 

labels_sdx_type_m<-gpi_domain_score %>%    
  filter(sex=="Male" & year==2017)%>% 
  group_by(age,Type, corr_sdx) %>% 
  dplyr::summarise()
labels_sdx_type_m$corr_sdx<-sprintf("italic(r) == %.3f", labels_sdx_type_m$corr_sdx)


ggplot(gpi_domain_score %>% filter(sex=="Male" & year==2017), 
       aes(sdx,score_domain, group=Type, color=Type))+ 
  geom_point_rast(aes(colour=Type, fill=Type),shape = 21,colour = "black",alpha=1/10, size=4,show.legend = FALSE)+ 
  geom_point_rast(alpha=1/25, size=3.5)+facet_grid(.~age)+  
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Standard Deviation",limits=c(10,22))+
  geom_text(x = 19.5, y = 3.9, aes(label = corr_sdx),size=4.5, parse=T,data = labels_sdx_type_m %>% filter(Type=="External Peace"),show.legend = F)+
  geom_text(x = 19.5, y = 3.6, aes(label = corr_sdx), size=4.5,parse=T,data = labels_sdx_type_m %>% filter(Type=="Internal Peace"),show.legend = F)+
  scale_color_manual(values = c('#882255','#009988')) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill=NA),
        strip.text = element_text(size=15))
dev.off()

#---------------------------------------------------------------------------------------------------------------------# 
# we can see that internal peace is the one that is most related to standard deviation, so
# the results we see is coming from the internal peace part. Syria and Israel have almost the same
# GPI domain score for external peace, but the sdx of Israel is 13.125, while the one
# for Syria is 21.366. This partly explains why for the total GPI specific countries have high levels of violence
# but low levels of standard deviation in ages at death. 
#---------------------------------------------------------------------------------------------------------------------# 

# Ploting the relationship between the GPI internal peace and standard deviation at death by sex, age and year
# FIG S1

GPI_int<-gpi_domain_score %>% 
  filter(Type=="Internal Peace")

labels_sdx_int<-GPI_int %>%                             # labels for graphing
  group_by(year,sex, age, corr_sdx) %>% 
  dplyr::summarise()
labels_sdx_int$corr_sdx<-sprintf("italic(r) == %.3f", labels_sdx_int$corr_sdx)

# Figure S1 : relationship between GPI and standard deviation by sex, year and age
pdf(file = here("GlobalViolence","Figures","SuppMaterial","FigS1.pdf"), width = 15,height = 20) 

ggplot(GPI_int, aes(sdx,score_domain, group=sex, color=sex))+ 
  geom_point_rast(aes(colour=sex, fill=sex),shape = 21,colour = "black",alpha=1/10, size=4,show.legend = FALSE)+ 
  geom_point_rast(alpha=1/40, size=3.3)+
  facet_grid(year~age)+  
  geom_smooth(method=lm, se=FALSE, size=1.5)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Standard Deviation",limits=c(8,21))+
  geom_text(x = 10, y = 3.9, aes(label = corr_sdx),size=5, parse=T,data = labels_sdx_int %>% filter(sex=="Female"),show.legend = F)+
  geom_text(x = 10, y = 3.4, aes(label = corr_sdx), size=5,parse=T,data = labels_sdx_int %>% filter(sex=="Male"),show.legend = F)+
  scale_color_manual(values = c('#882255','#009988')) +
  theme(legend.position = "bottom")+
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill=NA),
        strip.text = element_text(size=15))
dev.off()

# Same plot for pooled years (2008-2017)
# first estimating the correlations by sex, age and indicator type across all years (pooled results)

gpi_domain_score_pooled<-gpi_int_peace %>% 
  filter(age%in% c(10,15,20,25,30)) %>% 
  group_by(sex, age, Type) %>% 
  mutate(corr_sdx= cor.test(sdx,score_domain, method = "pearson", conf.level = 0.95)$estimate,
         corr_ex= cor.test(ex, score_domain, method = "pearson", conf.level = 0.95)$estimate,
         corr_sdx_test=cor.test(sdx, score_domain, method = "pearson", conf.level = 0.95)$p.value,
         corr_ex_test=cor.test(ex, score_domain, method = "pearson", conf.level = 0.95)$p.value)

GPI_int_pooled<-gpi_domain_score_pooled %>% 
  filter(Type=="Internal Peace")

labels_sdx_int_pooled<-GPI_int_pooled %>%                             # labels for graphing
  group_by(sex, age, corr_sdx) %>% 
  dplyr::summarise()
labels_sdx_int_pooled$corr_sdx<-sprintf("italic(r) == %.3f", labels_sdx_int_pooled$corr_sdx)


#Figure Pooled Results - don´t know yet whether JM will add this Figure or when. So left it as FigS3
# here but we can take out later or decide how to rename it/take it out.

pdf(file = here("GlobalViolence","Figures","SuppMaterial","FigS3.pdf"), width = 19,height = 5) 

ggplot(GPI_int_pooled, aes(sdx,score_domain, group=sex, color=sex))+
  geom_point_rast(aes(colour=sex, fill=sex),shape = 21,colour = "black",alpha=1/10, size=4,show.legend = FALSE)+ 
  geom_point_rast(alpha=1/40, size=3.3)+
  facet_grid(.~age)+  
  geom_smooth(method=lm, se=FALSE, size=1.5)+
  scale_y_continuous(name="GPI score",limits=c(1,4))+
  scale_x_continuous(name="Standard Deviation",limits=c(8,21))+
  geom_text(x = 10, y = 3.9, aes(label = corr_sdx),size=5, parse=T,data = labels_sdx_int_pooled %>% filter(sex=="Female"),show.legend = F)+
  geom_text(x = 10, y = 3.4, aes(label = corr_sdx), size=5,parse=T,data = labels_sdx_int_pooled %>% filter(sex=="Male"),show.legend = F)+
  scale_color_manual(values = c('#882255','#009988')) +
  theme(legend.position = "bottom")+
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill=NA),
        strip.text = element_text(size=15))
dev.off()

# saving the data with internal peace indicator only "GBD_GPI_int.rds" 
# and with both indicator types (internal and external - "GBD_GPI_int_ext.rds") for building the maps and 
# Figure 2 (bivariate scatterplot) in the main text.


saveRDS(GPI_int, file = file.path(here("GlobalViolence","Data","Results","GPI", "GBD_GPI_int.rds")))
saveRDS(gpi_domain_score, file = file.path(here("GlobalViolence","Data","Results","GPI", "GBD_GPI_int_ext.rds")))


# Following are additional checks to see whether the GPI internal Peace indicator with weighted averages is robust
# Additional comments and analysis.

#  gpi.test.rank<-gpi_test %>% 
#  select(c(1:4,10)) %>%
#  distinct() %>% 
#  group_by(Year) %>% 
#  mutate(my_rank = order(order(`Overall Score`, decreasing=F)),
#         diff_rank=Rank-my_rank)
#  ggplot(gpi.test.rank, aes(score_type,`Overall Score`))+geom_point()+
#  theme_bw()

# Constructing the indicator only by level of internal peace
# Read in GBD data estimated in the Results folder- selecting the mid version
# GBD_est <- readRDS(here("GlobalViolence","Data",
#                        "Results","GBD", "GBDmid.rds"))

# Transforming ISO codes into country names for better identification
# GBD_est$ISO3<-countrycode(GBD_est$location, 
#                         origin="country.name", 
#                          destination="iso3c")

# Read in GPI score values 
# GPI_ISO3 <- fread(here("GlobalViolence","Data",
#                       "Inputs","GPI", 
#                      "GPI_ISO3.csv"))

# GPI: Selecting only the matching years for the GPI and GBD and the score value
# GPI_years<-GPI_ISO3 %>% 
#  filter(type=="score"& year %in% c(2017))

# Checking for NAs and which cases are they
# check.na<-GPI_years%>% filter(is.na(value)) : 
# there are missing values for the scores for South Sudan and Palestine for some years,
# as there were no estimates for them due to biding territorial issues. Palestine from 2008 until 2015 and
# South Sudan until 2010. I took  them out for doing the correlation for all years and included them for the last year of analysis

# GBD: Selecting only the matching years for the GPI and GBD and the score value and restricting to ages 10 to 30 
#  GBD_years<-GBD_est %>% 
#  filter(age%in% c(10,15,20,25,30) & year %in% c(2017))

# Azerbaijan, Albania, Tunisia and Uzbekistan have NaN edx values.
# Substitute for 0 so it is not left out later (it does not affect what we do since the NaN values are only for the edaggers)

#GBD_years[is.na(GBD_years)] <- 0

# Joining GBD file with GPI file by ISO code and year. Keeping only the matching observations.

#setnames(GPI_years, "ISO3c","ISO3")
#GBD_GPI<-inner_join(GBD_years, GPI_years, by=c("ISO3","year")) 


# Checking the Syria x Israel comparison

# gpi_domain_y<-gpi_domain_score %>% 
#  filter(sex=="Male" & year==2017)

# gpi_domain_is<-gpi_domain_y %>% 
#  filter(location%in% c("Syria","Israel"))

# gpi_domain<-gpi_domain_is %>% 
#  group_by(location, Type) %>% 
#  slice(2) %>% 
#  summarize(GPI=mean(score_domain),sdx) %>% 
#  pivot_wider(names_from=Type,values_from=GPI)
