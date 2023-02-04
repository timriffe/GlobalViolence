#Some ANALYSIS

#rm(list=ls())

library(data.table)
library(readxl)
library(reshape2)
library(countrycode)
library(dplyr)
library(ggplot2)
library(biscale)
library(patchwork)
library(scales)

# Load data ---------------------------------------------------------------

#get data from GPI
GPI_2017 <- data.table(read_excel('GlobalViolence/Data/GPI complete/GPI_scores 2008-2019.xlsx',
                                  sheet="2017",
                                  range = 'A4:Z167',
                                  col_names = TRUE))

GPI_2017 <- GPI_2017[order(-Rank)]

#get GBD data, from Vanessa
GBDmid <- data.table(readRDS("GlobalViolence/Data/GBD_Vanessa/GBDmid.rds"))

# Get indicators for age 10 in 2017 with ISO names
GPI_2017  <-GPI_2017 %>%
  mutate(ISO3c=countrycode(GPI_2017$Country, "country.name", "iso3c")) %>%
  mutate(ISO3n=countrycode(GPI_2017$Country, "country.name", "iso3n"))

GPI_2017  <- data.table(GPI_2017)
countries <- unique(GPI_2017$ISO3c)
SD_2017   <- GBDmid[year == 2017 & sex == 'Male' & age == 10]
SD_2017.f   <- GBDmid[year == 2017 & sex == 'Female' & age == 10]

SD_2017.f   <- SD_2017.f[order(sdx)]

SD_2017   <- SD_2017[order(sdx)]
SD_2017   <- SD_2017[ISO3 %in% countries]

names(GPI_2017)[27] <- 'ISO3'

Data_2017 <- merge(SD_2017,GPI_2017,by = 'ISO3')
Data_2017 <- Data_2017[order(sdx)]
Data_2017$rank_sd  <- 1:162
Data_2017$rank_gpi <- Data_2017$Rank

Data_2017 <- data.table(bi_class(Data_2017, x = sdx, y = Rank, style = "quantile", dim = 3))

### get decomposition results with second group of countries
load("GlobalViolence/Data/Results/GBD/DECsd10_HIGHVIO2.rds")

gdata::keep(Data_2017,DECsd2,GBDmid,GPI_2017, sure = T)


# Decomposition figure (2nd selection) ------------------------------------
#1 males
#2 females

Data_2017 <- Data_2017[order(sex,-bi_class,-sdx)]

#load decomposition results
names(DECsd2)[4:6] <- c('Rest','Homicide','War-related')
names(DECsd2)[2] <- 'Sex'

DECsd2[, Sex:= ifelse(Sex == 'Male','Males','Females')]

Decomp_data       <- data.table(melt(DECsd2,id.vars = c('ISO3', 'Sex','Age'), variable.name = 'Cause',value.name = 'Contribution',variable.factor = F))
Decomp_data$Cause <- factor(Decomp_data$Cause, levels =  rev(c('Homicide','War-related', 'Rest')))
Decomp_data <- Decomp_data[Age >= 10]

Age.n  <- c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
labe <- c('10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85+')

Decomp_data[,Age.5:= cut(Age,c(Age.n,Inf),include.lowest = T,right = F,labels = labe)]

Decomp_data <- Decomp_data[,list(Contribution = sum(Contribution)), by = list(ISO3,Sex,Cause,Age.5)]

Decomp_data$Contribution <- -Decomp_data$Contribution

Decomp_data_total <- Decomp_data[, .(contribution = sum(Contribution)), by = .(ISO3, Sex, Cause)]



#Selection for graph
countries.graph <- data.table(ISO3 = c('VEN','COL','RUS','MEX','UKR','NER','YEM','LBY','CAF','SOM','SSD','SYR','AFG','IRQ','SLV'))
countries.graph$group1.order <-  c(14,13,12,11,10,9,8,7,6,5,4,3,2,1,15)
countries.graph$region <- c('Latin America','Latin America','Europe','Latin America','Europe','Africa','Middle East','Africa',
                            'Africa','Africa','Middle East','Middle East','Middle East','Middle East','Latin America')
countries.graph$country <- c('Venezuela','Colombia','Russia','Mexico','Ukraine','Nigeria','Yemen','Libya','C. African Republic','Somalia','South Sudan','Syria','Afghanistan','Iraq','El Salvador')


fig1.data <- Decomp_data_total[ISO3 %in% countries.graph$ISO3]

fig1.data <- merge.data.table(x = fig1.data,y = countries.graph)

fig1.data[, total_difference := sum(contribution), by = .(ISO3,Sex)]

fig1.data[, perc := (contribution)/total_difference, by = .(ISO3,Sex)]

fig1.data[perc > 1]$perc <- 1



unique(fig1.data$ISO3)



# fig1.data$Country <- group1.order[fig1.data$ISO3]
# fig1.data$Country <- factor(fig1.data$Country, levels = group1.order)
fig1.data$Sex <- factor(fig1.data$Sex, levels = c('Males', 'Females'))

fig1.data$country2 <- factor(fig1.data$country, levels = fig1.data[Sex == 'Males' & Cause == 'Rest' ]$country[order(fig1.data[Sex == 'Males'& Cause == 'Rest' ]$total_difference)])
fig1.data$region <- factor(fig1.data$region,levels = c('Middle East','Latin America','Africa', 'Europe' ))



# # 
# fig1.data$Region <- NA
# 
# fig1.data[,Region:= ifelse(Country %in% middle.east, Region <- 1,
#                            ifelse(Country %in% africa, Region <- 2,
#                                   ifelse(Country %in% latin.america, Region <- 3,
#                                                 ifelse(Country %in% eastern.europe, Region <-4, 
#                                                        ifelse(Country %in% east.asia, Region <- 5,Region <- NA)))))
# ]
# 
# fig1.data$Region <- as.factor(fig1.data$Region)
# levels(fig1.data$Region) <- c('Middle East','Africa','Latin America', 'Eastern Europe','Asia')
# 
# unique(fig1.data$Country)
# 
# 
# break.ind <- levels(fig1.data$Age.5)[seq(1,15,2)]
# age.lab <- as.character(seq(10,80,10))
# 
# 
# # Calculate % of violent deaths and total gap -----------------------------
# total.contrib <- fig1.data[, list(Total = round(sum(Contribution)),
#                                   Violence = round(sum(Contribution[which(Cause %in% c('Homicide','War-related'))])/sum(Contribution)*100)), 
#                            by = list(ISO3,Sex,Region,Country)]
# 
# total.contrib[,lab.text := paste0('G = ',Total,'y, V = ',Violence,'%')]
# 
# 
# fig1.data <- merge(fig1.data,total.contrib[,c(1,2,3,7)],by = c('ISO3','Sex','Region'),all.x = T)


################### Figure males
p1 <- ggplot(fig1.data[Cause=='Rest'], aes(x = total_difference , y = country2,col = Sex)) +
  geom_point(aes(group = Cause,shape = Sex), size = 3,)+
  scale_shape_manual(values=c(19,17))+
  facet_grid(region ~ ., scales = "free", space = "free") +
  theme_minimal()+
  scale_color_manual(values = rev(c('#B6407D','#11718A'))) +
  scale_x_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) +
  labs(x = 'Difference in standard deviation with peaceful nations', y = NULL,size=13)+
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),
        plot.subtitle =element_text(face = 'bold'),
        strip.text = element_text(size = 14, colour = "black"),
        panel.background = element_rect(fill = NA, color = "lightgrey"))
p1


brks <- c(.25,.50,.75,1)
p2 <- ggplot(fig1.data[perc>=0], aes(x = perc  , y = country2,fill = Cause)) +
  geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = T)+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = brks, col = 'grey', linetype = "dashed")+
  facet_grid(region ~ Sex, scales = "free", space = "free") +
  theme_minimal()+
  scale_x_continuous(breaks = brks, labels = scales::percent(brks)) +
  scale_fill_manual('Cause of death', values = rev(c('#724E95','#8DA3CA','#F1F1F1'))) + 
  labs(x = 'Proportion of the difference attributed to specific causes of death', y = NULL,size=13)+
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
        strip.text = element_text(size = 14, colour = "black"),
        panel.background = element_rect(fill = NA, color = "lightgrey"))
p2





pdf(file="GlobalViolence/Figures/Figure3_06082021.pdf",width=8,height=7,pointsize=4)
p1
dev.off()

pdf(file="GlobalViolence/Figures/Figure4_06082021.pdf",width=10,height=7,pointsize=4)
p2
dev.off()

# 1) 
#sdx_no_hw vs IPI
library(tidyverse)

DAT <- readRDS(here::here("GlobalViolence/Data/Results/GPI/GBD_GPI_int.rds"))
DAT
DAT %>% 
  filter(sex == "Male",
         age == 10,
         year == 2017) %>% 
  ggplot(aes(x = score_domain, y = sdx_no_hw, color = Region)) +
  geom_point() +
  ylim(12,26)

DAT %>% 
  filter(sex == "Male",
         age == 10) %>% 
  ggplot(aes(x = score_domain, y = sdx, color = Region)) +
  geom_point() +
  ylim(12,26)

DAT %>% colnames()
# 2)
# IPI v Difference of observed lifespan inequality and lifespan inequality without violent deaths
# (sdx - sdx_no_hw) vs IPI
DAT %>% 
  filter(sex == "Male",
         age == 10) %>% 
  ggplot(aes(x = score_domain, y = sdx - sdx_no_hw, color = Region)) +
  geom_point() +
  scale_y_log10() + 
  geom_smooth(method = "lm")

# note: 

# 
# p1 <- ggplot(fig1.data[Region %in% c('Middle East') & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Middle East')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p1
# 
# p2 <- ggplot(fig1.data[Region == 'Latin America' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Latin America')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p2
# 
# p3.1 <- ggplot(fig1.data[Country %in% c('Somalia', 'Libya', 'South Africa', 'C. African R.','Zimbabwe') & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Africa')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p3.1
# 
# p3.2 <- ggplot(fig1.data[Country %in% c('Guinea-Bissau','Nigeria','Eritrea','Cameroon',"Cote d'Ivoire") & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL)+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p3.2
# 
# p3.3 <- ggplot(fig1.data[Country %in% c('Kenya','Chad','Congo') & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL)+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p3.3
# 
# p4 <- ggplot(fig1.data[Region == 'Eastern Europe' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Eastern Europe')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p4
# 
# 
# p5 <- ggplot(fig1.data[Region == 'Asia' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_wrap(~ Country,ncol = 1)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Asia')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p5
# 
# 
# library(gridExtra)
# pdf(file="GlobalViolence/Figures/Figure2V2.pdf",width=14,height=14,pointsize=4)
# 
# grid.arrange(p1,p2, ncol = 2)
# 
# dev.off()
# 
# 
# 
# p5 <- ggplot(fig1.data[Region == 'Asia' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = T)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Eastern Asia')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p6
# 
# pdf(file="GlobalViolence/Figures/legend.pdf",width=5,height=5,pointsize=4)
# p6
# dev.off()
# 
# 
# # version 2 ---------------------------------------------------------------
# 
# 
# ################### Figure males
# 
# p1 <- ggplot(fig1.data[Region %in% c('Middle East') & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Middle East')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p1
# 
# p2 <- ggplot(fig1.data[Region == 'Latin America' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Latin America')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p2
# 
# p3.1 <- ggplot(fig1.data[Country %in% c('Somalia', 'Libya', 'South Africa', 'C. African R.','Zimbabwe') & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country,)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Africa')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p3.1
# 
# p3.2 <- ggplot(fig1.data[Country %in% c('Guinea-Bissau','Nigeria','Eritrea','Cameroon',"Cote d'Ivoire") & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country,)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL)+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p3.2
# 
# p3.3 <- ggplot(fig1.data[Country %in% c('Kenya','Chad','Congo') & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country,)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL)+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p3.3
# 
# p4 <- ggplot(fig1.data[Region == 'Eastern Europe' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Eastern Europe')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p4
# 
# 
# p5 <- ggplot(fig1.data[Region == 'Asia' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = F)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Asia')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p5
# 
# 
# pdf(file="GlobalViolence/Figures/Figure2V1.pdf",width=8,height=14,pointsize=4)
# 
# (p1 + plot_spacer() + plot_layout(widths = c(4,1))) / (p2 + plot_layout(widths = c(5))) / (p3.1 + plot_layout(widths = c(5))) / 
#   (p3.2 + plot_layout(widths = c(5))) / (p3.3 + plot_spacer() + plot_layout(widths = c(3,2))) /
#   (p4 + plot_spacer() + plot_layout(widths = c(2,3))) / (p5 + plot_spacer() + plot_layout(widths = c(1,4)))
# 
# dev.off()
# 
# 
# 
# p5 <- ggplot(fig1.data[Region == 'Asia' & Sex == 'Males' & Age.5 != '85+'], aes(x = Age.5, y = Contribution,fill = Cause)) +
#   geom_bar(aes(group = Cause), stat = "identity",position = "stack",show.legend = T)+
#   geom_text(data = . %>% filter(Age.5 == '45-49'),show.legend = F,aes(x=Age.5, y=-1, label=lab.text),size = 3)+
#   geom_hline(yintercept = 0)+
#   scale_x_discrete(breaks = break.ind,labels = age.lab)+
#   scale_y_continuous(limits = c(-1,2.8))+
#   facet_grid(~ Country)+
#   theme_minimal()+
#   scale_fill_manual('Cause of death', values = c('#704D9E','#CF63A6','#F3E79A')) + 
#   labs(x = NULL, y = NULL,size=13,subtitle = 'Eastern Asia')+
#   theme(text = element_text(size=14),axis.text.x = element_text(angle = 0, hjust = 1, size = 8),plot.subtitle =element_text(face = 'bold'),
#         strip.text = element_text(size = 14, colour = "black"),
#         panel.background = element_rect(fill = NA, color = "lightgrey"))
# p6
# 
# pdf(file="GlobalViolence/Figures/legend.pdf",width=5,height=5,pointsize=4)
# p6
# dev.off()
# 
# 
# 
# 
# 
# #### some numbers for the text
# Syrya.decomp <- fig1.data[Country %in% c('Syria')]
# 
# Syrya.decomp[,sum(abs(Contribution)), by = list(Sex,Cause)]
# 
# LAC.decomp <- fig1.data[Region == 'Latin America' & Sex == 'Females']
# 
# LAC.decomp[,sum((Contribution)), by = list(Sex,Cause)]
# 1.02/5.80
