## --------------- HEADER ------------------------------------------------------
## Script name: X_RCREC-presentation.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-29
## Date Last Modified: 2022-6-29
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script for generating some quick and dirt 
## analysis and visualization for the RCREC presentation

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)

# Clean slate
rm(list=ls())

# Bring in the data
d <- read.csv("Raw-data/RCREC.csv")

# Pivot longer
d <- d %>% 
	dplyr::select(-NOTES, -Pinus.sp.) %>% 
	pivot_longer(6:9, names_to = "SPECIES", values_to = 'SEEDS')

# Summarize
means.base <- d %>% 
	group_by(SEASON) %>% 
	summarize(mean = mean(SEEDS),
						sd = sd(SEEDS),
						n = n())

means.base <- means.base %>% 
	mutate(se = sd/sqrt(n))


levels(means.base$SEASON)[levels(means.base$SEASON)=="C/F"] <- "Fall"
levels(means.base$SEASON)[levels(means.base$SEASON)=="G/SU"] <- "Summer"
levels(means.base$SEASON)[levels(means.base$SEASON)=="S/SP"] <- "Spring"
levels(means.base$SEASON)[levels(means.base$SEASON)=="W/D"] <- "Winter"

library(gdata)
means.base$SEASON <- reorder.factor(means.base$SEASON, new.order = c('Fall', 'Winter', 'Spring', 'Summer'))

ggplot(means.base, aes(x = SEASON, y = mean))+
	geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0)+
	geom_point(size = 4)+
	ylab("Seeds per m2")+
	xlab("Seasons since burn")+
	theme_classic()+
	theme(text = element_text(size = 30),
				legend.title = element_blank(),
				legend.position = "none",
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
				strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 18,face ='bold'),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_line(size=1.2))

library(ggplot)
ggsave("Figures/RCREC-pheno.png")

means.base <- means.base %>% 
	filter(SEASON != "Spring")

# Spring 0.01136364

means.base <- means.base %>% 
	mutate(es = log((mean+1)/1.01136364))

ggplot(means.base, aes(x = SEASON, y = mean))+
	geom_point(size = 4)+
	ylab("Seeds per m2")+
	xlab("Effect size")+
	theme_classic()+
	theme(text = element_text(size = 30),
				legend.title = element_blank(),
				legend.position = "none",
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
				strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 18,face ='bold'),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_line(size=1.2))
