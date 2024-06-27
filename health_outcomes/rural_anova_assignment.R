setwd("D:/code/git/VSU_DSPG_SUM24/health_outcomes")
library(tidyverse)
library(sf)

all_data<-readRDS("acs_places_rural_food.RDS")

small_set<-all_data %>%
  st_drop_geometry()%>%
  select(pop_density, pct_poverty, ncrural_Desc, metro_Desc)

rxd_anova<-aov(pop_density ~ ncrural_Desc, data=all_data)
summary(rxd_anova)
rxd_tukey<-TukeyHSD(rxd_anova)
rxd_tukey

rxd_summary<-all_data %>%
  group_by(ncrural_Desc)%>%
  summarize(
    avg=mean(pop_density),
    se=sd(pop_density)/sqrt(n()))

small_set<-all_data %>%
  st_drop_geometry()%>%
  select(pop_density, pct_poverty, ncrural_Desc, metro_Desc)