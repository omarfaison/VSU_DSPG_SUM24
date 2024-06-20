setwd("D:/code/git/VSU_DSPG_SUM24/health_outcomes")
library(tidyverse)
library(sf)

all_data<-readRDS("acs_places_rural_food.RDS")

pdf("rural_classxpoverty.pdf", width = 11, height=8.5)
ggplot(all_data, aes(x=reorder(RUCC_Desc, RUCC_2013), y=pct_poverty))+
         geom_jitter()+
         coord_flip()+
         labs(x="RUCC class", y= "% poverty")+
         theme_minimal()

ggplot(all_data, aes(x=reorder(UIC_Desc, UIC_2013), y=pct_poverty))+
  geom_jitter()+
  coord_flip()+
  labs(x="UIC class", y= "% poverty")+
  theme_minimal()

ggplot(all_data, aes(x=reorder(ncrural_Desc, ncrural), y=pct_poverty))+
  geom_jitter()+
  coord_flip()+
  labs(x="NC rural class", y= "% poverty")+
  theme_minimal()

ggplot(all_data, aes(x=reorder(metro_Desc, metro_class), y=pct_poverty))+
  geom_jitter()+
  coord_flip()+
  labs(x="Metro class", y= "% poverty")+
  theme_minimal()

ggplot(all_data, aes(x=reorder(RUCC_Desc, RUCC_2013), y=pct_poverty))+
  stat_summary(fun = mean, geom = "col") +
  coord_flip()+
  labs(x="RUCC class", y= "% poverty")+
  theme_minimal()

ggplot(all_data, aes(x=reorder(UIC_Desc, UIC_2013), y=pct_poverty))+
  stat_summary(fun = mean, geom = "col") +
  coord_flip()+
  labs(x="UIC class", y= "% poverty")+
  theme_minimal()

ggplot(all_data, aes(x=reorder(ncrural_Desc, ncrural), y=pct_poverty))+
  stat_summary(fun = mean, geom = "col") +
  coord_flip()+
  labs(x="NC rural class", y= "% poverty")+
  theme_minimal()

ggplot(all_data, aes(x=reorder(metro_Desc, metro_class), y=pct_poverty))+
  stat_summary(fun = mean, geom = "col") +
  coord_flip()+
  labs(x="Metro class", y= "% poverty")+
  theme_minimal()
dev.off()