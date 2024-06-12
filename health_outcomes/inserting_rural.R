library(tidyverse)
library(BBmisc)
library(sf)
library(corrplot)
library(psych)
library(readxl)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(scales)
library(ggpubr)
#setwd("D:/code/merged_data_assignments")

data<-readRDS("acs_places_merged.RDS")

ncruraldat<-data %>%
  mutate(ncrural=case_when(pop_density>=750 ~ 1,
                           pop_density< 750 & pop_density > 250 ~ 2,
                           pop_density<=250 ~ 3)) %>%
  mutate(ncrural_Desc=case_when(ncrural==1 ~ "Urban",
         ncrural==2 ~ "Suburban or Regional City",
         ncrural==3 ~ "Rural")) %>%
  mutate(ncrural_fact = as.factor(ncrural))

rucc<-read_xls("ruralurbancodes2013.xls") %>%
  select(-c(2:4))%>%
  mutate(RUCC_fact=as.factor(RUCC_2013))%>%
  mutate(RUCC_Desc=as.factor(RUCC_Desc))

uic<-read_xls("UrbanInfluenceCodes2013.xls") %>%
  select(-c(2:4))%>%
  mutate(UIC_fact=as.factor(UIC_2013))%>%
  mutate(UIC_Desc=as.factor(UIC_Desc))

fa_access<-read_xls("FoodEnvironmentAtlas.xls", sheet="ACCESS") %>%
  select(FIPS, PCT_LACCESS_POP15)

fa_stores<-read_xls("FoodEnvironmentAtlas.xls", sheet="STORES") %>%
  select(FIPS, GROCPTH16, SUPERCPTH16, CONVSPTH16, SNAPSPTH17, WICSPTH16)

fa_rest<-read_xls("FoodEnvironmentAtlas.xls", sheet="RESTAURANTS") %>%
  select(FIPS, FFRPTH16, FSRPTH16)

food_access<-fa_access %>% left_join(fa_stores) %>% left_join(fa_rest)

all_states<-states(cb=T) 
state_wgs<-st_transform(all_states, crs="+proj=longlat +datum=WGS84")
va_wgs<-filter(state_wgs, NAME=="Virginia")
mainland_wgs<- state_wgs %>% filter(NAME != "Hawaii") %>% 
  filter(NAME != "Alaska") %>% filter(NAME != "United States Virgin Islands") %>% 
  filter(NAME != "Commonwealth of the Northern Mariana Islands") %>% 
  filter(NAME !=  "Guam") %>% 
  filter(NAME !=  "American Samoa") %>% 
  filter(NAME !=  "Puerto Rico")


merged<-ncruraldat %>% left_join(rucc, by=c("GEOID"="FIPS")) %>%
  left_join(uic, by=c("GEOID"="FIPS")) %>% left_join(food_access, by=c("GEOID"="FIPS"))

merged_mc<-merged %>%
  mutate(metro_class = case_when(RUCC_2013 < 4 ~ 1,
                                 RUCC_2013 == 4 | RUCC_2013 == 6 | RUCC_2013 == 8 ~ 2, 
                                 RUCC_2013 == 5 | RUCC_2013 == 7 | RUCC_2013 == 9 ~ 3),
         metro_Desc = case_when(metro_class == 1 ~ "metro",
                                metro_class == 2 ~ "metro-adjacent",
                                metro_class == 3 ~ "non-metro"),
         metro_fact = as.factor(metro_class))
#saveRDS(merged_mc, "acs_places_rural_food.RDS")

#no_rucc<-filter(merged, is.na(RUCC_2013))
merged_ml<-merged_mc %>% filter(state != "Hawaii") %>% 
  filter(state != "Alaska") %>% filter(state != "United States Virgin Islands") %>% 
  filter(state != "Commonwealth of the Northern Mariana Islands") %>% 
  filter(state !=  "Guam") %>% 
  filter(state !=  "American Samoa") %>% 
  filter(state !=  "Puerto Rico")
merged_va<-merged_mc %>% filter(state == "Virginia")



ggplot()+
  geom_sf(data=merged_ml, aes(fill=RUCC_2013), color=NA)+
  geom_sf(data=mainland_wgs, fill=NA)+
  theme_void()

ggplot()+
  geom_sf(data=merged_ml, aes(fill=metro_class), color=NA)+
  geom_sf(data=mainland_wgs, fill=NA)+
  theme_void()

ggplot()+
  geom_sf(data=merged_ml, aes(fill=RUCC_fact), color=NA)+
  geom_sf(data=mainland_wgs, fill=NA)+
  theme_void()

tm_shape(merged_ml)+
  tm_fill(col="RUCC_2013")+
  tm_shape(mainland_wgs)+
  tm_borders()

tm_shape(merged_ml)+
  tm_fill(col="RUCC_fact")+
tm_shape(mainland_wgs)+
  tm_borders()