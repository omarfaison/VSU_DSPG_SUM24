#setwd("D:/Users/mfaison/code/VSU_DSPG_SUM24/sandbox")
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)


va_counties<-counties(state="VA", cb=T)
va_zip<-zctas(state="VA", cb=F, year=2010)
va_tracts<-tracts(state="VA", cb=T)

ggplot() +
  geom_sf(data = va_tracts, aes(fill=ALAND), color = "white") +  # Fill census tracts
  geom_sf(data = va_counties, fill = NA, color = "black") +           # Outline counties
  theme_void()

ggplot() +
  geom_sf(data = va_zip, aes(fill=ALAND10), color = "white") +  # Fill census tracts
  geom_sf(data = va_counties, fill = NA, color = "black") +           # Outline counties
  theme_void()