setwd("D:/code/git/VSU_DSPG_SUM24/health_outcomes")
library(tidyverse)
library(sf)

all_data<-readRDS("acs_places_rural_food.RDS")


small_set<-all_data %>%
  st_drop_geometry()%>%
  select(pop_density, pct_poverty, ncrural_Desc, metro_Desc) %>%
  mutate(
    ncrural_Desc = as.factor(ncrural_Desc),
    metro_Desc = as.factor(metro_Desc))

continental<-all_data %>%
  filter(state != "Hawaii") %>% 
  filter(state != "Alaska") %>% filter(state != "United States Virgin Islands") %>% 
  filter(state != "Commonwealth of the Northern Mariana Islands") %>% 
  filter(state !=  "Guam") %>% 
  filter(state !=  "American Samoa") %>% 
  filter(state !=  "Puerto Rico")

pdf("anova_project_prelim.pdf")
rxd_anova<-aov(pop_density ~ ncrural_Desc, data=small_set)
summary(rxd_anova)
rxd_tukey<-TukeyHSD(rxd_anova)
rxd_tukey

rxd_summary<-small_set %>%
  group_by(ncrural_Desc)%>%
  summarize(
    avg=mean(pop_density),
    se=sd(pop_density)/sqrt(n()))

ggplot(rxd_summary, aes(x = ncrural_Desc, y = avg)) +
  geom_col(fill="blue") +
  geom_errorbar(aes(ymin = avg - se, ymax = avg + se),
                width = 0, position = position_dodge(width = 0.9)) +
  labs(
    title = "Population Density by NC Rural Classification",
    x = "NC Rural Classification",
    y = "Ppl/sq mi"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

mxd_anova<-aov(pop_density ~ metro_Desc, data=small_set)
summary(mxd_anova)
mxd_tukey<-TukeyHSD(mxd_anova)
mxd_tukey

mxd_summary<-small_set %>%
  filter(!is.na(metro_Desc))%>%
  group_by(metro_Desc)%>%
  summarize(
    avg=mean(pop_density),
    se=sd(pop_density)/sqrt(n()))

ggplot(mxd_summary, aes(x = metro_Desc, y = avg)) +
  geom_col(fill="blue") +
  geom_errorbar(aes(ymin = avg - se, ymax = avg + se),
                width = 0, position = position_dodge(width = 0.9)) +
  labs(
    title = "Population Density by Metro Classification",
    x = "Metro Classification",
    y = "Ppl/sq mi"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

rxp_anova<-aov(pct_poverty ~ ncrural_Desc, data=small_set)
summary(rxp_anova)
rxp_tukey<-TukeyHSD(rxp_anova)
rxp_tukey

rxp_summary<-small_set %>%
  filter(!is.na(pct_poverty))%>%
  group_by(ncrural_Desc)%>%
  summarize(
    avg=mean(pct_poverty),
    se=sd(pct_poverty)/sqrt(n()))

ggplot(rxp_summary, aes(x = ncrural_Desc, y = avg)) +
  geom_col(fill="blue") +
  geom_errorbar(aes(ymin = avg - se, ymax = avg + se),
                width = 0, position = position_dodge(width = 0.9)) +
  labs(
    title = "Percent Poverty by NC Rural Classification",
    x = "NC Rural Classification",
    y = "% poverty"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

mxp_anova<-aov(pct_poverty ~ metro_Desc, data=small_set)
summary(mxp_anova)
mxp_tukey<-TukeyHSD(mxp_anova)
mxp_tukey

mxp_summary<-small_set %>%
  filter(!is.na(metro_Desc))%>%
  filter(!is.na(pct_poverty))%>%
  group_by(metro_Desc)%>%
  summarize(
    avg=mean(pct_poverty),
    se=sd(pct_poverty)/sqrt(n()))

ggplot(mxp_summary, aes(x = metro_Desc, y = avg)) +
  geom_col(fill="blue") +
  geom_errorbar(aes(ymin = avg - se, ymax = avg + se),
                width = 0, position = position_dodge(width = 0.9)) +
  labs(
    title = "Percent Poverty by Metro Classification",
    x = "Metro Classification",
    y = "% poverty"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(continental)+
  geom_sf(aes(fill=metro_class))+
  theme_void()

ggplot(continental)+
  geom_sf(aes(fill=ncrural))+
  theme_void()

dev.off()
