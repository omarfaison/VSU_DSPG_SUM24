#setwd("D:/code/git/VSU_DSPG_SUM24/Assignment02")
library(tidyverse)

diabetes<-read.csv("us_rf_and_screening_county_05-06-2024.csv") %>%
  select(FIPS:State, Diabetes_DX)

afam<-read.csv("us_sociodemographics_county_05-06-2024.csv") %>%
  select(FIPS:State, Black)

graphdata<-diabetes %>%
  full_join(afam, by=c("FIPS","County","State"))%>%
  filter(State=="Virginia")

pdf("faison_assignment02.pdf")
ggplot(graphdata, aes(x=Diabetes_DX, y=Black*100))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  labs(x="Diabetes Diagnoses",
       y="% Black population")+
  theme_minimal()
dev.off()

saveRDS(graphdata, "assigment02data.RDS")