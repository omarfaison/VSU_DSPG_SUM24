#setwd("D:/Users/mfaison/code/VSU_DSPG_SUM24/Assignment01")
library(tidyverse)

#read in data
data<-read.csv("us_cancer_mortality_county_05-06-2024.csv", header=T)

#wrangle
proj_data<- data %>%
  filter(State=="Virginia", RE=="All", Sex=="All") %>%
  select(FIPS:County, All.Site, Pancreas) %>%
  mutate(pct_pancreas=Pancreas/All.Site)

#saveRDS(proj_data, "assignment01_finaldata.RDS")
#proj_data<-readRDS("assignment01_finaldata.RDS")

#version 1 of graphing
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  xlab("County") +
  ylab("All Site Count")

ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("All Site Count")

ggplot(proj_data, aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")

ggplot(na.omit(proj_data), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")

ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")

pdf("all.cases.pdf", width=8.5, height=11)
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("All Site Count")
dev.off()

pdf("both_graphs.pdf", width=8.5, height=11)
ggplot(proj_data, aes(x = reorder(County, All.Site), y = All.Site)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("All Site Count")

ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
dev.off()

pancreas_graph<-ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_col() +
  coord_flip()+
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")

pdf("pancreas_graph.pdf", width=8.5, height=11)
pancreas_graph
dev.off()

ggplot(drop_na(proj_data, pct_pancreas), aes(x = reorder(County, pct_pancreas), y = pct_pancreas)) +
  geom_point() +
  coord_flip()+
  xlab("County") +
  ylab("% Cancer cases that are pancreatic")
