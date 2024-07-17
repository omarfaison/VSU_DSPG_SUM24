library(tidyverse)
library(readxl)
library(networktools)
library(igraph)

data<-readRDS("collab_test.RDS")
colleges<-read.csv("dept_list_updated.csv")

c1<-select(colleges, dept1, col1)
c2<-select(colleges, dept2, col2)

add_col<-data %>% 
  left_join(c1, by="dept1") %>%
  left_join(c2, by="dept2")

saveRDS(add_col,"collab_test.RDS")
