library(lavaan)
library(ggpmisc)
library(readxl)
library(tidyverse)

rankings<-readRDS("normed_chr.RDS")
income<-read_excel("income.xlsx")
obes_fei<-read_excel("obes_fei.xlsx")

rankings<-read_excel("chr_rankings.xlsx")

n_rankings<-rankings %>%
  group_by(State) %>%
  mutate(ho_norm = BBmisc::normalize(ho_rank, method="range", range=c(0,1)),
         hf_norm = BBmisc::normalize(hf_rank, method="range", range=c(0,1)))

isaac<-n_rankings %>%
  left_join(income) %>%
  left_join(obes_fei)

state_incomes<-filter(income, is.na(County))
state_income_ranks<-state_incomes %>%
  mutate(income_rank=rank(desc(med_income)))

isaac<-isaac %>%
  mutate(ho_score=1-ho_norm,
         hf_score=1-hf_norm)

isaac_sir<-left_join(isaac, select(state_income_ranks, State,income_rank), by="State")

pdf("chr_demo.pdf", width=11, height=8)
ggplot(isaac_sir, aes(x=fct_reorder(as.factor(State), income_rank), y=med_income))+
  geom_point(aes(color=ho_score), alpha=0.5)+  
  scale_color_gradient(low="red",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ggplot(isaac_sir, aes(x=fct_reorder(as.factor(State), income_rank), y=fei))+
  geom_point(aes(color=ho_score), alpha=0.5)+  
  scale_color_gradient(low="red",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ggplot(isaac_sir, aes(x=fct_reorder(as.factor(State), income_rank), y=pct_obesity))+
  geom_point(aes(color=ho_score), alpha=0.5)+  
  scale_color_gradient(low="red",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ggplot(isaac_sir, aes(x=fct_reorder(as.factor(State), income_rank), y=pct_obesity))+
  geom_point(aes(color=ho_score), alpha=0.5)+  
  scale_color_gradient2(midpoint=0.5, low="red",mid="black",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ggplot(isaac_sir, aes(x=fei, y=pct_obesity))+
  geom_point(aes(color=ho_score), alpha=0.3)+  
  scale_color_gradient2(midpoint=0.5, low="red",mid="black",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ggplot(isaac_sir, aes(x=med_income, y=pct_obesity))+
  geom_point(aes(color=ho_score), alpha=0.3)+  
  scale_color_gradient2(midpoint=0.5, low="red",mid="black",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ggplot(isaac_sir, aes(x=fei, y=med_income))+
  geom_point(aes(color=ho_score), alpha=0.3)+  
  scale_color_gradient2(midpoint=0.5, low="red",mid="black",high="green")+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))
dev.off()