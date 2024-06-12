library(lavaan)
library(ggpmisc)
rankings<-readRDS("normed_chr.RDS")
income<-read_excel("income.xlsx")
obes_fei<-read_excel("obes_fei.xlsx")

rankings<-read_excel("chr_rankings.xlsx")

n_rankings<-rankings %>%
  group_by(State) %>%
  mutate(ho_norm = BBmisc::normalize(ho_rank, method="range", range=c(0,1)),
         hf_norm = BBmisc::normalize(hf_rank, method="range", range=c(0,1)))


isaac<-rankings %>%
  left_join(income) %>%
  left_join(obes_fei)

state_incomes<-filter(income, is.na(County))
state_income_ranks<-state_incomes %>%
  mutate(income_rank=rank(desc(med_income)))

  
top_5_states<-state_incomes %>% top_n(5, med_income) %>%
  pull(State)
bottom_5_states<-state_incomes %>% top_n(-5, med_income) %>%
  pull(State)

isaac<-isaac %>%
  mutate(ho_score=1-ho_norm,
         hf_score=1-hf_norm)

top_5<-isaac %>%
  filter(State %in% top_5_states)%>%
  mutate(group="top")

bottom_5<-isaac %>%
  filter(State %in% bottom_5_states)%>%
  mutate(group="bottom")

i_data<-rbind(top_5, bottom_5)

isaac_sir<-left_join(isaac, select(state_income_ranks, State,income_rank), by="State")

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

i_normed<-isaac_sir %>%
  mutate(inc_norm=BBmisc::normalize(med_income, method="range", range=c(0,1)),
         obs_norm=BBmisc::normalize(pct_obesity, method="range", range=c(0,1)),
         fei_norm=BBmisc::normalize(fei, method="range", range=c(0,1))) %>%
  ungroup()

i_cordata_raw<-select(i_normed, med_income:ho_score) 
i_cordata_norm<-select(i_normed, inc_norm:fei_norm, ho_score)

cor_raw<-cor(i_cordata_raw, use="complete.obs") 
cor_norm<-cor(i_cordata_norm, use="complete.obs")

#full interaction
mr_model<-lm(ho_score ~ med_income * pct_obesity * fei, data = i_normed)

#no interaction
mr_model2<-lm(ho_score ~ med_income + pct_obesity + fei, data = i_normed)

i_clean<-na.omit(i_normed)

#cfa
cfa_eq01<-"
fctr=~ inc_norm +obs_norm + fei_norm"

cfa01<-cfa(cfa_eq01, data=i_clean)

cfa_eq02<-"
fctr=~ inc_norm +obs_norm + fei_norm
ho_score ~ fctr"

cfa02<-cfa(cfa_eq02, data=i_clean)

summary(cfa02, fit.measures=T)

sem02<-sem(cfa_eq02, data=i_clean)

ggplot(i_data, aes(x=ho_score, y=med_income, color=group))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm", se=F)+
  stat_poly_eq()

inc_int_check<-lm(ho_score ~ med_income * group, data=i_data)

ggplot(i_data, aes(x=ho_score, y=fei, color=group))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm", se=F)+
  stat_poly_eq()

fei_int_check<-lm(ho_score ~ fei * group, data=i_data)

ggplot(i_data, aes(x=ho_score, y=pct_obesity, color=group))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm", se=F)+
  stat_poly_eq()

ob_int_check<-lm(ho_score ~ pct_obesity * group, data=i_data)