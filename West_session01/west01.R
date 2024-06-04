library(tidyverse)
library(psych)
library(haven)
library(ggpubr)
library(sjPlot)
library(skimr)
library(ggcorrplot)

#load bfi dataset from psych pkg
data01<-bfi

#create scoring keys
keys<-list(agree=c("-A1", "A2", "A3", "A4","A5"),
           ev=c("-E1", "-E2", "E3", "E4", "E5"),
           neur=c("N1","N2","N3","N4","N5"))

#create variables
bfiscores<-scoreItems(keys=keys, items=data01, min=1, max=6)

#append BFI scores to data1
data01<-cbind(data01,bfiscores$scores)

#create descriptives object
desc<-describe(data01[,29:31])

#generate formatted table
tab_df(desc)

#create histogram
gghistogram(data01$agree) #using ggpubr

ggplot(data01, aes(x=agree))+
  geom_histogram()

hist(data01$agree)

#split histogram by gender
ggplot(data01, aes(x=agree))+
  geom_histogram()+
  facet_wrap(vars(gender))

#create boxplots to look for outliers
ggboxplot(data=data01$agree)

#z score the data to look for outliers
data01$Zagree<-scale(data01$agree)
range(data01$Zagree) #values >= |3| are outliers


#load violence data
vdata<-read.csv("violence_data.csv")

#split out month/year
vdata<-vdata %>%
  mutate(month=month(date),
         year=year(date))

ggplot(vdata, aes(x=as.factor(month)))+
  geom_bar(stat="count")

#convert month into factored/labeled text
vdata$month<-factor(vdata$month, levels=c(1:12),
                    labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec"))

ggplot(vdata, aes(x=month))+
  geom_bar(stat="count")

#create seasons
vdata<-vdata %>%
  mutate(Nmonth=as.numeric(month),
         Season=case_when(
           Nmonth<3 | Nmonth==12 ~ "Winter",
           Nmonth>=3 & Nmonth<6 ~ "Spring",
           Nmonth>=6 & Nmonth<10 ~ "Summer",
           Nmonth>=10 & Nmonth<12 ~ "Fall"))

ggplot(vdata, aes(x=Season))+
  geom_bar(stat="count")