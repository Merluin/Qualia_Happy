#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           18/06/2021
# Description:    Onset Resolution ORT analysis
#
#################################################

############### Parameters ----
## library ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(hrbrthemes)
library(emmeans)
library(effectsize)


## loading data ----
rm(list=ls())
load("04.data_preprocessing/qualia_happy.RData")

############### ORT ----
# dataset ----
ORT<-onset_dataset%>%
  filter(subject!=1,  subject!=23)%>%
  drop_na()%>%
  select(subject, mimicry, onset, initial_percept)%>%
  'colnames<-'(c("subject", "mimicry", "onset", "initial_percept"))%>%
  mutate(onset = log(onset))

# summary ORT  ----
full<-ORT%>%
  group_by(mimicry,initial_percept) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("mimicry","initial_percept","onset","sd"))%>%
  as.data.frame()
ip<-ORT%>%
  group_by(initial_percept) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("initial_percept","onset","sd"))%>%
  as.data.frame()
mi<-ORT%>%
  group_by(mimicry) %>%
  summarise_at(vars(onset), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  'colnames<-'(c("mimicry","onset","sd"))%>%
  as.data.frame()


# data ANOVA
ORTANOVA<-ORT%>%
  group_by(subject,mimicry,initial_percept) %>%
  summarise_at(vars(onset), list(mean))%>%
  data.frame()

# plot ORT  ----
ORTANOVA%>%
  spread(mimicry,onset)%>%
  filter(blocked +  free !=0 )%>%
  data.frame()%>%
  'colnames<-'(c("subject",  "IP",  "blocked" ,   "free"))%>%
  ggplot(aes(x=blocked,y=free) )+
  geom_point(aes(  color=IP, shape=IP),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="ORT Free (log)",x="ORT Congruent (log)")+
  coord_fixed()+
  expand_limits( y=c(6,10),x=c(6,10))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))

ggsave("07.figures/ORThappy.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# barplot CT  ----
ORTANOVA%>%
  'colnames<-'(c("subject" ,"Mimicry", "IP","time"))%>%
  group_by(IP,Mimicry) %>%
  summarise( 
    n=n(),
    mean=mean(time),
    sd=sd(time))%>%
  mutate( se=sd/sqrt(n))%>%
  mutate(Mimicry = ifelse(Mimicry == "blocked","congruent","free"))%>%
  'colnames<-'(c( "IP","Mimicry","n","ORT", "sd" , "se"))%>%
  ggplot(aes(x=IP, y=ORT, fill= Mimicry)) +
  geom_bar(  stat="identity", position = position_dodge(width = 0.9)) +
  geom_errorbar( aes(x=IP, ymin=ORT-se, ymax=ORT+se), position = position_dodge(width = 0.9), width=0.4, alpha=0.9, size=.3)+
  labs(x="Initial Percept",y="Onset Resolution Time log(ms)")+
  theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("07.figures/bar_ORThappy.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# pirate plot CT  ----
ORTANOVA%>%
  'colnames<-'(c("subject" ,"Mimicry", "IP","time"))%>%
  group_by(IP,Mimicry) %>%
  ggplot(aes(x=IP, y=time, fill= Mimicry)) +
  geom_pirate()+
  labs(x="Initial Percept",y="Onset Resolution Time log(ms)")+
  theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c("#008b39","#fd345a"))

  
ORTANOVA<-ORTANOVA%>%
  filter(subject != 22)
# Anova ORT ----
a1<-aov_ez("subject", "onset", ORTANOVA,  within = c("mimicry", "initial_percept"))
table<-a1$anova_table
F_to_d(table[1,4],table[1,1],table[1,2])
F_to_d(table[2,4],table[2,1],table[2,2])
f_to_eta2(table[3,4],table[3,1],table[3,2])


emmeans(a1,pairwise~ mimicry,adjust="bonf")
emmeans(a1,pairwise~ initial_percept,adjust="bonf")


#################################################
# 
# END
#
#################################################