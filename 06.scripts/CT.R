#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           18/06/2021
# Description:    Cumulative Time CT analysis
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
library(bayestestR)
library(BayesFactor)
library(ggpirate)


## loading data ----
rm(list=ls())
load("04.data_preprocessing/qualia_happy.RData")

############### Cumulative Time CT
############### CT ----
# dataset  ----
CT<-rivalry_dataset%>%
  select(subject, mimicry, key,expression, trial,  duration)%>%
  group_by(subject, mimicry, expression, trial)%>%
  summarise_at(vars(duration), list(sum))%>%
  spread(expression,duration,fill=0)%>%
  select(subject ,mimicry ,trial, happy, mixed, neutral)%>%
  gather(expression,time,4:6)%>%
  as.data.frame()

# summary CT  ----
CT%>%
  group_by(mimicry, expression) %>%
  summarise_at(vars(time), list(mean,sd))%>%
  'colnames<-'(c("Mimicry","Emotion","Time","Sd"))%>%
  as.data.frame%>% 
  mutate(Time = Time/1000,Sd = Sd/1000) 

# data ANOVA
CTANOVA<-CT%>%
  filter(subject!=1,  subject!=23)%>%
 group_by(subject,mimicry,expression) %>%
  summarise_at(vars(time), list(mean))%>%
  as.data.frame()

# plot CT  ----
CTANOVA%>%
  filter(time!=0)%>%
  spread(mimicry,time)%>%
  data.frame()%>%
  'colnames<-'(c("subject", "Emotion" ,"blocked","free"))%>%
ggplot(aes(x=blocked,y=free) )+
  geom_point(aes( color=Emotion, shape=Emotion ),size=3)+ 
  #geom_text(aes(color=Emotion, shape=Emotion,label= subject))+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="CT Congruent (ms)",y="CT Free (ms)")+
  coord_fixed()+
  expand_limits( y=c(0,15000),x=c(0,15000))+
  theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# barplot CT  ----
CTANOVA%>%
  filter(time!=0)%>%
  'colnames<-'(c("subject" ,"Mimicry", "Emotion","time"))%>%
  mutate(Mimicry = ifelse(Mimicry == "blocked","congruent","free"))%>%
  group_by(Emotion,Mimicry) %>%
  # summarise( 
  #   n=n(),
  #   mean=mean(time),
  #   sd=sd(time))%>%
  # mutate( se=sd/sqrt(n))%>%
  #'colnames<-'(c( "Emotion","Mimicry","n","CT", "sd" , "se"))%>%
  ggplot(aes(x=Emotion, y=time, fill= Mimicry)) +
  geom_pirate()+
  #geom_bar(  stat="identity", position = position_dodge(width = 0.9)) +
  #geom_errorbar( aes(x=Emotion, ymin=CT-se, ymax=CT+se), position = position_dodge(width = 0.9), width=0.4, alpha=0.9, size=.3)+
  labs(x="Emotion",y="Cumulative Time (ms)")+
  theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c("#008b39","#fd345a"))


ggsave("07.figures/CThappy.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# Anova CT  ----

a1 <- aov_ez("subject", "time", CTANOVA,  within = c("mimicry", "expression"))
table<-a1$anova_table
F_to_d(table[1,4],table[1,1],table[1,2])
F_to_eta2(table[2,4],table[2,1],table[2,2])
F_to_eta2(table[3,4],table[3,1],table[3,2])



m1<-emmeans(a1,pairwise~ expression,adjust="bonf")
table<-m1$contrasts%>%data.frame
t_to_d(table$t.ratio[2],table$df[2])
t_to_d(table$t.ratio[1],table$df[1])



m2<-emmeans(a1,pairwise~ mimicry|expression,adjust="bonf")
t_to_d(3.35,27)
t_to_d(1.596,27)
t_to_d(-0.597,27)

#bayesfactor
CTANOVA<-CTANOVA%>%
  mutate(mimicry = as.factor(mimicry),
         expression = as.factor(expression),
         subject = as.factor(subject))

Bfhappy<-CTANOVA%>%
  filter(expression == "happy")
t.test(time ~ mimicry,Bfhappy, paired=TRUE)
ttestBF(x = Bfhappy$time[Bfhappy$mimicry == "blocked"],y  = Bfhappy$time[Bfhappy$mimicry == "free"], paired=TRUE)

Bfneutral<-CTANOVA%>%
  filter(expression == "neutral")
t.test(time ~ mimicry,Bfneutral, paired=TRUE)
ttestBF(x = Bfneutral$time[Bfneutral$mimicry == "blocked"],y  = Bfneutral$time[Bfneutral$mimicry == "free"], paired=TRUE)

Bfmixed<-CTANOVA%>%
  filter(expression == "mixed")
t.test(time ~ mimicry,Bfmixed, paired=TRUE)
ttestBF(x = Bfmixed$time[Bfmixed$mimicry == "blocked"],y  = Bfmixed$time[Bfmixed$mimicry == "free"], paired=TRUE)


#################################################
# 
# END
#
#################################################