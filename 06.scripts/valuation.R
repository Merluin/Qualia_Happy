#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           18/06/2021
# Description:    Stimuli Valuation analysis
#
#################################################

############### Parameters ----
## library ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(lsmeans)
library(afex)
library(ggpirate)
library(effectsize)

## loading data ----
rm(list=ls())
load("04.data_preprocessing/qualia_happy.RData")

## Data
############### Valuation ----
# dataset ----
VAVplot<-valuation_dataset %>%
  filter(subject!=1,  subject!=23)%>%
  select( subject,mimicry, stim.expression,  valence, arousal)%>%
  group_by(subject,mimicry,stim.expression )%>%
  summarise_at(vars(valence,arousal), list(mean))%>%
  as.data.frame()

DSV<-VAVplot%>%
  group_by(mimicry,stim.expression )%>%
  summarise_at(vars(valence,arousal), list(mean,sd))%>%
  mutate_if(is.numeric, round,digits=3)%>%
  select( mimicry, stim.expression, valence_fn1,valence_fn2,arousal_fn1, arousal_fn2)%>%
  'colnames<-'(c("mimicry","face_emotion","valence_mean","valence_sd","arousal_mean","arousal_sd"))%>%
  as.data.frame()

# DSV<-VAVplot%>%
#   group_by(stim.expression )%>%
#   summarise_at(vars(valence,arousal), list(mean,sd))%>%
#   mutate_if(is.numeric, round,digits=3)%>%
#   select( stim.expression, valence_fn1,valence_fn2,arousal_fn1, arousal_fn2)%>%
#   'colnames<-'(c("face_emotion","valence_mean","valence_sd","arousal_mean","arousal_sd"))%>%
#   as.data.frame()


# plot valuatioon  ----
#plot
ggplot(VAVplot, aes(x=valence, y=arousal, color=mimicry, shape=stim.expression,label= subject)) +
  geom_point(size=6, alpha=0.6)+
  #geom_text()+
  scale_colour_manual(values=c( "#008b39","#fd345a"))+
  coord_cartesian(ylim = c(1,7),xlim = c(-3,3))+
  labs(x="Valence evaluation",y="Arousal Valuation",fill="Categories")+theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")



############### ANOVA VALENCE ----
# dataset ----
temp<-valuation_dataset %>%
  filter(subject!=1,subject!=23)
x<-temp%>%
  select( subject,mimicry,stim.expression, valence)%>%
  group_by( subject,mimicry,stim.expression) %>%
  summarise_at(vars(valence), list(mean))
colnames(x)<-c("Subject","Condition","Emotion","score")
x$Subject<-as.factor(x$Subject)

x%>%
  group_by(Emotion) %>%
  summarise_at(vars(score), list(mean,sd))%>%
  'colnames<-'(c("Mimicry","Valence","Sd"))%>%
  as.data.frame()

a1 <- aov_ez("Subject", "score", x,  within = c("Condition", "Emotion"))

afex_plot(a1, x = "Emotion", trace = "Condition", error = "within",mapping = c("color", "fill"),
          data_geom = geom_boxplot, data_arg = list(width = 0.4),
          point_arg = list(size = 1.5), line_arg = list(size = 1))+theme_classic()

table<-a1$anova_table
F_to_d(table[1,4],table[1,1],table[1,2])
F_to_d(table[2,4],table[2,1],table[2,2])
f_to_eta2(table[3,4],table[3,1],table[3,2])

m1<-emmeans(a1,pairwise~ Emotion,adjust="bonf")

table<-m1$contrasts%>%data.frame
t_to_d(table$t.ratio,table$df)

############### ANOVA AROUSAL ----
# dataset ----
x<-temp%>%
  select( subject,mimicry,stim.expression, arousal)%>%
  group_by( subject,mimicry,stim.expression) %>%
  summarise_at(vars(arousal), list(mean))
colnames(x)<-c("Subject","Condition","Emotion","score")
x$Subject<-as.factor(x$Subject)

x%>%
  group_by( Emotion) %>%
  summarise_at(vars(score), list(mean,sd))%>%
  'colnames<-'(c("Mimicry","Arousal","Sd"))%>%
  as.data.frame()

a2 <- aov_ez("Subject", "score", x,  within = c("Condition", "Emotion"))
table<-a2$anova_table
F_to_d(table[1,4],table[1,1],table[1,2])
F_to_d(table[2,4],table[2,1],table[2,2])
f_to_eta2(table[3,4],table[3,1],table[3,2])


afex_plot(a2, x = "Emotion", trace = "Condition", error = "within",mapping = c("color", "fill"),
          data_geom = geom_boxplot, data_arg = list(width = 0.4),
          point_arg = list(size = 1.5), line_arg = list(size = 1))+theme_classic()

m2<-emmeans(a2,pairwise~ Emotion,adjust="bonf")
table<-m2$contrasts%>%data.frame
t_to_d(table$t.ratio,table$df)
#################################################
# 
# END
#
#################################################