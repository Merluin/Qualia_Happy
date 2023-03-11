#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           18/06/2021
# Description:    Questionaires Correlations analysis
#
#################################################
rm(list=ls())
############### Parameters ----
## library ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(lsmeans)
library(afex)
library(ggpirate)
library(PerformanceAnalytics)

############### loading data ----

load("04.data_preprocessing/qualia_happy.RData")

############### Data
# Questionnaire ----
Questionnaire<-Questionnaires%>%
  filter(id!=1,  id!=23)%>%
  select(id, age,fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)

#age
age<-Questionnaire%>%
  select(id, age)%>%
  summarise_at(vars(age), list(length,mean,sd))%>%
  'colnames<-'(c( "n","mean", "Sd"))

#age
hand<-Questionnaires%>%
  filter(id!=1,  id!=23)%>%
  select(id, hand)
table(hand$hand)

# IRI TAS summary
IRI<-Questionnaire%>%
  select(id, iri_tot)%>%
  drop_na()%>%
  summarise_at(vars(iri_tot), list(mean,sd))%>%
  'colnames<-'(c( "mean", "Sd"))
TAS<-Questionnaire%>%
  select(id, tas_tot)%>%
  summarise_at(vars( tas_tot), list(mean,sd))%>%
  'colnames<-'(c( "mean", "Sd"))


Questionnaire<-Questionnaire%>%
  select(fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)

# Valuation ----
val<-valuation_dataset %>%
  filter(subject!=1,subject!=23 )%>%
  select( subject,mimicry, stim.expression, valence,arousal) %>%
  group_by(subject,mimicry,stim.expression) %>%
  summarise_at(vars(valence,arousal), list(mean))%>%
  gather(cat,val,-c(subject,mimicry,stim.expression))%>%
  mutate(cond=paste0(cat,".",mimicry,".",stim.expression))%>%
  group_by(subject,cond,val) %>%
  select(subject,cond,val)%>%
  spread(cond,val)
val<-cbind(Questionnaire,val[,-1])%>%
  na.omit()

# OR ----
OR<-onset_dataset%>%
  filter(subject!=1,subject!=23 )%>%
  drop_na()%>%
  select(subject, mimicry, initial_percept, onset)%>%
  'colnames<-'(c("subject", "mimicry", "initial_pt", "onset"))%>%
  group_by(subject,mimicry,initial_pt) %>%
  summarise_at(vars(onset), list(mean))%>%
  as.data.frame()%>%
  mutate(cond=paste0("OR.",mimicry,".",initial_pt))%>%
  group_by(subject,cond,onset) %>%
  select(subject,cond,onset)%>%
  spread(cond,onset)
OR<-cbind(Questionnaire,OR[,-1])%>%
  na.omit()

# IP ----
IP<-onset_dataset %>%
  filter(subject!=1,subject!=23 )%>%
  select( subject,mimicry, initial_percept, onset)%>%
  mutate(freq = case_when(onset>=1 ~ 1))%>%
  na.omit()%>%
  group_by(subject,mimicry, initial_percept) %>%
  summarise_at(vars(freq), list(sum))%>%
  as.data.frame()%>%
  mutate(cond=paste0("IP.",mimicry,".",initial_percept))%>%
  group_by(subject,cond,freq) %>%
  select(subject,cond,freq)%>%
  spread(cond,freq)
IP<-cbind(Questionnaire,IP[,-1])%>%
  na.omit()

# Cumulative Time ----
  CT<-rivalry_dataset%>%
  filter(subject!=1,subject!=23 )%>%
  select(subject, mimicry, key,expression, trial,  duration)%>%
  group_by(subject, mimicry, expression, trial)%>%
  summarise_at(vars(duration), list(sum))%>%
  spread(expression,duration,fill=0)%>%
  select(subject ,mimicry ,trial, happy, mixed, neutral)%>%
  gather(emotion,time,4:6)%>%
  as.data.frame()%>%
  group_by(subject,mimicry,emotion) %>%
  summarise_at(vars(time), list(mean))%>%
  mutate(cond= paste0("CT.",mimicry,".",emotion))%>%
  group_by(subject) %>%
  select( subject,cond,time)%>%
  spread(cond,time)

CT<-cbind(Questionnaire,CT[,-1])%>%
  na.omit()         

############### Plot cor ----
jpeg("07.figures/cor_val_exp2.jpg", units="in", width=10, height=8, res=200)
val<-val%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"AR.BLO.HPY", "AR.BLO.NEU", "AR.FRE.HPY", "AR.FRE.NEU","VAL.BLO.HPY", "VAL.BLO.NEU", "VAL.FRE.HPY" ,"VAL.FRE.NEU"))
chart.Correlation(val, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(val[,2],val[,8])
p.adjust(p$p.value, method = "fdr", n = 6)
p<-cor.test(val[,3],val[,11])
p.adjust(p$p.value, method = "fdr", n = 6)
p<-cor.test(val[,4],val[,8])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI pd cor arousal congruent neutral r .52 p = 0.03
p<-cor.test(val[,5],val[,8])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI TOT cor arousal congruent neutral r .54 p = 0.023
p<-cor.test(val[,5],val[,10])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI TOT cor arousal free neutral r .5 p = 0.047
p<-cor.test(val[,5],val[,11])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI TOT cor valence congruent happy r -.5 p = 0.045

jpeg("07.figures/cor_ort_exp2.jpg", units="in", width=10, height=8, res=200)
OR<-OR%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"ORT.BLO.HPY", "ORT.BLO.NEU", "ORT.FRE.HPY", "ORT.FRE.NEU"))
chart.Correlation(OR, histogram=FALSE, pch=19,method ="pearson")
dev.off()


jpeg("07.figures/cor_ip_exp2.jpg", units="in", width=10, height=8, res=200)
IP<-IP%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"IP.BLO.HPY", "IP.BLO.NEU", "IP.FRE.HPY", "IP.FRE.NEU"))
chart.Correlation(IP, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(IP[,2],IP[,9])
p.adjust(p$p.value, method = "fdr", n = 3)
p<-cor.test(IP[,2],IP[,10])
p.adjust(p$p.value, method = "fdr", n = 3) # IRI.PT cor IP.free.neutral r = 0.5 p=0.029
p<-cor.test(IP[,5],IP[,10])  
p.adjust(p$p.value, method = "fdr", n = 3)


jpeg("07.figures/cor_ct_exp2.jpg", units="in", width=10, height=8, res=200)
CT<-CT%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"CT.BLO.HPY", "CT.BLO.MIX", "CT.BLO.NEU", "CT.FRE.HPY","CT.FRE.MIX", "CT.FRE.NEU"))
chart.Correlation(CT, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(CT[,2],CT[,7])
p.adjust(p$p.value, method = "bonferroni", n = 4)
p<-cor.test(CT[,2],CT[,12]) 
p.adjust(p$p.value, method = "bonferroni", n = 4)
p<-cor.test(CT[,3],CT[,11]) 
p.adjust(p$p.value, method = "bonferroni", n = 4)
p<-cor.test(CT[,4],CT[,8]) 
p.adjust(p$p.value, method = "bonferroni", n = 4)




#################################################
# 
# END
#
#################################################
