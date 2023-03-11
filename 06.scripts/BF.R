#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           11/03/2023
# Description:   BF
#
#################################################


library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(hrbrthemes)
library(emmeans)
library(rstanarm)
library(BayesFactor)
library(tidybayes)
library(tidyverse)

# Files

load("04.data_preprocessing/qualia_happy.RData")
# Anova Dataset

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
  #filter(subject!=1,subject!=22,subject!=23,subject!=25,subject!=26,subject!=27,subject!=28,subject!=29)%>%
  #filter( subject != 1,subject != 23,subject != 22, subject != 26)%>%
  group_by(subject,mimicry,expression) %>%
  summarise_at(vars(time), list(mean))%>%
  as.data.frame()

a1 <- aov_ez("subject", "time", CTANOVA,  within = c("mimicry", "expression"))
a1

m2<-emmeans(a1,pairwise~ mimicry|emotion,adjust="bonf")
m2



# "Bayesian" Analysis
dat <- split(CTANOVA, CTANOVA$expression)

# Bayes Factor 10 (alternative/null)

bf_list <- map(dat, function(x){
  bf <- ttestBF(x$time[x$mimicry == "free"], x$time[x$mimicry == "blocked"], paired = T)
  extractBF(bf)$bf
})
bf_list

# Actual Paired t.test

map(dat, function(x){
  t.test(x$time[x$mimicry == "free"], x$time[x$mimicry == "blocked"], paired = T)
})


CTANOVA$subject <- factor(CTANOVA$subject)
CTANOVA$mimicry <- factor(CTANOVA$mimicry)
CTANOVA$emotion <- factor(CTANOVA$expression)

bf = anovaBF(time ~ mimicry*emotion + subject, data = CTANOVA, 
             whichRandom="subject")
plot(bf)