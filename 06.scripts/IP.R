#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           18/06/2021
# Description:    spontaneous choice analysis
#
#################################################

rm(list=ls())
# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(broom.mixed)


# Loading Data ------------------------------------------------------------

load("04.data_preprocessing/qualia_happy.RData")


# Setting the factors reference level

onset_dataset <- onset_dataset %>%
  filter(subject!=1,  subject!=23)%>%
  drop_na(initial_percept) %>% 
  mutate(initial_percept = factor(initial_percept),
         mimicry = factor(mimicry)) %>% 
  tibble()

# Relevel the mimicry factor

onset_dataset$mimicry = relevel(onset_dataset$mimicry, ref = "free") # reference level mimicry-free
onset_dataset$initial_percept_01 = ifelse(onset_dataset$initial_percept == "happy", 1,0) # 1 is happy and 0 is neutral

# Models Fitting -----------------------------------------------------------

fit <- glmer(initial_percept_01 ~ mimicry + (1|subject),
             data = onset_dataset,
             family = binomial(link = "logit"))

tidy(fit)

confint(fit)

trials<-onset_dataset%>%
  mutate(initial_percept_01 = 1)

table(trials$initial_percept_01, trials$mimicry, trials$initial_percept)

#################################################
# 
# END
#
#################################################