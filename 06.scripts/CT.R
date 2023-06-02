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
library(ggh4x)
library(psych)

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
  ggplot() +
  geom_pirate(aes(x=Emotion, y=time), #, fill = Mimicry, shape = Mimicry),
               bars = FALSE,
               show.legend = TRUE,
              points_params = list(shape = 1, size = 1))+
  facet_grid(~Mimicry, 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x") +
labs(x="Emotion",y="Cumulative Time (ms)")+
  theme_classic()+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom")+
  scale_x_discrete(NULL, guide = "axis_nested")

dat<-CTANOVA%>%
  filter(time!=0)%>%
  'colnames<-'(c("subject" ,"Mimicry", "Emotion","time"))%>%
  mutate(Mimicry = ifelse(Mimicry == "blocked","congruent","free"),
         group = paste0(Emotion," ",Mimicry))%>%
  drop_na(time)%>%
  select(time,group)

descriptives = describeBy(x = dat$time, group = dat$group)
group = c('happy congruent',
          'happy free',
          'mixed congruent',
          'mixed free',
          'neutral congruent',
          'neutral free')
means = c(descriptives$`happy congruent`$mean,
          descriptives$`happy free`$mean,
          descriptives$`mixed congruent`$mean,
          descriptives$`mixed free`$mean,
          descriptives$`neutral congruent`$mean,
          descriptives$`neutral free`$mean)
se = c(descriptives$`happy congruent`$se,
       descriptives$`happy free`$se,
       descriptives$`mixed congruent`$se,
       descriptives$`mixed free`$se,
       descriptives$`neutral congruent`$se,
       descriptives$`neutral free`$se)
plotdat=data.frame(group, means, se)

apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(size=16,family='Times New Roman'),
        legend.title=element_blank(),
        legend.position="bottom",
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data frame of summary statistics ('plotdat')
#and map group to x-axis and means to y-axis

p1 = ggplot(data = plotdat, aes(x = group, y = means, shape = group))+
  #Insert bean plot based on data frame of raw data ('dat')
  #and map group to x-axis and raw dv scores to y-axis
  geom_violin(data= dat, aes(x = group, y = time))+
  #Likewise, add raw data points (jittered) with same mappings
  geom_jitter(data= dat, aes(x = group, y = time), shape = 1, width = .1)+
  #Add data points (with no unique mapping manually specified, the data
  #points will fall back on reflecting our originally-specified data,
  #the means of each group)
  geom_point(size = 3)+
  #Add error bars for 95% CIs of each group mean
  geom_errorbar(ymax= means+(1.96*se), ymin=means+(-1.96*se), width = 0.25)+
  labs(x="Emotion",y="Cumulative Time (ms)")+
  #Apply the APA-format theme object
  apatheme+
  xlab(label = "Emotion")+
  ylab(label ="Cumulative Time (ms)")





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