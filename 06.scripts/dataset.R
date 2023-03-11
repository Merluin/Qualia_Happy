###########################################################################
###########################################################################
#
#  Experiment:  QualiaHappy
#  Programmer:  UETTIER THOMAS
#  Date:        06/2021
#  Description: Responces from E-PRIME
#  Notes : A first step is to merge subject by E-Merge.
#   
###########################################################################
#                              SCRIPT QUALIA                              #
###########################################################################

rm(list=ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)

# Functions ---------------------------------------------------------------

source("05.functions/keypress.R")
source("05.functions/questionnaires.R")

# Importing Data ----------------------------------------------------------
eData <- read_delim(
  '03.original_data/QualiaHappy_merged.txt',   # The name of your *.txt file
  delim = '\t', 
) 
qualia<-eData%>%data.frame() # fix colname


# Subjects General Information---------------------------------------------
Info<-qualia%>%
  filter(Procedure.Block. != "TrainingProc")%>%
  select(Subject,Sex,Handedness, Chopstick, Session,FeliceNeutro.Session., Lenti, SessionDate)%>%
  mutate(Session = case_when(Chopstick=="YES"  ~ "free",
                             Chopstick=="NO"  ~ "blocked")) %>%
  mutate(Chopstick = case_when(Chopstick=="YES"  ~ "blocked",
                               Chopstick=="NO"  ~ "free")) %>%
  mutate(FeliceNeutro.Session. = case_when(FeliceNeutro.Session.=="F"  ~ "happy",
                                           FeliceNeutro.Session.=="N"  ~ "neutral")) %>%
  mutate(Lenti = case_when(Lenti=="VR"  ~ "green",
                           Lenti=="RV"  ~ "red"))


# Stimuli Valuation--------------------------------------------------------
# Valenza
qualia_valenza <- qualia %>%
  filter(Procedure.Trial. == "ValenzaTrialproc") %>%
  select(Subject,Chopstick,TAG,TAG1, COLORE1, ValImage.RESP)%>%
mutate(Chopstick = case_when(Chopstick=="YES"  ~ "free",
                           Chopstick=="NO"  ~ "blocked"),
        COLORE1 = case_when(COLORE1=="VERDE"  ~ "green",
                             COLORE1=="ROSSO"  ~ "red"),
        TAG1 = case_when(TAG1=="FELICE"  ~ "happy",
                          TAG1=="NEUTRO"  ~ "neutral"),
        TAG = case_when(TAG=="V_F1.png"  ~ "female",
                         TAG=="V_F2.png"  ~ "male",
                         TAG=="V_N1.png"  ~ "female",
                         TAG=="V_N2.png"  ~ "male",
                         TAG=="R_F1.png"  ~ "female",
                         TAG=="R_F2.png"  ~ "male",
                         TAG=="R_N1.png"  ~ "female",
                         TAG=="R_N2.png"  ~ "male"),
      ValImage.RESP = case_when(ValImage.RESP=="q"  ~ c(-3),
                                   ValImage.RESP=="w"  ~ c(-2),
                                   ValImage.RESP=="e"  ~ c(-1),
                                   ValImage.RESP=="r"  ~ c(0),
                                   ValImage.RESP=="t"  ~ c(1),
                                   ValImage.RESP=="y"  ~ c(2),
                                   ValImage.RESP=="u"  ~ c(3)),
      valuation = "valence")%>%
  select(Subject,Chopstick,TAG,TAG1, COLORE1,valuation, ValImage.RESP)%>%
  'colnames<-'(c("subject" ,"mimicry","stim.gender","stim.expression","stim.color" ,"valuation","score"))
# Arousal
qualia_arousal <- qualia %>%
  filter(Procedure.Trial. == "ArousalTrialproc") %>%
select(Subject,Chopstick,TAG,TAG1, COLORE1, ArouImage.RESP)%>%
  mutate(Chopstick = case_when(Chopstick=="YES"  ~ "free",
                               Chopstick=="NO"  ~ "blocked"),
         COLORE1 = case_when(COLORE1=="VERDE"  ~ "green",
                             COLORE1=="ROSSO"  ~ "red"),
         TAG1 = case_when(TAG1=="FELICE"  ~ "happy",
                          TAG1=="NEUTRO"  ~ "neutral"),
         TAG = case_when(TAG=="V_F1.png"  ~ "female",
                         TAG=="V_F2.png"  ~ "male",
                         TAG=="V_N1.png"  ~ "female",
                         TAG=="V_N2.png"  ~ "male",
                         TAG=="R_F1.png"  ~ "female",
                         TAG=="R_F2.png"  ~ "male",
                         TAG=="R_N1.png"  ~ "female",
                         TAG=="R_N2.png"  ~ "male"),
         valuation = "arousal")%>%
  select(Subject,Chopstick,TAG,TAG1, COLORE1,valuation, ArouImage.RESP)%>%
  'colnames<-'(c("subject" ,"mimicry","stim.gender","stim.expression","stim.color" ,"valuation","score"))

valuation_dataset<-rbind(qualia_valenza,qualia_arousal)%>%
  spread(valuation,score)


       
### Rivalry ---------------------------------------------------------------

maxresp<- (length(qualia[1,])-84) / 2 # eData is 84 columns str without ryvalry responses (Key press & kes RT)
keypress(maxresp) # create strings for selecting response columns

qualia_riv <- qualia %>%
  filter(Procedure.Trial. == "FeliceNeutroProc" | Procedure.Trial. == "NeutroFeliceProc" ) %>%
  select(Subject, Session, Trial,Chopstick, Handedness, Lenti, Sex, TAG, b, m, ColoreB, ColoreM, RESP, RT) %>%
  arrange(Subject)

for(i in 1:maxresp){ # replace Key pressed by visual awareness label 
  x<-qualia_riv%>%
    select(b,m,RESP[i])%>%
    'colnames<-'(c("B","M","R"))%>%
    mutate(R = case_when(B == "NEUTRO" & R == "b" ~ "neutral",
                         B == "FELICE" & R == "b" ~ "happy",
                         M == "NEUTRO" & R == "m" ~ "neutral",
                         M == "FELICE" & R == "m" ~ "happy",
                         R == "n" ~ "mixed"))
  
  j<-i+12 # 10 = nb columns before resp
  qualia_riv[,j]<-x$R
  
}

# Keys Duration -----------------------------------------------------------

x <- qualia_riv%>% # Keys Duration
  select(RT) # copy for computation
x[is.na(x)] <- 15000 # all NA value to 30000 (trial duration 30sec)
x<-x%>%mutate(max = 15000) # new column for last key
x <- x[,-1] - unlist(x) # unlist() for making difference with different length
colnames(x) <- DUR # Renaming Columns

# Final Dataframe 

qualia_riv <- cbind(qualia_riv, x)

# Cleaning 

qualia_rt <- qualia_riv %>%
  select( -RESP, -DUR)%>%
  gather(key = keys, value = rt, RT)

qualia_resp <- qualia_riv %>%
  select( -RT, -DUR)%>%
  gather(key = keys, value = resp, RESP)

qualia_dur <- qualia_riv %>%
  select( -RESP, -RT)%>%
  gather(key = keys, value = dur, DUR)
         
x<-cbind(qualia_resp,qualia_dur$dur,qualia_rt$rt)   
qualia_long <- x%>%
  'colnames<-'(c("subject","session","trial","mimicry" ,"handedness","lenti","gender","file" ,"B" ,"M", "b.color" , "m.color" ,"key"   
                 , "expression" ,  "duration" ,"rt" ))%>%
  mutate(key = parse_number(key),
         rt=as.numeric(rt),
         subject=as.numeric(subject),
         last.key = ifelse(is.na(rt),"yes","no"))%>%
  'row.names<-'(c(1:length(x[,1])))%>%
  arrange(subject,trial,session)%>%
  mutate(mimicry = case_when(mimicry=="YES"  ~ "free",
                             mimicry=="NO"  ~ "blocked"),
         lenti = case_when(lenti=="VR"  ~ "green",
                           lenti=="RV"  ~ "red"))
  




a<-which(qualia_long$last.key=="yes") # identify the trunked (end trial) response
b<-a-1
qualia_long$last.key[b]<-"yes"
xp_resp<-qualia_riv
rivalry_dataset<-qualia_long%>%
  select(subject,trial,mimicry,file,key,expression,duration,rt)
  

# ONSET dataset ----------------------------------------------------------------

ORT<-rivalry_dataset%>%
  filter(key==1)%>%
  select(subject,mimicry,rt,expression)%>%
  'colnames<-'(c("subject", "mimicry", "onset", "initial_percept"))

z<-qualia_riv%>% # identify max nb of key press before press happy or neutral as IP 
  select(RESP)
z[z!="mixed"]<-"xx"
rep=0
while(!is.na(z[1,1])){
  z<-z[z[,1]=="mixed",]
  z<-z[,-1]
  rep<-rep+1
}
rep<-rep

resp<-rivalry_dataset%>%
  filter(key==1)%>%
  select(expression,rt)

for(i in 2:rep){ #replace mixed by firrt IP and correct time for ORT
  nextresp<-rivalry_dataset%>%
    filter(key==i)%>%select(expression,rt)
  
  mix<-which(resp$expression=="mixed")
  
  resp$expression[mix]<-nextresp$expression[mix]
  resp$rt[mix]<- resp$rt[mix]+nextresp$rt[mix]
  
}

onset_dataset<-cbind(ORT[,1:2],resp)%>%
  'colnames<-'(c("subject", "mimicry", "initial_percept", "onset"))%>%
  drop_na(initial_percept)

# Questionnaires ---------------------------------------------------------------

subjectorder<-c( "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
Questionnaires<-questionnaires("03.original_data/questionnaires.csv",subjectorder)

# Save -----

save(onset_dataset,
     rivalry_dataset,
     valuation_dataset,
     Questionnaires,
     xp_resp,
     file = "04.data_preprocessing/qualia_happy.RData")
###########################################################################
#                                   END                                   #
###########################################################################





