questionnaires <- function(filename,subjectorder)
  
{

# needed packages
library(tidyverse)
library(anytime)
library(readr)
library(car)

#load data

dati <- read.csv(filename, sep=",")

## Preparing dataframe
#### Removing unused variables
dati <- dati[,-1]
rownames(dati) <- c()

IRI <- paste0("iri_", 1:28)
TAS <- paste0("tas_", 1:20)
colnames(dati)[7:34] <- IRI #rinomino item IRI
colnames(dati)[35:54] <- TAS #rinomino item TAS
colnames(dati)[1] <- "name"
colnames(dati)[2] <- "surname"
colnames(dati)[3] <- "gender"
colnames(dati)[4] <- "age"
colnames(dati)[5] <- "hand"
colnames(dati)[6] <- "eye"

ordine.corretto<-subjectorder
dati<-dati[ordine.corretto,]

id <- 1 : nrow(dati)
dati<-cbind(id,dati)

# Crearing ID factor for repeated measure analysis

dati$id <- as.factor(dati$id)

rownames(dati) <- c()
dati <- dati[,-c(2,3)]

#IRI
dati$iri_3 <- recode(dati$iri_3, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_4 <- recode(dati$iri_4, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_7 <- recode(dati$iri_7, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_12 <- recode(dati$iri_12, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_13 <- recode(dati$iri_13, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_14 <- recode(dati$iri_14, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_15 <- recode(dati$iri_15, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_18 <- recode(dati$iri_18, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$iri_19 <- recode(dati$iri_19, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")

iri_tot <- apply(dati[,IRI],1,sum)

# For IRI, scoring of subscales is possibile too. Subscales are:  
#   
#   - **Fantasy** Items: 1,5,7,12,16,23,26
# - **Perspective Taking** Items: 3,8,11,15,21,25,28
# - **Empathic Concern** Items: 2,4,9,14,18,20,22
# - **Personal Distress** Items: 10,13,17,19,24,27  

fantasy <- apply(dati[,IRI[c(1,5,7,12,16,23,26)]],1,sum)
perspective_taking <- apply(dati[,IRI[c(3,8,11,15,21,25,28)]],1,sum)
empathic_concern <- apply(dati[,IRI[c(2,4,9,14,18,20,22)]],1,sum)
personal_distress <- apply(dati[,IRI[c(6,10,13,17,19,24,27)]],1,sum)

#TAS
dati$tas_4 <- recode(dati$tas_4, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_5 <- recode(dati$tas_5, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_10 <- recode(dati$tas_10, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_18 <- recode(dati$tas_18, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_19 <- recode(dati$tas_19, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")

tas_tot <- apply(dati[,TAS],1,sum)

dati <- cbind(dati,iri_tot, fantasy, perspective_taking, personal_distress, empathic_concern, tas_tot)
attach(dati)

scoring <- data.frame(id,age,gender,eye,hand,fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)

detach(dati)


return(dati)
}


# write.csv(dati, "Data/Questionnaires_cleaned.csv")
# # write.csv(scoring, "Scoring/Datasets_Scoring/scoring_questionari.csv")
# save(Questionnaires, file ="DATA/Questionnaires_cleaned.rda")
# # save(scoring, file = "Scoring/Datasets_Scoring/scoring_questionari.rda")
# # save(iri_tot,fantasy,empathic_concern,perspective_taking, personal_distress, file = "Scoring/Datasets_Scoring/iri.rda")
# # save(tas_tot, file = "Scoring/Datasets_Scoring/tas.rda")


###########################################################################
#                                   END                                   #
###########################################################################