####Define SMI at trial start date####

rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)

####Load 2023 data####
load("StatinCPRD/Data/Trial_Phys.rdata")

load("VariableExtracts/CPRD2023/MergedObs/SMI2023.Rdata")

PatSMI<-subset(PatSMI, patid %in% Trial$patid)

Dates<-select(Trial, patid, FirstAPTrial3Date, Trial1StatDate)
PatSMI<-merge(x=PatSMI, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

SMIStat<-PatSMI%>%
  subset(eventdate<=Trial1StatDate)%>%
  mutate(Schiz=case_when(Group=="schizophrenia" ~ 1,
                         TRUE ~ 0),
         Bipolar=case_when(Group=="bipolar" ~ 1,
                         TRUE ~ 0),
         Other=case_when(Group=="other psychosis" ~ 1,
                           TRUE ~ 0))%>%
  group_by(patid)%>%
  mutate(EverSchiz=sum(Schiz, na.rm=TRUE), EverBipolar=sum(Bipolar, na.rm=TRUE), EverOther=sum(Other, na.rm=TRUE))%>%
  mutate(SMIDiagStat=case_when(EverSchiz>0 ~ "Schiz",
                                 EverBipolar>0 ~ "Bipolar",
                                 EverOther>0 ~ "Other",
                                 TRUE ~ "Undefined"))
  
SMIAP<-PatSMI%>%
  subset(eventdate<=FirstAPTrial3Date)%>%
  mutate(Schiz=case_when(Group=="schizophrenia" ~ 1,
                         TRUE ~ 0),
         Bipolar=case_when(Group=="bipolar" ~ 1,
                           TRUE ~ 0),
         Other=case_when(Group=="other psychosis" ~ 1,
                         TRUE ~ 0))%>%
  group_by(patid)%>%
  mutate(EverSchiz=sum(Schiz, na.rm=TRUE), EverBipolar=sum(Bipolar, na.rm=TRUE), EverOther=sum(Other, na.rm=TRUE))%>%
  mutate(SMIDiagAP=case_when(EverSchiz>0 ~ "Schiz",
                                 EverBipolar>0 ~ "Bipolar",
                                 EverOther>0 ~ "Other",
                                 TRUE ~ "Undefined"))  
  
SMIStat<-select(SMIStat, patid, SMIDiagStat)
SMIStat<-distinct(SMIStat)
length(unique(SMIStat$patid))
length(which(!is.na(Trial$Trial1StatDate)))
table(SMIStat$SMIDiagStat)

SMIAP<-select(SMIAP, patid, SMIDiagAP)
SMIAP<-distinct(SMIAP)
length(unique(SMIAP$patid))
length(which(!is.na(Trial$FirstAPTrial3Date)))

Trial<-merge(x=Trial, y=SMIStat, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=SMIAP, by="patid", all.x=TRUE, all.y=FALSE)

Trial$SMIDiagAP<-case_when(is.na(Trial$SMIDiagAP)~ "Undefined",
                           TRUE ~Trial$SMIDiagAP )

table(Trial$SMIDiagAP, useNA="ifany")

save(Trial, file="StatinCPRD/Data/Trial_SMI.rdata")  
