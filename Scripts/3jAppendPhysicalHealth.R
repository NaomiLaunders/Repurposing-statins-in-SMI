rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)

####Load 2023 data####
load("StatinCPRD/Data/Trial_BMI.rdata")

load("VariableExtracts/CPRD2023/MergedObs/PhysicalAll.Rdata")
PatPhysicalAll<-subset(PatPhysicalAll, patid %in% Trial$patid)

#Did they occur before date of birth
YoB<-select(Trial, patid, yob)
PatPhysicalAll<-merge(x=PatPhysicalAll, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
PatPhysicalAll<-subset(PatPhysicalAll, yob<year(eventdate))

table(PatPhysicalAll$cat, useNA="ifany")

#Sort heirarchy of diabetes

Diab<-subset(PatPhysicalAll, grepl("diab", cat, ignore.case=TRUE))

Diab<-Diab%>%
  group_by(patid)%>%
  mutate(EverT2=sum(cat=="Diabetes_Type 2"), EverT1 = sum(cat=="Diabetes_Type 1"), EverGest=sum(grepl("gest", cat, ignore.case=TRUE)), EverOther=sum(cat=="Diabetes_Other"))%>%
  mutate(Type=case_when(EverT2>0 ~ "T2",
                        EverT1>0 ~ "T1",
                        EverGest>0 ~ "Gest",
                        EverOther>0 ~ "Other",
                        TRUE ~ "Unknown"))

table(Diab$Type, useNA="ifany")

Diab<-subset(Diab, Type=="T2"|Type=="Unknown")
Hypertension<-subset(PatPhysicalAll, cat=="Hypertension")
MI<-subset(PatPhysicalAll, cat=="MI")
Cerebrovascular<-subset(PatPhysicalAll, cat=="Cerebrovascular")
CHF<-subset(PatPhysicalAll, cat=="CHF")

#create first diagnosis variables
Diab<-Diab%>%
  group_by(patid)%>%
  summarise(FirstDiab=min(eventdate, na.rm=TRUE))

Hypertension<-Hypertension%>%
  group_by(patid)%>%
  summarise(FirstHyp=min(eventdate, na.rm=TRUE))

MI<-MI%>%
  group_by(patid)%>%
  summarise(FirstMI=min(eventdate, na.rm=TRUE))

Cerebrovascular<-Cerebrovascular%>%
  group_by(patid)%>%
  summarise(FirstCerebrovascular=min(eventdate, na.rm=TRUE))

CHF<-CHF%>%
  group_by(patid)%>%
  summarise(FirstCHF=min(eventdate, na.rm=TRUE))

#Append first diagnosis dates to trial table
Trial<-merge(x=Trial, y=Diab, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=Hypertension, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=MI, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=Cerebrovascular, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=CHF, by="patid", all.x=TRUE, all.y=FALSE)

#Create variables for first diagnosis at statin or at AP
Trial<-Trial%>%
  mutate(DiabStat = case_when(FirstDiab<=Trial1StatDate ~ 1,
                        TRUE ~ 0),
         HypertensionStat = case_when(FirstHyp<=Trial1StatDate ~ 1,
                              TRUE ~ 0),
         MIStat = case_when(FirstMI<=Trial1StatDate ~ 1,
                              TRUE ~ 0),
         CerebrovascularStat = case_when(FirstCerebrovascular<=Trial1StatDate ~ 1,
                              TRUE ~ 0),
         CHFStat = case_when(FirstCHF<=Trial1StatDate ~ 1,
                              TRUE ~ 0),
         DiabAP = case_when(FirstDiab<=FirstAPTrial3Date ~ 1,
                              TRUE ~ 0),
         HypertensionAP = case_when(FirstHyp<=FirstAPTrial3Date ~ 1,
                                      TRUE ~ 0),
         MIAP = case_when(FirstMI<=FirstAPTrial3Date ~ 1,
                            TRUE ~ 0),
         CerebrovascularAP = case_when(FirstCerebrovascular<=FirstAPTrial3Date ~ 1,
                                         TRUE ~ 0),
         CHFAP = case_when(FirstCHF<=FirstAPTrial3Date ~ 1,
                             TRUE ~ 0))

save(Trial, file = "StatinCPRD/Data/Trial_Phys.rdata")

         




