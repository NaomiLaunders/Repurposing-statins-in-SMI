rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(tidyverse)
library(tableone)
library(lubridate)

####Load SMI cohort####
load("MergedFile/SMI2023Final.Rdata")
CPRD<-AllSMI2023Final
rm(AllSMI2023Final)

####Sort out statins####
load("VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")
YOB<-select(CPRD, patid, yob)
length(which(AllStat$patid %in% CPRD$patid))

AllStat<-subset(AllStat, patid %in% CPRD$patid)

AllStat<-merge(x=AllStat, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

StatProd<-subset(AllStat, issuedate<="2019-12-31" & year(issuedate)>yob) #Because ever diagnosed
rm(AllStat)

table(StatProd$Statin)

####First Prescribed statin####

Stat<-StatProd%>%
  group_by(patid)%>%
  mutate(FirstStatDate=min(issuedate), LastStatDate = max(issuedate))%>%
  select(patid, FirstStatDate, LastStatDate)%>%
  distinct()

length(unique(Stat$patid))

CPRD<-merge(x=CPRD, y=Stat, by="patid", all.x=TRUE, all.y=FALSE)
rm(Stat)

####First simvastatin####
Sim<-StatProd%>%
  subset(Statin=="Simvastatin")%>%
  group_by(patid)%>%
  mutate(FirstSimDate=min(issuedate), LastSimDate = max(issuedate))%>%
  select(patid, FirstSimDate, LastSimDate)%>%
  distinct()

length(unique(Sim$patid))

CPRD<-merge(x=CPRD, y=Sim, by="patid", all.x=TRUE, all.y=FALSE)
rm(Sim)

####First atorvastatin####
Ator<-StatProd%>%
  subset(Statin=="Atorvastatin")%>%
  group_by(patid)%>%
  mutate(FirstAtorDate=min(issuedate), LastAtorDate = max(issuedate))%>%
  select(patid, FirstAtorDate, LastAtorDate)%>%
  distinct()

length(unique(Ator$patid))

CPRD<-merge(x=CPRD, y=Ator, by="patid", all.x=TRUE, all.y=FALSE)
rm(Ator)

####First pravastatin####
Prav<-StatProd%>%
  subset(Statin=="Pravastatin")%>%
  group_by(patid)%>%
  mutate(FirstPravDate=min(issuedate), LastPravDate = max(issuedate))%>%
  select(patid, FirstPravDate, LastPravDate)%>%
  distinct()

length(unique(Prav$patid))

CPRD<-merge(x=CPRD, y=Prav, by="patid", all.x=TRUE, all.y=FALSE)
rm(Prav)

####First Rosuvastatin####
Rosu<-StatProd%>%
  subset(Statin=="Rosuvastatin")%>%
  group_by(patid)%>%
  mutate(FirstRosuDate=min(issuedate), LastRosuDate = max(issuedate))%>%
  select(patid, FirstRosuDate, LastRosuDate)%>%
  distinct()

length(unique(Rosu$patid))

CPRD<-merge(x=CPRD, y=Rosu, by="patid", all.x=TRUE, all.y=FALSE)
rm(Rosu, StatProd)

####Any statin indication####
load("VariableExtracts/CPRD2023/MergedObs/StatMedAll.Rdata")

length(which(PatStatMed$patid %in% CPRD$patid))
PatStatMed<-subset(PatStatMed, patid %in% CPRD$patid)

PatStatMed<-merge(x=PatStatMed, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

StatMed<-subset(PatStatMed, eventdate<="2019-12-31" & year(eventdate)>yob) #Because ever diagnosed
rm(PatStatMed)

table(StatMed$Term)

####First medcode statin####
Statin<-StatMed%>%
  group_by(patid)%>%
  mutate(FirstEvidenceStatinDate=min(eventdate))%>%
  select(patid, FirstEvidenceStatinDate)%>%
  distinct()

length(unique(Statin$patid))

CPRD<-merge(x=CPRD, y=Statin, by="patid", all.x=TRUE, all.y=FALSE)
rm(Statin, StatMed)

save(CPRD, file = "StatinCPRD/Data/StatAppend.Rdata")

####AP Products####
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")

PatAPAll<-subset(PatAPAll, patid %in% CPRD$patid)

PatAPAll<-merge(x=PatAPAll, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

APProd<-subset(PatAPAll, eventdate<="2019-12-31" & year(eventdate)>yob) #Because ever diagnosed
rm(PatAPAll)

table(APProd$AP)
table(APProd$Gen)

####First any psych####
Psych<-APProd%>%
  group_by(patid)%>%
  mutate(FirstPsychDate=min(eventdate), LastPsychDate = max(eventdate))%>%
  select(patid, FirstPsychDate, LastPsychDate)%>%
  distinct()

length(unique(Psych$patid))

CPRD<-merge(x=CPRD, y=Psych, by="patid", all.x=TRUE, all.y=FALSE)
rm(Psych)

####First Prescribed AP####
AP<-subset(APProd, Gen!=3)
AP<-AP%>%
  group_by(patid)%>%
  mutate(FirstAPDate=min(eventdate), LastAPDate = max(eventdate))%>%
  select(patid, FirstAPDate, LastAPDate)%>%
  distinct()

length(unique(AP$patid))

CPRD<-merge(x=CPRD, y=AP, by="patid", all.x=TRUE, all.y=FALSE)
rm(AP)

####First BP med####
BP<-subset(APProd, Gen==3)

BP<-BP%>%
  group_by(patid)%>%
  mutate(FirstBPDate=min(eventdate), lastBPDate=max(eventdate))%>%
  select(patid, FirstBPDate, lastBPDate)%>%
  distinct()

CPRD<-merge(x=CPRD, y=BP, by="patid", all.x=TRUE, all.y=FALSE)
rm(BP)

####First risperidone####
Risp<-APProd%>%
  subset(AP=="Risperidone")%>%
  group_by(patid)%>%
  mutate(FirstRispDate=min(eventdate), lastRispDate=max(eventdate))%>%
  select(patid, FirstRispDate, lastRispDate)%>%
  distinct()

length(unique(Risp$patid))

CPRD<-merge(x=CPRD, y=Risp, by="patid", all.x=TRUE, all.y=FALSE)
rm(Risp)


####First aripiprazole####

Arip<-APProd%>%
  subset(AP=="Aripiprazole")%>%
  group_by(patid)%>%
  mutate(FirstAripDate=min(eventdate), lastAripDate=max(eventdate))%>%
  select(patid, FirstAripDate, lastAripDate)%>%
  distinct()

length(unique(Arip$patid))

CPRD<-merge(x=CPRD, y=Arip, by="patid", all.x=TRUE, all.y=FALSE)
rm(Arip)

####First olanzapine####
Olan<-APProd%>%
  subset(AP=="Olanzapine")%>%
  group_by(patid)%>%
  mutate(FirstOlanDate=min(eventdate), lastOlanDate=max(eventdate))%>%
  select(patid, FirstOlanDate, lastOlanDate)%>%
  distinct()

length(unique(Olan$patid))

CPRD<-merge(x=CPRD, y=Olan, by="patid", all.x=TRUE, all.y=FALSE)


rm(Olan)
rm(APProd)

####Any AP indication####
load("VariableExtracts/CPRD2023/MergedObs/APMed.Rdata")

PatAPMedAll<-subset(PatAPMedAll, patid %in% CPRD$patid)
PatAPMedAll<-merge(x=PatAPMedAll, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

APMed<-subset(PatAPMedAll, eventdate<="2019-12-31" & year(eventdate)>yob) #Because ever diagnosed
rm(PatAPMedAll)

table(APMed$AP)
table(APMed$term)

APMed<-subset(APMed, term!="fetal valproate syndrome" & term!="Fetal valproate syndrome")

table(APMed$term[APMed$AP=="EvidenceOfInjectables"])

####First any psych
Psych<-APMed%>%
  group_by(patid)%>%
  mutate(FirstEvidencePsychDate=min(eventdate))%>%
  select(patid, FirstEvidencePsychDate)%>%
  distinct()

length(unique(Psych$patid))

CPRD<-merge(x=CPRD, y=Psych, by="patid", all.x=TRUE, all.y=FALSE)
rm(Psych)

####First medcode AP####
AP<-subset(APMed, AP!="EvidenceOfBP")
AP<-AP%>%
  group_by(patid)%>%
  mutate(FirstEvidenceAPDate=min(eventdate))%>%
  select(patid, FirstEvidenceAPDate)%>%
  distinct()

length(unique(AP$patid))

CPRD<-merge(x=CPRD, y=AP, by="patid", all.x=TRUE, all.y=FALSE)
rm(AP)

####First medcode BP####
BP<-subset(APMed, AP=="EvidenceOfBP")
BP<-BP%>%
  group_by(patid)%>%
  mutate(FirstEvidenceBPDate=min(eventdate))%>%
  select(patid, FirstEvidenceBPDate)%>%
  distinct()

length(unique(BP$patid))

CPRD<-merge(x=CPRD, y=BP, by="patid", all.x=TRUE, all.y=FALSE)
rm(BP)

####First medcode injectables####
Inj<-subset(APMed, AP=="EvidenceOfInjectables")
Inj<-Inj%>%
  group_by(patid)%>%
  mutate(FirstEvidenceInjDate=min(eventdate))%>%
  select(patid, FirstEvidenceInjDate)%>%
  distinct()

length(unique(Inj$patid))

CPRD<-merge(x=CPRD, y=Inj, by="patid", all.x=TRUE, all.y=FALSE)
rm(Inj)



save(CPRD, file = "StatinCPRD/Data/StatAP.Rdata")
