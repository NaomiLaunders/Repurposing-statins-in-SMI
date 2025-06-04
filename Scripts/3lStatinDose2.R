rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)

####Load 2023 data####
load("StatinCPRD/Data/Trial_SMI.rdata")
load("StatinCPRD/Data/ValidStatin.rdata")

####For statin trials####
#Limit to those in our cohort
Valid<-subset(Valid, patid %in% Trial$patid)

#Bring in dates
Dates<-select(Trial, patid, Trial1StatDate, Trial2StatDate, FirstAPTrial3Date)
Valid<-merge(x=Valid, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

#Find valid of first statin trial 1
Stat<-Valid%>%
  subset(issuedate==Trial1StatDate)%>%
  subset(Statin!="Fluvastatin" & Statin!="Other")%>%
  group_by(patid)%>%
  select(patid, Statin, StatDose1=StatDose, ExactStatDose1=TotalDose)%>%
  distinct()%>%
  mutate(n=n())

length(which(Stat$n>1))
table(Stat$Statin)

Check<-subset(Stat, n>1)
#Make the one with both, moderate
Stat$StatDose1[Stat$n>1]<-"Moderate"
Stat<-Stat%>%
  group_by(patid)%>%
  mutate(ExactStatDose1=case_when(n>1 ~ median(ExactStatDose1),
                                  TRUE ~ ExactStatDose1))
Check<-subset(Stat, n>1)

Stat<-select(Stat, -n, -Statin)
Stat<-distinct(Stat)

#How many 
length(which(!is.na(Trial$Trial1StatDate))) #18610/19033

Trial<-merge(x=Trial, y=Stat, by="patid", all.x=TRUE, all.y=FALSE)

####For antipsychotic trials####

#Simvastatin and atorvastatin have the same dosage so can combine
AP<-Valid%>%
  subset(issuedate<=FirstAPTrial3Date)%>%
  subset(Statin=="Simvastatin"|Statin=="Atorvastatin")%>%
  group_by(patid)%>%
  mutate(MaxStat=max(issuedate))%>%
  subset(MaxStat==issuedate)%>%
  select(patid, Statin, StatDoseTrial3=StatDose, StatDateTrial3=issuedate, ExactStatDose3=TotalDose)%>%
  distinct()%>%
  mutate(n=n())

Check<-subset(AP, n>1)
table(AP$Statin)
AP<-select(AP, -n, -Statin)

length(which(!is.na(Trial$FirstAPTrial3Date))) #8543/8601

Trial<-merge(x=Trial, y=AP, by="patid", all.x=TRUE, all.y=FALSE)

####Last statin####
APLast<-Valid%>%
  subset(issuedate<=FirstAPTrial3Date)%>%
  subset(Statin=="Simvastatin"|Statin=="Atorvastatin")%>%
  group_by(patid)%>%
  mutate(MaxStat=max(issuedate))%>%
  subset(MaxStat==issuedate)%>%
  select(patid, Statin, StatDoseTrial3=StatDose, StatDateTrial3=issuedate)%>%
  distinct()%>%
  mutate(n=n())

APLast$LastStatin<-APLast$Statin
APLast<-select(APLast, patid, LastStatin)
AP<-distinct(APLast)

Trial<-merge(x=Trial, y=AP, by="patid", all.x=TRUE, all.y=FALSE)

#Need to add in the rest if the statin dose is not known
load("VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")
AllStat<-merge(x=AllStat, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)
APStat<-subset(AllStat, patid %in% Trial$patid & issuedate<=FirstAPTrial3Date & Statin=="Simvastatin"|Statin=="Atorvastatin" & !is.na(FirstAPTrial3Date))
APStat<-subset(APStat, !patid %in% AP$patid)

APStat<-APStat%>%
  group_by(patid)%>%
  mutate(MaxStat=max(issuedate))%>%
  subset(MaxStat==issuedate)%>%
  select(patid, LastStatin=Statin)%>%
  distinct()%>%
  mutate(n=n())

APStat<-select(APStat, -n)

Trial<-merge(x=Trial, y=APStat, by="patid", all.x=TRUE, all.y=FALSE)

Trial<-Trial%>%
  mutate(LastStatin = coalesce(LastStatin.x, LastStatin.y))%>%
  select(-LastStatin.x, -LastStatin.y)

table(Trial$LastStatin)

save(Trial, file="StatinCPRD/Data/Trial_Stat.rdata")
