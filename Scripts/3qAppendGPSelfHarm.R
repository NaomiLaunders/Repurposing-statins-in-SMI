
rm(list = ls(all.names = TRUE))

library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)
library(lubridate)

load("StatinCPRD/Data/Trial_Time.rdata")

load("VariableExtracts/CPRD2023/MergedObs/SelfHarm.Rdata")
PatSelfHarmAll<-subset(PatSelfHarmAll, patid %in% Trial$patid)

####If it has suicide; did they have a date of death within 30 days?####
length(unique(PatSelfHarmAll$patid))
table(PatSelfHarmAll$Group)

Suicide<-subset(PatSelfHarmAll, grepl("suicide", PatSelfHarmAll$term, ignore.case=TRUE) & grepl("and|or|\\+", PatSelfHarmAll$term, ignore.case=TRUE))
table(Suicide$term)

Death<-select(Trial, patid, died, deathdate)
Suicide<-merge(x=Suicide, y=Death, by="patid", all.x=TRUE, all.y=FALSE)

Suicide$TimeToDeath<-as.numeric(Suicide$deathdate - Suicide$eventdate)
summary(Suicide$TimeToDeath)

Suicide<-subset(Suicide, TimeToDeath<30)
Suicide$patdate<-paste0(Suicide$patid, "-", Suicide$eventdate)
PatSelfHarmAll$patdate<-paste0(PatSelfHarmAll$patid, "-", PatSelfHarmAll$eventdate)

PatSelfHarmAll<-subset(PatSelfHarmAll, !(patdate %in% Suicide$patdate))

####Self harm in the 6 months prior####
Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date, end)
PatSelfHarmAll<-merge(x=PatSelfHarmAll, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Only include if before end
PatSelfHarmAll<-subset(PatSelfHarmAll, eventdate<=end & eventdate<=as.Date("2019-12-31"))

PriorSHGPStat<-PatSelfHarmAll%>%
  subset(eventdate<=Trial1StatDate & Trial1StatDate-eventdate<=182.625)%>%
  group_by(patid)%>%
  summarise(PriorSHGPStatAll=n(),PriorSHGPStatIntent=sum(Group=="unknown intent"), PriorSHGPStatSH=sum(Group=="self harm"))

length(unique(PriorSHGPStat$patid))

table(PriorSHGPStat$PriorSHGPStatIntent, PriorSHGPStat$PriorSHGPStatSH)
table(PriorSHGPStat$PriorSHGPStatAll)

PriorSHGPAP<-PatSelfHarmAll%>%
  subset(eventdate<=FirstAPTrial3Date & FirstAPTrial3Date-eventdate<=182.625)%>%
  group_by(patid, Group)%>%
  summarise(PriorSHGPAPAll=n(),PriorSHGPAPIntent=sum(Group=="unknown intent"), PriorSHGPAPSH=sum(Group=="self harm"))

length(unique(PriorSHGPAP$patid))

Trial$PriorSHGPStat<-0
Trial$PriorSHGPStat[Trial$patid %in% PriorSHGPStat$patid]<-1

Trial$PriorSHGPAP<-0
Trial$PriorSHGPAP[Trial$patid %in% PriorSHGPAP$patid]<-1

####Outcomes: statin####
OutcomeGPSHStat12w<-PatSelfHarmAll%>%
  subset(eventdate>Trial1StatDate & eventdate-Trial1StatDate<=84)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPStat12wk=n())

OutcomeGPSHStat6mo<-PatSelfHarmAll%>%
  subset(eventdate>Trial1StatDate & eventdate-Trial1StatDate<=182.625)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPStat6mo=n())

OutcomeGPSHStat12mo<-PatSelfHarmAll%>%
  subset(eventdate>Trial1StatDate & eventdate-Trial1StatDate<=365.25)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPStat12mo=n())

OutcomeGPSHStat24mo<-PatSelfHarmAll%>%
  subset(eventdate>Trial1StatDate & eventdate-Trial1StatDate<=730.5)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPStat24mo=n(), FirstSHGPStat24mo=min(eventdate))


####Outcomes: AP####
#These need to be shifted by 30.4375 days
OutcomeGPSHAP12w<-PatSelfHarmAll%>%
  subset(eventdate>FirstAPTrial3Date & eventdate-FirstAPTrial3Date>=30.4375 & eventdate-FirstAPTrial3Date<=84+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPAP12wk=n())

OutcomeGPSHAP6mo<-PatSelfHarmAll%>%
  subset(eventdate>FirstAPTrial3Date & eventdate-FirstAPTrial3Date>=30.4375 & eventdate-FirstAPTrial3Date<=182.625+30.437)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPAP6mo=n())

OutcomeGPSHAP12mo<-PatSelfHarmAll%>%
  subset(eventdate>FirstAPTrial3Date & eventdate-FirstAPTrial3Date>=30.4375 & eventdate-FirstAPTrial3Date<=365.25+30.437)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPAP12mo=n())

OutcomeGPSHAP24mo<-PatSelfHarmAll%>%
  subset(eventdate>FirstAPTrial3Date & eventdate-FirstAPTrial3Date>=30.4375 & eventdate-FirstAPTrial3Date<=730.5+30.437)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGPAP24mo=n(), FirstSHGPAP24mo=min(eventdate))

Trial<-merge(x=Trial, y=OutcomeGPSHStat12w, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSHStat6mo, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSHStat12mo, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSHStat24mo, by="patid", all.x=TRUE, all.y=FALSE)

Trial<-merge(x=Trial, y=OutcomeGPSHAP12w, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSHAP6mo, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSHAP12mo, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSHAP24mo, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file = "StatinCPRD/Data/SH.rdata")
  

