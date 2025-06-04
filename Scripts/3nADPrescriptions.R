####Check which An the six months before start####

rm(list = ls(all.names = TRUE))

library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)
library(lubridate)

load("StatinCPRD/Data/Trial_Chol.rdata")
load("VariableExtracts/CPRD2023/MergedObs/ADMedAurum.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/GoldADMed.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/GoldADProd.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/AurumADProd.Rdata")

AurumAD$patid<-paste0(AurumAD$patid, "-A")
AurumAD<-select(AurumAD, patid, eventdate, AD)
GoldAD$patid<-paste0(GoldAD$patid, "-G")
GoldAD<-select(GoldAD, patid, eventdate, AD)

AllAD<-rbind(AurumAD, GoldAD)
AllAD<-subset(AllAD, patid %in% Trial$patid)

rm(AurumAD, GoldAD)

Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date)
AllAD<-merge(x=AllAD, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

####Label up####
AllAD<-AllAD%>%
  mutate(Type=case_when(AD=="citalopram" | AD=="dapoxetine"|AD=="escitalopram"|AD=="fluoxetine"|
                          AD=="fluvoxamine"|AD=="paroxetine"|AD=="sertraline"|AD=="vortioxetine" ~ "SSRI",
         AD=="amitriptyline" | AD=="clomipramine"|AD=="dosulepin"|AD=="imipramine"|
                          AD=="lofepramine"|AD=="nortriptyline"|AD=="doxepin" |AD=="maprotiline"|AD=="mianserin"|
          AD=="mirtazapine" |AD=="trimipramine"~ "TCA",
         TRUE ~ "Other"))

table(AllAD$Type, AllAD$AD)
table(AllAD$Type)

#6 months before statins
StatAD<-AllAD%>%
  subset(eventdate<=Trial1StatDate)%>%
  subset(Trial1StatDate-eventdate<=182.625)

length(unique(StatAD$patid))/length(which(!is.na(Trial$Trial1StatDate)))

SSRIStat<-subset(StatAD, Type=="SSRI")
TCAStat<-subset(StatAD, Type=="TCA")
OtherStat<-subset(StatAD, Type=="Other")

#6 months before APs
APAD<-AllAD%>%
  subset(eventdate<=FirstAPTrial3Date)%>%
  subset(FirstAPTrial3Date-eventdate<=182.625)

SSRIAP<-subset(APAD, Type=="SSRI")
TCAAP<-subset(APAD, Type=="TCA")
OtherAP<-subset(APAD, Type=="Other")

length(unique(APAD$patid))/length(which(!is.na(Trial$FirstAPTrial3Date)))

Trial$SSRIStat<-0
Trial$SSRIStat[Trial$patid %in% SSRIStat$patid]<-1

Trial$TCAStat<-0
Trial$TCAStat[Trial$patid %in% TCAStat$patid]<-1

Trial$OtherADStat<-0
Trial$OtherADStat[Trial$patid %in% OtherStat$patid]<-1

Trial$SSRIAP<-0
Trial$SSRIAP[Trial$patid %in% SSRIAP$patid]<-1

Trial$TCAAP<-0
Trial$TCAAP[Trial$patid %in% TCAAP$patid]<-1

Trial$OtherADAP<-0
Trial$OtherADAP[Trial$patid %in% OtherAP$patid]<-1

save(Trial, file = "StatinCPRD/Data/Trial_AD.rdata")

