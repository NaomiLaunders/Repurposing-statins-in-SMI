
rm(list = ls(all.names = TRUE))

library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)
library(lubridate)

load("StatinCPRD/Data/Trial_DisLip.rdata")


#Year of start
Trial$YearStat<-year(Trial$Trial1StatDate)
Trial$YearAP<-year(Trial$FirstAPTrial3Date)

#Time since first statin prescription (trial 3)
Trial$StatTime<-as.numeric(Trial$FirstAPTrial3Date - Trial$FirstStatDate)
summary(Trial$StatTime)

#Time since first SMI evidence
Trial$SMIStatTime<-as.numeric(Trial$Trial1StatDate - Trial$SMIEv)
summary(Trial$SMIStatTime)

length(which(is.na(Trial$SMIStatTime)& !is.na(Trial$Trial1StatDate)))

Trial$SMIAPTime<-as.numeric(Trial$FirstAPTrial3Date - Trial$SMIEv)
summary(Trial$SMIAPTime)

length(which(is.na(Trial$SMIAPTime)& !is.na(Trial$FirstAPTrial3Date)))

#Time since first antipsychotic/mood stabiliser prescription
Trial$FirstPsychStatTime<-as.numeric(Trial$Trial1StatDate - Trial$FirstPsychDate)
summary(Trial$FirstPsychStatTime)

length(which(is.na(Trial$FirstPsychStatTime)& !is.na(Trial$Trial1StatDate)))

Trial$FirstPsychAPTime<-as.numeric(Trial$FirstAPTrial3Date - Trial$FirstPsychDate)
summary(Trial$FirstPsychAPTime)

length(which(is.na(Trial$FirstPsychAPTime)& !is.na(Trial$FirstAPTrial3Date)))

#Most recent antipsychotic
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
PatAPAll<-subset(PatAPAll, patid %in% Trial$patid)
Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date)
PatAPAll<-merge(x=PatAPAll, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

APStat<-PatAPAll%>%
  group_by(patid)%>%
  subset(eventdate<=Trial1StatDate)%>%
  mutate(max=max(eventdate))%>%
  subset(max==eventdate)%>%
  select(patid, AP)%>%
  distinct()%>%
  mutate(n=n())%>%
  mutate(APs=case_when(n>1 ~ "Multiple",
                       TRUE ~ AP))

APStat<-APStat%>%
  group_by(patid)%>%
  pivot_wider(names_from = "AP", values_from="AP")

APStat<-APStat%>%
  unite("Multiple", 4:32, sep = "_", remove = FALSE, na.rm = TRUE)


APStat$AP<-case_when(grepl("Lithium", APStat$Multiple) & grepl("Valproate", APStat$Multiple) & grepl("Lamotrigine", APStat$Multiple) & APStat$n==3 ~ "BP",
                     grepl("Lithium", APStat$Multiple) & grepl("Valproate", APStat$Multiple) & APStat$n==2 ~ "BP",
                     grepl("Lithium", APStat$Multiple) & grepl("Lamotrigine", APStat$Multiple) & APStat$n==2 ~ "BP",
                     grepl("Valproate", APStat$Multiple) & grepl("Lamotrigine", APStat$Multiple) & APStat$n==2 ~ "BP",
                     !(grepl("Lithium|Valproate|Lamotrigine", APStat$Multiple)) & APStat$n>1 ~ "AP",
                       APStat$n>1 ~ "Mixed",
                     TRUE ~ APStat$APs)

length(unique(APStat$patid))

table(APStat$AP)

APStat<-select(APStat, patid, APStat=AP)

Trial<-merge(x=Trial, y=APStat, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file="StatinCPRD/Data/Trial_Time.rdata")
