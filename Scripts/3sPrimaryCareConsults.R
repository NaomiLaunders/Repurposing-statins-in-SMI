####Number of consultations in the 6 months prior to diagnosis####
rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/Trial_Complete.rdata")
library(haven)
library(dplyr)
library(lubridate)

####Primary care consultations in the year prior to index####

length(unique(AllF2FCons$patid))

#Dont include the date of index because they had to attend!
Dates<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date)
AllCons<-merge(x=AllF2FCons, y=Dates, by="patid", all.x=FALSE, all.y=FALSE)

PriorGPStat<-AllCons%>%
  subset(Trial1StatDate-consdate>0&Trial1StatDate-consdate<=182.625)%>%
  group_by(patid)%>%
  summarise(PriorGPStat=n())

PriorGPAP<-AllCons%>%
  subset(FirstAPTrial3Date-consdate>0&FirstAPTrial3Date-consdate<=182.625)%>%
  group_by(patid)%>%
  summarise(PriorGPAP=n())

Trial<-merge(x=Trial, y=PriorGPStat, by="patid", all.x=TRUE, all.y=FALSE)

Trial<-merge(x=Trial, y=PriorGPAP, by="patid", all.x=TRUE, all.y=FALSE)


Trial$PriorGPStat[is.na(Trial$PriorGPStat)]<-0
Trial$PriorGPAP[is.na(Trial$PriorGPAP)]<-0

save(Trial, file="StatinCPRD/Data/Trial_CompletePlus.rdata")





