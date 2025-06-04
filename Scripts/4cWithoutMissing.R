rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(lubridate)
library(tableone)
library(mice)
library(survival)

####Load trials####
load("StatinCPRD/Data/Trial1.rdata")
load("StatinCPRD/Data/Trial2.rdata")
load("StatinCPRD/Data/Trial3.rdata")

####If stat dose is NA use median####
length(which(is.na(Trial1$ExactStatDose1)))

Median<-Trial1%>%
  group_by(Trial1Statin)%>%
  summarise(Median = median(ExactStatDose1, na.rm=TRUE))
    
tapply(Median$Median,Median$Trial1Statin, summary)
Median$Group[Median$Trial1Statin=="Ator"]<-"High"
  Median$Group[Median$Trial1Statin=="AtorPrav"]<-"Moderate"
  Median$Group[Median$Trial1Statin=="Prav"]<-"Low"
  Median$Group[Median$Trial1Statin=="Rosu"]<-"High"
  Median$Group[Median$Trial1Statin=="Sim"]<-"Moderate"

Trial1<-merge(x=Trial1, y=Median, by="Trial1Statin", all.x=TRUE, all.y=FALSE)  
Trial1$ExactStatDose1[is.na(Trial1$ExactStatDose1)]<-Trial1$Median[is.na(Trial1$ExactStatDose1)]
Trial1$StatDose1[is.na(Trial1$StatDose1)]<-Trial1$Group[is.na(Trial1$StatDose1)]
Trial1<-select(Trial1, -Median, -Group)

####Code up statin dose - TRIAL 2####
table(Trial2$Trial2Statin)

#If stat dose is NA use median
length(which(is.na(Trial2$ExactStatDose1)))

Trial2<-merge(x=Trial2, y=Median, by.x="Trial2Statin", by.y="Trial1Statin", all.x=TRUE, all.y=FALSE)  
Trial2$ExactStatDose1[is.na(Trial2$ExactStatDose1)]<-Trial2$Median[is.na(Trial2$ExactStatDose1)]
Trial2$StatDose1[is.na(Trial2$StatDose1)]<-Trial2$Group[is.na(Trial2$StatDose1)]

table(Trial2$StatDose1, useNA="ifany")
Trial2<-select(Trial2, -Median, -Group)
Trial2<-subset(Trial2, !is.na(StatDose1))

##########NEED TO DROP A FEW THAT ARE HIGH DOSE#############

##54 removed

####Code up statin dose - TRIAL 3####
table(Trial3$LastStatin, useNA="ifany")

#If stat dose is NA use median
length(which(is.na(Trial3$ExactStatDose3)))

Median<-Trial3%>%
  group_by(LastStatin)%>%
  summarise(Median = median(ExactStatDose3, na.rm=TRUE))

tapply(Median$Median,Median$LastStatin, summary)
Median$Group[Median$LastStatin=="Atorvastatin"]<-"High"
Median$Group[Median$LastStatin=="Simvastatin"]<-"Moderate"

Trial3<-merge(x=Trial3, y=Median, by="LastStatin", all.x=TRUE, all.y=FALSE)  
Trial3$ExactStatDose3[is.na(Trial3$ExactStatDose3)]<-Trial3$Median[is.na(Trial3$ExactStatDose3)]
Trial3$StatDoseTrial3[is.na(Trial3$StatDoseTrial3)]<-Trial3$Group[is.na(Trial3$StatDoseTrial3)]

table(Trial3$StatDoseTrial3, useNA="ifany")
Trial3<-select(Trial3, -Median, -Group)

####If time since dyslip is NA set to zero####

Trial1$DislipTimeStat[is.na(Trial1$DislipTimeStat)]<-0
Trial2$DislipTimeStat[is.na(Trial2$DislipTimeStat)]<-0
Trial3$DislipTimeAP[is.na(Trial3$DislipTimeAP)]<-0

summary(Trial1$DislipTimeStat)
summary(Trial2$DislipTimeStat)
summary(Trial3$DislipTimeAP)

####If ethnicity is unknown set to missing####
Trial1$ethnicity[Trial1$ethnicity=="Unknown"]<-NA
Trial2$ethnicity[Trial2$ethnicity=="Unknown"]<-NA
Trial3$ethnicity[Trial3$ethnicity=="Unknown"]<-NA

Trial1$ethnicity<-as.character(Trial1$ethnicity)
Trial1$ethnicity<-as.factor(Trial1$ethnicity)

Trial2$ethnicity<-as.character(Trial2$ethnicity)
Trial2$ethnicity<-as.factor(Trial2$ethnicity)

Trial3$ethnicity<-as.character(Trial3$ethnicity)
Trial3$ethnicity<-as.factor(Trial3$ethnicity)

####IMD####
Trial1$FullIMD<-Trial1$PatIMD
Trial1$FullIMD[is.na(Trial1$FullIMD)]<-Trial1$PracIMD[is.na(Trial1$FullIMD)]

Trial2$FullIMD<-Trial2$PatIMD
Trial2$FullIMD[is.na(Trial2$FullIMD)]<-Trial2$PracIMD[is.na(Trial2$FullIMD)]

Trial3$FullIMD<-Trial3$PatIMD
Trial3$FullIMD[is.na(Trial3$FullIMD)]<-Trial3$PracIMD[is.na(Trial3$FullIMD)]

table(Trial1$FullIMD, useNA="ifany")
table(Trial2$FullIMD, useNA="ifany")
table(Trial3$FullIMD, useNA="ifany")
prop.table(table(Trial1$FullIMD, useNA="ifany"))*100
prop.table(table(Trial2$FullIMD, useNA="ifany"))*100
prop.table(table(Trial3$FullIMD, useNA="ifany"))*100

save(Trial1, file="StatinCPRD/Data/Trial1.rdata")
save(Trial2, file="StatinCPRD/Data/Trial2.rdata")
save(Trial3, file="StatinCPRD/Data/Trial3.rdata")

         