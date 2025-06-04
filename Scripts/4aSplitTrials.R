rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(lubridate)
library(tableone)

####Load final file####
load("StatinCPRD/Data/Trial_CompletePlusIMD.rdata")
Trial<-select(Trial, -n.x, -n.y)
names(Trial)
lapply(Trial[,144:205], class)

####Change NAs to zeros for outcomes####
Vars<-c(names(Trial))
MainVars<-Vars[c(144:149, 151:154, 156:176, 178,180, 182, 184:196, 198, 200, 202, 204, 205 )]
lapply(Trial[,c(MainVars)], class)

Trial <- Trial %>% 
  mutate_at(c(MainVars), ~replace_na(.,0))

####Set dislip time to years
Trial$DislipTimeStat<-Trial$DislipTimeStat/365.25
Trial$DislipTimeAP<-Trial$DislipTimeAP/365.25

####Add in follow up####
Trial$Trial1FU<-as.numeric(Trial$end-Trial$Trial1StatDate)/365.25
Trial$Trial2FU<-as.numeric(Trial$end-Trial$Trial2StatDate)/365.25
Trial$Trial3FU<-as.numeric(Trial$end-Trial$FirstAPTrial3Date-30.4375) /365.25

####Add in max 2 year follow up####
Trial$MaxFU1<-Trial$Trial1FU
Trial$MaxFU1[Trial$MaxFU1>2]<-2

Trial$MaxFU2<-Trial$Trial2FU
Trial$MaxFU2[Trial$MaxFU2>2]<-2

#For trial 3, need to ignore the first 30 days
summary(Trial$Trial3FU)

Trial$MaxFU3<-Trial$Trial3FU
Trial$MaxFU3[Trial$MaxFU3>2]<-2

####Died in FU####
Trial$Died2yr1<-0
Trial$Died2yr1[Trial$deathdate-Trial$Trial1StatDate<730.5]<-1

Trial$Died2yr3<-0
Trial$Died2yr3[Trial$deathdate-Trial$FirstAPTrial3Date<730.5]<-1

###YOB integer###
Trial$yob<-as.integer(Trial$yob)

####Age at index####
Trial$AgeTrial1<-year(Trial$Trial1StatDate) - Trial$yob
Trial$AgeTrial3<-year(Trial$FirstAPTrial3Date) - Trial$yob

####Change some dates to years####

Trial$FirstPsychStatTime<-as.numeric(Trial$FirstPsychStatTime)/365.25
Trial$SMIStatTime<-as.numeric(Trial$SMIStatTime)/365.25
Trial$StatTime<-as.numeric(Trial$StatTime)/365.25
Trial$SMIAPTime<-as.numeric(Trial$SMIAPTime)/365.25
Trial$FirstPsychAPTime<-as.numeric(Trial$FirstPsychAPTime)/365.25

####BMI missing####
Trial$StatBMIValMiss<-0
Trial$StatBMIValMiss[is.na(Trial$StatBMIVal)]<-1
Trial$APBMIValMiss<-0
Trial$APBMIValMiss[is.na(Trial$APBMIVal)]<-1

####Missing Stat dose####
Trial$StatDose1Miss<-0
Trial$StatDose1Miss[is.na(Trial$StatDose1)]<-1
Trial$StatDose3Miss<-0
Trial$StatDose3Miss[is.na(Trial$StatDoseTrial3)]<-1

####Missing cholesterol measure
Trial$StatCholMiss<-0
Trial$StatCholMiss[is.na(Trial$StatChol)]<-1
Trial$APCholMiss<-0
Trial$APCholMiss[is.na(Trial$APChol)]<-1

####Check for any hospital vars####
#Trial 1
Trial$PriorMHBinStat<-0
Trial$PriorMHBinStat[Trial$PriorMHStat>0]<-1

Trial$PriorSHBinStat<-0
Trial$PriorSHBinStat[Trial$PriorSHStat>0]<-1

Trial$PriorPhysicalBinStat<-0
Trial$PriorPhysicalBinStat[Trial$PriorPhysicalStat>0]<-1

Trial$PriorAccidentBinStat<-0
Trial$PriorAccidentBinStat[Trial$PriorAccidentStat>0]<-1

Trial$PriorGPBinStat<-0
Trial$PriorGPBinStat[Trial$PriorGPStat>0]<-1

Trial$PriorGPSHBinStat<-0
Trial$PriorGPSHBinStat[Trial$PriorSHGPStat>0]<-1

#Trial 3

Trial$PriorMHBinAP<-0
Trial$PriorMHBinAP[Trial$PriorMHAP>0]<-1

Trial$PriorSHBinAP<-0
Trial$PriorSHBinAP[Trial$PriorSHAP>0]<-1

Trial$PriorPhysicalBinAP<-0
Trial$PriorPhysicalBinAP[Trial$PriorPhysicalAP>0]<-1

Trial$PriorAccidentBinAP<-0
Trial$PriorAccidentBinAP[Trial$PriorAccidentAP>0]<-1

Trial$PriorGPBinAP<-0
Trial$PriorGPBinAP[Trial$PriorGPAP>0]<-1

Trial$PriorGPSHBinAP<-0
Trial$PriorGPSHBinAP[Trial$PriorSHGPAP>0]<-1

####Comparator or active####
Trial$Active1<-0
Trial$Active1[Trial$Trial1Statin=="Sim"]<-1

Trial$Active2<-0
Trial$Active2[Trial$Trial2Statin=="Sim"|Trial$Trial2Statin=="Ator"]<-1

Trial$Active3<-1
Trial$Active3[Trial$APTrial3=="Quetiapine"]<-0

####Combined SH outcome####
Trial$OutcomeCombSHStat12w<-Trial$OutcomeHospSHStat12w+Trial$OutcomeSHGPStat12wk

Trial$OutcomeCombSHStat6mo<-Trial$OutcomeHospSHStat6mo+Trial$OutcomeSHGPStat6mo

Trial$OutcomeCombSHStat12mo<-Trial$OutcomeHospSHStat12mo+Trial$OutcomeSHGPStat12mo

Trial$OutcomeCombSHStat24mo<-Trial$OutcomeHospSHStat24mo+Trial$OutcomeSHGPStat24mo
Trial$OutcomeCombSHStat24moDate<-pmin(Trial$OutcomeHospSHStat24moDate, Trial$FirstSHGPStat24mo, na.rm=TRUE)

Trial$OutcomeCombSHAP12w<-Trial$OutcomeHospSHAP12w+Trial$OutcomeSHGPAP12wk

Trial$OutcomeCombSHAP6mo<-Trial$OutcomeHospSHAP6mo+Trial$OutcomeSHGPAP6mo

Trial$OutcomeCombSHAP12mo<-Trial$OutcomeHospSHAP12mo+Trial$OutcomeSHGPAP12mo

Trial$OutcomeCombSHAP24mo<-Trial$OutcomeHospSHAP24mo+Trial$OutcomeSHGPAP24mo
Trial$OutcomeCombSHAP24moDate<-pmin(Trial$OutcomeHospSHAP24moDate, Trial$FirstSHGPAP24mo, na.rm=TRUE)

####Current registration####

Trial$RegTimeStat<-as.numeric(Trial$Trial1StatDate - Trial$regstartdate)/365.25
Trial$RegTimeAP<-as.numeric(Trial$FirstAPTrial3Date - Trial$regstartdate)/365.25

####Split into trials####
Trial1<-subset(Trial, !is.na(Trial1StatDate))
Trial1<-select(Trial1, patid, pracid, StatBMIValDate, StatBMICatDate, end, Active1, Trial1Statin, APStat, AgeTrial1, gender, yob, Died2yr1, region, ethnicity, Trial1FU,
               MaxFU1, Trial1StatDate, YearStat, StatDose1, StatDose1Miss, ExactStatDose1,
               SMIDiagStat, SMIStatTime, FirstPsychStatTime,  SSRIStat, TCAStat, OtherADStat, StatBMIVal, StatBMIValMiss, StatBMICat,             
               DiabStat,  HypertensionStat, MIStat, CerebrovascularStat,CHFStat,  StatChol, StatCholMiss, DyslipStat, DislipTimeStat,
                PriorMHBinStat, PriorSHBinStat, PriorPhysicalBinStat, PriorAccidentBinStat, PriorGPBinStat, PriorGPSHBinStat, PriorMHStat,  PriorSHGPStat, PriorSHStat, PriorPhysicalStat,      
               PriorAccidentStat, PriorGPStat, RegTimeStat, PatIMD, PracIMD, OutcomeMHStat12w,        OutcomeHospSHStat12w, OutcomeSHGPStat12wk,       OutcomePhysicalStat12w, 
               OutcomeAccidentStat12w,  OutcomeMHStat6mo,        OutcomeHospSHStat6mo,   OutcomeSHGPStat6mo,     OutcomePhysicalStat6mo, 
               OutcomeAccidentStat6mo,  OutcomeMHStat12mo,       OutcomeHospSHStat12mo,  OutcomeSHGPStat12mo,     OutcomePhysicalStat12mo,
               OutcomeAccidentStat12mo, OutcomeMHStat24mo,       OutcomeHospSHStat24mo, OutcomeSHGPStat24mo,     OutcomePhysicalStat24mo,
              OutcomeAccidentStat24mo,  FirstSHGPStat24mo, OutcomeMHStat24moDate, OutcomeHospSHStat24moDate,    OutcomePhysicalStat24moDate=OutcomePhsycialStat24moDate, OutcomeAccidentStat24moDate,
              OutcomeCombSHStat12w, OutcomeCombSHStat6mo, OutcomeCombSHStat12mo, OutcomeCombSHStat24mo,OutcomeCombSHStat24moDate)
             
Trial2<-subset(Trial, !is.na(Trial2StatDate))
Trial2<-select(Trial2,  patid, pracid, StatBMIValDate, StatBMICatDate, end, Active2, Trial2Statin, APStat, AgeTrial1, gender, yob, Died2yr1, region, ethnicity, Trial2FU,
               MaxFU2, Trial2StatDate, YearStat, StatDose1, StatDose1Miss, ExactStatDose1,
               SMIDiagStat, SMIStatTime, FirstPsychStatTime, SSRIStat, TCAStat, OtherADStat, StatBMIVal, StatBMIValMiss, StatBMICat,             
               DiabStat,  HypertensionStat, MIStat, CerebrovascularStat,CHFStat, StatChol, StatCholMiss, DyslipStat,  DislipTimeStat,
                PriorMHBinStat, PriorSHBinStat, PriorPhysicalBinStat, PriorAccidentBinStat, PriorGPBinStat, PriorGPSHBinStat, PriorMHStat,  PriorSHGPStat, PriorSHStat, PriorPhysicalStat,      
               PriorAccidentStat, PriorGPStat, RegTimeStat, PatIMD, PracIMD, OutcomeMHStat12w,        OutcomeHospSHStat12w,   OutcomeSHGPStat12wk,     OutcomePhysicalStat12w, 
               OutcomeAccidentStat12w,  OutcomeMHStat6mo,        OutcomeHospSHStat6mo,  OutcomeSHGPStat6mo,      OutcomePhysicalStat6mo, 
               OutcomeAccidentStat6mo,  OutcomeMHStat12mo,       OutcomeHospSHStat12mo, OutcomeSHGPStat12mo,      OutcomePhysicalStat12mo,
               OutcomeAccidentStat12mo, OutcomeMHStat24mo,       OutcomeHospSHStat24mo,  OutcomeSHGPStat24mo,    OutcomePhysicalStat24mo,
               OutcomeAccidentStat24mo,  FirstSHGPStat24mo,  OutcomeMHStat24moDate, OutcomeHospSHStat24moDate,    OutcomePhysicalStat24moDate=OutcomePhsycialStat24moDate, OutcomeAccidentStat24moDate,
               OutcomeCombSHStat12w, OutcomeCombSHStat6mo, OutcomeCombSHStat12mo, OutcomeCombSHStat24mo, OutcomeCombSHStat24moDate)

Trial3<-subset(Trial, !is.na(FirstAPTrial3Date))
Trial3<-select(Trial3, patid, pracid, APBMICatDate, APBMIValDate, end, Active3, APTrial3, LastStatin, AgeTrial3, gender, yob, Died2yr3, region, ethnicity,Trial3FU, 
               MaxFU3, FirstAPTrial3Date, YearAP,  StatTime, StatDoseTrial3, StatDose3Miss, ExactStatDose3,
               SMIDiagAP,  SMIAPTime, FirstPsychAPTime, SSRIAP, TCAAP, OtherADAP, APBMIVal, APBMIValMiss, APBMICat,             
              DiabAP,  HypertensionAP, MIAP, CerebrovascularAP, CHFAP,  APChol, APCholMiss, DyslipAP, DislipTimeAP,     
               PriorMHBinAP, PriorSHBinAP, PriorPhysicalBinAP, PriorAccidentBinAP, PriorGPBinAP, PriorGPSHBinAP, PriorMHAP,  PriorSHGPAP, PriorSHAP, PriorPhysicalAP,      
               PriorAccidentAP, PriorGPAP, RegTimeAP, PatIMD, PracIMD, OutcomeMHAP12w,        OutcomeHospSHAP12w,    OutcomeSHGPAP12wk,    OutcomePhysicalAP12w, 
               OutcomeAccidentAP12w,  OutcomeMHAP6mo,        OutcomeHospSHAP6mo, OutcomeSHGPAP6mo,       OutcomePhysicalAP6mo, 
               OutcomeAccidentAP6mo,  OutcomeMHAP12mo,       OutcomeHospSHAP12mo,  OutcomeSHGPAP12mo,     OutcomePhysicalAP12mo,
               OutcomeAccidentAP12mo, OutcomeMHAP24mo,       OutcomeHospSHAP24mo,  OutcomeSHGPAP24mo,    OutcomePhysicalAP24mo,
               OutcomeAccidentAP24mo, FirstSHGPAP24mo,  OutcomeMHAP24moDate,  OutcomeHospSHAP24moDate, OutcomePhysicalAP24moDate=OutcomePhsycialAP24moDate,  
              OutcomeAccidentAP24moDate, OutcomeCombSHAP12w, OutcomeCombSHAP6mo, OutcomeCombSHAP12mo, OutcomeCombSHAP24mo,OutcomeCombSHAP24moDate)

####Sort out time to event####
Trial1$TimeToMH<- as.numeric(Trial1$OutcomeMHStat24moDate - Trial1$Trial1StatDate)
Trial1$TimeToSH<- as.numeric(Trial1$OutcomeCombSHStat24moDate - Trial1$Trial1StatDate)
Trial1$TimeToPhys<- as.numeric(Trial1$OutcomePhysicalStat24moDate - Trial1$Trial1StatDate)
Trial1$TimeToAccident<- as.numeric(Trial1$OutcomeAccidentStat24moDate - Trial1$Trial1StatDate)

Trial2$TimeToMH<- as.numeric(Trial2$OutcomeMHStat24moDate - Trial2$Trial2StatDate)
Trial2$TimeToSH<- as.numeric(Trial2$OutcomeCombSHStat24moDate - Trial2$Trial2StatDate)
Trial2$TimeToPhys<- as.numeric(Trial2$OutcomePhysicalStat24moDate - Trial2$Trial2StatDate)
Trial2$TimeToAccident<- as.numeric(Trial2$OutcomeAccidentStat24moDate - Trial2$Trial2StatDate)

Trial3$TimeToMH<- as.numeric(Trial3$OutcomeMHAP24moDate - (Trial3$FirstAPTrial3Date+30.4375))
Trial3$TimeToSH<- as.numeric(Trial3$OutcomeCombSHAP24moDate - (Trial3$FirstAPTrial3Date+30.4375))
Trial3$TimeToPhys<- as.numeric(Trial3$OutcomePhysicalAP24moDate - (Trial3$FirstAPTrial3Date+30.4375))
Trial3$TimeToAccident<- as.numeric(Trial3$OutcomeAccidentAP24moDate - (Trial3$FirstAPTrial3Date+30.4375))

summary(Trial1$TimeToMH)
summary(Trial1$MaxFU1)

#If they dont have an event then it is time to end of follow up.

Trial1$TimeToMH[is.na(Trial1$TimeToMH)]<-Trial1$MaxFU1[is.na(Trial1$TimeToMH)]*365.25
Trial1$TimeToSH[is.na(Trial1$TimeToSH)]<-Trial1$MaxFU1[is.na(Trial1$TimeToSH)]*365.25
Trial1$TimeToPhys[is.na(Trial1$TimeToPhys)]<-Trial1$MaxFU1[is.na(Trial1$TimeToPhys)]*365.25
Trial1$TimeToAccident[is.na(Trial1$TimeToAccident)]<-Trial1$MaxFU1[is.na(Trial1$TimeToAccident)]*365.25

Trial2$TimeToMH[is.na(Trial2$TimeToMH)]<-Trial2$MaxFU2[is.na(Trial2$TimeToMH)]*365.25
Trial2$TimeToSH[is.na(Trial2$TimeToSH)]<-Trial2$MaxFU2[is.na(Trial2$TimeToSH)]*365.25
Trial2$TimeToPhys[is.na(Trial2$TimeToPhys)]<-Trial2$MaxFU2[is.na(Trial2$TimeToPhys)]*365.25
Trial2$TimeToAccident[is.na(Trial2$TimeToAccident)]<-Trial2$MaxFU2[is.na(Trial2$TimeToAccident)]*365.25

Trial3$TimeToMH[is.na(Trial3$TimeToMH)]<-Trial3$MaxFU3[is.na(Trial3$TimeToMH)]*365.25
Trial3$TimeToSH[is.na(Trial3$TimeToSH)]<-Trial3$MaxFU3[is.na(Trial3$TimeToSH)]*365.25
Trial3$TimeToPhys[is.na(Trial3$TimeToPhys)]<-Trial3$MaxFU3[is.na(Trial3$TimeToPhys)]*365.25
Trial3$TimeToAccident[is.na(Trial3$TimeToAccident)]<-Trial3$MaxFU3[is.na(Trial3$TimeToAccident)]*365.25

####Sort most recent AP at dose####
load("VariableExtracts/CPRD2023/MergedObs/BP_dose.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/AP_dose.Rdata")

StatDate1<-select(Trial1, patid, Trial1StatDate)
StatDate2<-select(Trial2, patid, Trial2StatDate)

BPDose<-select(BPDoseFinal, patid, eventdate, AP)
APDose<-select(apDoseFinal, patid, eventdate, AP)
AllDose<-rbind(BPDose, APDose)
AllDose<-distinct(AllDose)
AllDose1<-merge(x=AllDose, y=StatDate1, by="patid", all.x=FALSE, all.y=FALSE)

AllDose6mo1<-subset(AllDose1, Trial1StatDate-eventdate<=182.625 & Trial1StatDate-eventdate>=0) #Have a prescription in 6 months before statin

APStat1<-AllDose6mo1%>%
  group_by(patid)%>%
  mutate(max=max(eventdate))%>%
  subset(max==eventdate)%>%
  select(patid, AP)%>%
  distinct()%>%
  mutate(n=n())%>%
  mutate(APs=case_when(n>1 ~ "Multiple",
                       TRUE ~ AP))%>%
  distinct()

APStat1<-APStat1%>%
  group_by(patid)%>%
  pivot_wider(names_from = "AP", values_from="AP")

APStat1<-APStat1%>%
  unite("Multiple", 4:33, sep = "_", remove = FALSE, na.rm = TRUE)


APStat1$AP<-case_when(grepl("lithium", APStat1$Multiple) & grepl("valproate", APStat1$Multiple) & grepl("lamotrigine", APStat1$Multiple) & APStat1$n==3 ~ "BP",
                     grepl("lithium", APStat1$Multiple) & grepl("valproate", APStat1$Multiple) & APStat1$n==2 ~ "BP",
                     grepl("lithium", APStat1$Multiple) & grepl("lamotrigine", APStat1$Multiple) & APStat1$n==2 ~ "BP",
                     grepl("valproate", APStat1$Multiple) & grepl("lamotrigine", APStat1$Multiple) & APStat1$n==2 ~ "BP",
                     !(grepl("lithium|valproate|lamotrigine", APStat1$Multiple)) & APStat1$n>1 ~ "AP",
                     APStat1$n>1 ~ "Mixed",
                     TRUE ~ APStat1$APs)

length(unique(APStat1$patid))

table(APStat1$AP, useNA="ifany")

APStat1<-select(APStat1, patid, APStatTrial1=AP)
Trial1<-select(Trial1, -APStat)
Trial1<-merge(x=Trial1, y=APStat1, by="patid", all.x=TRUE, all.y=FALSE)

#Trial2

AllDose2<-merge(x=AllDose, y=StatDate2, by="patid", all.x=FALSE, all.y=FALSE)

AllDose6mo2<-subset(AllDose2, Trial2StatDate-eventdate<=182.625 & Trial2StatDate-eventdate>=0) #Have a prescription in 6 months before statin

APStat2<-AllDose6mo2%>%
  group_by(patid)%>%
  mutate(max=max(eventdate))%>%
  subset(max==eventdate)%>%
  select(patid, AP)%>%
  distinct()%>%
  mutate(n=n())%>%
  mutate(APs=case_when(n>1 ~ "Multiple",
                       TRUE ~ AP))%>%
  distinct()

APStat2<-APStat2%>%
  group_by(patid)%>%
  pivot_wider(names_from = "AP", values_from="AP")

APStat2<-APStat2%>%
  unite("Multiple", 4:26, sep = "_", remove = FALSE, na.rm = TRUE)


APStat2$AP<-case_when(grepl("lithium", APStat2$Multiple) & grepl("valproate", APStat2$Multiple) & grepl("lamotrigine", APStat2$Multiple) & APStat2$n==3 ~ "BP",
                      grepl("lithium", APStat2$Multiple) & grepl("valproate", APStat2$Multiple) & APStat2$n==2 ~ "BP",
                      grepl("lithium", APStat2$Multiple) & grepl("lamotrigine", APStat2$Multiple) & APStat2$n==2 ~ "BP",
                      grepl("valproate", APStat2$Multiple) & grepl("lamotrigine", APStat2$Multiple) & APStat2$n==2 ~ "BP",
                      !(grepl("lithium|valproate|lamotrigine", APStat2$Multiple)) & APStat2$n>1 ~ "AP",
                      APStat2$n>1 ~ "Mixed",
                      TRUE ~ APStat2$APs)


length(unique(APStat2$patid))

table(APStat2$AP, useNA="ifany")

APStat2<-select(APStat2, patid, APStatTrial2=AP)
Trial2<-select(Trial2, -APStat)
Trial2<-merge(x=Trial2, y=APStat2, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial1, file="StatinCPRD/Data/Trial1.rdata")
save(Trial2, file="StatinCPRD/Data/Trial2.rdata")
save(Trial3, file="StatinCPRD/Data/Trial3.rdata")
save(Trial, file="StatinCPRD/Data/Trial_Analysis.rdata")
