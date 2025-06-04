rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(lubridate)
library(tableone)
library(mice)
library(survival)
library(survminer)
library(tidyselect)
library(tidyr)
library(miceadds)

####Load trials####
load("StatinCPRD/Data/Trial1_toimpute.rdata")
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")

StatDate<-select(Trial1, patid, Trial1StatDate, Active1, end)

#Only include statins within the two year follow up...
StatDate$Yr2_1<-as.Date(StatDate$Trial1StatDate+730.5)
StatDate$EndDate1<-pmin(StatDate$end, StatDate$Yr2_1)

TrialStat<-subset(AllStat, patid %in% Trial1$patid)
TrialStat<-merge(x=TrialStat, y=StatDate, by="patid", all.x=TRUE, all.y=FALSE)

#Per protocol analysis so need to account for switching and discontinuation

#Time to different antipsychotic/statin
#TRIAL 1: Simvastatin to any other statin; arto, prav or rosuv to any other statin

####Trial 1####
#AP switching - NA

PatAPAll<-subset(PatAPAll, patid %in% Trial1$patid)

AP<-merge(x=PatAPAll, y=StatDate, by="patid", all.x=FALSE, all.y=FALSE)
#Include those from 6 months before to 2 years after/end of fu
Trial1AP<-subset(AP, !is.na(Trial1StatDate) & eventdate-Trial1StatDate>=-182.625 & eventdate<=EndDate1)
Trial1AP<-subset(Trial1AP, !is.na(Trial1StatDate))

#Discontinued antipsychotic: Gap of more than 90 days

FirstStudyAP<-Trial1AP%>%
  subset(eventdate-Trial1StatDate<=0)%>%
  group_by(patid)%>%
  mutate(FirstStudyAP=max(eventdate))%>%
  subset(FirstStudyAP==eventdate)%>%
  select(patid, FirstStudyAP)%>%
  distinct()

length(unique(FirstStudyAP$patid))

Trial1AP<-merge(x=Trial1AP, y=FirstStudyAP, by="patid", all.x=TRUE, all.y=TRUE)

Trial1AP<-Trial1AP%>%
  subset(eventdate>=FirstStudyAP)%>%
  group_by(patid)%>%
  arrange(patid, eventdate)%>%
  mutate(TimeToNext=as.numeric(lead(eventdate)-eventdate))%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(EndDate1-eventdate),
                              TRUE ~ TimeToNext))%>%
  subset(TimeToNext>90)%>%
  mutate(Min=min(eventdate))%>%
  subset(eventdate==Min)%>%
  mutate(Discont=eventdate+90)%>%
  select(patid, Discont)%>%
  distinct()

length(unique(Trial1AP$patid))

#Switched statin:Treatment: First non-simvastatin prescription
Trial1Stat<-subset(TrialStat, !is.na(Trial1StatDate))
Trial1Stat<-subset(Trial1Stat, issuedate<=EndDate1 & issuedate-Trial1StatDate>=0)

#First switch
T1AS<-subset(Trial1Stat, Statin!="Simvastatin" & Active1==1)
T1AS<-T1AS%>%
  group_by(patid)%>%
  mutate(FirstSwitch=min(issuedate))%>%
  subset(FirstSwitch==issuedate)%>%
  select(patid, FirstSwitch)%>%
  distinct()
  
#Switched statin:Comparator: First simvastatin, fluvastatin or "other"
T1CS<-subset(Trial1Stat, (Statin=="Simvastatin"|Statin=="Fluvastatin"|Statin=="Other") & Active1==0)
T1CS<-T1CS%>%
  group_by(patid)%>%
  mutate(FirstSwitch=min(issuedate))%>%
  subset(FirstSwitch==issuedate)%>%
  select(patid, FirstSwitch)%>%
  distinct()

StatSwitch<-rbind(T1AS, T1CS)

length(unique(StatSwitch$patid))

#Discontinued statin: Gap of more than 90 days
StatDis<-Trial1Stat%>%
  group_by(patid)%>%
  arrange(patid, issuedate)%>%
  mutate(TimeToNext=as.numeric(lead(issuedate)-issuedate))%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(EndDate1-issuedate),
                              TRUE ~ TimeToNext))%>%
  subset(TimeToNext>90)%>%
  mutate(Min=min(issuedate))%>%
  subset(issuedate==Min)%>%
  mutate(DiscontStat=issuedate+90)%>%
  select(patid, DiscontStat)%>%
  distinct()

length(unique(StatDis$patid))

PPA<-merge(x=StatDis, y=StatSwitch, by="patid", all.x=TRUE, all.y=TRUE)
PPA<-merge(x=PPA, y=Trial1AP, by="patid", all.x=TRUE, all.y=TRUE)
PPA$PPADate<-pmin(PPA$DiscontStat, PPA$Discont, PPA$FirstSwitch, na.rm=TRUE)
PPA<-select(PPA, patid, PPADate)

#####Now bind back on to trial####
Trial1<-merge(x=Trial1, y=PPA, by="patid", all.x=TRUE, all.y=TRUE)
Trial1$Yr2_1<-as.Date(Trial1$Trial1StatDate+730.5)

Trial1$PPAEnd<-pmin(Trial1$end, Trial1$Yr2_1, Trial1$PPADate, na.rm=TRUE)
Trial1$PPAFU<-as.numeric(Trial1$PPAEnd-Trial1$Trial1StatDate)
summary(Trial1$PPAFU)

Trial1<-Trial1%>%
  mutate(MHBin24=case_when(TimeToMH24>PPAFU ~ 0, TRUE ~ MHBin24),
         SHBin24=case_when(TimeToSH24>PPAFU ~ 0, TRUE ~ SHBin24),
         PhysicalBin24=case_when(TimeToPhys24>PPAFU ~ 0, TRUE ~ PhysicalBin24),
         AccidentBin24=case_when(TimeToAccident24>PPAFU ~ 0, TRUE ~ AccidentBin24),
         MHBin12=case_when(TimeToMH12>PPAFU ~ 0, TRUE ~ MHBin12),
         SHBin12=case_when(TimeToSH12>PPAFU ~ 0, TRUE ~ SHBin12),
         PhysicalBin12=case_when(TimeToPhys12>PPAFU ~ 0, TRUE ~ PhysicalBin12),
         AccidentBin12=case_when(TimeToAccident12>PPAFU ~ 0, TRUE ~ AccidentBin12),
         MHBin6=case_when(TimeToMH6>PPAFU ~ 0, TRUE ~ MHBin6),
         SHBin6=case_when(TimeToSH6>PPAFU ~ 0, TRUE ~ SHBin6),
         PhysicalBin6=case_when(TimeToPhys6>PPAFU ~ 0, TRUE ~ PhysicalBin6),
         AccidentBin6=case_when(TimeToAccident6>PPAFU ~ 0, TRUE ~ AccidentBin6),
         MHBin3=case_when(TimeToMH3>PPAFU ~ 0, TRUE ~ MHBin3),
         SHBin3=case_when(TimeToSH3>PPAFU ~ 0, TRUE ~ SHBin3),
         PhysicalBin3=case_when(TimeToPhys3>PPAFU ~ 0, TRUE ~ PhysicalBin3),
         AccidentBin3=case_when(TimeToAccident3>PPAFU ~ 0, TRUE ~ AccidentBin3),
         
         TimeToMH24=case_when(TimeToMH24>PPAFU ~ PPAFU, TRUE ~ TimeToMH24),
         TimeToSH24=case_when(TimeToSH24>PPAFU ~ PPAFU, TRUE ~ TimeToSH24),
         TimeToPhys24=case_when(TimeToPhys24>PPAFU ~ PPAFU, TRUE ~ TimeToPhys24),
         TimeToAccident24=case_when(TimeToAccident24>PPAFU ~ PPAFU, TRUE ~ TimeToAccident24),
         TimeToMH12=case_when(TimeToMH12>PPAFU ~ PPAFU, TRUE ~ TimeToMH12),
         TimeToSH12=case_when(TimeToSH12>PPAFU ~ PPAFU, TRUE ~ TimeToSH12),
         TimeToPhys12=case_when(TimeToPhys12>PPAFU ~ PPAFU, TRUE ~ TimeToPhys12),
         TimeToAccident12=case_when(TimeToAccident12>PPAFU ~ PPAFU, TRUE ~ TimeToAccident12),
         TimeToMH6=case_when(TimeToMH6>PPAFU ~ PPAFU, TRUE ~ TimeToMH6),
         TimeToSH6=case_when(TimeToSH6>PPAFU ~ PPAFU, TRUE ~ TimeToSH6),
         TimeToPhys6=case_when(TimeToPhys6>PPAFU ~ PPAFU, TRUE ~ TimeToPhys6),
         TimeToAccident6=case_when(TimeToAccident6>PPAFU ~ PPAFU, TRUE ~ TimeToAccident6),
         TimeToMH3=case_when(TimeToMH3>PPAFU ~ PPAFU, TRUE ~ TimeToMH3),
         TimeToSH3=case_when(TimeToSH3>PPAFU ~ PPAFU, TRUE ~ TimeToSH3),
         TimeToPhys3=case_when(TimeToPhys3>PPAFU ~ PPAFU, TRUE ~ TimeToPhys3),
         TimeToAccident3=case_when(TimeToAccident3>PPAFU ~ PPAFU, TRUE ~ TimeToAccident3))

NewFields<-select(Trial1, patid, MHBin24, MHBin12,MHBin6,MHBin3,SHBin24,SHBin12,SHBin6,SHBin3,
                  PhysicalBin24,PhysicalBin12,PhysicalBin6,PhysicalBin3,AccidentBin24,AccidentBin12,AccidentBin6,AccidentBin3,
                  TimeToMH24, TimeToMH12, TimeToMH6, TimeToMH3, TimeToSH24, TimeToSH12, TimeToSH6, TimeToSH3,
                  TimeToPhys24, TimeToPhys12, TimeToPhys6, TimeToPhys3, TimeToAccident24,TimeToAccident12, TimeToAccident6, TimeToAccident3, PPAFU)

load("StatinCPRD/Data/ImputeTrial1.Rdata")
datlist<-mids2datlist(ImputedData1)

datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    select(-MHBin24, -MHBin12,-MHBin6,-MHBin3,-SHBin24,-SHBin12,-SHBin6,-SHBin3,
           -PhysicalBin24,-PhysicalBin12,-PhysicalBin6,-PhysicalBin3,-AccidentBin24,-AccidentBin12,-AccidentBin6,-AccidentBin3,
           -TimeToMH24, -TimeToMH12, -TimeToMH6, -TimeToMH3, -TimeToSH24, -TimeToSH12, -TimeToSH6, -TimeToSH3,
           -TimeToPhys24, -TimeToPhys12, -TimeToPhys6, -TimeToPhys3, -TimeToAccident24,-TimeToAccident12, -TimeToAccident6, -TimeToAccident3)
Data<-merge(x=Data, y=NewFields, by="patid", all.x=TRUE, all.y=TRUE)  

#Need to remove those who have negative follow up
Data<-subset(Data, PPAFU>0)

  })

#Set variables
Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24", "TimeToSH24", "TimeToPhys24", "TimeToAccident24",
        "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")
           
#Do the first one to create data
for (i in (1:1)) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active1+strata(pracid)")), data=data)
  })
  UnadjResult<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active1+strata(pracid)")), data=data)
  })
  Unadj<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResult<-rbind(Unadj, UnadjResult)
}

TimePoint<-c(replicate(4,"3"),replicate(4,"6"),replicate(4,"24"),replicate(4,"12"))
Group<-c("Accident", "Phys","SH", "MH")
Group<-c(replicate(4, Group))
UnadjResult<-cbind(UnadjResult, TimePoint, Group) 
UnadjResult$Result<-paste0(format(round(UnadjResult$estimate, 2), nsmall=2), " (", format(round(UnadjResult$`2.5 %`, 2), nsmall=2), "-", format(round(UnadjResult$`97.5 %`, 2), nsmall=2), ")")

#Adjusted
#Group APs that are less than 100
datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    group_by(APStatTrial1)%>%
    mutate(n=n())%>%
    mutate(APStatTrial1 = case_when(n<100 ~"Other",
                                    TRUE ~as.character(APStatTrial1)))%>%
    mutate(APStatTrial1=as.factor(APStatTrial1))%>%
    ungroup()%>%
    mutate(ethnicity = case_when(ethnicity=="Mixed" ~"Other",
                                 TRUE ~as.character(ethnicity)))%>%
    mutate(ethnicity=as.factor(ethnicity))%>%
    mutate(YearStat = case_when(YearStat=="2000"|YearStat=="2001"|YearStat=="2002"|YearStat=="2003"|YearStat=="2004" ~"2000-2004",
                                TRUE ~as.character(YearStat)))%>%
    mutate(YearStat=as.factor(YearStat))%>%
    group_by(patid)%>%
    mutate(Cardio=sum(as.numeric(as.character(HypertensionStat)), as.numeric(as.character(MIStat)), 
                      as.numeric(as.character(CHFStat)), as.numeric(as.character(CerebrovascularStat))))%>%
    ungroup()
})

datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    mutate(APStatTrial1=relevel(APStatTrial1, ref="olanzapine"))
})
#Do the first one to create data
for (i in (1:1)) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active1+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+SMIDiagStat+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  APStatTrial1+PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)")), data=data)
  })
  AdjResult<-summary(pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  AdjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active1+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+SMIDiagStat+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  APStatTrial1+PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)")), data=data)
  })
  Adj<-summary(pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  Adj$Outcome<-Outcome[i]
  AdjResult<-rbind(Adj, AdjResult)
}

TimePoint<-c(replicate(236,"3"),replicate(236,"6"),replicate(236,"24"),replicate(236,"12"))
Group<-c(replicate(59,"Accident"), replicate(59, "Phys"),replicate(59,"SH"),replicate(59,"MH"))
Group<-c(replicate(4, Group))
AdjResult<-cbind(AdjResult, TimePoint, Group) 
AdjResult$Result<-paste0(format(round(AdjResult$estimate, 2), nsmall=2), " (", format(round(AdjResult$`2.5 %`, 2), nsmall=2), "-", format(round(AdjResult$`97.5 %`, 2), nsmall=2), ")")

write.csv(UnadjResult, "StatinCPRD/Outputs/Trial1Unadj_PPA.csv")
write.csv(AdjResult, "StatinCPRD/Outputs/Trial1Adj_PPA.csv")   
  
  


  
  
  
  
  


