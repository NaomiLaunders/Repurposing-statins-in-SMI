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
load("StatinCPRD/Data/Trial3_toimpute.rdata")
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")

StatDate<-select(Trial3, patid, FirstAPTrial3Date, Active3, end)

#Only include statins within the two year follow up...
StatDate$Yr2_1<-as.Date(StatDate$FirstAPTrial3Date+730.5+30.4375)
StatDate$EndDate1<-pmin(StatDate$end, StatDate$Yr2_1)

TrialStat<-subset(AllStat, patid %in% Trial3$patid)
TrialStat<-merge(x=TrialStat, y=StatDate, by="patid", all.x=TRUE, all.y=FALSE)

#Per protocol analysis so need to account for switching and discontinuation

#Time to different antipsychotic/statin
#TRIAL 3: Simvastatin/atorvastatin to any other statin; prav or rosuv to any other statin; risperidone, aripiprazole, olanzapine to any other.

####Trial 3####
#AP switching
PatAPAll<-subset(PatAPAll, patid %in% Trial3$patid)
AP<-merge(x=PatAPAll, y=StatDate, by="patid", all.x=FALSE, all.y=FALSE)

#Include those from first index to end
Trial3AP<-subset(AP, !is.na(FirstAPTrial3Date) & eventdate-FirstAPTrial3Date>=0 & eventdate<=EndDate1)

#Switched arm:cases
Trial3SwitchCase<-subset(Trial3AP, AP=="Quetiapine" & Active3==1)

length(unique(Trial3SwitchCase$patid))

Trial3SwitchCase<-Trial3SwitchCase%>%
  group_by(patid)%>%
  mutate(FirstSwitchAP=min(eventdate))%>%
  subset(FirstSwitchAP==eventdate)%>%
  select(patid, FirstSwitchAP)%>%
  distinct()

length(unique(Trial3SwitchCase$patid))

#Switched from current:controls
Trial3SwitchControl<-subset(Trial3AP, (AP=="Aripiprazole" | AP=="Risperidone" | AP=="Olanzapine") & Active3==0)

length(unique(Trial3SwitchControl$patid))

Trial3SwitchControl<-Trial3SwitchControl%>%
  group_by(patid)%>%
  mutate(FirstSwitchAP=min(eventdate))%>%
  subset(FirstSwitchAP==eventdate)%>%
  select(patid, FirstSwitchAP)%>%
  distinct()

length(unique(Trial3SwitchControl$patid))

Trial3APSwitch<-rbind(Trial3SwitchControl, Trial3SwitchCase)

length(unique(Trial3APSwitch$patid))

#Discontinued antipsychotic: Gap of more than 90 days
Trial3Discont<-subset(Trial3AP, AP=="Aripiprazole" | AP=="Risperidone" | AP=="Olanzapine"| AP=="Quetiapine")

Trial3APDis<-Trial3Discont%>%
  group_by(patid)%>%
  arrange(patid, eventdate)%>%
  mutate(TimeToNext=as.numeric(lead(eventdate)-eventdate))%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(EndDate1-eventdate),
                              TRUE ~ TimeToNext))%>%
  subset(TimeToNext>90)%>%
  mutate(Min=min(eventdate))%>%
  subset(eventdate==Min)%>%
  mutate(DiscontAP=eventdate+90)%>%
  select(patid, DiscontAP)%>%
  distinct()

length(unique(Trial3APDis$patid))

#Discontinued statin: Gap of more than 90 days
Trial3Stat<-subset(TrialStat, !is.na(FirstAPTrial3Date))
Trial3Stat<-subset(Trial3Stat, issuedate<=EndDate1 & issuedate-FirstAPTrial3Date>=-182.625)
Trial3Stat<-subset(Trial3Stat, Statin=="Simvastatin" | Statin=="Atorvastatin")

FirstStudyStat<-Trial3Stat%>%
  subset(issuedate-FirstAPTrial3Date<=0)%>%
  group_by(patid)%>%
  mutate(FirstStudyStatin=max(issuedate))%>%
  subset(FirstStudyStatin==issuedate)%>%
  select(patid, FirstStudyStatin)%>%
  distinct()

length(unique(FirstStudyStat$patid))
    
Trial3Stat<-merge(x=Trial3Stat, y=FirstStudyStat, by="patid", all.x=TRUE, all.y=TRUE)
  
StatDis<-Trial3Stat%>%
  subset(issuedate>=FirstStudyStatin)%>%
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

PPA<-merge(x=StatDis, y=Trial3APDis, by="patid", all.x=TRUE, all.y=TRUE)
PPA<-merge(x=PPA, y=Trial3APSwitch, by="patid", all.x=TRUE, all.y=TRUE)

PPA$PPADate<-pmin(PPA$DiscontStat, PPA$DiscontAP, PPA$FirstSwitchAP, na.rm=TRUE)
PPA<-select(PPA, patid, PPADate)

#####Now bind back on to trial####
Trial3<-merge(x=Trial3, y=PPA, by="patid", all.x=TRUE, all.y=TRUE)
Trial3$Yr2_1<-as.Date(Trial3$FirstAPTrial3Date+730.5+30.4375)

Trial3$PPAEnd<-pmin(Trial3$end, Trial3$Yr2_1, Trial3$PPADate, na.rm=TRUE)
Trial3$PPAFU<-as.numeric(Trial3$PPAEnd-Trial3$FirstAPTrial3Date-30.4375)
summary(Trial3$PPAFU)

Trial3<-Trial3%>%
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

NewFields<-select(Trial3, patid, MHBin24, MHBin12,MHBin6,MHBin3,SHBin24,SHBin12,SHBin6,SHBin3,
                  PhysicalBin24,PhysicalBin12,PhysicalBin6,PhysicalBin3,AccidentBin24,AccidentBin12,AccidentBin6,AccidentBin3,
                  TimeToMH24, TimeToMH12, TimeToMH6, TimeToMH3, TimeToSH24, TimeToSH12, TimeToSH6, TimeToSH3,
                  TimeToPhys24, TimeToPhys12, TimeToPhys6, TimeToPhys3, TimeToAccident24,TimeToAccident12, TimeToAccident6, TimeToAccident3, PPAFU)

load("StatinCPRD/Data/ImputeTrial3.Rdata")
datlist<-mids2datlist(ImputedData3)

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
Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24","PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24","TimeToSH24", "TimeToPhys24", "TimeToAccident24",
        "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")

#Do the first one to create data
for (i in (1:1)) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3+strata(pracid)")), data=data)
  })
  UnadjResult<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3+strata(pracid)")), data=data)
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
    mutate(ethnicity = case_when(ethnicity=="Mixed" ~"Other",
                                 TRUE ~as.character(ethnicity)))%>%
    mutate(ethnicity=as.factor(ethnicity))%>%
    mutate(YearAP = case_when(YearAP=="2000"|YearAP=="2001"|YearAP=="2002"|YearAP=="2003"|YearAP=="2004" ~"2000-2004",
                                TRUE ~as.character(YearAP)))%>%
    mutate(YearAP=as.factor(YearAP))%>%
    group_by(patid)%>%
    mutate(Cardio=sum(as.numeric(as.character(HypertensionAP)), as.numeric(as.character(MIAP)), 
                      as.numeric(as.character(CHFAP)), as.numeric(as.character(CerebrovascularAP))))%>%
    ungroup()
})

#Do the first one to create data
for (i in (1:1)) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+SMIDiagAP+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)")), data=data)
  })
  AdjResult<-summary(pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  AdjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+SMIDiagAP+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)")), data=data)
  })
  Adj<-summary(pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  Adj$Outcome<-Outcome[i]
  AdjResult<-rbind(Adj, AdjResult)
}


TimePoint<-c(replicate(184,"3"),replicate(184,"6"),replicate(184,"24"),replicate(184,"12"))
Group<-c(replicate(46,"Accident"), replicate(46, "Phys"),replicate(46,"SH"),replicate(46,"MH"))
Group<-c(replicate(4, Group))
AdjResult<-cbind(AdjResult, TimePoint, Group) 

AdjResult$Result<-paste0(format(round(AdjResult$estimate, 2), nsmall=2), " (", format(round(AdjResult$`2.5 %`, 2), nsmall=2), "-", format(round(AdjResult$`97.5 %`, 2), nsmall=2), ")")

write.csv(UnadjResult, "StatinCPRD/Outputs/Trial3Unadj_PPA.csv")
write.csv(AdjResult, "StatinCPRD/Outputs/Trial3Adj_PPA.csv")   

####Just do a visual check####
km_fit <- survfit(Surv(TimeToMH12, MHBin12) ~ Active3, data=Trial3)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           ylim=c(0.75,1), xlim=c(0, 370))