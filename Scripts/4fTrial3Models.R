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
load("StatinCPRD/Data/Trial3.rdata")

####Imputation Trial 1####
#Define outcome#
Trial3$MHBin24<-0
Trial3$MHBin24[Trial3$OutcomeMHAP24mo>0]<-1
Trial3$SHBin24<-0
Trial3$SHBin24[Trial3$OutcomeCombSHAP24mo>0]<-1
Trial3$PhysicalBin24<-0
Trial3$PhysicalBin24[Trial3$OutcomePhysicalAP24mo>0]<-1
Trial3$AccidentBin24<-0
Trial3$AccidentBin24[Trial3$OutcomeAccidentAP24mo>0]<-1

Trial3$MHBin12<-0
Trial3$MHBin12[Trial3$OutcomeMHAP12mo>0]<-1
Trial3$SHBin12<-0
Trial3$SHBin12[Trial3$OutcomeCombSHAP12mo>0]<-1
Trial3$PhysicalBin12<-0
Trial3$PhysicalBin12[Trial3$OutcomePhysicalAP12mo>0]<-1
Trial3$AccidentBin12<-0
Trial3$AccidentBin12[Trial3$OutcomeAccidentAP12mo>0]<-1

Trial3$MHBin6<-0
Trial3$MHBin6[Trial3$OutcomeMHAP6mo>0]<-1
Trial3$SHBin6<-0
Trial3$SHBin6[Trial3$OutcomeCombSHAP6mo>0]<-1
Trial3$PhysicalBin6<-0
Trial3$PhysicalBin6[Trial3$OutcomePhysicalAP6mo>0]<-1
Trial3$AccidentBin6<-0
Trial3$AccidentBin6[Trial3$OutcomeAccidentAP6mo>0]<-1

Trial3$MHBin3<-0
Trial3$MHBin3[Trial3$OutcomeMHAP12w>0]<-1
Trial3$SHBin3<-0
Trial3$SHBin3[Trial3$OutcomeCombSHAP12w>0]<-1
Trial3$PhysicalBin3<-0
Trial3$PhysicalBin3[Trial3$OutcomePhysicalAP12w>0]<-1
Trial3$AccidentBin3<-0
Trial3$AccidentBin3[Trial3$OutcomeAccidentAP12w>0]<-1

#Set time as integer
Trial3$TimeToMH24<-as.integer(round(Trial3$TimeToMH))
Trial3$TimeToSH24<-as.integer(round(Trial3$TimeToSH))
Trial3$TimeToPhys24<-as.integer(round(Trial3$TimeToPhys))
Trial3$TimeToAccident24<-as.integer(round(Trial3$TimeToAccident))

Trial3$TimeToMH12<-as.integer(round(Trial3$TimeToMH))
Trial3$TimeToMH12[Trial3$TimeToMH12>365]<-as.integer(365)
Trial3$TimeToSH12<-as.integer(round(Trial3$TimeToSH))
Trial3$TimeToSH12[Trial3$TimeToSH12>365]<-as.integer(365)
Trial3$TimeToPhys12<-as.integer(round(Trial3$TimeToPhys))
Trial3$TimeToPhys12[Trial3$TimeToPhys12>365]<-as.integer(365)
Trial3$TimeToAccident12<-as.integer(round(Trial3$TimeToAccident))
Trial3$TimeToAccident12[Trial3$TimeToAccident12>365]<-as.integer(365)

Trial3$TimeToMH6<-as.integer(round(Trial3$TimeToMH))
Trial3$TimeToMH6[Trial3$TimeToMH6>182]<-as.integer(182)
Trial3$TimeToSH6<-as.integer(round(Trial3$TimeToSH))
Trial3$TimeToSH6[Trial3$TimeToSH6>182]<-as.integer(182)
Trial3$TimeToPhys6<-as.integer(round(Trial3$TimeToPhys))
Trial3$TimeToPhys6[Trial3$TimeToPhys6>182]<-as.integer(182)
Trial3$TimeToAccident6<-as.integer(round(Trial3$TimeToAccident))
Trial3$TimeToAccident6[Trial3$TimeToAccident6>182]<-as.integer(182)

Trial3$TimeToMH3<-as.integer(round(Trial3$TimeToMH))
Trial3$TimeToMH3[Trial3$TimeToMH3>90]<-as.integer(90)
Trial3$TimeToSH3<-as.integer(round(Trial3$TimeToSH))
Trial3$TimeToSH3[Trial3$TimeToSH3>90]<-as.integer(90)
Trial3$TimeToPhys3<-as.integer(round(Trial3$TimeToPhys))
Trial3$TimeToPhys3[Trial3$TimeToPhys3>90]<-as.integer(90)
Trial3$TimeToAccident3<-as.integer(round(Trial3$TimeToAccident))
Trial3$TimeToAccident3[Trial3$TimeToAccident3>90]<-as.integer(90)

#Find the nelson aalen for 2 year outcomes
HazardMH <- basehaz(coxph(Surv(TimeToMH24, MHBin24)~1,data=Trial3))
HazardMH<-select(HazardMH, HazardMH=hazard, TimeToMH24=time)
HazardSH<- basehaz(coxph(Surv(TimeToSH24, SHBin24)~1,data=Trial3))
HazardSH<-select(HazardSH, HazardSH=hazard, TimeToSH24=time)
HazardPhysical<- basehaz(coxph(Surv(TimeToPhys24, PhysicalBin24)~1,data=Trial3))
HazardPhysical<-select(HazardPhysical, HazardPhysical=hazard, TimeToPhys24=time)
HazardAccident<- basehaz(coxph(Surv(TimeToAccident24, AccidentBin24)~1,data=Trial3))
HazardAccident<-select(HazardAccident, HazardAccident=hazard, TimeToAccident24=time)

Trial3<-merge(x=Trial3, y=HazardMH, by="TimeToMH24", all.x=TRUE, all.y=FALSE)
Trial3<-merge(x=Trial3, y=HazardSH, by="TimeToSH24", all.x=TRUE, all.y=FALSE)
Trial3<-merge(x=Trial3, y=HazardPhysical, by="TimeToPhys24", all.x=TRUE, all.y=FALSE)
Trial3<-merge(x=Trial3, y=HazardAccident, by="TimeToAccident24", all.x=TRUE, all.y=FALSE)

save(Trial3, file="StatinCPRD/Data/Trial3_toimpute.rdata")

####Basic comparison of trial arms####
names(Trial3)

#Table 3 - Cohort basics
MyVars<-c("AgeTrial3", "YearAP", "SMIAPTime", "DislipTimeAP", "DyslipAP","StatDoseTrial3", "StatDose3Miss","gender", "region",
          "ethnicity", "SMIDiagAP", "PriorMHBinAP",  "PriorSHBinAP", "PriorGPSHBinAP","FirstPsychAPTime", 
          "HypertensionAP", "MIAP", "CHFAP", "CerebrovascularAP", "DiabAP", "APChol", "APBMIVal", "SSRIAP", "TCAAP", "OtherADAP", "LastStatin",
          "StatTime", "APTrial3", "PriorPhysicalBinAP", "PriorAccidentBinAP", "PriorGPAP","FullIMD", "PatIMD", "MaxFU3", "Died2yr3",
          "APCholMiss", "APBMIValMiss")

Table1<-CreateTableOne(vars=MyVars,  data=Trial3, strata="Active3", includeNA = TRUE)
print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table1Exp <- print(Table1, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table1Exp, file = "StatinCPRD/Outputs/Trial3Arms.csv")


#In order as per protocol
ToImpute3<-select(Trial3, patid, pracid, TimeToMH24, TimeToSH24, TimeToPhys24, TimeToAccident24, TimeToMH12, TimeToSH12, TimeToPhys12, TimeToAccident12, 
                  TimeToMH6, TimeToSH6, TimeToPhys6, TimeToAccident6, TimeToMH3, TimeToSH3, TimeToPhys3, TimeToAccident3, 
                  MHBin12, SHBin12, AccidentBin12, PhysicalBin12, MHBin6, SHBin6, AccidentBin6, PhysicalBin6, MHBin3, SHBin3, AccidentBin3, PhysicalBin3,#Not included
                  Active3, #Exposure
                  AgeTrial3, YearAP, SMIAPTime, DislipTimeAP, StatDoseTrial3, gender, region,
                  ethnicity, SMIDiagAP, PriorMHBinAP, PriorSHBinAP, PriorGPSHBinAP, FirstPsychAPTime,  
                  HypertensionAP, MIAP, CHFAP, CerebrovascularAP,DiabAP, APChol, APBMIVal,SSRIAP, TCAAP, OtherADAP,LastStatin,
                  StatTime, PriorPhysicalBinAP, PriorAccidentBinAP,PriorGPAP, FullIMD, #confounders
                  MaxFU3, Died2yr3, HazardMH, HazardSH, HazardAccident, HazardPhysical, MHBin24, SHBin24, AccidentBin24, PhysicalBin24) #outcomes

sapply(ToImpute3, class)

Basic<-mice(data=ToImpute3, m=1, seed=500)
Pred <- Basic$predictorMatrix
Pred

Pred[c(1:30), "ethnicity"] <- 0
Pred[c(1:30), "APChol"] <- 0
Pred[c(1:30), "APBMIVal"] <- 0

ImputedData3<-mice(data=ToImpute3, predictorMatrix=Pred, m=10, seed=500)

summary(ImputedData3$imp$ethnicity)
summary(ImputedData3$imp$APBMIVal)
summary(ImputedData3$imp$APChol)

PredFinal <- ImputedData3$predictorMatrix
PredFinal

ImputedData3$method
save(ImputedData3, file="StatinCPRD/Data/ImputeTrial3.Rdata")

####Mental health####
#Kaplan meier
km_fit <- survfit(Surv(TimeToMH12, MHBin12) ~ Active3, data=Trial3)

ggsurvplot(km_fit,
                     pval = TRUE, conf.int = TRUE,
                     linetype = "strata", # Change line type by groups
                     ggtheme = theme_bw(), # Change ggplot2 theme
           risk.table = TRUE, # Add risk table
                     palette = c("#E7B800", "#2E9FDF"),
           legend.labs = c("Quetiapine", "O/R/A"),
           xlab = "Time in days",
           title = "Psychiatric admissions",
                     ylim=c(0.75,1), xlim=c(0, 370))

CoxMH12 <- coxph(Surv(TimeToMH12, MHBin12) ~ Active3, data = Trial3)
summary(CoxMH12) 
Proportional<-cox.zph(CoxMH12)
Proportional
plot(Proportional, main="Psychiatric admissions")


####Self harm####
km_fit <- survfit(Surv(TimeToSH12, SHBin12) ~ Active3, data=Trial3)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           legend.labs = c("Quetiapine", "O/R/A"),
           xlab = "Time in days",
           title = "Self harm events",
           ylim=c(0.75,1), , xlim=c(0, 370))

CoxSH12 <- coxph(Surv(TimeToSH12, SHBin12) ~ Active3, data = Trial3)
summary(CoxSH12) 
Proportional<-cox.zph(CoxSH12)
Proportional
plot(Proportional, main="Self harm events")

####Physical####
km_fit <- survfit(Surv(TimeToPhys12, PhysicalBin12) ~ Active3, data=Trial3)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           legend.labs = c("Quetiapine", "O/R/A"),
           xlab = "Time in days",
           title = "Physical health admissions",
           ylim=c(0.70,1), , xlim=c(0, 370))

CoxPhysical12 <- coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3, data = Trial3)
summary(CoxPhysical12) 
Proportional<-cox.zph(CoxPhysical12)
Proportional
plot(Proportional, main="Physical health admissions")

####Accident####
km_fit <- survfit(Surv(TimeToAccident12, AccidentBin12) ~ Active3, data=Trial3)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           legend.labs = c("Quetiapine", "O/R/A"),
           xlab = "Time in days",
           title = "Accident/injury admissions",
           ylim=c(0.75,1), , xlim=c(0, 365))

CoxAccident12 <- coxph(Surv(TimeToAccident12, AccidentBin12) ~ Active3, data = Trial3)
summary(CoxAccident12) 
Proportional<-cox.zph(CoxAccident12)
Proportional
plot(Proportional, main="Accident/injury admissions")

####Patients experiencing an event####

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

EventsTrial3<-CreateTableOne(vars=Outcome, strata ="Active3", factorVars = Outcome, data=Trial3, includeNA = FALSE)
print(EventsTrial3, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = FALSE)

EventsTrial3Exp <- print(EventsTrial3,  printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = FALSE, showAllLevels = FALSE)

write.csv(EventsTrial3Exp, file = "StatinCPRD/Outputs/EventsTrial3.csv")

####Incidence####
Inc<-Trial3

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24", "TimeToSH24", "TimeToPhys24", "TimeToAccident24",
        "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")


#Change 0 to 1 and divide by 365 so by year
Inc[Time]<-lapply(Inc[Time], function(x) {case_when(x==0 ~ 1,
                                                    TRUE ~ x)})
Inc[Time]<-lapply(Inc[Time], function(x) {x/365})

Active<-subset(Inc, Active3==1)
Comp<-subset(Inc, Active3==0)

for (i in (1:1)) {
  Unadj<-glm(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], "))")), family = poisson, data=Active)
  
  UnadjResult<-tidy(Unadj, conf.int=FALSE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

for (i in (2:length(Outcome))) {
  Unadj <- glm(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], "))")), family = poisson, data=Active)
  Unadj<-tidy(Unadj, conf.int=FALSE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResult<-rbind(UnadjResult, Unadj)
}

UnadjResult$Group<-"Active"

#Comparator

for (i in (1:1)) {
  Unadj<-glm(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], "))")),family = poisson,  data=Comp)
  
  UnadjResultComp<-tidy(Unadj, conf.int=FALSE, exponentiate=TRUE)
  UnadjResultComp$Outcome<-Outcome[i]}

for (i in (2:length(Outcome))) {
  Unadj <- glm(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], "))")), family = poisson, data=Comp)
  Unadj<-tidy(Unadj, conf.int=FALSE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResultComp<-rbind(UnadjResultComp, Unadj)
}
UnadjResultComp$Group<-"Comp"

FinalResults<-rbind(UnadjResult, UnadjResultComp)
FinalResults$estimate<-FinalResults$estimate*100
FinalResults<-dplyr::select(FinalResults, estimate, Outcome, Group)

#Export
write.csv(FinalResults, "StatinCPRD/Outputs/IncTrial3.csv")

####Final models####
#Undjusted
Unadj <- with(data=ImputedData3, coxph(Surv(TimeToMH12, MHBin12) ~ Active3))
Unadj <- summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24", "TimeToSH24", "TimeToPhys24", "TimeToAccident24",
           "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")

datlist<-mids2datlist(ImputedData3)

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

write.csv(UnadjResult, "StatinCPRD/Outputs/Trial3Unadj.csv")
write.csv(AdjResult, "StatinCPRD/Outputs/Trial3Adj.csv")
save(UnadjResult, file="StatinCPRD/Outputs/Trial3Unadj.Rdata")
save(AdjResult, file="StatinCPRD/Outputs/Trial3Adj.Rdata")

####Interaction terms####

#Do the first one to create data
for (i in (1:1)) {
  Int <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3*SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)")), data=data)
  })
  IntResult<-summary(pool(Int), conf.int=TRUE, exponentiate=TRUE)
  IntResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Int <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3*SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)")), data=data)
  })
  Int<-summary(pool(Int), conf.int=TRUE, exponentiate=TRUE)
  Int$Outcome<-Outcome[i]
  IntResult<-rbind(Int, IntResult)
}

TimePoint<-c(replicate(196,"3"),replicate(196,"6"),replicate(196,"24"),replicate(196,"12"))
Group<-c(replicate(49,"Accident"), replicate(49, "Phys"),replicate(49,"SH"),replicate(49,"MH"))
Group<-c(replicate(4, Group))
IntResult<-cbind(IntResult, TimePoint, Group) 
IntResult$Result<-paste0(format(round(IntResult$estimate, 2), nsmall=2), " (", format(round(IntResult$`2.5 %`, 2), nsmall=2), "-", format(round(IntResult$`97.5 %`, 2), nsmall=2), ")")

write.csv(IntResult, "StatinCPRD/Outputs/Trial3Int.csv")

####Test 12 month interaction terms####
ImputedData<-datlist2mids(datlist, progress=FALSE)

#Mental
MainMH<-with(data=ImputedData, coxph(Surv(TimeToMH12, MHBin12) ~ Active3+SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                       ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                       Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                       StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

IntMH<-with(data=ImputedData, coxph(Surv(TimeToMH12, MHBin12) ~ Active3*SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                      ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                      Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                      StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

#Wald test
D1(IntMH, MainMH)

#SH
MainSH<-with(data=ImputedData, coxph(Surv(TimeToSH12, SHBin12) ~ Active3+SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                       ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                       Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                       StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

IntSH<-with(data=ImputedData, coxph(Surv(TimeToSH12, SHBin12) ~ Active3*SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                      ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                      Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                      StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))
#Wald test
D1(IntSH, MainSH)

#Accident
MainAccident<-with(data=ImputedData, coxph(Surv(TimeToAccident12, AccidentBin12) ~ Active3+SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                             ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                             Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                             StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

IntAccident<-with(data=ImputedData, coxph(Surv(TimeToAccident12, AccidentBin12) ~ Active3*SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                            ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                            Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                            StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))
#Wald test
D1(IntAccident, MainAccident)

#Phys
MainPhys<-with(data=ImputedData, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3+SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                         ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                         Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                         StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

IntPhys<-with(data=ImputedData, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3*SMIDiagAP+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                        ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                        Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                        StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))
#Wald test
D1(IntPhys, MainPhys)

####Stratified####

#12month Schiz
Schiz<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    subset(SMIDiagAP=="Schiz")
})

Schiz<-datlist2mids(Schiz, progress=FALSE)

MH12Schiz<-with(data=Schiz, coxph(Surv(TimeToMH12, MHBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

MH12Schiz<-summary(pool(MH12Schiz), conf.int=TRUE, exponentiate=TRUE)

#12month bipolar
BP<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    subset(SMIDiagAP=="Bipolar")
})

BP<-datlist2mids(BP, progress=FALSE)

MH12BP<-with(data=BP, coxph(Surv(TimeToMH12, MHBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                    ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                    Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                    StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

MH12BP<-summary(pool(MH12BP), conf.int=TRUE, exponentiate=TRUE)

#12month Other
Other<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    subset(SMIDiagAP=="Other")
})

Other<-datlist2mids(Other, progress=FALSE)

MH12Other<-with(data=Other, coxph(Surv(TimeToMH12, MHBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                              ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                              Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                              StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

MH12Other<-summary(pool(MH12Other), conf.int=TRUE, exponentiate=TRUE)

#12month unknown
Unkn<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    subset(SMIDiagAP=="Undefined")
})

Unkn<-datlist2mids(Unkn, progress=FALSE)

MH12Unkn<-with(data=Unkn, coxph(Surv(TimeToMH12, MHBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                    ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                    Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                    StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

MH12Unkn<-summary(pool(MH12Unkn), conf.int=TRUE, exponentiate=TRUE)

MH12Unkn$Diag<-"Unknown"
MH12Schiz$Diag<-"Schizophrenia"
MH12Other$Diag<-"Other psychoses"
MH12BP$Diag<-"Bipolar disorder"

MHStrat<-rbind(MH12Schiz, MH12BP, MH12Other, MH12Unkn)
save(MHStrat, file="StatinCPRD/Outputs/Trial3_MHStrat.Rdata")

####Physical health####

#12month Schiz
Phys12Schiz<-with(data=Schiz, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                    ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                    Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                    StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

Phys12Schiz<-summary(pool(Phys12Schiz), conf.int=TRUE, exponentiate=TRUE)

#12month bipolar
Phys12BP<-with(data=BP, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                              ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                              Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                              StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

Phys12BP<-summary(pool(Phys12BP), conf.int=TRUE, exponentiate=TRUE)

#12month Other
Phys12Other<-with(data=Other, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                    ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                    Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                    StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

Phys12Other<-summary(pool(Phys12Other), conf.int=TRUE, exponentiate=TRUE)

#12month unknown
Phys12Unkn<-with(data=Unkn, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                  ethnicity+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+strata(pracid)))

Phys12Unkn<-summary(pool(Phys12Unkn), conf.int=TRUE, exponentiate=TRUE)


Phys12Unkn$Diag<-"Unknown"
Phys12Schiz$Diag<-"Schizophrenia"
Phys12Other$Diag<-"Other psychoses"
Phys12BP$Diag<-"Bipolar disorder"

PhysStrat<-rbind(Phys12Schiz, Phys12BP, Phys12Other, Phys12Unkn)
save(PhysStrat, file="StatinCPRD/Outputs/Trial3_PhysStrat.Rdata")
