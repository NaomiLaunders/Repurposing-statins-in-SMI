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
load("StatinCPRD/Data/Trial2.rdata")

####Imputation Trial 1####
#Define outcome#
Trial2$MHBin24<-0
Trial2$MHBin24[Trial2$OutcomeMHStat24mo>0]<-1
Trial2$SHBin24<-0
Trial2$SHBin24[Trial2$OutcomeCombSHStat24mo>0]<-1
Trial2$PhysicalBin24<-0
Trial2$PhysicalBin24[Trial2$OutcomePhysicalStat24mo>0]<-1
Trial2$AccidentBin24<-0
Trial2$AccidentBin24[Trial2$OutcomeAccidentStat24mo>0]<-1

Trial2$MHBin12<-0
Trial2$MHBin12[Trial2$OutcomeMHStat12mo>0]<-1
Trial2$SHBin12<-0
Trial2$SHBin12[Trial2$OutcomeCombSHStat12mo>0]<-1
Trial2$PhysicalBin12<-0
Trial2$PhysicalBin12[Trial2$OutcomePhysicalStat12mo>0]<-1
Trial2$AccidentBin12<-0
Trial2$AccidentBin12[Trial2$OutcomeAccidentStat12mo>0]<-1

Trial2$MHBin6<-0
Trial2$MHBin6[Trial2$OutcomeMHStat6mo>0]<-1
Trial2$SHBin6<-0
Trial2$SHBin6[Trial2$OutcomeCombSHStat6mo>0]<-1
Trial2$PhysicalBin6<-0
Trial2$PhysicalBin6[Trial2$OutcomePhysicalStat6mo>0]<-1
Trial2$AccidentBin6<-0
Trial2$AccidentBin6[Trial2$OutcomeAccidentStat6mo>0]<-1

Trial2$MHBin3<-0
Trial2$MHBin3[Trial2$OutcomeMHStat12w>0]<-1
Trial2$SHBin3<-0
Trial2$SHBin3[Trial2$OutcomeCombSHStat12w>0]<-1
Trial2$PhysicalBin3<-0
Trial2$PhysicalBin3[Trial2$OutcomePhysicalStat12w>0]<-1
Trial2$AccidentBin3<-0
Trial2$AccidentBin3[Trial2$OutcomeAccidentStat12w>0]<-1

#Set time as integer
Trial2$TimeToMH24<-as.integer(round(Trial2$TimeToMH))
Trial2$TimeToSH24<-as.integer(round(Trial2$TimeToSH))
Trial2$TimeToPhys24<-as.integer(round(Trial2$TimeToPhys))
Trial2$TimeToAccident24<-as.integer(round(Trial2$TimeToAccident))

Trial2$TimeToMH12<-as.integer(round(Trial2$TimeToMH))
Trial2$TimeToMH12[Trial2$TimeToMH12>365]<-as.integer(365)
Trial2$TimeToSH12<-as.integer(round(Trial2$TimeToSH))
Trial2$TimeToSH12[Trial2$TimeToSH12>365]<-as.integer(365)
Trial2$TimeToPhys12<-as.integer(round(Trial2$TimeToPhys))
Trial2$TimeToPhys12[Trial2$TimeToPhys12>365]<-as.integer(365)
Trial2$TimeToAccident12<-as.integer(round(Trial2$TimeToAccident))
Trial2$TimeToAccident12[Trial2$TimeToAccident12>365]<-as.integer(365)

Trial2$TimeToMH6<-as.integer(round(Trial2$TimeToMH))
Trial2$TimeToMH6[Trial2$TimeToMH6>182]<-as.integer(182)
Trial2$TimeToSH6<-as.integer(round(Trial2$TimeToSH))
Trial2$TimeToSH6[Trial2$TimeToSH6>182]<-as.integer(182)
Trial2$TimeToPhys6<-as.integer(round(Trial2$TimeToPhys))
Trial2$TimeToPhys6[Trial2$TimeToPhys6>182]<-as.integer(182)
Trial2$TimeToAccident6<-as.integer(round(Trial2$TimeToAccident))
Trial2$TimeToAccident6[Trial2$TimeToAccident6>182]<-as.integer(182)

Trial2$TimeToMH3<-as.integer(round(Trial2$TimeToMH))
Trial2$TimeToMH3[Trial2$TimeToMH3>90]<-as.integer(90)
Trial2$TimeToSH3<-as.integer(round(Trial2$TimeToSH))
Trial2$TimeToSH3[Trial2$TimeToSH3>90]<-as.integer(90)
Trial2$TimeToPhys3<-as.integer(round(Trial2$TimeToPhys))
Trial2$TimeToPhys3[Trial2$TimeToPhys3>90]<-as.integer(90)
Trial2$TimeToAccident3<-as.integer(round(Trial2$TimeToAccident))
Trial2$TimeToAccident3[Trial2$TimeToAccident3>90]<-as.integer(90)

#Find the nelson aalen for 2 year outcomes
HazardMH <- basehaz(coxph(Surv(TimeToMH24, MHBin24)~1,data=Trial2))
HazardMH<-select(HazardMH, HazardMH=hazard, TimeToMH24=time)
HazardSH<- basehaz(coxph(Surv(TimeToSH24, SHBin24)~1,data=Trial2))
HazardSH<-select(HazardSH, HazardSH=hazard, TimeToSH24=time)
HazardPhysical<- basehaz(coxph(Surv(TimeToPhys24, PhysicalBin24)~1,data=Trial2))
HazardPhysical<-select(HazardPhysical, HazardPhysical=hazard, TimeToPhys24=time)
HazardAccident<- basehaz(coxph(Surv(TimeToAccident24, AccidentBin24)~1,data=Trial2))
HazardAccident<-select(HazardAccident, HazardAccident=hazard, TimeToAccident24=time)

Trial2<-merge(x=Trial2, y=HazardMH, by="TimeToMH24", all.x=TRUE, all.y=FALSE)
Trial2<-merge(x=Trial2, y=HazardSH, by="TimeToSH24", all.x=TRUE, all.y=FALSE)
Trial2<-merge(x=Trial2, y=HazardPhysical, by="TimeToPhys24", all.x=TRUE, all.y=FALSE)
Trial2<-merge(x=Trial2, y=HazardAccident, by="TimeToAccident24", all.x=TRUE, all.y=FALSE)

save(Trial2, file="StatinCPRD/Data/Trial2_toimpute.rdata")

####Basic comparison of trial arms####
names(Trial2)

#Table 1 - Cohort basics
MyVars<-c("AgeTrial1", "YearStat", "SMIStatTime", "DislipTimeStat", "DyslipStat", "StatDose1", "StatDose1Miss", "gender", "region",
          "ethnicity", "SMIDiagStat", "PriorMHBinStat", "PriorSHBinStat", "PriorGPSHBinStat","FirstPsychStatTime", 
          "HypertensionStat", "MIStat", "CHFStat", "CerebrovascularStat", "DiabStat", "StatChol", "StatBMIVal", "SSRIStat", "TCAStat", "OtherADStat",
          "APStatTrial2", "Trial2Statin", "PriorPhysicalBinStat", "PriorAccidentBinStat", "PriorGPStat", "FullIMD", "PatIMD", "MaxFU2", "Died2yr1",
          "StatCholMiss", "StatBMIValMiss")

Table1<-CreateTableOne(vars=MyVars,  data=Trial2, strata="Active2", includeNA = TRUE)
print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table1Exp <- print(Table1, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table1Exp, file = "StatinCPRD/Outputs/Trial2Arms.csv")

#In order as per protocol
ToImpute1<-select(Trial2, patid, pracid, TimeToMH24, TimeToSH24, TimeToPhys24, TimeToAccident24, TimeToMH12, TimeToSH12, TimeToPhys12, TimeToAccident12, 
                  TimeToMH6, TimeToSH6, TimeToPhys6, TimeToAccident6, TimeToMH3, TimeToSH3, TimeToPhys3, TimeToAccident3, 
                  MHBin12, SHBin12, AccidentBin12, PhysicalBin12, MHBin6, SHBin6, AccidentBin6, PhysicalBin6, MHBin3, SHBin3, AccidentBin3, PhysicalBin3,#Not included
                  Active2, #Exposure
                  AgeTrial1, YearStat, SMIStatTime, DislipTimeStat, StatDose1, gender, region,
                  ethnicity, SMIDiagStat, PriorMHBinStat,  PriorSHBinStat, PriorGPSHBinStat, FirstPsychStatTime,
                  HypertensionStat, MIStat, CHFStat, CerebrovascularStat,DiabStat, StatChol, StatBMIVal,SSRIStat, TCAStat, OtherADStat,
                  PriorPhysicalBinStat, PriorAccidentBinStat, PriorGPStat, FullIMD, #confounders
                  APStatTrial2, MaxFU2, Died2yr1, HazardMH, HazardSH, HazardAccident, HazardPhysical, MHBin24, SHBin24, AccidentBin24, PhysicalBin24) #outcomes

sapply(ToImpute1, class)

Basic<-mice(data=ToImpute1, m=1, seed=500)
Pred <- Basic$predictorMatrix
Pred

Pred[c(1:30), "ethnicity"] <- 0
Pred[c(1:30), "StatChol"] <- 0
Pred[c(1:30), "StatBMIVal"] <- 0

ImputedData1<-mice(data=ToImpute1, predictorMatrix=Pred, m=10, seed=500)

summary(ImputedData1$imp$ethnicity)
summary(ImputedData1$imp$StatBMIVal)
summary(ImputedData1$imp$StatChol)

PredFinal <- ImputedData1$predictorMatrix
PredFinal

ImputedData1$method
save(ImputedData1, file="StatinCPRD/Data/ImputeTrial2.Rdata")

####Mental health####
#Kaplan meier
km_fit <- survfit(Surv(TimeToMH12, MHBin12) ~ Active2, data=Trial2)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           xlab = "Time in days",
           legend.labs=c("Pravastatin" ,"Simvastatin/Atorvastatin"),
           title = "Psychiatric admissions",
           ylim=c(0.75,1), xlim=c(0, 370))

CoxMH12 <- coxph(Surv(TimeToMH12, MHBin12) ~ Active2, data = Trial2)
summary(CoxMH12) 
Proportional<-cox.zph(CoxMH12)
Proportional
plot(Proportional, main="Psychiatric admissions")


####Self harm####
km_fit <- survfit(Surv(TimeToSH12, SHBin12) ~ Active2, data=Trial2)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           title = "Self harm events",
           xlab = "Time in days",
           legend.labs=c("Pravastatin" ,"Simvastatin/Atorvastatin"),
           ylim=c(0.75,1), xlim=c(0, 370))

CoxSH12 <- coxph(Surv(TimeToSH12, SHBin12) ~ Active2, data = Trial2)
summary(CoxSH12) 
Proportional<-cox.zph(CoxSH12)
Proportional
plot(Proportional, main="Self harm events")

####Physical####
km_fit <- survfit(Surv(TimeToPhys12, PhysicalBin12) ~ Active2, data=Trial2)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           title = "Physical health admissions",
           xlab = "Time in days",
           legend.labs=c("Pravastatin" ,"Simvastatin/Atorvastatin"),
           ylim=c(0.75,1), xlim=c(0, 370))

CoxPhysical12 <- coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active2, data = Trial2)
summary(CoxPhysical12) 
Proportional<-cox.zph(CoxPhysical12)
Proportional
plot(Proportional, main="Physical health admissions")

####Accident####
km_fit <- survfit(Surv(TimeToAccident12, AccidentBin12) ~ Active2, data=Trial2)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           title = "Accident/injury admissions",
           legend.labs=c("Pravastatin" ,"Simvastatin/Atorvastatin"),
           xlab = "Time in days",
           ylim=c(0.75,1), xlim=c(0, 365))

CoxAccident12 <- coxph(Surv(TimeToAccident12, AccidentBin12) ~ Active2, data = Trial2)
summary(CoxAccident12) 
Proportional<-cox.zph(CoxAccident12)
Proportional
plot(Proportional, main="Accident/injury health admissions")


####Patients experiencing an event####

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

EventsTrial2<-CreateTableOne(vars=Outcome, strata ="Active2", factorVars = Outcome, data=Trial2, includeNA = FALSE)
print(EventsTrial2, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = FALSE)

EventsTrial2Exp <- print(EventsTrial2,  printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = FALSE, showAllLevels = FALSE)

write.csv(EventsTrial2Exp, file = "StatinCPRD/Outputs/EventsTrial2.csv")

#Incidence
Inc<-Trial2

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24", "TimeToSH24", "TimeToPhys24", "TimeToAccident24",
        "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")


#Change 0 to 1 and divide by 365 so by year
Inc[Time]<-lapply(Inc[Time], function(x) {case_when(x==0 ~ 1,
                                                    TRUE ~ x)})
Inc[Time]<-lapply(Inc[Time], function(x) {x/365})

Active<-subset(Inc, Active2==1)
Comp<-subset(Inc, Active2==0)

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
write.csv(FinalResults, "StatinCPRD/Outputs/IncTrial2.csv")

####Final models####
#Undjusted
Unadj <- with(data=ImputedData1, coxph(Surv(TimeToMH12, MHBin12) ~ Active2))
Unadj <- summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24", "TimeToSH24", "TimeToPhys24", "TimeToAccident24",
        "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")

datlist<-mids2datlist(ImputedData1)

#Do the first one to create data
for (i in (1:1)) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active2+strata(pracid)")), data=data)
  })
  UnadjResult<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active2+strata(pracid)")), data=data)
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
    mutate(YearStat = case_when(YearStat=="2000"|YearStat=="2001"|YearStat=="2002"|YearStat=="2003"|YearStat=="2004" ~"2000-2004",
                                TRUE ~as.character(YearStat)))%>%
    mutate(YearStat=as.factor(YearStat))%>%
    group_by(patid)%>%
    mutate(Cardio=sum(as.numeric(as.character(HypertensionStat)), as.numeric(as.character(MIStat)), 
                      as.numeric(as.character(CHFStat)), as.numeric(as.character(CerebrovascularStat))))%>%
    ungroup()
})

#Do the first one to create data
for (i in (1:1)) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active2+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+SMIDiagStat+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)")), data=data)
  })
  AdjResult<-summary(pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  AdjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active2+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+SMIDiagStat+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)")), data=data)
  })
  Adj<-summary(pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  Adj$Outcome<-Outcome[i]
  AdjResult<-rbind(Adj, AdjResult)
}

TimePoint<-c(replicate(172,"3"),replicate(172,"6"),replicate(172,"24"),replicate(172,"12"))
Group<-c(replicate(43,"Accident"), replicate(43, "Phys"),replicate(43,"SH"),replicate(43,"MH"))
Group<-c(replicate(4, Group))
AdjResult<-cbind(AdjResult, TimePoint, Group) 
AdjResult$Result<-paste0(format(round(AdjResult$estimate, 2), nsmall=2), " (", format(round(AdjResult$`2.5 %`, 2), nsmall=2), "-", format(round(AdjResult$`97.5 %`, 2), nsmall=2), ")")

write.csv(UnadjResult, "StatinCPRD/Outputs/Trial2Unadj.csv")
write.csv(AdjResult, "StatinCPRD/Outputs/Trial2Adj.csv")
save(UnadjResult, file="StatinCPRD/Outputs/Trial2Unadj.Rdata")
save(AdjResult, file="StatinCPRD/Outputs/Trial2Adj.Rdata")

####Interaction terms####

#Do the first one to create data
for (i in (1:1)) {
  Int <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active2*SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)")), data=data)
  })
  IntResult<-summary(pool(Int), conf.int=TRUE, exponentiate=TRUE)
  IntResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Int <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active2*SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)")), data=data)
  })
  Int<-summary(pool(Int), conf.int=TRUE, exponentiate=TRUE)
  Int$Outcome<-Outcome[i]
  IntResult<-rbind(Int, IntResult)
}

TimePoint<-c(replicate(180,"3"),replicate(180,"6"),replicate(180,"24"),replicate(180,"12"))
Group<-c(replicate(45,"Accident"), replicate(45, "Phys"),replicate(45,"SH"),replicate(45,"MH"))
Group<-c(replicate(4, Group))
IntResult<-cbind(IntResult, TimePoint, Group) 
IntResult$Result<-paste0(format(round(IntResult$estimate, 2), nsmall=2), " (", format(round(IntResult$`2.5 %`, 2), nsmall=2), "-", format(round(IntResult$`97.5 %`, 2), nsmall=2), ")")

write.csv(IntResult, "StatinCPRD/Outputs/Trial2Int.csv")

####Test 12 month interaction terms####
ImputedData<-datlist2mids(datlist, progress=FALSE)

#Mental
MainMH<-with(data=ImputedData, coxph(Surv(TimeToMH12, MHBin12) ~ Active2+SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                       ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                       Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                       PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))

IntMH<-with(data=ImputedData, coxph(Surv(TimeToMH12, MHBin12) ~ Active2*SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                      ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                      Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                      PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))

#Wald test
D1(IntMH, MainMH)

#SH
MainSH<-with(data=ImputedData, coxph(Surv(TimeToSH12, SHBin12) ~ Active2+SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                       ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                       Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                       PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))

IntSH<-with(data=ImputedData, coxph(Surv(TimeToSH12, SHBin12) ~ Active2*SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                      ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                      Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                      PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))
#Wald test
D1(IntSH, MainSH)

#Accident
MainAccident<-with(data=ImputedData, coxph(Surv(TimeToAccident12, AccidentBin12) ~ Active2+SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                             ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                             Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                             PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))

IntAccident<-with(data=ImputedData, coxph(Surv(TimeToAccident12, AccidentBin12) ~ Active2*SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                            ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                            Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                            PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))
#Wald test
D1(IntAccident, MainAccident)

#Phys
MainPhys<-with(data=ImputedData, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active2+SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                         ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                         Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                         PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))

IntPhys<-with(data=ImputedData, coxph(Surv(TimeToPhys12, PhysicalBin12) ~ Active2*SMIDiagStat+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                                        ethnicity+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                                        Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                                        PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD+strata(pracid)))
#Wald test
D1(IntPhys, MainPhys)

