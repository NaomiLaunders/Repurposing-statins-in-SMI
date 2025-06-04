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
library(performance)
library(MASS)

####Load trials####
load("StatinCPRD/Data/Trial3.rdata")

####Imputation Trial 1####
#Set time as integer and if zero set to 1 day
Trial3$MaxFU24<-as.integer(round(Trial3$MaxFU3*365.25))
Trial3$MaxFU24[Trial3$MaxFU24==0]<-1

Trial3$MaxFU12<-as.integer(round(Trial3$MaxFU3*365.25))
Trial3$MaxFU12[Trial3$MaxFU12>365]<-as.integer(365)
Trial3$MaxFU12[Trial3$MaxFU12==0]<-1

Trial3$MaxFU6<-as.integer(round(Trial3$MaxFU3*365.25))
Trial3$MaxFU6[Trial3$MaxFU6>182]<-as.integer(182)
Trial3$MaxFU6[Trial3$MaxFU6==0]<-1

Trial3$MaxFU3<-as.integer(round(Trial3$MaxFU3*365.25))
Trial3$MaxFU3[Trial3$MaxFU3>90]<-as.integer(90)
Trial3$MaxFU3[Trial3$MaxFU3==0]<-1

#In order as per protocol
ToImpute1<-dplyr::select(Trial3, patid, pracid, OutcomeAccidentAP12mo, OutcomePhysicalAP12mo, OutcomeCombSHAP12mo, OutcomeMHAP12mo,
                  OutcomeAccidentAP6mo, OutcomePhysicalAP6mo, OutcomeCombSHAP6mo, OutcomeMHAP6mo,
                  OutcomeAccidentAP12w, OutcomePhysicalAP12w, OutcomeCombSHAP12w, OutcomeMHAP12w, MaxFU12, MaxFU6, MaxFU3,#Not included
                  Active3, #Exposure
                  AgeTrial3, YearAP, SMIAPTime, DislipTimeAP, StatDoseTrial3, gender, region,
                  ethnicity, SMIDiagAP, PriorMHBinAP, PriorSHBinAP, PriorGPSHBinAP, FirstPsychAPTime,  
                  HypertensionAP, MIAP, CHFAP, CerebrovascularAP,DiabAP, APChol, APBMIVal,SSRIAP, TCAAP, OtherADAP,LastStatin,
                  StatTime, PriorPhysicalBinAP, PriorAccidentBinAP,PriorGPAP, FullIMD, #confounders
                  MaxFU24, Died2yr3, OutcomeAccidentAP24mo, OutcomePhysicalAP24mo, OutcomeCombSHAP24mo, OutcomeMHAP24mo) #outcomes

sapply(ToImpute1, class)

Basic<-mice(data=ToImpute1, m=1, seed=500)
Pred <- Basic$predictorMatrix
Pred

Pred[c(1:17), "ethnicity"] <- 0
Pred[c(1:17), "APChol"] <- 0
Pred[c(1:17), "APBMIVal"] <- 0

ImputedData1<-mice(data=ToImpute1, predictorMatrix=Pred, m=10, seed=500)


summary(ImputedData1$imp$ethnicity)
summary(ImputedData1$imp$APBMIVal)
summary(ImputedData1$imp$APChol)

PredFinal <- ImputedData1$predictorMatrix
PredFinal

ImputedData1$method
save(ImputedData1, file="StatinCPRD/Data/ImputeTrial3_sens.Rdata")

####Mental health####
Pois<-glm(OutcomeMHAP12mo ~ offset(log(MaxFU12))+Active3, family=poisson, data=Trial3)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomeMHAP12mo ~ offset(log(MaxFU12))+Active3, data=Trial3)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)

####Self harm####
#Dispersion
Pois<-glm(OutcomeCombSHAP12mo ~ offset(log(MaxFU12))+Active3, family=poisson, data=Trial3)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomeCombSHAP12mo ~ offset(log(MaxFU12))+Active3, data=Trial3)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)

####Physical####
#dispersion
Pois<-glm(OutcomePhysicalAP12mo ~ offset(log(MaxFU12))+Active3, family=poisson, data=Trial3)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomePhysicalAP12mo ~ offset(log(MaxFU12))+Active3, data=Trial3)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)

####Accident####
#dispersion
Pois<-glm(OutcomeAccidentAP12mo ~ offset(log(MaxFU12))+Active3, family=poisson, data=Trial3)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomeAccidentAP12mo ~ offset(log(MaxFU12))+Active3, data=Trial3)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)


####Final models####
#Undjusted
Unadj <- with(data=ImputedData1, glm.nb(OutcomeCombSHAP12mo ~ offset(log(MaxFU12))+Active3))
Unadj <- summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

Outcome<-c("OutcomeMHAP12mo", "OutcomeCombSHAP12mo", "OutcomePhysicalAP12mo", "OutcomeAccidentAP12mo", "OutcomeMHAP24mo", "OutcomeCombSHAP24mo", "OutcomePhysicalAP24mo", "OutcomeAccidentAP24mo",
           "OutcomeMHAP6mo", "OutcomeCombSHAP6mo", "OutcomePhysicalAP6mo", "OutcomeAccidentAP6mo", "OutcomeMHAP12w", "OutcomeCombSHAP12w", "OutcomePhysicalAP12w", "OutcomeAccidentAP12w")

Time<-c("MaxFU12", "MaxFU12", "MaxFU12", "MaxFU12", "MaxFU24", "MaxFU24", "MaxFU24", "MaxFU24",
           "MaxFU6", "MaxFU6", "MaxFU6", "MaxFU6","MaxFU3", "MaxFU3", "MaxFU3", "MaxFU3")

datlist<-mids2datlist(ImputedData1)

#Do the first one to create data
for (i in (1:1)) {
  Unadj <- lapply(datlist, FUN=function(data){
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active3")), data=data)
  })
  UnadjResult<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

#Then bind them on
 
for (i in (2:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active3")), data=data)
  })
  Unadj<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResult<-rbind(Unadj, UnadjResult)
  }

UnadjResult<-subset(UnadjResult, term!="(Intercept)")
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
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+SMIDiagAP+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD")), data=data)
  })
  betas <- lapply(Adj, coef)
  vars <- lapply(Adj, FUN = function(x){vcovCL(x, cluster = datlist[[1]]$pracid)})
  AdjResults<-summary(pool_mi(betas, vars))
  AdjResults$Outcome<-Outcome[i]
  AdjResults$estimate<-exp(AdjResults$results)
  AdjResults$Low<-exp(AdjResults$`(lower`)
  AdjResults$Up<-exp(AdjResults$`upper)`)}

#Then bind them on

for (i in (2:length(Outcome))) {
  Adj <- lapply(datlist, FUN=function(data){
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active3+AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                  ethnicity+SMIDiagAP+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                  Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                  StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD")), data=data)
  })
  betas <- lapply(Adj, coef)
  vars <- lapply(Adj, FUN = function(x){vcovCL(x, cluster = datlist[[1]]$pracid)})
  Adj<-summary(pool_mi(betas, vars))
  Adj$Outcome<-Outcome[i]
  Adj$estimate<-exp(Adj$results)
  Adj$Low<-exp(Adj$`(lower`)
  Adj$Up<-exp(Adj$`upper)`)
  AdjResults<-rbind(Adj, AdjResults)
}

TimePoint<-c(replicate(188,"3"),replicate(188,"6"),replicate(188,"24"),replicate(188,"12"))
Group<-c(replicate(47,"Accident"), replicate(47, "Phys"),replicate(47,"SH"),replicate(47,"MH"))
Group<-c(replicate(4, Group))

AdjResults<-cbind(AdjResults, TimePoint, Group) 
AdjResults$Result<-paste0(format(round(AdjResults$estimate, 2), nsmall=2), " (", format(round(AdjResults$Low, 2), nsmall=2), "-", format(round(AdjResults$Up, 2), nsmall=2), ")")

write.csv(UnadjResult, "StatinCPRD/Outputs/Trial3UnadjSens.csv")
write.csv(AdjResults, "StatinCPRD/Outputs/Trial3AdjSens.csv")
save(AdjResults, file="StatinCPRD/Outputs/Trial3AdjSens.Rdata")

