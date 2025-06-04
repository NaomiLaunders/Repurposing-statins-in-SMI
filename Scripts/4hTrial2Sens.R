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
load("StatinCPRD/Data/Trial2.rdata")

####Imputation Trial 1####
#Set time as integer and if zero set to 1 day
Trial2$MaxFU24<-as.integer(round(Trial2$MaxFU2*365.25))
Trial2$MaxFU24[Trial2$MaxFU24==0]<-1

Trial2$MaxFU12<-as.integer(round(Trial2$MaxFU2*365.25))
Trial2$MaxFU12[Trial2$MaxFU12>365]<-as.integer(365)
Trial2$MaxFU12[Trial2$MaxFU12==0]<-1

Trial2$MaxFU6<-as.integer(round(Trial2$MaxFU2*365.25))
Trial2$MaxFU6[Trial2$MaxFU6>182]<-as.integer(182)
Trial2$MaxFU6[Trial2$MaxFU6==0]<-1

Trial2$MaxFU3<-as.integer(round(Trial2$MaxFU2*365.25))
Trial2$MaxFU3[Trial2$MaxFU3>90]<-as.integer(90)
Trial2$MaxFU3[Trial2$MaxFU3==0]<-1

#In order as per protocol
ToImpute1<-dplyr::select(Trial2, patid, patid, pracid, OutcomeAccidentStat12mo, OutcomePhysicalStat12mo, OutcomeCombSHStat12mo, OutcomeMHStat12mo,
                         OutcomeAccidentStat6mo, OutcomePhysicalStat6mo, OutcomeCombSHStat6mo, OutcomeMHStat6mo,
                         OutcomeAccidentStat12w, OutcomePhysicalStat12w, OutcomeCombSHStat12w, OutcomeMHStat12w, MaxFU12, MaxFU6, MaxFU3,#Not included
                         Active2, #Exposure
                         AgeTrial1, YearStat, SMIStatTime, DislipTimeStat, StatDose1, gender, region,
                         ethnicity, SMIDiagStat, PriorMHBinStat,  PriorSHBinStat, PriorGPSHBinStat, FirstPsychStatTime,
                         HypertensionStat, MIStat, CHFStat, CerebrovascularStat,DiabStat, StatChol, StatBMIVal,SSRIStat, TCAStat, OtherADStat,
                         PriorPhysicalBinStat, PriorAccidentBinStat, PriorGPStat, FullIMD, #confounders
                         APStatTrial2, MaxFU24,  OutcomeAccidentStat24mo, OutcomePhysicalStat24mo, OutcomeCombSHStat24mo, OutcomeMHStat24mo) #outcomes

sapply(ToImpute1, class)

Basic<-mice(data=ToImpute1, m=1, seed=500)
Pred <- Basic$predictorMatrix
Pred

Pred[c(1:17), "ethnicity"] <- 0
Pred[c(1:17), "StatChol"] <- 0
Pred[c(1:17), "StatBMIVal"] <- 0

ImputedData1<-mice(data=ToImpute1, predictorMatrix=Pred, m=10, seed=500)


summary(ImputedData1$imp$ethnicity)
summary(ImputedData1$imp$StatBMIVal)
summary(ImputedData1$imp$StatChol)

PredFinal <- ImputedData1$predictorMatrix
PredFinal

ImputedData1$method
save(ImputedData1, file="StatinCPRD/Data/ImputeTrial2_sens.Rdata")

####Mental health####
Pois<-glm(OutcomeMHStat12mo ~ offset(log(MaxFU12))+Active2, family=poisson, data=Trial2)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomeMHStat12mo ~ offset(log(MaxFU12))+Active2, data=Trial2)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)

####Self harm####
#Dispersion
Pois<-glm(OutcomeCombSHStat12mo ~ offset(log(MaxFU12))+Active2, family=poisson, data=Trial2)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomeCombSHStat12mo ~ offset(log(MaxFU12))+Active2, data=Trial2)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)

####Physical####
#dispersion
Pois<-glm(OutcomePhysicalStat12mo ~ offset(log(MaxFU12))+Active2, family=poisson, data=Trial2)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomePhysicalStat12mo ~ offset(log(MaxFU12))+Active2, data=Trial2)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)

####Accident####
#dispersion
Pois<-glm(OutcomeAccidentStat12mo ~ offset(log(MaxFU12))+Active2, family=poisson, data=Trial2)

check_zeroinflation(Pois, tolerance = 0.05)
check_overdispersion(Pois)

#Is it still if use neg bin?
NegBin<-glm.nb(OutcomeAccidentStat12mo ~ offset(log(MaxFU12))+Active2, data=Trial2)

check_zeroinflation(NegBin, tolerance = 0.05)
check_overdispersion(NegBin)


####Final models####
#Undjusted
Unadj <- with(data=ImputedData1, glm.nb(OutcomeCombSHStat12mo ~ offset(log(MaxFU12))+Active2))
Unadj <- summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

Outcome<-c("OutcomeMHStat12mo", "OutcomeCombSHStat12mo", "OutcomePhysicalStat12mo", "OutcomeAccidentStat12mo", "OutcomeMHStat24mo", "OutcomeCombSHStat24mo", "OutcomePhysicalStat24mo", "OutcomeAccidentStat24mo",
           "OutcomeMHStat6mo", "OutcomeCombSHStat6mo", "OutcomePhysicalStat6mo", "OutcomeAccidentStat6mo", "OutcomeMHStat12w", "OutcomeCombSHStat12w", "OutcomePhysicalStat12w", "OutcomeAccidentStat12w")

Time<-c("MaxFU12", "MaxFU12", "MaxFU12", "MaxFU12", "MaxFU24", "MaxFU24", "MaxFU24", "MaxFU24",
           "MaxFU6", "MaxFU6", "MaxFU6", "MaxFU6","MaxFU3", "MaxFU3", "MaxFU3", "MaxFU3")

datlist<-mids2datlist(ImputedData1)

#Do the first one to create data
for (i in (1:1)) {
  Unadj <- lapply(datlist, FUN=function(data){
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active2")), data=data)
  })
  UnadjResult<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

#Then bind them on
 
for (i in (2:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active2")), data=data)
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
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active2+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+SMIDiagStat+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD")), data=data)
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
    glm.nb(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], ")) +Active2+AgeTrial1+YearStat+SMIStatTime+DislipTimeStat+StatDose1+gender+
                  ethnicity+SMIDiagStat+PriorMHBinStat+FirstPsychStatTime+PriorSHBinStat+PriorGPSHBinStat+PriorGPStat+
                  Cardio+DiabStat+StatChol+StatBMIVal+SSRIStat+TCAStat+OtherADStat+
                  PriorPhysicalBinStat+PriorAccidentBinStat+FullIMD")), data=data)
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

TimePoint<-c(replicate(176,"3"),replicate(176,"6"),replicate(176,"24"),replicate(176,"12"))
Group<-c(replicate(44,"Accident"), replicate(44, "Phys"),replicate(44,"SH"),replicate(44,"MH"))
Group<-c(replicate(4, Group))
AdjResults<-cbind(AdjResults, TimePoint, Group) 
AdjResults$Result<-paste0(format(round(AdjResults$estimate, 2), nsmall=2), " (", format(round(AdjResults$Low, 2), nsmall=2), "-", format(round(AdjResults$Up, 2), nsmall=2), ")")

save(AdjResults, file="StatinCPRD/Outputs/Trial2AdjSens.Rdata")
write.csv(UnadjResult, "StatinCPRD/Outputs/Trial2UnadjSens.csv")
write.csv(AdjResults, "StatinCPRD/Outputs/Trial2AdjSens.csv")
