#https://cran.r-project.org/web/packages/ipw/ipw.pdf

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
library(cobalt)


load("StatinCPRD/Data/ImputeTrial3.Rdata")

datlist<-mids2datlist(ImputedData3)

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

Imp<-datlist2mids(datlist)

Weights <- MatchThem::weightthem(Active3 ~ AgeTrial3+YearAP+SMIAPTime+DislipTimeAP+StatDoseTrial3+gender+
                                   ethnicity+SMIDiagAP+PriorMHBinAP+FirstPsychAPTime+PriorSHBinAP+PriorGPSHBinAP+PriorGPAP+
                                   Cardio+DiabAP+APChol+APBMIVal+SSRIAP+TCAAP+OtherADAP+
                                   StatTime+PriorPhysicalBinAP+PriorAccidentBinAP+FullIMD+Died2yr3+MaxFU3, #confounders
                               datasets = Imp, # the mids object created by mice
                                              approach = "within",  # use a within methods rather than across (within methods have been shown to be more consistent)
                                              method = "ps", # default propensity score method which uses logistic regression
                                              stabilize = TRUE, # stabilize weights to reduce the impact of extreme weights
                                              estimand = "ATE") # specify estimand of interest
# Observe success of covariate balance
summary(Weights)

Labs<-c(AgeTrial3="Age", YearAP="Year", SMIAPTime="Time since SMI", DislipTimeAP="Time since dyslipidaemia", StatDoseTrial3="Statin dose", gender="Sex", 
        ethnicity="Ethnicity", SMIDiagAP="SMI diagnosis", PriorMHBinAP="Psychiatric admissions", 
        FirstPsychAPTime="Time on antipsychotics", PriorSHBinAP="Self harm admissions", PriorGPSHBinAP="Self harm events", PriorGPAP="GP consultations",
        Cardio="Cardiovascular disease", DiabAP="Diabetes", APChol="Cholesterol", 
        APBMIVal="BMI", SSRIAP="SSRI", TCAAP="Tricyclic", OtherADAP="Other antidepressant", StatTime="Time on statins", PriorPhysicalBinAP="Physical health admission", 
        PriorAccidentBinAP="Accident/injury admission", FullIMD="IMD", Died2yr3="Deaths", MaxFU3="Follow up time")

love.plot(Weights, stars="std", var.names=Labs, limits=c(-1, 1), threshold=c(-0.1, 0.1))

bal.tab(Weights,
        stats = c("m", "ks"),
        imp.fun="max")

Unadj <- with(data=Weights, coxph(Surv(TimeToMH12, MHBin12) ~ Active3, weights = Weights))
Unadj <- summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

Unadj <- with(data=ImputedData3, coxph(Surv(TimeToMH12, MHBin12) ~ Active3))
Unadj <- summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

####Weighted model####
FinalWeights<-list(complete(Weights, action=1), complete(Weights, action=2), complete(Weights, action=3), complete(Weights, action=4), 
                complete(Weights, action=5), complete(Weights, action=6), complete(Weights, action=7), complete(Weights, action=8),
                complete(Weights, action=9), complete(Weights, action=10))

Outcome<-c("MHBin12", "SHBin12", "PhysicalBin12", "AccidentBin12", "MHBin24", "SHBin24", "PhysicalBin24", "AccidentBin24",
           "MHBin6", "SHBin6", "PhysicalBin6", "AccidentBin6","MHBin3", "SHBin3", "PhysicalBin3", "AccidentBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToPhys12", "TimeToAccident12", "TimeToMH24", "TimeToSH24", "TimeToPhys24", "TimeToAccident24",
        "TimeToMH6", "TimeToSH6", "TimeToPhys6", "TimeToAccident6","TimeToMH3", "TimeToSH3", "TimeToPhys3", "TimeToAccident3")

#Do the first one to create data
for (i in (1:1)) {
  Unadj <- lapply(FinalWeights, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3+strata(pracid)")), data=data, weights=weights)
  })
  UnadjResult<-summary(pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  UnadjResult$Outcome<-Outcome[i]}

#Then bind them on

for (i in (2:length(Outcome))) {
  Unadj <- lapply(FinalWeights, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Active3+strata(pracid)")), data=data, weights=weights)
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

write.csv(UnadjResult, "StatinCPRD/Outputs/Trial3IPW.csv")
save(UnadjResult, file="StatinCPRD/Outputs/Trial3IPW.Rdata")
