library(tidyverse)
library(lubridate)
library(tableone)

#Clear environment

rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/Hospselect.rdata")

####Trial one - entry = statin prescription####
Consort1<-data.frame("Count" = length(HospAll$patid), "Description" = c("Original"))

Trial1<-subset(HospAll, !is.na(HospAll$FirstSimDate)| !is.na(HospAll$FirstAtorDate)| !is.na(HospAll$FirstPravDate)| !is.na(HospAll$FirstRosuDate))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Not prescribed statin of interest"))
Consort1<- rbind(Consort1,Consort1a)

#Set which group they are and start date
Trial1$FirstStatOfInterestDate<-pmin(Trial1$FirstSimDate, Trial1$FirstAtorDate, Trial1$FirstPravDate, Trial1$FirstRosuDate, na.rm = TRUE)
Trial1$Sim<-0
Trial1$Sim[Trial1$FirstStatOfInterestDate==Trial1$FirstSimDate]<-1
Trial1$Ator<-0
Trial1$Ator[Trial1$FirstStatOfInterestDate==Trial1$FirstAtorDate]<-1
Trial1$Prav<-0
Trial1$Prav[Trial1$FirstStatOfInterestDate==Trial1$FirstPravDate]<-1
Trial1$Rosu<-0
Trial1$Rosu[Trial1$FirstStatOfInterestDate==Trial1$FirstRosuDate]<-1

####Statin before 31/12/2018####
Trial1<-subset(Trial1, FirstStatOfInterestDate<=as.Date("2018-12-31"))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("After 2018"))
Consort1<- rbind(Consort1,Consort1a)

####Statin after exit####
Trial1<-subset(Trial1, FirstStatOfInterestDate<=end)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("After end"))
Consort1<- rbind(Consort1,Consort1a)

#First statin before entry to cohort
Trial1<-subset(Trial1, FirstStatOfInterestDate>=enter)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("First statin before entry to cohort"))
Consort1<- rbind(Consort1,Consort1a)

#Drop any that have simvastatin and another statin
table(Trial1$Sim, Trial1$Ator)
table(Trial1$Sim, Trial1$Prav)
table(Trial1$Sim, Trial1$Rosu)

Trial1<-subset(Trial1, !(Trial1$Sim==1 & Trial1$Ator==1) &!(Trial1$Sim==1 & Trial1$Prav==1)& !(Trial1$Sim==1 & Trial1$Rosu==1))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Multiple first statin and in both arms"))
Consort1<- rbind(Consort1,Consort1a)

#Prescribed another statin before statin of interest
Trial1<-subset(Trial1, FirstStatOfInterestDate<=FirstStatDate)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Prescribed statin before statin of interest"))
Consort1<- rbind(Consort1,Consort1a)

#Drop those with evidence of statin use before first presc.
Trial1<-subset(Trial1, FirstStatOfInterestDate<=FirstEvidenceStatinDate | is.na(FirstEvidenceStatinDate))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Evidence of statin prior to prescription"))
Consort1<- rbind(Consort1,Consort1a)

#SMI prior to entry
Trial1<-subset(Trial1, FirstStatOfInterestDate>=diagnosis_date)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("No evidence of SMI before entry"))
Consort1<- rbind(Consort1,Consort1a)

#Age 18
Trial1<-subset(Trial1, year(FirstStatOfInterestDate)-yob>=18)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Under 18 at statin prescription"))
Consort1<- rbind(Consort1,Consort1a)

#Less than 6 months baseline
Trial1<-subset(Trial1, Trial1$FirstStatOfInterestDate - Trial1$regstartdate>182.625)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Less than 6 months baseline"))
Consort1<- rbind(Consort1,Consort1a)

#Pull in APs
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
PatAPAll<-subset(PatAPAll, patid %in% Trial1$patid)
StatDate<-select(Trial1, patid, FirstStatOfInterestDate)
PatAPAll<-merge(x=PatAPAll, y=StatDate, by="patid", all.x=FALSE, all.y=FALSE)

#Received injectables in the 3 months before statin
Inj<-subset(PatAPAll, Route=="Injection") # 
Inj<-subset(Inj, FirstStatOfInterestDate-eventdate<=91.3125 & FirstStatOfInterestDate-eventdate>=0)#Have injection in 3 months before statin

Trial1<-subset(Trial1, !(patid %in% Inj$patid))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Injectables in the 3 months before statin use"))
Consort1<- rbind(Consort1,Consort1a)

#No evidence of psychotropic medication prior to statin use
Trial1<-subset(Trial1, FirstStatOfInterestDate>FirstPsychDate)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("No evidence of antipsychotic use prior to statin use"))
Consort1<- rbind(Consort1,Consort1a)

#Didnt receive oral psychotropic in the 6 months prior to statin therapy
Pat6mo<-subset(PatAPAll, FirstStatOfInterestDate-eventdate<=182.625 & FirstStatOfInterestDate-eventdate>=0) #Have a prescription in 6 months before statin
Oral<-subset(Pat6mo, Route=="Oral")

Trial1<-subset(Trial1, (patid %in% Oral$patid))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("No psychotropics in the 6 months before statin"))
Consort1<- rbind(Consort1,Consort1a)

#At least one in those six months at therapeutic dose
load("VariableExtracts/CPRD2023/MergedObs/BP_dose.Rdata")
load("VariableExtracts/CPRD2023/MergedObs/AP_dose.Rdata")

BPDose<-select(BPDoseFinal, patid, eventdate, AP)
APDose<-select(apDoseFinal, patid, eventdate, AP)
AllDose<-rbind(BPDose, APDose)
AllDose<-distinct(AllDose)
AllDose<-merge(x=AllDose, y=StatDate, by="patid", all.x=FALSE, all.y=FALSE)

AllDose6mo<-subset(AllDose, FirstStatOfInterestDate-eventdate<=182.625 & FirstStatOfInterestDate-eventdate>=0) #Have a prescription in 6 months before statin

Trial1<-subset(Trial1, patid %in% AllDose6mo$patid)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Not at therapeutic dose"))
Consort1<- rbind(Consort1,Consort1a)

Trial1Codes<-select(Trial1, patid, FirstStatOfInterestDate, Sim, Ator, Prav, Rosu)
Trial1Codes$Trial1Statin<-"Sim"
Trial1Codes$Trial1Statin[Trial1Codes$Ator==1]<-"Ator"
Trial1Codes$Trial1Statin[Trial1Codes$Prav==1]<-"Prav"
Trial1Codes$Trial1Statin[Trial1Codes$Rosu==1]<-"Rosu"
Trial1Codes$Trial1Statin[Trial1Codes$Ator==1 & Trial1Codes$Prav==1]<-"AtorPrav"

Trial1Codes<-select(Trial1Codes, patid, Trial1StatDate=FirstStatOfInterestDate, Trial1Statin)

HospAll<-merge(x=HospAll, y=Trial1Codes, by="patid", all.x=TRUE, all.y=FALSE)

Consort1<-Consort1 %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort1<-select(Consort1, -Lag)
save(Consort1, file = "StatinCPRD/Outputs/FlowChartTrial1.Rdata")
write.csv(Consort1, file = "StatinCPRD/Outputs/FlowChartTrial1.csv")

table(HospAll$Trial1Statin)

####Trial 2####
#Remove rosuvastatin
Trial2<-subset(Trial1, Sim==1 | Ator==1 | Prav==1)

Consort2<- data.frame("Count" =length(Trial1$patid), "Description" = c("Original"))
Consort2a<- data.frame("Count" =length(Trial2$patid), "Description" = c("Rosuvastatin"))
Consort2<- rbind(Consort2,Consort2a)

#Limit to aripiprazole, risperidone, olanzapine
OralTrial2<-AllDose6mo%>%
  group_by(patid)%>%
  mutate(Max=max(eventdate))%>%
  subset(Max==eventdate)%>%
  subset(AP=="olanzapine" | AP=="risperidone" | AP=="aripiprazole")

Trial2<-subset(Trial2, patid %in% OralTrial2$patid )

Consort2a<- data.frame("Count" =length(Trial2$patid), "Description" = c("Not olanzapine, risperidone or aripiprazole"))
Consort2<- rbind(Consort2,Consort2a)

#Remove high dose statins
load("StatinCPRD/Data/ValidStatin.Rdata")

HighDose<-subset(Valid, (patid %in% Trial2$patid & StatDose=="High" ))
Date<-select(Trial2, patid, FirstStatOfInterestDate)
HighDose<-merge(x=HighDose, y=Date, by="patid", all.x=TRUE, all.y=FALSE)
HighDose<-subset(HighDose, issuedate==FirstStatOfInterestDate)

Trial2<-subset(Trial2, !(patid %in% HighDose$patid))

Consort2a<- data.frame("Count" =length(Trial2$patid), "Description" = c("High dose"))
Consort2<- rbind(Consort2,Consort2a)

Consort2<-Consort2 %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort2<-select(Consort2, -Lag)
save(Consort2, file = "StatinCPRD/Outputs/FlowChartTrial2.Rdata")
write.csv(Consort2, file = "StatinCPRD/Outputs/FlowChartTrial2.csv")

Trial2Codes<-select(Trial2, patid, FirstStatOfInterestDate, Sim, Ator, Prav, Rosu)
Trial2Codes$Trial2Statin<-"Sim"
Trial2Codes$Trial2Statin[Trial2Codes$Ator==1]<-"Ator"
Trial2Codes$Trial2Statin[Trial2Codes$Prav==1]<-"Prav"
Trial2Codes$Trial2Statin[Trial2Codes$Rosu==1]<-"Rosu"

Trial2Codes<-select(Trial2Codes, patid, Trial2StatDate=FirstStatOfInterestDate, Trial2Statin)

HospAll<-merge(x=HospAll, y=Trial2Codes, by="patid", all.x=TRUE, all.y=FALSE)

table(HospAll$Trial2Statin)

####Trial 3####
Consort3<-data.frame("Count" = length(HospAll$patid), "Description" = c("Original"))
rm(AllDose, AllDose6mo, APDose, BPDoseFinal, Oral, OralTrial2, PatAPAll)

#Aripiprazole, olanzapine, risperidone, quetiapine
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
APTrial3All<-subset(PatAPAll, AP=="Aripiprazole"|AP=="Olanzapine"| AP=="Risperidone"|AP=="Quetiapine")
APTrial3All<-subset(APTrial3All, patid %in% HospAll$patid)

APTrial3<-APTrial3All%>%
  group_by(patid)%>%
  mutate(FirstAPTrial3=min(eventdate, na.rm=FALSE))%>%
  subset(eventdate==FirstAPTrial3)

length(unique(APTrial3$patid))

Trial3<-subset(HospAll, patid %in% APTrial3$patid)

Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("Not AP of interest"))
Consort3<- rbind(Consort3,Consort3a)

APTrial3<-APTrial3%>%
  group_by(patid)%>%
  select(patid, AP, FirstAPTrial3)%>%
  distinct()%>%
  pivot_wider(names_from = "AP", values_from = "AP")

APTrial3$AP<-paste0(APTrial3$Olanzapine,APTrial3$Risperidone,APTrial3$Aripiprazole,APTrial3$Quetiapine)
APTrial3$AP<-str_replace(APTrial3$AP, "NA", "")
APTrial3$AP<-str_replace(APTrial3$AP, "NA", "")
APTrial3$AP<-str_replace(APTrial3$AP, "NA", "")
table(APTrial3$AP)

####AP before 31/12/2018####
APTrial3Code<-select(APTrial3, patid, APTrial3=AP, FirstAPTrial3Date=FirstAPTrial3)
Trial3<-merge(x=Trial3, y=APTrial3Code, by="patid", all.x=TRUE, all.y=FALSE)
Trial3<-subset(Trial3, FirstAPTrial3Date<=as.Date("2018-12-31"))
Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("After 2018"))
Consort3<- rbind(Consort3,Consort3a)

#First AP before entry to cohort
Trial3<-subset(Trial3, FirstAPTrial3Date>=enter)
Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("First AP of interest before entry to cohort"))
Consort3<- rbind(Consort3,Consort3a)

#AP in both arms##
APTrial3a<-subset(APTrial3, AP!="OlanzapineQuetiapine" & AP!="AripiprazoleQuetiapine" & AP!="RisperidoneQuetiapine")

Trial3<-subset(Trial3, patid %in% APTrial3a$patid)

Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("AP from both arms"))
Consort3<- rbind(Consort3,Consort3a)

#Less than 6 months baseline
Trial3<-subset(Trial3, Trial3$FirstAPTrial3Date - Trial3$regstartdate>182.625)
Consort3a<-data.frame("Count" =length(Trial3$patid), "Description" = c("Less than 6 months baseline"))
Consort3<- rbind(Consort3,Consort3a)

#Received injectables in the 3 months before oral AP
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
PatAPAll<-subset(PatAPAll, patid %in% Trial3$patid)
APDate<-select(Trial3, patid, FirstAPTrial3Date)
PatAPAll<-merge(x=PatAPAll, y=APDate, by="patid", all.x=FALSE, all.y=FALSE)
PatAPAll<-subset(PatAPAll, Route=="Injection") # 
Inj<-subset(PatAPAll, FirstAPTrial3Date-eventdate<=91.3125& FirstAPTrial3Date-eventdate>=0)#Have injection in 3 months before statin

Trial3<-subset(Trial3, !(patid %in% Inj$patid))
Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("Injectable AP in the three months before AP"))
Consort3<- rbind(Consort3,Consort3a)

#Age 18
Trial3<-subset(Trial3, year(FirstAPTrial3Date)-yob>=18)
Consort3a<-data.frame("Count" =length(Trial3$patid), "Description" = c("Under 18 at AP prescription"))
Consort3<- rbind(Consort3,Consort3a)

#Prescribed statins in the six months before
load("VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")
AllStat<-subset(AllStat, patid %in% Trial3$patid)
AllStat<-merge(x=AllStat, y=APDate, by="patid", all.x=FALSE, all.y=FALSE)
Pat6moStat<-subset(AllStat, FirstAPTrial3Date-issuedate<=182.625 & FirstAPTrial3Date-issuedate>=0) #Have a prescription in 6 months before statin

Trial3<-subset(Trial3, (patid %in% Pat6moStat$patid))
Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("No statin in the six months before"))
Consort3<- rbind(Consort3,Consort3a)

#Prescribed Simvastatin or atorvastatin as last statin
Pat6moStat<-Pat6moStat%>%
  group_by(patid)%>%
  mutate(max=max(issuedate))%>%
  subset(max==issuedate)%>%
  subset(Statin=="Atorvastatin"|Statin=="Simvastatin")

Trial3<-subset(Trial3, (patid %in% Pat6moStat$patid))

Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("Most recent not simvastatin or atorvastatin"))
Consort3<- rbind(Consort3,Consort3a)

####Not more than one statin####

Pat6moStat<-Pat6moStat%>%
  group_by(patid)%>%
  mutate(max=max(issuedate))%>%
  subset(max==issuedate)%>%
  select(patid, Trial3Statin=Statin)%>%
  distinct()%>%
  mutate(n=n())%>%
  subset(n==1)

Trial3<-subset(Trial3, patid %in% Pat6moStat$patid)
Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("Not multiple statins"))

####No injections in the one month after initiation####
Inj2<-subset(PatAPAll, FirstAPTrial3Date-eventdate<0 & FirstAPTrial3Date-eventdate>=-30.4375)#Have injection in 1 month after statin

Trial3<-subset(Trial3, !(patid %in% Inj2$patid))
Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("Injectable AP in the months after AP"))
Consort3<- rbind(Consort3,Consort3a)

####Dont drop out in the first month####
Trial3<-subset(Trial3, end-FirstAPTrial3Date>=30.4375)#Have injection in 1 month after statin

Consort3a<- data.frame("Count" =length(Trial3$patid), "Description" = c("LTFU in first month"))
Consort3<- rbind(Consort3,Consort3a)

Consort3<-Consort3 %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort3<-select(Consort3, -Lag)

save(Consort3, file = "StatinCPRD/Outputs/FlowChartTrial3.Rdata")
write.csv(Consort3, file = "StatinCPRD/Outputs/FlowChartTrial3.csv")

Trial3Codes<-select(Trial3, patid, FirstAPTrial3Date, APTrial3)
Trial3Codes<-merge(x=Trial3Codes, y=Pat6moStat, by="patid", all.x=TRUE, all.y=FALSE)


HospAll<-merge(x=HospAll, y=Trial3Codes, by="patid", all.x=TRUE, all.y=FALSE)

table(HospAll$APTrial3)
table(HospAll$Trial3Statin)

save(HospAll, file = "StatinCPRD/Data/StudyPops.rdata")





