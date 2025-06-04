
rm(list = ls(all.names = TRUE))

library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)
library(lubridate)

load("StatinCPRD/Data/Trial_AD.rdata")

load("VariableExtracts/CPRD2023/MergedObs/FinalLipids.Rdata")

####If total cholesterol is >=5 or non-HDL >=4 or TC:HDL>6 or ldl>=3 or HDL<1.2 (F) or HDL <1 (M)####

HighChol<-subset(FinalLipids, tchol>=5 | nonhdl>=4| tchdlratio>6|ldl>=3)
PossHDL<-subset(FinalLipids, hdl<1.2)

table(Trial$gender)
Trial$gender[Trial$gender==2]<-"Female"
Trial$gender[Trial$gender==1]<-"Male"

Gender<-select(Trial, patid, gender)

PossHDL<-merge(x=PossHDL, y=Gender, by="patid", all.x=TRUE, all.y=FALSE)
PossHDL<-subset(PossHDL, gender=="female" | (gender=="male" & hdl<1))

HighChol<-rbind(HighChol, PossHDL)

Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date, yob)
HighChol<-merge(x=HighChol, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

HighChol<-HighChol%>%
  subset(yob<year(eventdate))%>% 
  group_by(patid)%>% 
  mutate(Min=min(eventdate), na.rm=TRUE)%>%
  subset(Min==eventdate)

####Code for dislipidaemia####
load("VariableExtracts/CPRD2023/Gold/LipidGoldClinical.Rdata")
tqu <- read.delim("LookUps/202303_Lookups_CPRDGold/TXTFILES/TQU.txt")
entity <- read.delim("LookUps/202303_Lookups_CPRDGold/entity.txt")
Ent<-select(entity, enttype, description)

PatClin<-merge(x=PatClin, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)
EntCheck<-subset(entity, enttype %in% PatClin$enttype)

Additional<-read.table("GOLD/Additional/SMI_GOLD_Extract_Additional_001.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))

#Only select those with values
Additional$patad<-paste0(Additional$patid, "-G-", Additional$adid)
Additional<-select(Additional, -patid, -adid)
PatClin$patad<-paste0(PatClin$patid, "-", PatClin$adid)
Check<-subset(Additional, patad %in% PatClin$patad) # No additional details in the clinical table
Check<-merge(x=Check, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)
table(Check$description) #None are useful

PatClin<-merge(x=PatClin, y=Additional, by="patad", all.x=TRUE, all.y=FALSE)
#Look at categories
TQUVals<-subset(PatClin, data1>0)
TQUVals<-merge(x=TQUVals, y=tqu, by.x="data1", by.y="Code", all.x=TRUE, all.y=FALSE)

TQUVals2<-subset(PatClin, enttype.x!=214 & enttype.x!=363)
TQUVals2<-subset(TQUVals2, data4>0)
TQUVals2<-merge(x=TQUVals2, y=tqu, by.x="data4", by.y="Code", all.x=TRUE, all.y=FALSE)
TQUVals<-rbind(TQUVals, TQUVals2)

table(TQUVals$desc)   
TQUVals<-rename(TQUVals, Term = desc)

table(TQUVals$Test.Qualifier)

#No useful info

####Gold categories part 2####
load("VariableExtracts/CPRD2023/Gold/LipidGoldClinical.Rdata")
load("VariableExtracts/CPRD2023/Gold/LipidGoldTest.Rdata")


PatClin<-select(PatClin, patid, medcode, Term=desc, eventdate)
PatTest<-select(PatTest, patid, medcode,Term=desc, eventdate)

CatGold<-rbind(PatClin, PatTest)

NaomiCatGold<-select(CatGold, -patid, -eventdate)
NaomiCatGold<-distinct(NaomiCatGold)

NaomiCatGold<-NaomiCatGold%>%
  mutate(Qual = case_when((grepl("borderline", Term)) ~ "Borderline",
                          (grepl("dyslip", Term)| (grepl("hyper", Term) & !grepl("screen", Term)) | grepl("therapy", Term) | (grepl("aemia", Term) & !grepl("screen", Term)) | grepl("disord", Term))  ~ "Diagnosis",
                          (grepl("raised", Term) | grepl("high", Term) | grepl("indicated", Term) |
                             grepl("reduction", Term) | grepl("lowering", Term)| grepl("diet", Term)) & !grepl("non high", Term) 
                          & !grepl("not indicated", Term) & !grepl("contra", Term) & !grepl("no longer", Term) & !grepl("declined", Term)& !grepl("studies", Term) & medcode!="102004" 
                          & medcode!= "7574"~ "High",
                          medcode=="160850"|medcode=="10899" ~"High",
                          (grepl("abnormal", Term) | grepl("management", Term)) & !(grepl("stopped", Term)) ~ "Abnormal",
                          (grepl("normal", Term) & !grepl("abnormal", Term)) ~ "Normal",
                          TRUE ~ "Drop"))

CheckDrop<-subset(NaomiCatGold, Qual=="Drop") 
CheckDiag<-subset(NaomiCatGold, Qual=="Diagnosis") 
table(NaomiCatGold$Term[NaomiCatGold$Qual=="High"])
table(NaomiCatGold$Term[NaomiCatGold$Qual=="Abnormal"]) 
table(NaomiCatGold$Term[NaomiCatGold$Qual=="Low"]) 
table(NaomiCatGold$Term[NaomiCatGold$Qual=="Normal"]) 
table(NaomiCatGold$Term[NaomiCatGold$Qual=="Borderline"]) 

NaomiCatGold<-subset(NaomiCatGold, Qual=="Diagnosis"|Qual=="High")

CatGold<-subset(CatGold, medcode %in% NaomiCatGold$medcode)

#Need to look at Aurum categories and codes that are diagnoses
load("VariableExtracts/CPRD2023/Aurum/LipidAurum.Rdata")

NaomiCatAurum<-select(PatLipidAurum, medcode, Term=desc)
NaomiCatAurum<-distinct(NaomiCatAurum)

NaomiCatAurum<-NaomiCatAurum%>%
  mutate(Qual = case_when((grepl("borderline", Term)) ~ "Borderline",
                          (grepl("familial", Term) | grepl("dyslip", Term)| (grepl("hyper", Term)) & !grepl("screen", Term) | (grepl("therapy", Term) & !grepl("not indicated", Term))| (grepl("aemia", Term) & !grepl("screen", Term)) | grepl("disord", Term))  ~ "Diagnosis",
                          
                          (grepl("raised", Term)|grepl("high", Term) | grepl("indicated", Term) |
                             grepl("reduction", Term) | grepl("lowering", Term)| grepl("diet", Term)) & (!grepl("non high", Term) & !grepl("concentration", Term) &
                          !grepl("contra", Term) & !grepl("density", Term) & !grepl("no longer", Term) & !grepl("declined", Term)& !grepl("studies", Term) & medcode!="219121000000113" 
                          & medcode!= "458370015" & !grepl("not indicated", Term)) ~ "High",
                          
                          (grepl("abnormal", Term) | grepl("disord", Term)| 
                             grepl("management", Term)) ~ "Abnormal",
                          
                          (grepl("normal", Term) & !grepl("abnormal", Term)) ~ "Normal",
                          
                          Term=="cholesterol reduction program - declined" ~ "Diagnosis",
                          
                          TRUE ~ "Drop"))

CheckDrop<-subset(NaomiCatAurum, Qual=="Drop") 
CheckDiag<-subset(NaomiCatAurum, Qual=="Diagnosis") 
table(NaomiCatAurum$Term[NaomiCatAurum$Qual=="High"])
table(NaomiCatAurum$Term[NaomiCatAurum$Qual=="Abnormal"]) 
table(NaomiCatAurum$Term[NaomiCatAurum$Qual=="Low"]) 
table(NaomiCatAurum$Term[NaomiCatAurum$Qual=="Normal"]) 
table(NaomiCatAurum$Term[NaomiCatAurum$Qual=="Borderline"])

NaomiCatAurum<-subset(NaomiCatAurum, Qual=="Diagnosis"|Qual=="High")

CatAurum<-subset(PatLipidAurum, medcode %in% NaomiCatAurum$medcode)
CatAurum<-select(CatAurum, patid, medcode, Term=desc, eventdate)
Cat<-rbind(CatGold, CatAurum)

####Find earliest####
Cat<-merge(x=Cat, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

Cat<-Cat%>%
  subset(yob<year(eventdate))%>% 
  group_by(patid)%>% 
  mutate(Min=min(eventdate), na.rm=TRUE)%>%
  subset(Min==eventdate)

####Combine high chol and code suggesting it####
HighChol<-select(HighChol, patid, eventdate, Trial1StatDate, FirstAPTrial3Date)
Cat<-select(Cat, patid, eventdate, Trial1StatDate, FirstAPTrial3Date)

AnyChol<-rbind(HighChol, Cat)


AnyChol<-AnyChol%>%
  group_by(patid)%>%
  mutate(Min=min(eventdate), na.rm=TRUE)%>%
  subset(Min==eventdate)%>%
  distinct()
length(unique(AnyChol$patid))

AnyChol<-select(AnyChol, patid, DislipDate=eventdate)
AnyChol<-distinct(AnyChol)

Trial<-merge(x=Trial, y=AnyChol, by="patid", all.x=TRUE, all.y=FALSE)

#Create variables for first diagnosis at statin or at AP
Trial<-Trial%>%
  mutate(DyslipStat = case_when(DislipDate<=Trial1StatDate ~ 1,
                              TRUE ~ 0),
         DyslipAP = case_when(DislipDate<=FirstAPTrial3Date ~ 1,
                           TRUE ~ 0))


#Time since displidaemia (before index)
Trial$DislipTimeStat<-as.numeric(Trial$Trial1StatDate - Trial$DislipDate)
Trial$DislipTimeStat[Trial$DyslipStat==0]<-NA
summary(Trial$DislipTimeStat)

length(which(is.na(Trial$DislipTimeStat)& !is.na(Trial$Trial1StatDate)))

Trial$DislipTimeAP<-as.numeric(Trial$FirstAPTrial3Date - Trial$DislipDate)
Trial$DislipTimeAP[Trial$DyslipAP==0]<-NA

summary(Trial$DislipTimeAP)

length(which(is.na(Trial$DislipTimeAP)))
length(which(Trial$DyslipAP==1 &is.na(Trial$DislipTimeAP)))

save(Trial, file = "StatinCPRD/Data/Trial_DisLip.rdata")
  

