#Clear environment
rm(list = ls(all.names = TRUE))

#~~~Libraries
library(tidyverse)
library(lubridate)

####Load product dictionaries####
AurumStat<-read.table("Codelists/Statin22/StatinAurum.txt", header=TRUE, sep="\t", dec = ".", colClasses = c(ProdCodeId="character"))
GoldStat<-read.table("Codelists/Statin22/StatinGold.txt", header=TRUE, sep="\t", dec = ".",colClasses = c(prodcode="character"))

####Create import functions####
#Import function
ReadObsA <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", colClasses=c(prodcodeid="character", patid="character"))
}

#Import function
ReadObsG <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", colClasses=c(prodcode="character", patid="character"))
}

####GOLD product####

#Generate file numbers

Observation_files <- list.files(path="/GOLD/Therapy", pattern = ("\\.txt$"))
Observation_files<-paste0("", Observation_files)

#~~~Select only observations that are statin based
PatObsGold<-data.frame()

for (i in (length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadObsG(FileName)
  PatObs<-select(PatObs, -bnfcode)
  PatObs<-merge(x=GoldStat, y=PatObs, by ="prodcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, prodcode,eventdate, sysdate, dosageid, bnfcode, qty, numdays, numpacks, packtype, issueseq, productname, drugsubstance, strength, bnfchapter, route, Statin)
  PatObsGold<-rbind(PatObsGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

save(PatObsGold, file="VariableExtracts/CPRD2023/MergedObs/StatinGold.Rdata")

####Aurum product#####
Observation_files <- list.files("Aurum/DrugIssue", pattern = "\\.txt$")
Observation_files<-paste0("", Observation_files)

#~~~Select only observations that are Statin based

PatObsAurum<-data.frame()

for (i in (length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadObsA(FileName)
  PatObs<-select(PatObs, -bnfcode)
  PatObs<-ReadObsA(FileName)
  PatObs<-merge(x=AurumStat, y=PatObs, by.x="ProdCodeId", by.y="prodcodeid", all.y = FALSE, all.x=FALSE)
  PatObsAurum<-rbind(PatObsAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

save(PatObsAurum, file="VariableExtracts/CPRD2023/MergedObs/StatinAurum.Rdata")

####Sort out look ups####
QuantG<-read.table("LookUps/202303_Lookups_CPRDGold/packtype.txt", header=T, sep="\t", fill=TRUE, dec = ".", quote="" )
QuantA<-read.table("LookUps/202205_Lookups_CPRDAurum/QuantUnit.txt", header=T, sep="\t", fill=TRUE, dec = ".", quote="")
DoseG<-read.table("LookUps/202303_Lookups_CPRDGold/common_dosages.txt", header=T, sep="\t", fill=TRUE, dec = ".", quote="")
DoseA<-read.table("LookUps/202205_Lookups_CPRDAurum/common_dosages.txt", header=T, sep="\t", fill=TRUE, dec = ".", quote="")

GoldStat<-merge(x=PatObsGold, y=QuantG, by="packtype", all.x=TRUE, all.y=FALSE)
rm(PatObsGold)
GoldStat<-merge(x=GoldStat, y=DoseG, by="dosageid", all.x=TRUE, all.y=FALSE)
save(GoldStat, file="VariableExtracts/CPRD2023/MergedObs/StatinGold.Rdata")

rm(QuantG, DoseG, GoldStat)

AurumStat<-merge(x=PatObsAurum, y=QuantA, by="quantunitid", all.x=TRUE, all.y=FALSE)
rm(PatObsAurum)
AurumStat<-merge(x=AurumStat, y=DoseA, by="dosageid", all.x=TRUE, all.y=FALSE)

rm(QuantA, DoseA)

####Look ups for Aurum####
AurumStat<-AurumStat%>%
  select(patid, issuedate, enterdate, quantity, packtype=Description, duration,  productname=ProductName, 
         ingredient=DrugSubstanceName, strength=SubstanceStrength, bnfchapter=BNFChapter, Statin, Route=RouteOfAdministration, Formulation, dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
         dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration, prodcode=ProdCodeId)

AurumStat$issuedate<-as.Date(AurumStat$issuedate,  format= "%d/%m/%Y")
AurumStat$enterdate<-as.Date(AurumStat$enterdate,  format= "%d/%m/%Y")
summary(AurumStat$issuedate)
AurumStat$issuedate[year(AurumStat$issuedate)<1901|year(AurumStat$issuedate)>2023]<-AurumStat$enterdate[year(AurumStat$issuedate)<1901|year(AurumStat$issuedate)>2023]

AurumStat$issuedate[AurumStat$issuedate<="1900/01/01"]<-NA
AurumStat$enterdate[AurumStat$sysdate<="1900/01/01"]<-NA

AurumStat$issuedate[is.na(AurumStat$issuedate)]<-AurumStat$enterdate[is.na(AurumStat$issuedate)]

save(AurumStat, file="VariableExtracts/CPRD2023/MergedObs/AurumStat.Rdata")

####Look ups for Gold###

load("VariableExtracts/CPRD2023/MergedObs/StatinGold.Rdata")

GoldStat<-GoldStat%>%
  select(patid, issuedate=eventdate, enterdate=sysdate, quantity=qty, packtype,  duration=numdays, productname,
         ingredient=drugsubstance, strength, bnfchapter, Statin, Route=route, numpacks, issueseq,     bnfchapter, packtype_desc, dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
         dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration, prodcode)

####Work out what is missing####
GoldStat$issuedate<-as.Date(GoldStat$issuedate,  format= "%d/%m/%Y")
GoldStat$enterdate<-as.Date(GoldStat$enterdate,  format= "%d/%m/%Y")
summary(GoldStat$issuedate)

#If issue date <1901 or >2019 take enterdate
GoldStat$issuedate[year(GoldStat$issuedate)<1901|year(GoldStat$issuedate)>2023]<-GoldStat$enterdate[year(GoldStat$issuedate)<1901|year(GoldStat$issuedate)>2023]

GoldStat$issuedate[GoldStat$issuedate<="1900/01/01"]<-NA
GoldStat$enterdate[GoldStat$sysdate<="1900/01/01"]<-NA

GoldStat$issuedate[is.na(GoldStat$issuedate)]<-GoldStat$enterdate[is.na(GoldStat$issuedate)]

save(GoldStat, file="VariableExtracts/CPRD2023/MergedObs/GoldStat.Rdata")

AurumStat<-select(AurumStat, -Formulation)
AurumStat$patid<-paste0(AurumStat$patid, "-A")
GoldStat<-select(GoldStat, -numpacks, -issueseq, -packtype_desc)
GoldStat$patid<-paste0(GoldStat$patid, "-G")

AllStat<-rbind(AurumStat, GoldStat)

save(AllStat, file="VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")

####Look for med codes####

GoldStatCode<-read.table("Codelists/Statin22/StatinGoldmed.txt", header=TRUE, sep="\t", dec = ".", colClasses = c(medcode="character"))
AurumStatCode<-read.table("Codelists/Statin22/StatinAurummed.txt", header=TRUE, sep="\t", dec = ".", colClasses = c(MedCodeId="character"))

####Create new import functions
ReadObsG <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", colClasses=c(medcode="character", patid="character"))
}

ReadObsA <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", colClasses=c(medcodeid="character", patid="character"))
}

####Gold med####
Observation_files <- list.files("GOLD/Clinical", pattern = "\\.txt$")
Observation_files<-paste0("", Observation_files)

Observation_files1 <- list.files("GOLD/Referral", pattern = "\\.txt$")
Observation_files1<-paste0("", Observation_files1)

Observation_files2 <- list.files("GOLD/Test", pattern = "\\.txt$")
Observation_files2<-paste0("", Observation_files2)

#Select those that are SMI 
PatSMIGold<-data.frame()

for (i in (length(Observation_files))) {
  FileName<-Observation_files[i]
  load(FileName)
  PatObs<-ReadObsG(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate)
  PatObs<-merge(x=GoldStatCode, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, medcode, desc, eventdate, sysdate)
  PatSMIGold<-rbind(PatSMIGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

for (i in (length(Observation_files1))) {
  FileName<-Observation_files1[i]
  load(FileName)
  PatObs<-ReadObsG(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate)
  PatObs<-merge(x=GoldStatCode, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, medcode, desc, eventdate, sysdate)
  PatSMIGold<-rbind(PatSMIGold, PatObs)
  print(Observation_files1[i])
}
rm(PatObs)

for (i in (length(Observation_files2))) {
  FileName<-Observation_files2[i]
  load(FileName)
  PatObs<-ReadObsG(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate)
  PatObs<-merge(x=GoldStatCode, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, medcode, desc, eventdate, sysdate)
  PatSMIGold<-rbind(PatSMIGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

PatSMIGold$eventdate<-as.Date(PatSMIGold$eventdate, format="%d/%m/%Y")
PatSMIGold$sysdate<-as.Date(PatSMIGold$sysdate, format="%d/%m/%Y")

length(which(is.na(PatSMIGold$eventdate)))
length(which(is.na(PatSMIGold$sysdate)))

length(which(PatSMIGold$eventdate<="1900/01/01"))
length(which(PatSMIGold$sysdate<="1900/01/01"))

PatSMIGold$eventdate[PatSMIGold$eventdate<="1900/01/01"]<-NA
PatSMIGold$sysdate[PatSMIGold$sysdate<="1900/01/01"]<-NA

PatSMIGold$eventdate[is.na(PatSMIGold$eventdate)]<-PatSMIGold$sysdate[is.na(PatSMIGold$eventdate)]

save(PatSMIGold, file="VariableExtracts/CPRD2023/MergedObs/StatMedGold.Rdata")

####Aurum medical####

Observation_files <- list.files("Aurum/Observation", pattern = "\\.txt$")
Observation_files<-paste0("", Observation_files)

#Select those that are SMI 
PatSMIAurum<-data.frame()


#Select those that are SMI 

for (i in (length(Observation_files2))) {
  FileName<-Observation_files2[i]
  load(FileName)
  PatObs<-ReadObsA(FileName)
  PatObs<-select(PatObs, patid, MedCodeId=medcodeid, obsdate, enterdate)
  PatObs<-merge(x=AurumStatCode, y=PatObs, by ="MedCodeId", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, medcodeid=MedCodeId, Term, eventdate=obsdate, sysdate=enterdate)
  PatSMIAurum<-rbind(PatSMIAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

PatSMIAurum<-rename(PatSMIAurum, medcode=medcodeid)

PatSMIAurum$eventdate<-as.Date(PatSMIAurum$eventdate, format="%d/%m/%Y")
PatSMIAurum$sysdate<-as.Date(PatSMIAurum$sysdate, format="%d/%m/%Y")

length(which(is.na(PatSMIAurum$eventdate)))
length(which(is.na(PatSMIAurum$sysdate)))

length(which(PatSMIAurum$eventdate<="1900/01/01"))
length(which(PatSMIAurum$sysdate<="1900/01/01"))

PatSMIAurum$eventdate[PatSMIAurum$eventdate<="1900/01/01"]<-NA
PatSMIAurum$sysdate[PatSMIAurum$sysdate<="1900/01/01"]<-NA

PatSMIAurum$eventdate[is.na(PatSMIAurum$eventdate)]<-PatSMIAurum$sysdate[is.na(PatSMIAurum$eventdate)]

save(PatSMIAurum, file="VariableExtracts/CPRD2023/MergedObs/StatMedAurum.Rdata")

####Merge to one file####
PatSMIAurum$patid<-paste0(PatSMIAurum$patid, "-A")
PatSMIGold$patid<-paste0(PatSMIGold$patid, "-G")

names(PatSMIAurum)
names(PatSMIGold)
PatSMIGold<-rename(PatSMIGold, Term=desc)

PatStatMed<-rbind(PatSMIAurum, PatSMIGold)

save(PatStatMed, file="VariableExtracts/CPRD2023/MergedObs/StatMedAll.Rdata")
