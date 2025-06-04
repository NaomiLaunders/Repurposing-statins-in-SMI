rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)

####Load older lipid lists####
LipidAurum<-read.table("CodeLists/Lipids/NaomiLipidsAurumFinalScreenAndValue.txt", header=TRUE, sep="\t", colClasses=c(MedCodeId="character"))
LipidGold<-read.table("CodeLists/Lipids/NaomiLipidsGoldFinalScreenAndValue.txt", header=TRUE, sep="\t", colClasses=c(medcode="character"))

#Look ups
AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Check for new lipid codes Aurum####
NewLipidAurum<-subset(AurumMed, !(MedCodeId %in% LipidAurum$MedCodeId))


NewLipidAurum<-NewLipidAurum%>%
  mutate(Lipid = case_when(grepl("cholesterolosis|retina|diet$|keratopathy|intake|lipid storage|lipase|fractionation|embolus|gall|mildly|prophylaxis|information|granuloma|renogram|cholesterin|lipidoses|lipidosis|cerebral|calculus|phenotype|apolipoprotein e profile|genotyp|antiphospholipid|antibody|blastoma|dehydr|time|eatoma|cholestea|target|adverse|risk|family|fh|swallow|receptor|therapy|scale|declined|cholesta|carcinoma|tumour", Term, ignore.case=TRUE) ~0,
    grepl("lipid|cholest|ldl|hdl|triglyc|lipoprot", Term, ignore.case=TRUE) ~1,
                          TRUE ~ 0))%>%
  subset(Lipid==1)

LipidAurum<-select(LipidAurum, MedCodeId, CleansedReadCode, Term)
NewLipidAurum<-select(NewLipidAurum, MedCodeId, CleansedReadCode, Term)
LipidAurum<-rbind(LipidAurum, NewLipidAurum)

####Check for new Lipid codes Gold####
NewLipidGold<-subset(GoldMed, !(medcode %in% LipidGold$medcode))

NewLipidGold<-NewLipidGold%>%
  mutate(Lipid = case_when(grepl("supplementation|correction|loudness|cholesterolosis|adv|retina|diet$|keratopathy|intake|lipase|fractionation|embolus|storage|gall|mildly|prophylaxis|information|granuloma|renogram|cholesterin|lipidoses|lipidosis|cerebral|calculus|phenotype|apolipoprotein e profile|genotyp|antiphospholipid|antibody|blastoma|dehydr|time|eatoma|cholestea|target|adverse|risk|family|fh|swallow|receptor|therapy|scale|declined|cholesta|carcinoma|tumour", desc, ignore.case=TRUE) ~0,
                           grepl("lipid|cholest|ldl|hdl|triglyc|lipoprot", desc, ignore.case=TRUE) ~1,
                           TRUE ~ 0))%>%
  subset(Lipid==1)

LipidGold<-select(LipidGold, medcode, readcode, desc=readterm)
NewLipidGold<-select(NewLipidGold, medcode, readcode, desc)
LipidGold<-rbind(LipidGold, NewLipidGold)

####Compare Gold and Aurum####
LipidAurum$Term<-tolower(LipidAurum$Term)
LipidAurum$Term<-gsub('"', '', LipidAurum$Term)
LipidGold$desc<-tolower(LipidGold$desc)

GoldLipidCheck<-subset(LipidGold, !(desc %in% LipidAurum$Term)& desc!="")
AddtoAurumT<-subset(AurumMed, Term %in% GoldLipidCheck$desc) #6 to add

AurumLipidCheck<-subset(LipidAurum, !(Term %in% LipidGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumLipidCheck$Term) #None to add on term

GoldLipidCheck<-subset(LipidGold, !(readcode %in% LipidAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldLipidCheck$readcode) #None to add

AurumLipidCheck<-subset(LipidAurum, !(CleansedReadCode %in% LipidGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumLipidCheck$CleansedReadCode) #3 to add on readcode
AddtoGoldR<-subset(AddtoGoldR, medcode!="16085")

LipidGold<-rbind(LipidGold, AddtoGoldR)

####Save final lists####
write.table(LipidAurum, file = "Codelists/Lipids22/LipidAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(LipidGold, file = "Codelists/Lipids22/LipidGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
