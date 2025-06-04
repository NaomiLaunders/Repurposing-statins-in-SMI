rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)

####Load historical BMI lists####

AurumBMI<-read.table("Codelists/BMI screen/BMIAurumFinal.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses=c(MedCodeId="character", SnomedCTConceptId = "character"))
GoldBMI<-read.table("Codelists/BMI screen/BMIGoldFinal.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses=c(medcode="character"))

#load lookups

AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Check for new BMI codes Aurum####
NewBMIAurum<-subset(AurumMed, !(MedCodeId %in% AurumBMI$MedCodeId))

NewBMIAurum<-NewBMIAurum%>%
  mutate(BMI = case_when(grepl("reference|disorder|nonobese|framework|baby|target|child|submitted|rfc|except|sample|weights|7-|7p|velocity|footwear|property|submit|ten year|sweat|calculus|molecular|diffusion", Term, ignore.case=TRUE) ~0,
                         grepl("transfer|lift|anaerobes|airflow|weightless|FH|lobes|sepsis|fetal|bear|massage|septic|risk|birth|premature|placenta|waterlow|infant|eyelid|circum|mri|weightlift", Term, ignore.case=TRUE) ~0,
                         grepl("BMI|body mass|overwei|obes|underwei|weight|body fat|adiposity", Term, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  subset(BMI==1)

AurumBMI<-select(AurumBMI, -QOF, -type, -BMICat)
NewBMIAurum<-select(NewBMIAurum, -Observations, -EmisCodeCategoryId, -BMI)
AurumBMI<-rbind(AurumBMI, NewBMIAurum)

AurumBMI<-AurumBMI%>%
  mutate(BMICat=case_when(grepl("maternal|underestimates|screen|overestimates", Term, ignore.case=TRUE)~ "None",
    grepl("obes|40", Term, ignore.case=TRUE)~ "Obese",
                          grepl("loss advised|advised to lose|over|adipos|high|counterweight|reduc|heavy", Term, ignore.case=TRUE)~ "Overweight",
                          grepl("protein-calorie|dietary education for weight gain|under|less|below|body mass index low", Term, ignore.case=TRUE)~ "Underweight",
                          grepl("^normal| normal|18.5\\-", Term, ignore.case=TRUE)~ "Normal",
                          TRUE ~ "None"))
Check<-subset(AurumBMI, BMICat=="None")   

####Check for new BMI codes Gold####
NewBMIGold<-subset(GoldMed, !(medcode %in% GoldBMI$medcode))

NewBMIGold<-NewBMIGold%>%
  mutate(BMI = case_when(grepl("disord|FH|lobes|sepsis|massage|sample|septic|baby|child|submitted|except|frax|target|sweat|calculus|molecular|transfer|weightless|fetal|bear|birth|premature|placenta|infant|eyelid", desc, ignore.case=TRUE) ~0,
           grepl("BMI|body mass|overwei|obes|underwei|weight|body fat|adiposity", desc, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(BMI==1)

GoldBMI<-select(GoldBMI, medcode, readcode, desc=readterm)
NewBMIGold<-select(NewBMIGold, -BMI)
GoldBMI<-rbind(GoldBMI, NewBMIGold)

GoldBMI<-GoldBMI%>%
  mutate(BMICat=case_when(grepl("maternal|underestimates|screen|overestimates", desc, ignore.case=TRUE)~ "None",
    grepl("obes|risk|40", desc, ignore.case=TRUE)~ "Obese",
                          grepl("loss advised|advised to lose|over|adipos|high|counterweight|reduc|heavy", desc, ignore.case=TRUE)~ "Overweight",
                          grepl("protein-calorie|dietary education for weight gain|under|less|below|body mass index low", desc, ignore.case=TRUE)~ "Underweight",
                          grepl("^normal| normal|18.5\\-", desc, ignore.case=TRUE)~ "Normal",
                          TRUE ~ "None"))
Check<-subset(GoldBMI, BMICat=="None")  

####Compare Gold and Aurum####
AurumBMI$Term<-tolower(AurumBMI$Term)
AurumBMI$Term<-gsub('"', '', AurumBMI$Term)
GoldBMI$desc<-tolower(GoldBMI$desc)

GoldBMICheck<-subset(GoldBMI, !(desc %in% AurumBMI$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldBMICheck$desc) #None to add

AurumBMICheck<-subset(AurumBMI, !(Term %in% GoldBMI$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumBMICheck$Term) #None to add on term

GoldBMICheck<-subset(GoldBMI, !(readcode %in% AurumBMI$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldBMICheck$readcode) #None to add

AurumBMICheck<-subset(AurumBMI, !(CleansedReadCode %in% GoldBMI$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumBMICheck$CleansedReadCode) #4 to add on readcode

AddtoGoldR$BMICat[AddtoGoldR$medcode=="11458"]<-"Overweight"
AddtoGoldR$BMICat[AddtoGoldR$medcode=="21744"]<-"Obese"
AddtoGoldR$BMICat[AddtoGoldR$medcode=="57410"]<-"Underweight"
AddtoGoldR$BMICat[AddtoGoldR$medcode=="98680"]<-"Underweight"

GoldBMI<-rbind(GoldBMI, AddtoGoldR)

GoldBMICheck<-subset(GoldBMI, !(desc %in% AurumBMI$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldBMICheck$desc) #None to add

AddtoAurumT<-select(AddtoAurumT, MedCodeId, Term, OriginalReadCode, CleansedReadCode, SnomedCTConceptId, SnomedCTDescriptionId, Release)
AddtoAurumT$BMICat<-"Overweight"

AurumBMI<-rbind(AurumBMI, AddtoAurumT)

#Check by category
#Obese
ObeseA<-subset(AurumBMI, BMICat=="Obese")
Obeseg<-subset(GoldBMI, BMICat=="Obese")

GoldBMICheck<-subset(Obeseg, !(desc %in% ObeseA$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldBMICheck$desc) #None to add

AurumBMICheck<-subset(ObeseA, !(Term %in% Obeseg$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumBMICheck$Term) #None to add on term

GoldBMICheck<-subset(Obeseg, !(readcode %in% ObeseA$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldBMICheck$readcode) #None to add

AurumBMICheck<-subset(ObeseA, !(CleansedReadCode %in% Obeseg$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumBMICheck$CleansedReadCode) #4 to add on readcode

#Overweight
ObeseA<-subset(AurumBMI, BMICat=="Overweight")
Obeseg<-subset(GoldBMI, BMICat=="Overweight")

GoldBMICheck<-subset(Obeseg, !(desc %in% ObeseA$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldBMICheck$desc) #None to add

AurumBMICheck<-subset(ObeseA, !(Term %in% Obeseg$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumBMICheck$Term) #None to add on term

GoldBMICheck<-subset(Obeseg, !(readcode %in% ObeseA$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldBMICheck$readcode) #None to add

AurumBMICheck<-subset(ObeseA, !(CleansedReadCode %in% Obeseg$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumBMICheck$CleansedReadCode) #4 to add on readcode

#Normal
ObeseA<-subset(AurumBMI, BMICat=="Normal")
Obeseg<-subset(GoldBMI, BMICat=="Normal")

GoldBMICheck<-subset(Obeseg, !(desc %in% ObeseA$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldBMICheck$desc) #None to add

AurumBMICheck<-subset(ObeseA, !(Term %in% Obeseg$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumBMICheck$Term) #None to add on term

GoldBMICheck<-subset(Obeseg, !(readcode %in% ObeseA$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldBMICheck$readcode) #None to add

AurumBMICheck<-subset(ObeseA, !(CleansedReadCode %in% Obeseg$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumBMICheck$CleansedReadCode) #4 to add on readcode

#underweight
ObeseA<-subset(AurumBMI, BMICat=="Underweight")
Obeseg<-subset(GoldBMI, BMICat=="Underweight")

GoldBMICheck<-subset(Obeseg, !(desc %in% ObeseA$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldBMICheck$desc) #None to add

AurumBMICheck<-subset(ObeseA, !(Term %in% Obeseg$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumBMICheck$Term) #None to add on term

GoldBMICheck<-subset(Obeseg, !(readcode %in% ObeseA$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldBMICheck$readcode) #None to add

AurumBMICheck<-subset(ObeseA, !(CleansedReadCode %in% Obeseg$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumBMICheck$CleansedReadCode) #4 to add on readcode
####Save final lists####
write.table(AurumBMI, file = "Codelists/BMI22/BMIAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(GoldBMI, file = "Codelists/BMI22/BMIGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")


####Check for new height codes in Aurum (not needed for Gold)####
AurumHeight<-subset(AurumMed, MedCodeId=="1910921000006114" |MedCodeId=="923831000006115"|MedCodeId=="253669010"|MedCodeId=="1910931000006112")
NewHeightAurum<-subset(AurumMed, !(MedCodeId %in% AurumHeight$MedCodeId))

NewHeightAurum<-NewHeightAurum%>%
  mutate(Height = case_when(grepl("weight for|relation|centile|ratio|child|fund|waterlow|sit|decline|fall|fear|uterus|furniture|unfit|footwear|heightened|jumping|knee|parental|predicted|step|unsuit", Term, ignore.case=TRUE) ~0,
    grepl("height|tall for age|tall stat", Term, ignore.case=TRUE) ~1,
                            TRUE ~ 0))%>%
  subset(Height==1)%>%
  select(-Height)

AurumHeight<-rbind(AurumHeight, NewHeightAurum)

####Save final lists####
write.table(AurumHeight, file = "Codelists/BMI22/HeightAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")



