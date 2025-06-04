library(tidyverse)
library(lubridate)
library(tableone)

#Clear environment

rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/Trial_ethn.rdata")

####Aurum BMI values####
load("VariableExtracts/CPRD2023/Aurum/BMIAurum.Rdata")

PatBMIAurum$patid<-paste0(PatBMIAurum$patid, "-A")
BMIAurum<-subset(PatBMIAurum, patid %in% Trial$patid)

rm(PatBMIAurum)

#For Aurum, limit to those with value
BMIValueAurum<-BMIAurum%>%
  subset(!is.na(value))%>%
  select(patid, value, eventdate, desc, numunitid)

#Seperate out weight and BMI
Units<-read.table("LookUps/202205_Lookups_CPRDAurum/numunit.txt", header=TRUE, quote="", fill=TRUE, sep="\t")
BMIValueAurum<-merge(x=BMIValueAurum, y=Units, by="numunitid", all.x=TRUE, all.y=FALSE)

Term<-select(BMIValueAurum, desc, Description)
Term<-distinct(Term)
Term$Class<-case_when(Term$desc=="abnormal weight loss" ~ "Weight",
                      grepl("weight loss", Term$desc, ignore.case=TRUE) ~ "Drop",
                      Term$desc=="ideal body weight" |Term$desc=="total body fat"| Term$desc=="bmi centile"|Term$Description=="%"~ "Drop",
                      grepl("BMI|body mass ind", Term$desc, ignore.case=TRUE) ~ "BMI",
                      grepl("weight", Term$desc, ignore.case=TRUE) & (Term$Description=="kg"|Term$Description=="kgs") ~ "Weight",
                             TRUE ~ "Weight")
Term$Key<-paste0(Term$desc, Term$Description)

BMIValueAurum$Key<-paste0(BMIValueAurum$desc, BMIValueAurum$Description)

BMIValueAurum<-merge(x=BMIValueAurum, y=Term, by="Key", all.x=TRUE, all.y=FALSE)

#Extract weight
BMIWeightAurum<-subset(BMIValueAurum, Class=="Weight")

#Extract BMI
BMIValueAurum<-subset(BMIValueAurum, Class=="BMI")

summary(BMIValueAurum$value)
BMIValueAurum$value[BMIValueAurum$value<0]<-BMIValueAurum$value[BMIValueAurum$value<0]*-1

Check<-subset(BMIValueAurum, value>80 | value<10)

BMIValueAurum<-subset(BMIValueAurum, value<=80 & value>=10)


####Calculated####
summary(BMIWeightAurum$value)
BMIWeightAurum$value[BMIWeightAurum$value<0]<-BMIWeightAurum$value[BMIWeightAurum$value<0]*-1

Check<-subset(BMIWeightAurum, value>500 | value<40)
table(BMIWeightAurum$Description.x)

#If in decimal stones then convert
BMIWeightAurum$value<-case_when(BMIWeightAurum$`Description.x`=="decimal stones" ~ BMIWeightAurum$value*6.35029,
                                TRUE ~ BMIWeightAurum$value)

summary(BMIWeightAurum$value)
Check1<-subset(BMIWeightAurum, value>500 | value<40)
BMIWeightAurum<-subset(BMIWeightAurum, value<500 & value>=40)

#Pull in height
load("VariableExtracts/CPRD2023/MergedObs/AurumHeight.Rdata")
AurumHeight<-subset(PatEthnAurum, patid %in% Trial$patid)
rm(PatEthnAurum)
#Convert to metres
AurumHeight<-merge(x=AurumHeight, y=Units, by="numunitid", all.x=TRUE, all.y=FALSE)
AurumHeight$Description<-as.character(AurumHeight$Description)
table(AurumHeight$Description, AurumHeight$numunitid)
table(AurumHeight$Description, AurumHeight$numunitid)

AurumHeight$value<-case_when(AurumHeight$numunitid==122|AurumHeight$numunitid==408 ~ AurumHeight$value/100, 
                             AurumHeight$numunitid==3424 ~ AurumHeight$value*0.3048,
                             TRUE ~ AurumHeight$value)
summary(AurumHeight$value)

#If less than 250 but over 100 assume that it is cm
AurumHeight$value<-case_when(AurumHeight$value>=100& AurumHeight$value<=250 ~ AurumHeight$value/100, 
                             TRUE ~ AurumHeight$value)

#Height taken as adult
YoB<-select(Trial, patid, yob)
AurumHeight<-merge(x=AurumHeight, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
AurumHeight$heightage<-as.numeric(format(AurumHeight$eventdate, "%Y"))-AurumHeight$yob
table(AurumHeight$heightage)
AurumHeight<-subset(AurumHeight, heightage>=18)

summary(AurumHeight$value)
table(AurumHeight$value[AurumHeight$value<1])
table(AurumHeight$Description[AurumHeight$value<1])

#If less than 0.025 *100, otherwise remove if under 1m
AurumHeight$value<-case_when(AurumHeight$value<0.025~ AurumHeight$value*100,
                             TRUE ~ AurumHeight$value)
length(which(AurumHeight$value<1))

#remove those above Caliber threshold
AurumHeight<-subset(AurumHeight, value<=2.5)
AurumHeight<-subset(AurumHeight, value>=1)
summary(AurumHeight$value)

#Take mean height per patient
AurumHeight<-AurumHeight%>%
  group_by(patid)%>%
  summarise(MeanHeight=mean(value))
summary(AurumHeight$MeanHeight)

#Calculate BMI

AurumBMICalc<-merge(x=AurumHeight, y=BMIWeightAurum, by="patid", all.x=FALSE, all.y=FALSE)
AurumBMICalc$BMICalc<-AurumBMICalc$value/(AurumBMICalc$MeanHeight^2)
summary(AurumBMICalc$BMICalc)
length(which(AurumBMICalc$BMICalc>80))
AurumBMICalc<-subset(AurumBMICalc, BMICalc<=80&BMICalc>=10)
summary(AurumBMICalc$BMICalc)

####BMI categories####
BMICatAurum<-BMIAurum%>%
  subset(BMICat!="None")%>%
  select(patid, BMICat, eventdate)

####Final BMI record####
BMIValueAurum$Type<-"Value"
BMIValueAurum$BMICat<-NA

AurumBMICalc$Type<-"Calc"
AurumBMICalc$BMICat<-NA

BMICatAurum$Type<-"Cat"
BMICatAurum$value<-NA

BMIValueAurum<-select(BMIValueAurum, patid, value, BMICat, eventdate, Type)
AurumBMICalc<-select(AurumBMICalc, patid, value=BMICalc, BMICat, eventdate, Type)
BMICatAurum<-select(BMICatAurum, patid, value, BMICat, eventdate, Type)

BMIFinal<-rbind(BMIValueAurum, AurumBMICalc, BMICatAurum)
BMIFinalA<-BMIFinal

save(BMIFinalA, file="VariableExtracts/CPRD2023/Aurum/BMIAurumTrialPop.Rdata")

####Gold value####
rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/Trial_ethn.rdata")
load("VariableExtracts/CPRD2023/Gold/BMIGold.Rdata")

PatBMIMedGold$patid<-paste0(PatBMIMedGold$patid, "-G")
BMIGold<-subset(PatBMIMedGold, patid %in% Trial$patid)

rm(PatBMIMedGold)

BMIValueGold<-BMIGold%>%
  subset(!is.na(value))%>%
  select(patid, value, eventdate, desc, numunitid) #There are none with values so go to additional table

#Is there any BMI data in the test file?
BMITest<-read.table("GOLD/Test/SMI_GOLD_Extract_Test_001.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMITest<-subset(BMITest, enttype==13| enttype==14)#None in here

BMITest1<-read.table("GOLD/Test/SMI_GOLD_Extract_Test_002.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMITest1<-subset(BMITest1, enttype==13| enttype==14)#None in here

BMITest2<-read.table("GOLD/Test/SMI_GOLD_Extract_Test_003.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMITest2<-subset(BMITest2, enttype==13| enttype==14)#None in here

BMITest3<-read.table("GOLD/Test/SMI_GOLD_Extract_Test_004.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMITest3<-subset(BMITest3, enttype==13| enttype==14)#None in here

#Bring all additional info in to look for values
BMIG<-read.table("GOLD/Additional/SMI_GOLD_Extract_Additional_001.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
BMIG$patid<-paste0(BMIG$patid, "-G")
BMIG<-subset(BMIG, patid %in% Trial$patid)

#Only keep if height or weight
BMIG<-subset(BMIG, enttype==13| enttype==14)

#Bring in eventdate
ReadObsG <- function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", colClasses=c(medcode="character", patid="character"))
}

Observation_files <- list.files("GOLD/Clinical", pattern = "\\.txt$")
num<-c(1:length(Observation_files))
num[num<10]<-paste0("00", num[num<10])

PatEthnGold<-ReadObsG("Gold/Clinical/SMI_gold_Extract_Clinical_001.txt")
PatEthnGold<-subset(PatEthnGold, enttype==13|enttype==14)

for (i in (2:length(num))) {
  FileName<-paste0("Gold/Clinical/SMI_gold_Extract_Clinical_", num[i], ".txt")
  PatObs<-ReadObsG(FileName)
  PatObs<-subset(PatObs, enttype==13|enttype==14)
  PatEthnGold<-rbind(x=PatEthnGold, y=PatObs)
  print(num[i])
}
rm(PatObs)

PatEthnGold$patid<-paste0(PatEthnGold$patid, "-G")
PatEthnGold$patad<-paste0(PatEthnGold$patid, "-", PatEthnGold$adid)
PatEthnGold<-select(PatEthnGold, patad, eventdate, sysdate, medcode)
BMIG$patad<-paste0(BMIG$patid, "-", BMIG$adid)

BMIdate<-merge(x=BMIG, y=PatEthnGold, by="patad", all.x=TRUE, all.y=FALSE)

BMIdate$eventdate<-as.Date(BMIdate$eventdate, format= "%d/%m/%Y")
BMIdate$sysdate<-as.Date(BMIdate$sysdate, format= "%d/%m/%Y")

length(which(is.na(BMIdate$eventdate)))
length(which(is.na(BMIdate$sysdate)))

length(which(BMIdate$eventdate<="1900/01/01"))
length(which(BMIdate$sysdate<="1900/01/01"))

BMIdate$eventdate[BMIdate$eventdate<="1900/01/01"]<-NA
BMIdate$sysdate[BMIdate$sysdate<="1900/01/01"]<-NA

BMIdate$eventdate[is.na(BMIdate$eventdate)]<-BMIdate$sysdate[is.na(BMIdate$eventdate)]

#save weight and height for later
WeightHeightG<-select(BMIdate, patid, data1, data2, enttype, eventdate, medcode)

#Back to BMI
BMIG<-select(BMIdate, patid, data3, enttype, eventdate, medcode)
BMIG$BMI<-BMIG$data3
#Just check BMI isn't in enttype 14 (height)
table(BMIG$BMI[BMIG$enttype==14], BMIG$enttype[BMIG$enttype==14])
BMIG<-subset(BMIG, !is.na(BMI))
BMIG$BMI<-as.numeric(BMIG$BMI)

summary(BMIG$BMI)
#BMIG$BMI[BMIG$BMI<0]<-BMIG$BMI[BMIG$BMI<0]*-1

Check<-subset(BMIG, BMI>80 | BMI<10)

BMIValueGold<-subset(BMIG, BMI<=80 & BMI>=10)
BMIValueGold<-rename(BMIValueGold, value=BMI)

rm(BMIG, BMIdate, Check, PatEthnGold)

#Check which have correct terms
Check<-BMIValueGold%>%
  group_by(medcode)%>%
  summarise(Count=n())

GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
GoldMed$desc<-tolower(GoldMed$desc)

Check<-merge(x=Check, y=GoldMed, by.x="medcode", by.y="medcode", all.x=TRUE, all.y=FALSE)

Check2<-merge(x=BMIValueGold, y=GoldMed, by="medcode", all.x=TRUE, all.y=FALSE)

Check2<-Check2%>%
  subset(medcode=="93")

####BMI calculated Gold####
#Gold weight
GoldWeight<-subset(WeightHeightG, enttype==13)
GoldWeight$WeightG<-as.numeric(GoldWeight$data1)
length(which(GoldWeight$WeightG<40))
table(GoldWeight$WeightG[GoldWeight$WeightG<40])
GoldWeight<-subset(GoldWeight, WeightG>=40 & WeightG<500)

#Gold height
GoldHeight<-subset(WeightHeightG, enttype==14)
GoldHeight$HeightG<-as.numeric(GoldHeight$data1)
#Height taken as adult
YoB<-select(Trial, patid, yob)
GoldHeight<-merge(x=GoldHeight, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
GoldHeight$HeightAge<-as.numeric(format(GoldHeight$eventdate, "%Y"))-GoldHeight$yob
table(GoldHeight$HeightAge)
GoldHeight<-subset(GoldHeight, HeightAge>=18)

summary(GoldHeight$HeightG)
table(GoldHeight$HeightG[GoldHeight$HeightG<1])

#If less than 0.025 *100
GoldHeight$HeightG[GoldHeight$HeightG<0.025]<-GoldHeight$HeightG[GoldHeight$HeightG<0.025]*100
length(which(GoldHeight$HeightG<1))
GoldHeight<-subset(GoldHeight, HeightG>=1)
summary(GoldHeight$HeightG)

#Take mean height per patient
GoldHeight<-GoldHeight%>%
  group_by(patid)%>%
  summarise(MeanHeightG=mean(HeightG))
summary(GoldHeight$MeanHeightG)
length(unique(GoldHeight$patid))

#Gold calculated BMI
GoldBMICalc<-merge(x=GoldHeight, y=GoldWeight, by="patid", all.x=FALSE, all.y=FALSE)
GoldBMICalc$BMICalc<-GoldBMICalc$WeightG/(GoldBMICalc$MeanHeightG^2)
summary(GoldBMICalc$BMICalc)
length(which(GoldBMICalc$BMICalc>80))
GoldBMICalc<-subset(GoldBMICalc, BMICalc<=80&BMICalc>=10)
summary(GoldBMICalc$BMICalc)

####BMI categories####
BMICatGold<-BMIGold%>%
  subset(BMICat!="None")%>%
  select(patid, BMICat, eventdate)

####Final BMI record####
BMIValueGold$Type<-"Value"
BMIValueGold$BMICat<-NA

GoldBMICalc$Type<-"Calc"
GoldBMICalc$BMICat<-NA

BMICatGold$Type<-"Cat"
BMICatGold$value<-NA

BMIValueGold<-select(BMIValueGold, patid, value, BMICat, eventdate, Type)
GoldBMICalc<-select(GoldBMICalc, patid, value=BMICalc, BMICat, eventdate, Type)
BMICatGold<-select(BMICatGold, patid, value, BMICat, eventdate, Type)

BMIFinal<-rbind(BMIValueGold, GoldBMICalc, BMICatGold)
BMIFinalG<-BMIFinal
save(BMIFinalG, file="VariableExtracts/CPRD2023/Gold/BMIGoldTrialPop.Rdata")



