#STATIN AND AP DOSE AND CONTINUOUS PRESCRIPTIONS
library(tidyverse)
library(lubridate)
library(tableone)

#Clear environment

rm(list = ls(all.names = TRUE))
load("StatinCPRD/Data/Hospselect.rdata")

####Antipsychotic prescriptions####
load("VariableExtracts/CPRD2023/MergedObs/APProd.Rdata")
PatAPAll<-subset(PatAPAll, patid %in% HospAll$patid)
PatAPAll<-subset(PatAPAll, Route=="Oral")
PatAPAll<-subset(PatAPAll, eventdate>=as.Date("1999-06-01")& eventdate<=as.Date("2019-12-12"))
End<-select(HospAll, patid, end)
PatAPAll<-merge(x=PatAPAll, y=End, by="patid", all.x=TRUE, all.y=FALSE)
PatAPAll<-subset(PatAPAll, eventdate<=end)

PatAPAll$PRN<-case_when(grepl("PRN|REQ|NECESSARY|MDU|UNKNOWN|NEEDED", PatAPAll$dosage_text, ignore.case=TRUE) ~1,
                        TRUE ~0)
table(PatAPAll$PRN)
Check<-subset(PatAPAll, PRN==1)

#Check daily dose
length(which(is.na(PatAPAll$daily_dose)))
table(PatAPAll$daily_dose)
DD<-subset(PatAPAll, is.na(PatAPAll$daily_dose)|PatAPAll$daily_dose==0) #All are blank so can't find them

#Daily dose from text
DD<-subset(DD, !is.na(dosage_text))
DD<-select(DD, dosage_text)
DD<-distinct(DD)
DD<-subset(DD, !grepl("PRN|AS|dir|TRAFFIC|COMMENT|as per|drop|ISSUE|REQ|NECESSARY|MDU|UNKNOWN|PUFF|SHARED CARE|DIRETED|INSTRUCTED|IMMEDIATELY|NEEDED", dosage_text, ignore.case=TRUE))
DD<-subset(DD, dosage_text!="-" & dosage_text!="MITTE" & dosage_text!="TAKE"& dosage_text!="50" & dosage_text!="28")

DD$Duration<-0
DD$Duration[DD$dosage_text=="MITTE 56 DAYS"]<-56
DD$Duration[DD$dosage_text=="MITTE 7 DAYS"]<-7
DD$Duration[DD$dosage_text=="MITTE 28 DAYS"]<-28
DD$Duration[DD$dosage_text=="1 MONTH"]<-30

DD$Quantity<-0
DD$Quantity[DD$dosage_text=="42"]<-42
DD$Quantity[DD$dosage_text=="120"]<-120
DD$Quantity[DD$dosage_text=="180"]<-180
DD$Quantity[DD$dosage_text=="100"]<-100
DD$Quantity[DD$dosage_text=="30"]<-30
DD$Quantity[DD$dosage_text=="38"]<-38

DD$dose<-0
DD$dose<-case_when(DD$dosage_text=="1 THREE A DAY" ~ 3,
                   DD$dosage_text=="10" ~ 10,
                   DD$dosage_text=="14" ~ 14,
                   DD$dosage_text=="13" ~ 13,
                   DD$dosage_text=="12" ~ 12,
                         DD$dosage_text=="5-10MLS"|DD$dosage_text=="5-10MLTDSPR" ~ 7.5,
                         DD$dosage_text=="5ML A DAY"|DD$dosage_text=="50D"|DD$dosage_text=="FIVE" ~ 5,
                   DD$dosage_text=="1B" ~ 2,
                   DD$dosage_text=="1T" ~ 3,
                         grepl("1.5", DD$dosage_text) | DD$dosage_text=="1 OR 2"|DD$dosage_text=="ONE AND A H"|	DD$dosage_text=="ONE AND A HALF"|
                     DD$dosage_text=="1 AND HALF"|DD$dosage_text=="TAKE ONE AND A HALF"|DD$dosage_text=="1-2"|DD$dosage_text=="1 AND HALF"| DD$dosage_text=="11/2"|
                     DD$dosage_text=="TAKE ONE OR TWO"|DD$dosage_text=="ONE AND HALF"|	
                     DD$dosage_text=="ONE OR TWO"|DD$dosage_text=="1 1/2" ~ 1.5,
  DD$dosage_text=="1000" |DD$dosage_text=="1 MONTH" |DD$dosage_text=="0.1"|DD$dosage_text=="FP34"|DD$dosage_text=="120"|DD$dosage_text==":[100]"|
    DD$dosage_text=="120"|DD$dosage_text=="180" |DD$dosage_text=="AUTH=12"|DD$dosage_text=="100"|DD$dosage_text=="30"|DD$dosage_text=="38"~ 0,
  grepl("DAYS", DD$dosage_text, ignore.case=TRUE)|DD$dosage_text=="42" ~ 0,
  DD$dosage_text=="20 MG" ~ 20,
  DD$dosage_text=="400MG" ~ 400,
  DD$dosage_text=="200MG" ~ 200,
  DD$dosage_text=="100MG" ~ 100,
  DD$dosage_text=="10MG" ~ 10,
  DD$dosage_text=="5MG" ~ 5,
  DD$dosage_text=="300MG" ~ 300,
  DD$dosage_text=="15MG" ~ 15,
  DD$dosage_text=="0.5MG" ~ 0.5,
  DD$dosage_text=="0.5 MG" ~ 0.5,
  DD$dosage_text=="5ML" ~ 5,
  DD$dosage_text=="5 ML" ~ 5,
  DD$dosage_text=="150 MG" ~ 150,
  DD$dosage_text=="0.1" ~ 0.1,
  DD$dosage_text=="1 ML" ~ 1,
  DD$dosage_text=="20  MG" ~ 20,
  DD$dosage_text=="2.5ML" ~ 2.5,
  DD$dosage_text=="2.5 ML" ~ 2.5,
  DD$dosage_text=="20MG A DAY" ~ 20,
  DD$dosage_text=="10MGS AT NIGHT" ~ 10,
  DD$dosage_text=="1 EOD"| DD$dosage_text=="HALF A TABL"|DD$dosage_text==".5"|DD$dosage_text=="HALF A TAB"|DD$dosage_text=="HALF A TABLET"|
    DD$dosage_text=="1/2" ~ 0.5,
  DD$dosage_text=="HALF TABLET"|DD$dosage_text=="HALF TAB DA"|DD$dosage_text=="ONE HALF A DAY" ~ 0.5,
  DD$dosage_text=="2.5"|DD$dosage_text=="2 1/2"|DD$dosage_text=="TWO AND HALF" ~ 2.5,
  DD$dosage_text=="TAKE ONE 3 A DAY"|DD$dosage_text=="1 TSD"| DD$dosage_text=="TAKE ONE TREE TIMES A DAY"~ 3,
  DD$dosage_text=="1 TWICE"|DD$dosage_text=="IBD"|DD$dosage_text=="1 BDAY" |DD$dosage_text=="TWICW A DAY"|DD$dosage_text=="TWICE"~ 2,
  DD$dosage_text=="2.5"| DD$dosage_text=="DB"|DD$dosage_text=="2 MONE" |DD$dosage_text=="1DB"~ 2,
  grepl("ONE THREE|TD|THREE TIME|TOD", DD$dosage_text, ignore.case=TRUE) ~ 3,
  grepl("ONE Tw", DD$dosage_text, ignore.case=TRUE) ~ 2,
  DD$dosage_text=="2 THREE A DAY"|DD$dosage_text=="6 D" |DD$dosage_text=="6"~ 6,
  DD$dosage_text=="ION"| DD$dosage_text=="AM"|DD$dosage_text=="MORNE"|DD$dosage_text=="HALF TWICE"|DD$dosage_text=="OD" |DD$dosage_text=="ODS"~ 1,
  DD$dosage_text=="NIGHTLY"| DD$dosage_text=="DAY"|DD$dosage_text=="O"~ 1,
                         grepl("1|ONE|ONCE|PM", DD$dosage_text) ~ 1,
  grepl("2|TWO", DD$dosage_text) ~ 2,
  grepl("3|THREE", DD$dosage_text) ~ 3,
  grepl("4|FOUR", DD$dosage_text) ~ 4,
  TRUE ~ 0)

DD$Unit<-""
DD$Unit[grepl("ml", DD$dosage_text, ignore.case=TRUE)]<-"ML"      
DD$Unit[grepl("mg", DD$dosage_text, ignore.case=TRUE)]<-"MG" 

#Replace fields in main table if missing
PatAPAll<-merge(x=PatAPAll, y=DD, by="dosage_text", all.x=TRUE, all.y=FALSE)
PatAPAll$daily_dose[is.na(PatAPAll$daily_dose) | PatAPAll$daily_dose==0] <-PatAPAll$dose[is.na(PatAPAll$daily_dose) | PatAPAll$daily_dose==0]
PatAPAll$dose_unit[is.na(PatAPAll$dose_unit) | PatAPAll$dose_unit==0 ]<-PatAPAll$Unit[is.na(PatAPAll$dose_unit) | PatAPAll$dose_unit==0 ]
PatAPAll$duration[is.na(PatAPAll$duration) | PatAPAll$duration==0 ]<-PatAPAll$Duration[is.na(PatAPAll$duration) | PatAPAll$duration==0 ]
PatAPAll$quantity[is.na(PatAPAll$quantity) | PatAPAll$quantity==0 ]<-PatAPAll$Quantity[is.na(PatAPAll$quantity) | PatAPAll$quantity==0 ]

PatAPAll<-select(PatAPAll, -dose, -Duration, -Unit, -Quantity)
table(PatAPAll$daily_dose, useNA="ifany")

#Can we get missing strength from text?

length(which(PatAPAll$strength==""))

ST<-subset(PatAPAll, strength==""|is.na(strength)|strength==0)
ST<-subset(ST, !is.na(productname))
ST<-select(ST, productname)
ST<-distinct(ST)
ST$MG<-0
ST$ML<-0

ST<-ST%>%
  mutate(MG=case_when(grepl("\\d+([.,]\\d+)+mg", productname, ignore.case=TRUE) ~ as.numeric(str_extract(productname, "\\d+([.,]\\d+)(?=mg)")),
                      grepl("\\d+mg", productname, ignore.case=TRUE) ~ as.numeric(str_extract(productname, "\\d+(?=mg)")),
                      grepl("\\d+([.,]\\d+)+[[:space:]]+mg", productname, ignore.case=TRUE) ~ as.numeric(str_extract(productname, "\\d+([.,]\\d+)")),
                      grepl(" mg", productname, ignore.case=TRUE) ~ as.numeric(str_extract(productname, "\\d+")),
                      grepl("microgram", productname, ignore.case=TRUE) ~ (as.numeric(str_extract(productname, "\\d+([.,]\\d+)(?=microgram)")))/1000,
                      grepl("microgram", productname, ignore.case=TRUE) ~ (as.numeric(str_extract(productname, "\\d+(?=microgram)")))/1000,
                      grepl("g", productname, ignore.case=TRUE) ~ (as.numeric(str_extract(productname, "\\d+([.,]\\d+)(?=g)")))*1000,
                      grepl("g", productname, ignore.case=TRUE) ~ (as.numeric(str_extract(productname, "\\d+(?=g)")))*1000,
                      TRUE ~ as.numeric(str_extract(productname, "\\d+"))),
         ML=case_when(grepl("5 ml", productname, ignore.case=TRUE) ~ 5,
                      grepl("ml", productname, ignore.case=TRUE) ~ 1,                                                   
                      TRUE ~ 0))

#conversion for lithium
ST$MG[grepl("5.4 mmol", ST$productname, ignore.case=TRUE)]<-509
ST$MG[grepl("10.8 mmol", ST$productname, ignore.case=TRUE)]<-1018

ST$MG[is.na(ST$MG)]<-as.numeric(str_extract(ST$productname[is.na(ST$MG)], "\\d+([.,]\\d+)"))
ST$MG[is.na(ST$MG)]<-as.numeric(str_extract(ST$productname[is.na(ST$MG)], "\\d+"))

#COnvert everything to MG/ML for ease
ST<-ST%>%
  mutate(ST=case_when(ML==0 ~ MG,
                      TRUE ~ MG/ML))

#Deal with starter packs later

PatAPAll<-merge(x=PatAPAll, y=ST, by="productname", all.x=TRUE, all.y=FALSE)

#Check strength and convert to mg
#If we have strength take it from there
table(PatAPAll$strength)
PatAPAll$StrengthNew<-as.numeric(str_extract(PatAPAll$strength, "\\d+\\.*\\d*"))

table(PatAPAll$StrengthNew, useNA="ifany")
table(PatAPAll$strength, useNA="ifany")

PatAPAll$StrengthNew[is.na(PatAPAll$strength)|PatAPAll$strength==0|PatAPAll$strength==""]<-PatAPAll$ST[is.na(PatAPAll$strength)|PatAPAll$strength==0|PatAPAll$strength==""]
PatAPAll<-select(PatAPAll, -ST, -MG, -ML)

table(PatAPAll$strength)

Check<-as.data.frame(table(PatAPAll$StrengthNew, PatAPAll$strength,useNA="ifany"))
Check<-subset(Check, Freq!=0)

PatAPAll$StrengthNew<-case_when(grepl("microgram",PatAPAll$strength, ignore.case=TRUE) ~ PatAPAll$StrengthNew/1000,
                              grepl("gram", PatAPAll$strength, ignore.case=TRUE) ~ PatAPAll$StrengthNew*1000,
                              grepl("5ml",PatAPAll$strength, ignore.case=TRUE) ~ PatAPAll$StrengthNew/5,
                              TRUE ~ PatAPAll$StrengthNew)

#Calculate daily dose - prn will be set to zero
PatAPAll<-PatAPAll%>%
  mutate(DailyDose=case_when(dose_unit=="ML" ~ daily_dose*StrengthNew,
                             dose_unit=="MG" ~ daily_dose,
                             TRUE ~ daily_dose*StrengthNew))

table(PatAPAll$DailyDose, useNA="ifany")

Check<-subset(PatAPAll, DailyDose==0 | is.na(DailyDose))  

#Look at starter packs
Starter<-subset(PatAPAll, grepl("starter",PatAPAll$productname, ignore.case=TRUE))
Starter<-select(Starter, productname)
Starter<-distinct(Starter)

#Set daily dose for starter packs
PatAPAll$DailyDose[grepl("starter&quetiapine&150",PatAPAll$productname, ignore.case=TRUE)]<-300
PatAPAll$DailyDose[grepl("starter&quetiapine&100",PatAPAll$productname, ignore.case=TRUE)&is.na(PatAPAll$DailyDose)]<-200
PatAPAll$DailyDose[grepl("starter&Seroquel&150",PatAPAll$productname, ignore.case=TRUE)]<-300

#If just blank assume its 250
PatAPAll$DailyDose[grepl("starter",PatAPAll$productname, ignore.case=TRUE)&is.na(PatAPAll$DailyDose)]<-250

#Check duration and quantity
Quant<-subset(PatAPAll, dose_unit=="ML")
table(PatAPAll$quantity[is.na(PatAPAll$DailyDose)])
PatAPAll$quantity<-case_when(PatAPAll$quantity<0 ~ PatAPAll$quantity*-1, TRUE ~ PatAPAll$quantity)
Check<-subset(PatAPAll, quantity>1000)#Most of the highest ones are in ML

table(PatAPAll$duration)
PatAPAll$duration<-case_when(PatAPAll$duration<0 ~ PatAPAll$duration*-1, TRUE ~ PatAPAll$duration)
length(which(PatAPAll$duration>112))

PatAPAll$duration<-case_when(PatAPAll$duration>112 ~ 0, TRUE ~ PatAPAll$duration)
table(PatAPAll$duration)

#Because I am only interested in those at right dosage, calculate duration to next prescription
Dur<-PatAPAll%>%
  group_by(patid)%>%
  arrange(eventdate)%>%
  mutate(TimeToNext=lead(eventdate)-eventdate)

Dur$TimeToNext<-as.numeric(Dur$TimeToNext)

Dur<-Dur%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(end-eventdate),
         TRUE ~ TimeToNext))

#If time to next is zero set it to be the next time to next
Zeros<-Dur%>%
  subset(TimeToNext==0)%>%
  group_by(patid)%>%
  summarise(Zero=n())

#If its zero take  the biggest on that day
Dur<-Dur%>%
  mutate(patdate=paste0(patid, eventdate))%>%
  group_by(patdate)%>%
  mutate(TimeToNext=case_when(TimeToNext==0 ~ max(TimeToNext),
                              TRUE ~ TimeToNext))

length(which(Dur$TimeToNext==0))

#Are these zero because they are at the same time as end?
length(which(Dur$TimeToNext==0&Dur$eventdate==Dur$end)) # Yes they are - leave them in as they wont be the ones we need anyway, and coverage will be set to NA

#If quantity is 1 and duration is reasonable, assume it's one per day
table(Dur$duration[Dur$quantity==1])

Dur$Newquantity<-case_when(Dur$quantity==1 & is.na(Dur$DailyDose) & Dur$duration>=7 ~ Dur$duration,
                        TRUE ~ NA)

#If daily dose is unknown take quantity by duration
Dur<-Dur%>%
  ungroup()%>%
  mutate(CalcDose=case_when(quantity>0 & duration>0 ~ (quantity/duration)*StrengthNew,
                             TRUE ~ NA_real_),
         CalcDose2=case_when(Newquantity>0 & duration>0 ~ (Newquantity/duration)*StrengthNew,
                             TRUE ~ NA_real_),
         Coverage=case_when(quantity>0 & TimeToNext>0~ (quantity/TimeToNext)*StrengthNew,
                            TRUE ~ NA_real_),
         Coverage2=case_when(Newquantity>0 & TimeToNext>0~ (Newquantity/TimeToNext)*StrengthNew,
                            TRUE ~ NA_real_))


length(which(Dur$duration>112))
length(which(Dur$TimeToNext>112))
length(which(Dur$quantity>1000))

Dur$DailyDose[Dur$DailyDose==0]<-NA
Dur$Coverage[Dur$TimeToNext>112]<-NA

summary(Dur$DailyDose)
summary(Dur$Coverage)

Dur$AP<-tolower(Dur$AP)

save(Dur, file="VariableExtracts/CPRD2023/MergedObs/AP_interim.Rdata")

####Equivalence####
library(chlorpromazineR)

DDD_consensus <- chlorpromazineR::add_key(base = leucht2016, add = gardner2010, trim = TRUE)
DDD_consensus <- chlorpromazineR::add_key(base = DDD_consensus, add = leucht2020, trim = TRUE)

Dur <- as.data.frame(Dur)

APlist<-Dur %>% 
  select(AP)%>%
  distinct(AP)

APlist <- as.data.frame(APlist)

check_ap(APlist, ap_label = "AP",  route = "oral", key = DDD_consensus)
DurAP<-subset(Dur, AP!="lithium" & AP!="valproate" & AP!="lamotrigine")
DurBP<-subset(Dur, AP=="lithium" | AP=="valproate" | AP=="lamotrigine")

rm(Dur)

apDose1 <- chlorpromazineR::to_ap(DurAP, convert_to_ap = "olanzapine", convert_to_route = "oral", 
                                 ap_label = "AP", dose_label = "DailyDose", key = DDD_consensus)

apDose1<-rename(apDose1, DD1eq=ap_eq)

apDose2 <- chlorpromazineR::to_ap(DurAP, convert_to_ap = "olanzapine", convert_to_route = "oral", 
                                 ap_label = "AP", dose_label = "CalcDose", key = DDD_consensus)

apDose2<-select(apDose2, DD2eq=ap_eq)

apDose3 <- chlorpromazineR::to_ap(DurAP, convert_to_ap = "olanzapine", convert_to_route = "oral", 
                                 ap_label = "AP", dose_label = "CalcDose2", key = DDD_consensus)

apDose3<-select(apDose3, DD3eq=ap_eq)

apCoverage1 <- chlorpromazineR::to_ap(DurAP, convert_to_ap = "olanzapine", convert_to_route = "oral", 
                                 ap_label = "AP", dose_label = "Coverage", key = DDD_consensus)

apCoverage1<-select(apCoverage1, Cov1eq=ap_eq)

apCoverage2 <- chlorpromazineR::to_ap(DurAP, convert_to_ap = "olanzapine", convert_to_route = "oral", 
                                 ap_label = "AP", dose_label = "Coverage2", key = DDD_consensus)

apCoverage2<-select(apCoverage2, Cov2eq=ap_eq)


apDose<-cbind(apDose1, apDose2, apDose3, apCoverage1, apCoverage2)

#Need to consider those on multiple doses per day
apDose<-apDose%>%
  group_by(patdate)%>%
  mutate(TotalDose1=sum(DD1eq, na.rm=TRUE), TotalDose2=sum(DD2eq, na.rm=TRUE), TotalDose3=sum(DD3eq, na.rm=TRUE), 
         TotalCoverage1=sum(Cov1eq, na.rm=TRUE), TotalCoverage2=sum(Cov2eq, na.rm=TRUE))%>%
  ungroup()

#For olanzapine; 6.5mg should be effective dose, and 20mg max (with 10 mg leeway for conversion)
#If total dose is out of range take total coverage
apDose<-apDose%>%
  mutate(TotalDose=case_when(TotalDose1>=6.5&TotalDose1<=30~TotalDose1,
                             TotalDose2>=6.5&TotalDose2<=30~TotalDose2,
                             TotalDose3>=6.5&TotalDose3<=30~TotalDose3,
                             TotalCoverage1>=6.5&TotalCoverage1<=30~TotalCoverage1,
                             TotalCoverage2>=6.5&TotalCoverage2<=30~TotalCoverage2,
                             TRUE ~ NA_real_))
                             
summary(apDose$TotalDose)

apDoseFinal<-subset(apDose, TotalDose>=6.5 & TotalDose<=30)
length(unique(apDoseFinal$patid))
summary(apDoseFinal$TotalDose)

#Check those that are not at equivalent dose
apCheck<-subset(apDose, !patdate %in% apDoseFinal$patdate)

#If dailydose is the same for all then probably a mistake
apCheck<-apCheck%>%
  group_by(patdate, DailyDose, Coverage, AP)%>%
  mutate(Duplicate=n())%>%
  ungroup()%>%
  group_by(patdate)%>%
  mutate(n=n())%>%
  subset(Duplicate==n & n>1)%>% #Those that have multiple prescriptions that are all the same
  mutate(NewDose1=TotalDose1/n, NewDose2=TotalDose2/n,NewDose3=TotalDose3/n,NewCoverage1=TotalCoverage1/n, NewCoverage2=TotalCoverage2/n)

apCheck<-apCheck%>%
  mutate(TotalDose=case_when(NewDose1>=6.5&NewDose1<=30~NewDose1,
                             NewDose2>=6.5&NewDose2<=30~NewDose2,
                             NewDose3>=6.5&NewDose3<=30~NewDose3,
                             NewCoverage1>=6.5&NewCoverage1<=30~NewCoverage1,
                             NewCoverage2>=6.5&NewCoverage2<=30~NewCoverage2,
                             TRUE ~ NA_real_))

apCheck<-select(apCheck, -n, -Duplicate, -NewDose1, -NewDose2,-NewDose3,-NewCoverage1, -NewCoverage2)

apCheck<-subset(apCheck, !is.na(TotalDose))

summary(apCheck$TotalDose)

apDoseFinal<-rbind(apDoseFinal, apCheck)
length(unique(apDoseFinal$patid))

####Check those that aren't at dose####
apCheck<-subset(apDose, !patdate %in% apDoseFinal$patdate)
summary(apCheck$TotalDose)

length(which(apCheck$TotalDose1==0 &apCheck$TotalDose2==0 & apCheck$TotalDose3==0 & apCheck$TotalCoverage1==0 & apCheck$TotalCoverage2==0))

save(apDoseFinal, file="VariableExtracts/CPRD2023/MergedObs/AP_dose.Rdata")
save(apCheck, file="VariableExtracts/CPRD2023/MergedObs/Missing_AP_dose.Rdata")

rm(apDose, apDose1, apDose2, apDose3, apCheck, apDoseFinal)

#For bipolar meds, set effective dose for lithium as any, valproate as 1-2g daily, lamotrogine depending on what with
#If Total is missing take the other
DurBP<-DurBP%>%
  group_by(patdate)%>%
  mutate(TotalDose1=sum(DailyDose, na.rm=TRUE), TotalDose2=sum(CalcDose, na.rm=TRUE),TotalDose3=sum(CalcDose2, na.rm=TRUE),
         TotalCoverage1=sum(Coverage), TotalCoverage2=sum(Coverage2))%>%
  ungroup()

#Need to account for multiple on same day
BPDose<-DurBP%>%
  group_by(patdate, AP)%>%
  mutate(TotalDose1=sum(DailyDose, na.rm=TRUE), TotalDose2=sum(CalcDose, na.rm=TRUE),TotalDose3=sum(CalcDose2, na.rm=TRUE),
         TotalCoverage1=sum(Coverage), TotalCoverage2=sum(Coverage2))%>%
  group_by(patdate)%>%
  mutate(Lithium=sum(AP == "lithium", na.rm=TRUE), Valproate=sum(AP == "valproate", na.rm=TRUE), Lamotrigine=sum(AP == "lamotrigine", na.rm=TRUE))%>%
  ungroup()

#If just lithium keep
length(which(BPDose$Lithium==1 & BPDose$Valproate==0 & BPDose$Lamotrigine==0))
length(which(BPDose$Lithium==1 & BPDose$Valproate==1 & BPDose$Lamotrigine==0))
length(which(BPDose$Lithium==1 & BPDose$Valproate==0 & BPDose$Lamotrigine==1))
length(which(BPDose$Lithium==1 & BPDose$Valproate==1 & BPDose$Lamotrigine==1))
length(which(BPDose$Lithium==0 & BPDose$Valproate==1 & BPDose$Lamotrigine==0))
length(which(BPDose$Lithium==0 & BPDose$Valproate==1 & BPDose$Lamotrigine==1))
length(which(BPDose$Lithium==0 & BPDose$Valproate==0 & BPDose$Lamotrigine==1))

#With half again

BPDose<-BPDose%>%
  mutate(KeepDose=case_when(AP=="lithium" & TotalDose1>=200 & TotalDose1<=2250 ~ 1, 
                        AP=="valproate" & TotalDose1>=1000 & TotalDose1<=3000 ~ 1,
                        AP=="lamotrigine" & Valproate==0 &  (TotalDose1>=200 & TotalDose1<=600) ~ 1,
                        AP=="lamotrigine" & Valproate==1 &  (TotalDose1>=100 & TotalDose1<=300) ~ 1,
                        
                        AP=="lithium" & TotalDose2>=200 & TotalDose2<=2250 ~ 2, 
                        AP=="valproate" & TotalDose2>=1000 & TotalDose2<=2100 ~ 2,
                        AP=="lamotrigine" & Valproate==0 &  (TotalDose2>=200 & TotalDose2<=600) ~ 2,
                        AP=="lamotrigine" & Valproate==1 &  (TotalDose2>=100 & TotalDose2<=300) ~ 2,
                        
                        AP=="lithium" & TotalDose3>=200 & TotalDose3<=2250 ~ 3, 
                        AP=="valproate" & TotalDose3>=1000 & TotalDose3<=2100 ~ 3,
                        AP=="lamotrigine" & Valproate==0 &  (TotalDose3>=200 & TotalDose3<=600) ~ 3,
                        AP=="lamotrigine" & Valproate==1 &  (TotalDose3>=100 & TotalDose3<=300) ~ 3,
                        
                        AP=="lithium" & TotalCoverage1>=200 & TotalCoverage1<=2250 ~ 4, 
                        AP=="valproate" & TotalCoverage1>=1000 & TotalCoverage1<=2100 ~ 4,
                        AP=="lamotrigine" & Valproate==0 &  (TotalCoverage1>=200 & TotalCoverage1<=600) ~ 4,
                        AP=="lamotrigine" & Valproate==1 &  (TotalCoverage1>=100 & TotalCoverage1<=300) ~ 4,
                        
                        AP=="lithium" & TotalCoverage2>=200 & TotalCoverage2<=2250 ~ 5, 
                        AP=="valproate" & TotalCoverage2>=1000 & TotalCoverage2<=2100 ~ 5,
                        AP=="lamotrigine" & Valproate==0 &  (TotalCoverage2>=200 & TotalCoverage2<=600) ~ 5,
                        AP=="lamotrigine" & Valproate==1 &  (TotalCoverage2>=100 & TotalCoverage2<=300) ~ 5,
                        TRUE ~ 0))

BPDose<-BPDose%>%
  mutate(TotalDose=case_when(KeepDose==1 ~ TotalDose1,
                             KeepDose==2 ~ TotalDose2,
                             KeepDose==3 ~ TotalDose3,
                             KeepDose==4 ~ TotalCoverage1,
                             KeepDose==5 ~ TotalCoverage2,
                             TRUE ~ NA_real_))

summary(BPDose$TotalDose)

BPDoseFinal<-subset(BPDose, !is.na(TotalDose))

summary(BPDoseFinal$TotalDose)

#check those that aren't included
BPCheck<-subset(BPDose, !patdate %in% BPDoseFinal$patdate)

BPCheck<-BPCheck%>%
  group_by(patdate, DailyDose, Coverage, AP)%>%
  mutate(Duplicate=n())%>%
  ungroup()%>%
  group_by(patdate)%>%
  mutate(n=n())%>%
  subset(Duplicate==n & n>1)%>% #Those that have multiple prescriptions that are all the same
  mutate(NewDose1=TotalDose1/n, NewDose2=TotalDose2/n,NewDose3=TotalDose3/n,NewCoverage1=TotalCoverage1/n, NewCoverage2=TotalCoverage2/n)

BPCheck<-BPCheck%>%
  mutate(KeepDose=case_when(AP=="lithium" & NewDose1>=200 & NewDose1<=2250 ~ 1,
                            AP=="valproate" & NewDose1>=1000 & NewDose1<=3000 ~ 1,
                            AP=="lamotrigine" & Valproate==0 &  (NewDose1>=200 & NewDose1<=600) ~ 1,
                            AP=="lamotrigine" & Valproate==1 &  (NewDose1>=100 & NewDose1<=300) ~ 1,
                            
                            AP=="lithium" & NewDose2>=200 & NewDose2<=2250 ~ 2, 
                            AP=="valproate" & NewDose2>=1000 & NewDose2<=2100 ~ 2,
                            AP=="lamotrigine" & Valproate==0 &  (NewDose2>=200 & NewDose2<=600) ~ 2,
                            AP=="lamotrigine" & Valproate==1 &  (NewDose2>=100 & NewDose2<=300) ~ 2,
                                      
                            AP=="lithium" & NewDose3>=200 & NewDose3<=2250 ~ 3, 
                            AP=="valproate" & NewDose3>=1000 & NewDose3<=2100 ~ 3,
                            AP=="lamotrigine" & Valproate==0 &  (NewDose3>=200 & NewDose3<=600) ~ 3,
                            AP=="lamotrigine" & Valproate==1 &  (NewDose3>=100 & NewDose3<=300) ~ 3,
                                      
                            AP=="lithium" & NewCoverage1>=200 & NewCoverage1<=2250 ~ 4, 
                            AP=="valproate" & NewCoverage1>=1000 & NewCoverage1<=2100 ~ 4,
                            AP=="lamotrigine" & Valproate==0 &  (NewCoverage1>=200 & NewCoverage1<=600) ~ 4,
                            AP=="lamotrigine" & Valproate==1 &  (NewCoverage1>=100 & NewCoverage1<=300) ~ 4,
                                      
                            AP=="lithium" & NewCoverage2>=200 & NewCoverage2<=2250 ~ 5, 
                            AP=="valproate" & NewCoverage2>=1000 & NewCoverage2<=2100 ~ 5,
                            AP=="lamotrigine" & Valproate==0 &  (NewCoverage2>=200 & NewCoverage2<=600) ~ 5,
                            AP=="lamotrigine" & Valproate==1 &  (NewCoverage2>=100 & NewCoverage2<=300) ~ 5,
                            TRUE ~ 0))

BPCheck<-BPCheck%>%
  mutate(TotalDose=case_when(KeepDose==1 ~ NewDose1,
                             KeepDose==2 ~ NewDose2,
                             KeepDose==3 ~ NewDose3,
                             KeepDose==4 ~ NewCoverage1,
                             KeepDose==5 ~ NewCoverage2,
                             TRUE ~ NA_real_))

summary(BPCheck$TotalDose)

BPCheck<-subset(BPCheck, !is.na(TotalDose))

BPCheck<-select(BPCheck, -n, -Duplicate, -NewDose1, -NewDose2,-NewDose3,-NewCoverage1, -NewCoverage2)

BPDoseFinal<-rbind(BPDoseFinal, BPCheck)

BPCheck<-subset(BPDose, !patdate %in% BPDoseFinal$patdate)

save(BPDoseFinal, file="VariableExtracts/CPRD2023/MergedObs/BP_dose.Rdata")

save(BPCheck, file="VariableExtracts/CPRD2023/MergedObs/MissingBP_dose.Rdata")
