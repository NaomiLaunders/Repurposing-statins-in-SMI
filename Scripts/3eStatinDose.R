####Dose at first statin prescription for trial 1 & 2 (or if blank within 6 months), dose at closest statin prescription for trial 3

rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)

####Load 2023 data####
load("StatinCPRD/Data/Hospselect.rdata")

load("VariableExtracts/CPRD2023/MergedObs/AllStatins.Rdata")

AllStat<-subset(AllStat, patid %in% HospAll$patid)
length(unique(AllStat$patid))

#Check route
table(AllStat$Route, useNA="ifany")

#Select the prescription that are for our patients
Dates<-select(HospAll, patid, enter, end)
AllStat<-merge(x=AllStat, y=HospAll, by="patid", all.x=TRUE, all.y=FALSE)

Stat<-subset(AllStat, issuedate>=(enter-182.625)& issuedate<=end)
#Check which ones have dosage information. If no dosage information look at next one

#No PRN
table(Stat$dosage_text, useNA="ifany")

Stat$PRN<-case_when(grepl("PRN|REQ|NECESSARY|MDU|UNKNOWN|NEEDED", Stat$dosage_text, ignore.case=TRUE) ~1,
                        TRUE ~0)
table(Stat$PRN)

Check<-subset(Stat, PRN==1)

#Not really PRN, ignore
Stat<-select(Stat, -PRN)

#Remove those not issued
Stat$NotIssued<-case_when(grepl("not issue", Stat$dosage_text, ignore.case=TRUE) ~1,
                          TRUE ~0)

Check<-subset(Stat, NotIssued==1)
Stat<-subset(Stat, NotIssued==0)
Stat<-select(Stat, -NotIssued)
#Check daily dose
table(Stat$daily_dose, useNA="ifany")
DD<-subset(Stat, is.na(Stat$daily_dose)|Stat$daily_dose==0) 

#Daily dose from text
DD<-subset(DD, !is.na(dosage_text))
DD<-select(DD, dosage_text)
DD<-distinct(DD)

DD$Quantity<-0
DD$Quantity[DD$dosage_text=="28"]<-28

DD$dose<-0
DD$dose<-case_when(DD$dosage_text=="1 THREE A DAY" ~ 3,
                   DD$dosage_text=="14" ~ 14,
                   DD$dosage_text=="13" ~ 13,
                   DD$dosage_text=="12" ~ 12,
                   DD$dosage_text=="10 D" ~ 10,
                   DD$dosage_text=="10 MLS"|DD$dosage_text=="10" ~ 10,
                   DD$dosage_text=="5-10MLS"|DD$dosage_text=="5-10MLTDSPR" ~ 7.5,
                   DD$dosage_text=="5ML A DAY"|DD$dosage_text=="50D"|DD$dosage_text=="FIVE" ~ 5,
                   grepl("1.5", DD$dosage_text) | DD$dosage_text=="1 OR 2"|DD$dosage_text=="ONE AND A H"|	DD$dosage_text=="ONE AND A HALF"|
                     DD$dosage_text=="1 AND HALF"|DD$dosage_text=="TAKE ONE AND A HALF"|DD$dosage_text=="1-2"|DD$dosage_text=="1 AND HALF"| DD$dosage_text=="11/2"|
                     DD$dosage_text=="TAKE ONE OR TWO"|DD$dosage_text=="ONE AND HALF"|	
                     DD$dosage_text=="ONE OR TWO" ~ 1.5,
                   DD$dosage_text=="1000" |DD$dosage_text=="1 MONTH" |DD$dosage_text=="0.1"|DD$dosage_text=="FP34"|DD$dosage_text=="120"|DD$dosage_text==":[100]"|
                     DD$dosage_text=="120"|DD$dosage_text=="180" |DD$dosage_text=="AUTH=12"|DD$dosage_text=="100"|DD$dosage_text=="30"|DD$dosage_text=="38"|DD$dosage_text=="28"~ 0,
                   grepl("DAYS|NURSE", DD$dosage_text, ignore.case=TRUE) ~ 0,
                   DD$dosage_text=="400MG" ~ 400,
                   DD$dosage_text=="200MG" ~ 200,
                   DD$dosage_text=="100MG" ~ 100,
                   DD$dosage_text=="10MG" ~ 10,
                   DD$dosage_text=="10 MG" ~ 10,
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
                   DD$dosage_text=="NIGHTLY"| DD$dosage_text=="DAY"|DD$dosage_text=="O"|DD$dosage_text=="IOD"|DD$dosage_text=="AD"|DD$dosage_text=="A DAY"|DD$dosage_text=="TAKE A DAY"|DD$dosage_text=="0NE A DAY"~ 1,
                   grepl("1|ONE|ONCE|PM", DD$dosage_text) ~ 1,
                   grepl("2|TWO", DD$dosage_text) ~ 2,
                   grepl("3|THREE", DD$dosage_text) ~ 3,
                   grepl("4|FOUR", DD$dosage_text) ~ 4,
                   DD$dosage_text=="ID"|DD$dosage_text=="INOCTE" ~1,
                   TRUE ~ 0)

Check<-subset(DD, dose==0)
table(Check$dosage_text)

DD$Unit<-""
DD$Unit[grepl("mg", DD$dosage_text, ignore.case=TRUE)]<-"MG" 
DD$Unit[grepl("mg", DD$dosage_text, ignore.case=TRUE)]<-"ML" 

#Replace fields in main table if missing
Stat<-merge(x=Stat, y=DD, by="dosage_text", all.x=TRUE, all.y=FALSE)
Stat$daily_dose[is.na(Stat$daily_dose) | Stat$daily_dose==0] <-Stat$dose[is.na(Stat$daily_dose) | Stat$daily_dose==0]
Stat$dose_unit[is.na(Stat$dose_unit) | Stat$dose_unit==0 ]<-Stat$Unit[is.na(Stat$dose_unit) | Stat$dose_unit==0 ]
Stat$quantity[is.na(Stat$quantity) | Stat$quantity==0 ]<-Stat$Quantity[is.na(Stat$quantity) | Stat$quantity==0 ]

Stat<-select(Stat, -dose, -Unit)
table(Stat$daily_dose, useNA="ifany")

#Can we get missing strength from text?

length(which(Stat$strength==""))

ST<-subset(Stat, strength==""|is.na(strength)|strength==0) #no missing strength

#Check strength and convert to mg
#If we have strength take it from there
table(Stat$strength, useNA="ifany")
Stat$StrengthNew<-as.numeric(str_extract(Stat$strength, "\\d+\\.*\\d*"))

table(Stat$StrengthNew, useNA="ifany")
table(Stat$strength, useNA="ifany")

Stat$StrengthNew<-case_when(grepl("microgram",Stat$strength, ignore.case=TRUE) ~ Stat$StrengthNew/1000,
                                grepl("gram", Stat$strength, ignore.case=TRUE) ~ Stat$StrengthNew*1000,
                                grepl("5ml",Stat$strength, ignore.case=TRUE) ~ Stat$StrengthNew/5,
                                grepl("\\+",Stat$strength, ignore.case=TRUE) & grepl("20",Stat$strength, ignore.case=TRUE) ~ 30,
                            grepl("\\+",Stat$strength, ignore.case=TRUE) & grepl("40",Stat$strength, ignore.case=TRUE) ~ 50,
                            grepl("\\+",Stat$strength, ignore.case=TRUE) & grepl("80",Stat$strength, ignore.case=TRUE) ~ 90,
                                TRUE ~ Stat$StrengthNew)



#Calculate daily dose - prn will be set to zero
Stat<-Stat%>%
  mutate(DailyDose=case_when(dose_unit=="ML" ~ daily_dose*StrengthNew,
                             dose_unit=="MG" ~ daily_dose,
                             TRUE ~ daily_dose*StrengthNew))

table(Stat$DailyDose, useNA="ifany")

Check<-subset(Stat, DailyDose==0 | is.na(DailyDose))  

#Check duration and quantity
Quant<-subset(Stat, dose_unit=="ML")
table(Stat$quantity[is.na(Stat$DailyDose)])
Stat$quantity<-case_when(Stat$quantity<0 ~ Stat$quantity*-1, TRUE ~ Stat$quantity)
Check<-subset(Stat, quantity>1000)#Most of the highest ones are in ML

table(Stat$duration)
Stat$duration<-case_when(Stat$duration<0 ~ Stat$duration*-1, TRUE ~ Stat$duration)
length(which(Stat$duration>366))

Stat$duration<-case_when(Stat$duration>366 ~ 0, TRUE ~ Stat$duration)
table(Stat$duration)


#Because I am only interested in those at right dosage, calculate duration to next prescription
Dur<-Stat%>%
  group_by(patid)%>%
  arrange(issuedate)%>%
  mutate(TimeToNext=lead(issuedate)-issuedate)

Dur$TimeToNext<-as.numeric(Dur$TimeToNext)

Dur<-Dur%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(end-issuedate),
                              TRUE ~ TimeToNext))

#If time to next is zero set it to be the next time to next
Zeros<-Dur%>%
  subset(TimeToNext==0)%>%
  group_by(patid)%>%
  summarise(Zero=n())

#If its zero take  the biggest on that day
Dur<-Dur%>%
  mutate(patdate=paste0(patid, issuedate))%>%
  group_by(patdate)%>%
  mutate(TimeToNext=case_when(TimeToNext==0 ~ max(TimeToNext),
                              TRUE ~ TimeToNext))

length(which(Dur$TimeToNext==0))

#Are these zero because they are at the same time as end?
length(which(Dur$TimeToNext==0&Dur$issuedate==Dur$end)) # Yes they are - leave them in as they wont be the ones we need anyway, and coverage will be set to NA

#If daily dose is unknown take quantity by duration

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
Dur$Coverage[Dur$TimeToNext>366]<-NA

summary(Dur$DailyDose)
summary(Dur$Coverage)

####Valid dose####
#Atorvastatin 10mg to 80mg
#Simvastatin 10mg to 80mg
#Pravastatin 10mg to 40mg - But latest NICE has 5 mg so change to 5
#Rosuvastatin 5mg to 40mg
#Fluvastatin 20mg to 80mg


table(Dur$productname[Dur$Statin=="Other"])

Dur<-Dur%>%
  group_by(patdate, Statin)%>%
  mutate(TotalDose1=sum(DailyDose, na.rm=TRUE), TotalDose2=sum(CalcDose, na.rm=TRUE),TotalDose3=sum(CalcDose2, na.rm=TRUE),
         TotalCoverage1=sum(Coverage), TotalCoverage2=sum(Coverage2))%>%
  ungroup()

Dur$Statin[Dur$Statin=="fluvastatin"]<-"Fluvastatin"

save(Dur, file="VariableExtracts/CPRD2023/MergedObs/StatinDose_interim.Rdata")

length(unique(Dur$patid))
Dur<-subset(Dur, patid %in% HospAll$patid)

#Is the  statin prescription a valid daily dose?
Dur<-Dur%>%
  group_by(patdate, Statin)%>%
  mutate(n=n())%>%
  ungroup()%>%
  mutate(valid=case_when(n==1 & grepl("cerivastatin", productname, ignore.case=TRUE) & TotalDose1>=0.2 & TotalDose1<=0.8 ~ 1,
                         n==1 & Statin=="Simvastatin" & TotalDose1>=9 & TotalDose1<=120~1,
                         n==1 & Statin=="Atorvastatin" & TotalDose1>=9 & TotalDose1<=120~1,
                         n==1 & Statin=="Pravastatin" & TotalDose1>=4 & TotalDose1<=60~1,
                         n==1 & Statin=="Rosuvastatin" & TotalDose1>=4 & TotalDose1<=60~1,
                         n==1 & Statin=="Fluvastatin" & TotalDose1>=19 & TotalDose1<=120~1,
                         n>1 & TotalDose1>=5 & TotalDose1<=120~1,
                         
                         n==1 & grepl("cerivastatin", productname, ignore.case=TRUE) & TotalDose2>=0.2 & TotalDose2<=0.8 ~ 2,
                         n==1 & Statin=="Simvastatin" & TotalDose2>=9 & TotalDose2<=120~2,
                         n==1 & Statin=="Atorvastatin" & TotalDose2>=9 & TotalDose2<=120~2,
                         n==1 & Statin=="Pravastatin" & TotalDose2>=4 & TotalDose2<=60~2,
                         n==1 & Statin=="Rosuvastatin" & TotalDose2>=4 & TotalDose2<=60~2,
                         n==1 & Statin=="Fluvastatin" & TotalDose2>=19 & TotalDose2<=120~2,
                         n>1 & TotalDose2>=5 & TotalDose2<=120~2,
                         
                         n==1 & grepl("cerivastatin", productname, ignore.case=TRUE) & TotalDose3>=0.2 & TotalDose3<=0.8 ~ 3,
                         n==1 & Statin=="Simvastatin" & TotalDose3>=9 & TotalDose3<=120~3,
                         n==1 & Statin=="Atorvastatin" & TotalDose3>=9 & TotalDose3<=120~3,
                         n==1 & Statin=="Pravastatin" & TotalDose3>=4 & TotalDose3<=60~3,
                         n==1 & Statin=="Rosuvastatin" & TotalDose3>=4 & TotalDose3<=60~3,
                         n==1 & Statin=="Fluvastatin" & TotalDose3>=19 & TotalDose3<=120~3,
                         n>1 & TotalDose3>=5 & TotalDose3<=120~3,
                         
                         n==1 & grepl("cerivastatin", productname, ignore.case=TRUE) & TotalCoverage1>=0.2 & TotalCoverage1<=0.8 ~ 4,
                         n==1 & Statin=="Simvastatin" & TotalCoverage1>=9 & TotalCoverage1<=120~4,
                         n==1 & Statin=="Atorvastatin" & TotalCoverage1>=9 & TotalCoverage1<=120~4,
                         n==1 & Statin=="Pravastatin" & TotalCoverage1>=4 & TotalCoverage1<=60~4,
                         n==1 & Statin=="Rosuvastatin" & TotalCoverage1>=4 & TotalCoverage1<=60~4,
                         n==1 & Statin=="Fluvastatin" & TotalCoverage1>=19 & TotalCoverage1<=120~4,
                         n>1 & TotalCoverage1>=5 & TotalCoverage1<=120~4,
                         
                         n==1 & grepl("cerivastatin", productname, ignore.case=TRUE) & TotalCoverage2>=0.2 & TotalCoverage2<=0.8 ~ 5,
                         n==1 & Statin=="Simvastatin" & TotalCoverage2>=9 & TotalCoverage2<=120~5,
                         n==1 & Statin=="Atorvastatin" & TotalCoverage2>=9 & TotalCoverage2<=120~5,
                         n==1 & Statin=="Pravastatin" & TotalCoverage2>=4 & TotalCoverage2<=60~5,
                         n==1 & Statin=="Rosuvastatin" & TotalCoverage2>=4 & TotalCoverage2<=60~5,
                         n==1 & Statin=="Fluvastatin" & TotalCoverage2>=19 & TotalCoverage2<=120~5,
                         n>1 & TotalCoverage2>=5 & TotalCoverage2<=120~5,
                         TRUE ~ 0))

Check<-subset(Dur, valid==0)

Dur<-Dur%>%
  mutate(TotalDose=case_when(valid==1 ~ TotalDose1,
                             valid==2 ~ TotalDose2,
                             valid==3 ~ TotalDose3,
                             valid==4 ~ TotalCoverage1,
                             valid==5 ~ TotalCoverage2,
                             TRUE ~ NA_real_))

Valid<-Dur%>%
  subset(valid>0)%>%
  select(patid, TotalDose, issuedate, patdate, Statin)%>%
  distinct()
length(unique(Valid$patdate))

ValidMulti<-Dur%>%
  subset(valid==0)%>%
  group_by(patdate, DailyDose, Coverage, Statin)%>%
  mutate(Duplicate=n())%>%
  ungroup()%>%
  group_by(patdate)%>%
  mutate(n=n())%>%
  subset(Duplicate==n & n>1)%>% #Those that have multiple prescriptions that are all the same
  mutate(NewDose1=TotalDose1/n, NewDose2=TotalDose2/n,NewDose3=TotalDose3/n,NewCoverage1=TotalCoverage1/n, NewCoverage2=TotalCoverage2/n)%>%
  mutate(valid=case_when(grepl("cerivastatin", productname, ignore.case=TRUE) & NewDose1>=0.2 & NewDose1<=0.8 ~ 1,
                         Statin=="Simvastatin" & NewDose1>=9 & NewDose1<=120~1,
                         Statin=="Atorvastatin" & NewDose1>=9 & NewDose1<=120~1,
                         Statin=="Pravastatin" & NewDose1>=4 & NewDose1<=60~1,
                         Statin=="Rosuvastatin" & NewDose1>=4 & NewDose1<=60~1,
                         Statin=="Fluvastatin" & NewDose1>=19 & NewDose1<=120~1,
                        
                         grepl("cerivastatin", productname, ignore.case=TRUE) & NewDose2>=0.2 & NewDose2<=0.8 ~ 2,
                         Statin=="Simvastatin" & NewDose2>=9 & NewDose2<=120~2,
                         Statin=="Atorvastatin" & NewDose2>=9 & NewDose2<=120~2,
                         Statin=="Pravastatin" & NewDose2>=4 & NewDose2<=60~2,
                         Statin=="Rosuvastatin" & NewDose2>=4 & NewDose2<=60~2,
                         Statin=="Fluvastatin" & NewDose2>=19 & NewDose2<=120~2,
       
                        grepl("cerivastatin", productname, ignore.case=TRUE) & NewDose3>=0.2 & NewDose3<=0.8 ~ 3,
                        Statin=="Simvastatin" & NewDose3>=9 & NewDose3<=120~3,
                        Statin=="Atorvastatin" & NewDose3>=9 & NewDose3<=120~3,
                        Statin=="Pravastatin" & NewDose3>=4 & NewDose3<=60~3,
                        Statin=="Rosuvastatin" & NewDose3>=4 & NewDose3<=60~3,
                        Statin=="Fluvastatin" & NewDose3>=19 & NewDose3<=120~3,
                        
                        grepl("cerivastatin", productname, ignore.case=TRUE) & NewCoverage1>=0.2 & NewCoverage1<=0.8 ~ 4,
                        Statin=="Simvastatin" & NewCoverage1>=9 & NewCoverage1<=120~4,
                        Statin=="Atorvastatin" & NewCoverage1>=9 & NewCoverage1<=120~4,
                        Statin=="Pravastatin" & NewCoverage1>=4 & NewCoverage1<=60~4,
                        Statin=="Rosuvastatin" & NewCoverage1>=4 & NewCoverage1<=60~4,
                        Statin=="Fluvastatin" & NewCoverage1>=19 & NewCoverage1<=120~4,
                      
                        grepl("cerivastatin", productname, ignore.case=TRUE) & NewCoverage2>=0.2 & NewCoverage2<=0.8 ~ 5,
                        Statin=="Simvastatin" & NewCoverage2>=9 & NewCoverage2<=120~5,
                        Statin=="Atorvastatin" & NewCoverage2>=9 & NewCoverage2<=120~5,
                        Statin=="Pravastatin" & NewCoverage2>=4 & NewCoverage2<=60~5,
                        Statin=="Rosuvastatin" & NewCoverage2>=4 & NewCoverage2<=60~5,
                        Statin=="Fluvastatin" & NewCoverage2>=19 & NewCoverage2<=120~5,
                        
                         TRUE ~ 0))

ValidMulti<-ValidMulti%>%
  mutate(TotalDose=case_when(valid==1 ~ NewDose1,
                             valid==2 ~ NewDose2,
                             valid==3 ~ NewDose3,
                             valid==4 ~ NewCoverage1,
                             valid==5 ~ NewCoverage2,
                             TRUE ~ NA_real_))


ValidMulti<-ValidMulti%>%
  ungroup()%>%
  subset(valid>0)%>%
  select(patid, TotalDose, issuedate, patdate, Statin)%>%
  distinct()

length(unique(ValidMulti$patdate))

Valid<-rbind(Valid, ValidMulti)
length(unique(Valid$patdate))

#Check the invalid ones
NotValid<-subset(Dur, !(patdate %in% Valid$patdate)) #60163/5125513


####For all obs####
Valid<-Valid%>%
  mutate(StatDose = case_when(Statin=="Atorvastatin" & TotalDose<20 ~ "Moderate",
                              Statin=="Atorvastatin" & TotalDose>=20 ~ "High", 
                              Statin=="Simvastatin" & TotalDose<20 ~ "Low", 
                              Statin=="Simvastatin" & TotalDose>=20 & TotalDose<=40~ "Moderate", 
                              Statin=="Simvastatin" & TotalDose>40 ~ "High", 
                              Statin=="Pravastatin" & TotalDose<=40 ~ "Low", 
                              Statin=="Pravastatin" & TotalDose>40 ~ "Moderate",
                              Statin=="Rosuvastatin" & TotalDose<10 ~ "Moderate", 
                              Statin=="Rosuvastatin" & TotalDose>=10 ~ "High",
                               TRUE ~ "Missing"))

table(Valid$StatDose)
Check<-subset(Valid, StatDose=="Missing")
table(Check$Statin)
table(Valid$Statin[Valid$Statin!="Other" & Valid$Statin!="Fluvastatin" & Valid$StatDose!="Missing"], Valid$StatDose[Valid$Statin!="Other" & Valid$Statin!="Fluvastatin" & Valid$StatDose!="Missing"])

####Save####
save(Valid, file="StatinCPRD/Data/ValidStatin.Rdata")
