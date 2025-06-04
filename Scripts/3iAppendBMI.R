####Append BMI in the last 5 years####

library(tidyverse)
library(lubridate)
library(tableone)

#Clear environment

rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/Trial_ethn.rdata")

load("VariableExtracts/CPRD2023/Aurum/BMIAurumTrialPop.Rdata")
load("VariableExtracts/CPRD2023/Gold/BMIGoldTrialPop.Rdata")

BMI<-rbind(BMIFinalA, BMIFinalG)
rm(BMIFinalA, BMIFinalG)

length(unique(BMI$patid))

#Find BMI value in the 3 years before statin - because trial 2 is subset, can just use trial 1
Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date)
BMI<-merge(x=BMI, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Trial 1 & 2
BMIStat<-subset(BMI, !is.na(BMI$Trial1StatDate))

#NUmber with BMI recorded
length(which(!is.na(Trial$Trial1StatDate)))
length(unique(BMIStat$patid)) # 98% complete

BMIStat<-subset(BMIStat, eventdate<=Trial1StatDate)
length(unique(BMIStat$patid)) # 95% complete

#How many have it in 3 years
#3 years = 86% complete
BMI3<-BMIStat%>%
  mutate(Time=Trial1StatDate-eventdate)%>%
  subset(Time<=1095.25)
length(unique(BMI3$patid))

#5 years = 89% complete
BMI5<-BMIStat%>%
  mutate(Time=Trial1StatDate-eventdate)%>%
  subset(Time<=1826.25)

length(unique(BMI5$patid))

####Calculate most recent value####
BMI5Value<-BMI5%>%
  subset(!is.na(value))%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)
  
length(unique(BMI5Value$patid)) #89% complete

#Take unique ones
BMI5Value<-BMI5Value%>%
  group_by(patid)%>%
  mutate(n=n(), EverValue=sum(Type=="Value"), EverCalc=(Type=="Calc"))

Unique<-subset(BMI5Value, n==1)

#If Multiple, take value over calculated and if multiple values take the average
Multi<-subset(BMI5Value, n>1)
  
Values<-Multi%>%
  group_by(patid)%>%
  subset(EverValue>0 & Type=="Value")%>%
  mutate(MeanBMI=mean(value))%>%
  select(patid, value=MeanBMI, BMICat, eventdate, Type, Trial1StatDate, FirstAPTrial3Date, Time)%>%
  distinct()

length(unique(Values$patid))

Unique<-rbind(Unique, Values)

#If just calculations
Calc<-subset(BMI5Value, ! patid %in% Unique$patid)
Calc<-Calc%>%
  group_by(patid)%>%
  mutate(MeanBMI=mean(value))%>%
  select(patid, value=MeanBMI, BMICat, eventdate, Type, Trial1StatDate, FirstAPTrial3Date, Time)%>%
  distinct()

BMIValue5<-rbind(Unique, Calc)
BMIValue5<-select(BMIValue5, -EverValue, -EverCalc, -maxDate, -n)
length(unique(BMIValue5$patid))

#####Calculate most recent category####

#Take most recent category
BMI5Cat<-subset(BMI5, Type=="Cat")

BMI5Cat<-BMI5Cat%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)%>%
  distinct()%>%
  mutate(n=n())

Unique<-subset(BMI5Cat, n==1)

Multi<-subset(BMI5Cat, n>1)

#If multiple, take the worst
Multi<-Multi%>%
  group_by(patid)%>%
  mutate(EverObese=sum(BMICat=="Obese"), EverOver=sum(BMICat=="Overweight"), EverNormal=sum(BMICat=="Normal"))%>%
  mutate(BMICat=case_when(EverObese>0 ~ "Obese",
                          EverOver>0 ~ "Overweight",
                          EverNormal>0 ~ "Normal",
                          TRUE ~ BMICat))%>%
  distinct()%>%
  select(-EverObese, -EverOver, -EverNormal)
length(unique(Multi$patid))

BMICat5<-rbind(Unique, Multi) 
BMICat5<-select(BMICat5, -maxDate, n)

#Combine unique values and categories and calculate category from value
UniqueBMI<-rbind(BMIValue5, BMICat5)

UniqueBMI<-UniqueBMI%>%
  mutate(BMICat=case_when(is.na(BMICat) & value<18.5 ~ "Underweight",
                          is.na(BMICat) & value>=18.5 & value<25 ~ "Normal",
                          is.na(BMICat) & value>=25 & value<30 ~ "Overweight",
                          is.na(BMICat) & value>=30 ~ "Obese",
                          TRUE ~ BMICat))
table(UniqueBMI$BMICat, useNA="ifany")

#Take the most recent
UniqueBMI<-UniqueBMI%>%
  select(-value, -Type, -n)%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)%>%
  distinct()%>%
  mutate(n=n())

length(unique(UniqueBMI$patid))

Unique<-subset(UniqueBMI, n==1)

Multi<-subset(UniqueBMI, n>1)

#If multiple, take the worst
Multi<-Multi%>%
  group_by(patid)%>%
  mutate(EverObese=sum(BMICat=="Obese"), EverOver=sum(BMICat=="Overweight"), EverNormal=sum(BMICat=="Normal"))%>%
  mutate(BMICat=case_when(EverObese>0 ~ "Obese",
                          EverOver>0 ~ "Overweight",
                          EverNormal>0 ~ "Normal",
                          TRUE ~ BMICat))%>%
  distinct()%>%
  select(-EverObese, -EverOver, -EverNormal)

length(unique(Multi$patid))

BMICat<-rbind(Unique, Multi) 

BMIValue<-select(BMIValue5, patid, StatBMIVal=value, StatBMIValDate=eventdate)
BMICat<-select(BMICat, patid, StatBMICat=BMICat, StatBMICatDate=eventdate)

Trial<-merge(x=Trial, y=BMIValue, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=BMICat, by="patid", all.x=TRUE, all.y=FALSE)

####Now remove all but Trial and do again for at time of AP####
rm(list=setdiff(ls(), "Trial"))

load("VariableExtracts/CPRD2023/Aurum/BMIAurumTrialPop.Rdata")
load("VariableExtracts/CPRD2023/Gold/BMIGoldTrialPop.Rdata")

BMI<-rbind(BMIFinalA, BMIFinalG)
rm(BMIFinalA, BMIFinalG)

length(unique(BMI$patid))

#Find BMI value in the 3 years before AP
Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date)
BMI<-merge(x=BMI, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Trial 3
BMIAP<-subset(BMI, !is.na(BMI$FirstAPTrial3Date))

#NUmber with BMI recorded
length(which(!is.na(Trial$FirstAPTrial3Date)))
length(unique(BMIAP$patid)) # 97% complete

BMIAP<-subset(BMIAP, eventdate<=FirstAPTrial3Date)
length(unique(BMIAP$patid)) # 94% complete

#How many have it in 3 years
#3 years = 85% complete
BMI3<-BMIAP%>%
  mutate(Time=FirstAPTrial3Date-eventdate)%>%
  subset(Time<=1095.25)
length(unique(BMI3$patid))

#5 years = 89% complete
BMI5<-BMIAP%>%
  mutate(Time=FirstAPTrial3Date-eventdate)%>%
  subset(Time<=1826.25)

length(unique(BMI5$patid))

####Calculate most recent value####
BMI5Value<-BMI5%>%
  subset(!is.na(value))%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)

length(unique(BMI5Value$patid)) #89% complete

#Take unique ones
BMI5Value<-BMI5Value%>%
  group_by(patid)%>%
  mutate(n=n(), EverValue=sum(Type=="Value"), EverCalc=(Type=="Calc"))

Unique<-subset(BMI5Value, n==1)

#If Multiple, take value over calculated and if multiple values take the average
Multi<-subset(BMI5Value, n>1)

Values<-Multi%>%
  group_by(patid)%>%
  subset(EverValue>0 & Type=="Value")%>%
  mutate(MeanBMI=mean(value))%>%
  select(patid, value=MeanBMI, BMICat, eventdate, Type, Trial1StatDate, FirstAPTrial3Date, Time)%>%
  distinct()

length(unique(Values$patid))

Unique<-rbind(Unique, Values)

#If just calculations
Calc<-subset(BMI5Value, ! patid %in% Unique$patid)
Calc<-Calc%>%
  group_by(patid)%>%
  mutate(MeanBMI=mean(value))%>%
  select(patid, value=MeanBMI, BMICat, eventdate, Type, Trial1StatDate, FirstAPTrial3Date, Time)%>%
  distinct()

BMIValue5<-rbind(Unique, Calc)
BMIValue5<-select(BMIValue5, -EverValue, -EverCalc, -maxDate, -n)
length(unique(BMIValue5$patid))

#####Calculate most recent category####

#Take most recent category
BMI5Cat<-subset(BMI5, Type=="Cat")

BMI5Cat<-BMI5Cat%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)%>%
  distinct()%>%
  mutate(n=n())

Unique<-subset(BMI5Cat, n==1)

Multi<-subset(BMI5Cat, n>1)

#If multiple, take the worst
Multi<-Multi%>%
  group_by(patid)%>%
  mutate(EverObese=sum(BMICat=="Obese"), EverOver=sum(BMICat=="Overweight"), EverNormal=sum(BMICat=="Normal"))%>%
  mutate(BMICat=case_when(EverObese>0 ~ "Obese",
                          EverOver>0 ~ "Overweight",
                          EverNormal>0 ~ "Normal",
                          TRUE ~ BMICat))%>%
  distinct()%>%
  select(-EverObese, -EverOver, -EverNormal)
length(unique(Multi$patid))

BMICat5<-rbind(Unique, Multi) 
BMICat5<-select(BMICat5, -maxDate, n)

#Combine unique values and categories and calculate category from value
UniqueBMI<-rbind(BMIValue5, BMICat5)

UniqueBMI<-UniqueBMI%>%
  mutate(BMICat=case_when(is.na(BMICat) & value<18.5 ~ "Underweight",
                          is.na(BMICat) & value>=18.5 & value<25 ~ "Normal",
                          is.na(BMICat) & value>=25 & value<30 ~ "Overweight",
                          is.na(BMICat) & value>=30 ~ "Obese",
                          TRUE ~ BMICat))
table(UniqueBMI$BMICat, useNA="ifany")

#Take the most recent
UniqueBMI<-UniqueBMI%>%
  select(-value, -Type, -n)%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)%>%
  distinct()%>%
  mutate(n=n())

length(unique(UniqueBMI$patid))

Unique<-subset(UniqueBMI, n==1)

Multi<-subset(UniqueBMI, n>1)

#If multiple, take the worst
Multi<-Multi%>%
  group_by(patid)%>%
  mutate(EverObese=sum(BMICat=="Obese"), EverOver=sum(BMICat=="Overweight"), EverNormal=sum(BMICat=="Normal"))%>%
  mutate(BMICat=case_when(EverObese>0 ~ "Obese",
                          EverOver>0 ~ "Overweight",
                          EverNormal>0 ~ "Normal",
                          TRUE ~ BMICat))%>%
  distinct()%>%
  select(-EverObese, -EverOver, -EverNormal)

length(unique(Multi$patid))

BMICat<-rbind(Unique, Multi) 

BMIValue<-select(BMIValue5, patid, APBMIVal=value, APBMIValDate=eventdate)
BMICat<-select(BMICat, patid, APBMICat=BMICat, APBMICatDate=eventdate)

Trial<-merge(x=Trial, y=BMIValue, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=BMICat, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file="StatinCPRD/Data/Trial_BMI.rdata")

table(Trial$APBMICat, useNA="ifany")

length(which(!is.na(Trial$APBMICat)))/length(which(!is.na(Trial$FirstAPTrial3Date)))*100
table(Trial$StatBMICat, useNA="ifany")

length(which(!is.na(Trial$StatBMICat)))/length(which(!is.na(Trial$Trial1StatDate)))*100
