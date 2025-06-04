#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pull in the files from my master clean version and clean for this analysis
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear environment

rm(list = ls(all.names = TRUE))

#~~~Libraries
library(tidyverse)
library(lubridate)

#Load full clean data

load("StatinCPRD/Data/StatAP.Rdata")

####Check dates####

#deathdate: Check all patients that died have a date of death. They do. 
length(which(CPRD$died==1&is.na(CPRD$deathdate)))

#Check eligible diagnosis dates and index dates
summary(CPRD$diagnosis_date)
summary(CPRD$regstartdate)
summary(CPRD$regenddate)
tapply(CPRD$regenddate, CPRD$source, summary)

####Generate new fields/missing data####

#dob: Create a date of birth from year. Set at 01/01 so technically including patients who were diagnosed in the year that they turned 18.
CPRD$dob<-paste0("01/01/", CPRD$yob)
CPRD$dob<-as.Date(CPRD$dob, "%d/%m/%Y")

#Date turned 18 and 100
CPRD$Date100<-CPRD$dob+years(100)
CPRD$Date18<-CPRD$dob+years(18)

#FirstEvidenceofSMI
CPRD$SMIEv<-pmin(CPRD$diagnosis_date, CPRD$FirstPsychDate, na.rm=TRUE)

#Create a enter, start and end date for each patient: For this study 01/01/00 to 31/12/19
CPRD$enter<-pmax(CPRD$regstartdate, CPRD$Date18, CPRD$SMIEv, as.Date("2000-01-01"), na.rm=TRUE)
CPRD$end<-pmin(as.Date("2019-12-31"), CPRD$regenddate, CPRD$deathdate, CPRD$lcd, CPRD$Date100, na.rm=TRUE)

Dates<-subset(CPRD, enter>end)

Dates<-select(Dates, patid, regstartdate,Date18, diagnosis_date, regenddate, deathdate, lcd, Date100, enter, end)

summary(CPRD$enter)
summary(CPRD$end)

#Age at enter
CPRD$AgeAtEnter<-as.numeric(CPRD$enter-CPRD$dob)/365.25
summary(CPRD$AgeAtEnter)

#Age at end
CPRD$AgeAtEnd<-as.numeric(CPRD$end-CPRD$dob)/365.25
summary(CPRD$AgeAtEnd)

#Age at SMI diagnosis
CPRD$AgeAtDiag<-as.numeric(CPRD$diagnosis_date-CPRD$dob)/365.25
summary(CPRD$AgeAtDiag)

#Age at first psych prescription
CPRD$AgeAtPsych<-as.numeric(CPRD$FirstPsychDate-CPRD$dob)/365.25
summary(CPRD$AgeAtPsych)

#Age at SMI
CPRD$AgeAtSMIEv<-as.numeric(CPRD$SMIEv-CPRD$dob)/365.25
summary(CPRD$AgeAtSMIEv)

#Age at statin
CPRD$AgeAtStatin<-as.numeric(CPRD$FirstStatDate-CPRD$dob)/365.25
summary(CPRD$AgeAtStatin)

####Drop invalid data####
#Create a table to store the results of exclusions so can easily create a flow chart for pubs
Consort<-data.frame("Count" = length(CPRD$patid), "Description" = c("Original"))

##1/ Age
#Drop those under 18 at diagnosis 
CPRD<-subset(CPRD,(year(CPRD$SMIEv)-CPRD$yob>=18))
Consort1<-data.frame("Count" =length(CPRD$patid), "Description" = c("Under 18 at first SMI evidence"))
Consort<- rbind(Consort,Consort1)
#Drop those over 100 at index
CPRD<-subset(CPRD,(year(CPRD$SMIEv)-CPRD$yob<=100))
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Over 100 at first SMI evidence"))
Consort<- rbind(Consort,Consort1)

##2/ End before index date
CPRD<-subset(CPRD,(end>=SMIEv))
Consort1<-data.frame("Count" =length(CPRD$patid), "Description" = c("Exit before first SMI evidence"))
Consort<- rbind(Consort,Consort1)

##3/Ends before 2000
CPRD<-subset(CPRD, CPRD$end>=as.Date("2000-01-01"))
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Ends before 2000"))
Consort<- rbind(Consort,Consort1)

##4/starts after 2019
CPRD<-subset(CPRD, CPRD$enter<=as.Date("2019-12-31"))
               Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Enters cohort after 2019"))
               Consort<- rbind(Consort,Consort1)

##5/ Drop any that are remaining with start after end 
StartEnd<-subset(CPRD, CPRD$enter>CPRD$end)
StartEnd<-select(StartEnd, patid, enter, end, lcd, regenddate, Date100, deathdate, regstartdate, Date18, diagnosis_date)
CPRD<-subset(CPRD, CPRD$enter<=CPRD$end)
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Main start after Main end"))
Consort<- rbind(Consort,Consort1)

##6/ Drop those who never have a statin before end of follow up
CPRD<-subset(CPRD, !is.na(FirstStatDate) & FirstStatDate<=end)
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Never prescribed statins"))
Consort<- rbind(Consort,Consort1)

##7/ Drop those never prescribed psychotropic medications before end of follow up
CPRD<-subset(CPRD, !is.na(FirstPsychDate) & FirstPsychDate<=end)
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Never prescribed psychotropics"))
Consort<- rbind(Consort,Consort1)


####Save numbers for flow chart####

#Generate consort
Consort
Consort<-Consort %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort<-select(Consort, -Lag)
save(Consort, file = "StatinCPRD/Outputs/FlowChart.Rdata")
write.csv(Consort, file = "StatinCPRD/Outputs/FlowChart.csv")

####What is the time period of the study?####
summary(CPRD$enter)
summary(CPRD$end)
summary(CPRD$diagnosis_date)
summary(CPRD$FirstStatDate)
summary(CPRD$SMIEv)
summary(CPRD$FirstPsychDate)

table(CPRD$source)
table(CPRD$FirstDiag)
table(CPRD$LastDiag)

####Other useful variables####
CPRD$TotalFU<-as.numeric(CPRD$end-CPRD$enter)/365.25
summary(CPRD$TotalFU)

CPRD$StudyFU<-as.numeric(CPRD$end-CPRD$FirstStatDate)/365.25
summary(CPRD$StudyFU)

CPRD$StudyBaseline<-as.numeric(CPRD$FirstStatDate-CPRD$enter)/365.25
summary(CPRD$StudyBaseline)

CPRD$TimeSinceDiag<-CPRD$end-CPRD$diagnosis_date
CPRD$TimeSinceDiag<-as.numeric(CPRD$TimeSinceDiag/365.25)
summary(CPRD$TimeSinceDiag)

CPRD$TimeSincePsych<-CPRD$end-CPRD$FirstPsychDate
CPRD$TimeSincePsych<-as.numeric(CPRD$TimeSincePsych/365.25)
summary(CPRD$TimeSincePsych)

CPRD$TimeSinceStat<-CPRD$end-CPRD$FirstStatDate
CPRD$TimeSinceStat<-as.numeric(CPRD$TimeSinceStat/365.25)
summary(CPRD$TimeSinceStat)

####Sort deaths and check dates####
summary(CPRD$deathdate)
####Sort deaths and check dates####
#Died after exit but within follow up - didnt die in this study

length(which(CPRD$deathdate>"2019-12-31"&CPRD$deathdate<="2023-03-31"))
CPRD$regenddate[CPRD$regenddate>as.Date("2023-03-31")]<-NA
CPRD$died[CPRD$deathdate>"2019-12-31"&CPRD$deathdate<="2023-03-31"]<-0
CPRD$deathdate[CPRD$deathdate>"2019-12-31"&CPRD$deathdate<="2023-03-31"]<-NA

#Died after 100, but within follow up - didnt die in this study
length(which(CPRD$deathdate>CPRD$Date100&CPRD$deathdate<="2023-03-31"))
CPRD$died[CPRD$deathdate>CPRD$Date100&CPRD$deathdate<="2023-03-31"]<-0
CPRD$deathdate[CPRD$deathdate>CPRD$Date100&CPRD$deathdate<="2023-03-31"]<-NA

#Who died after end date (436 patients)
length(which(CPRD$deathdate>CPRD$end))
length(which(CPRD$deathdate>CPRD$end&CPRD$end==CPRD$regenddate))

#If died more than 6 months after tod they didnt die
length(which(CPRD$died==1&is.na(CPRD$deathdate)))
CPRD$DeathTime<-CPRD$deathdate-CPRD$end
length(which(CPRD$DeathTime>182&CPRD$deathdate>CPRD$end))
CPRD$died[CPRD$DeathTime>182&CPRD$deathdate>CPRD$end]<-0
CPRD$deathdate[CPRD$DeathTime>182&CPRD$deathdate>CPRD$end]<-NA

#If died within 6 months, end date become date of death
length(which(CPRD$DeathTime<=182&CPRD$DeathTime>0))
died<-subset(CPRD,CPRD$DeathTime<=182&CPRD$DeathTime>0)
died<-select(died, patid, deathdate, end, regenddate, lcd, Date100, enter, regstartdate, Date18, source, DeathTime)
length(which(died$DeathTime>30))
CPRD$end[CPRD$DeathTime<=182&CPRD$DeathTime>0&!is.na(CPRD$DeathTime)]<-CPRD$deathdate[CPRD$DeathTime<=182&CPRD$DeathTime>0&!is.na(CPRD$DeathTime)]

#Check
CPRD$DeathTime<-if_else(CPRD$deathdate>CPRD$end, as.numeric(CPRD$deathdate-CPRD$end), 0)
length(which(CPRD$DeathTime>182&CPRD$deathdate>CPRD$end))
length(which(CPRD$DeathTime<=182&CPRD$DeathTime>0))
length(which(CPRD$deathdate>CPRD$end))

#index dates
summary(CPRD$diagnosis_date)
summary(CPRD$start)
summary(CPRD$end)
summary(CPRD$deathdate)

#Save clean file
save(CPRD, file = "StatinCPRD/Data/CleanCPRD.Rdata")

#Clear environment

rm(list = ls(all.names = TRUE))
