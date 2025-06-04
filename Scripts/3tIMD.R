####Number of consultations in the year prior to diagnosis####
rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/Trial_CompletePlus.rdata")
library(haven)
library(dplyr)
library(lubridate)

####How many are eligible####
table(Trial$lsoa_e)
PatIMDA<-read.table("Linkages/Results/Aurum_linked/Final/IMD/patient_2019_imd_21_000729.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(patid="character", pracid="character"))
PracIMDA<-read.table("Linkages/Results/Aurum_linked/Final/IMD/practice_imd_21_000729.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(pracid="character"))

PatIMDG<-read.table("Linkages/Results/Final Gold/IMD/patient_2019_imd_21_000729.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(patid="character", pracid="character"))
PracIMDG<-read.table("Linkages/Results/Final Gold/IMD/practice_imd_21_000729.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(pracid="character"))

PatIMDA$patid<-paste0(PatIMDA$patid, "-A")
PatIMDG$patid<-paste0(PatIMDG$patid, "-G")

PracIMDA$pracid<-paste0(PracIMDA$pracid, "-A")
PracIMDG$pracid<-paste0(PracIMDG$pracid, "-G")

PatIMD<-rbind(PatIMDA, PatIMDG)
PatIMD<-select(PatIMD, patid, PatIMD=e2019_imd_5)
PracIMD<-rbind(PracIMDA, PracIMDG)
PracIMD<-select(PracIMD, pracid, PracIMD=e2019_imd_5)

Trial<-merge(x=Trial, y=PatIMD, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=PracIMD, by="pracid", all.x=TRUE, all.y=FALSE)

save(Trial, file="StatinCPRD/Data/Trial_CompletePlusIMD.rdata")
