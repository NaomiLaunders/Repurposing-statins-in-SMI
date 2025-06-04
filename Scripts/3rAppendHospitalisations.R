#~~~Libraries
library(dplyr)
library(lubridate)
library(tableone)

#Clear environment

rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/SH.rdata")

####Spells####
#Pull in hospital file which shows spells
HESHospA<-read.table("Linkages/Results/Aurum_linked/Final/HES APC/hes_hospital_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))
HESHospG<-read.table("Linkages/Results/Final Gold/HES APC/hes_hospital_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))

length(unique(HESHospA$spno))
length(unique(HESHospG$spno))

HESHospA$spno<-paste0("A", HESHospA$spno)
HESHospG$spno<-paste0("G", HESHospG$spno)

length(unique(HESHospA$patid))
length(unique(HESHospG$patid))

HESHospA$patid<-paste0(HESHospA$patid, "-A")
HESHospG$patid<-paste0(HESHospG$patid, "-G")

HESHosp<-rbind(HESHospA, HESHospG)
HESHosp$admidate<-as.Date(HESHosp$admidate, format="%d/%m/%Y")
HESHosp$discharged<-as.Date(HESHosp$discharged, format="%d/%m/%Y")
HESHosp$admimeth<-as.character(HESHosp$admimeth)

#Limit to our patients
HESHosp<-subset(HESHosp, patid %in% Trial$patid)

#Is spell number unique?
length(unique(HESHosp$spno))

#Code up spells with detail. These are not excluded here, this was just a first look...
table(HESHosp$admimeth)

HESHosp$admiDet[HESHosp$admimeth=="11"|HESHosp$admimeth=="12"|HESHosp$admimeth=="13"]<-"Planned"
HESHosp$admiDet[HESHosp$admimeth=="21"]<-"A&E"
HESHosp$admiDet[HESHosp$admimeth=="22"]<-"GP emergency"
HESHosp$admiDet[HESHosp$admimeth=="23"]<-"Bed bureau emergency"
HESHosp$admiDet[HESHosp$admimeth=="24"]<-"Consultant emergency"
HESHosp$admiDet[HESHosp$admimeth=="25"]<-"MH emergency"
HESHosp$admiDet[HESHosp$admimeth=="28"]<-"Other emergency"
HESHosp$admiDet[HESHosp$admimeth=="2A"]<-"Another A&E"
HESHosp$admiDet[HESHosp$admimeth=="2B"]<-"Emergency hospital transfer"
HESHosp$admiDet[HESHosp$admimeth=="2C"]<-"Maternity"
HESHosp$admiDet[HESHosp$admimeth=="2D"]<-"Other emergency"
HESHosp$admiDet[HESHosp$admimeth=="31"]<-"Maternity"
HESHosp$admiDet[HESHosp$admimeth=="32"]<-"Maternity"
HESHosp$admiDet[HESHosp$admimeth=="81"]<-"Planned hospital transfer"
HESHosp$admiDet[HESHosp$admimeth=="82"]<-"Maternity"
HESHosp$admiDet[HESHosp$admimeth=="83"]<-"Maternity"
HESHosp$admiDet[HESHosp$admimeth=="89"]<-"Unknown"
HESHosp$admiDet[HESHosp$admimeth=="98"]<-"Unknown"
HESHosp$admiDet[HESHosp$admimeth=="99"]<-"Unknown"

table(HESHosp$admiDet, useNA="ifany")

#remove spells that are maternity
HESHosp<-subset(HESHosp, HESHosp$admiDet!="Maternity")
length(unique(HESHosp$spno))

#Remove any with missing discharge dates as these are unfinished spells captured in the next year
HESHosp<-subset(HESHosp, !is.na(discharged))

#Bring in episodes
HESEpisodeA<-read.table("Linkages/Results/Aurum_linked/Final/HES APC/hes_episodes_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))
HESEpisodeG<-read.table("Linkages/Results/Final Gold/HES APC/hes_episodes_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))

#Code up to match
HESEpisodeA$spno<-paste0("A", HESEpisodeA$spno)
HESEpisodeG$spno<-paste0("G", HESEpisodeG$spno)

HESEpisodeA$epikey<-paste0("A", HESEpisodeA$epikey)
HESEpisodeG$epikey<-paste0("G", HESEpisodeG$epikey)

HESEpisodeA$patid<-paste0(HESEpisodeA$patid, "-A")
HESEpisodeG$patid<-paste0(HESEpisodeG$patid, "-G")

HESEpisode<-rbind(HESEpisodeA, HESEpisodeG)

#Limit it to our patients
HESEpisode<-subset(HESEpisode, patid %in% Trial$patid)

#Is spell number unique to a patient?
HESEpisode<-HESEpisode%>%
  group_by(spno)%>%
  mutate(patid_n=length(unique(patid)))
table(HESEpisode$patid_n)
#Yes

#Is episode number unique to a patient?
HESEpisode<-HESEpisode%>%
  ungroup()%>%
  group_by(epikey)%>%
  mutate(patid_n=length(unique(patid)))
table(HESEpisode$patid_n)
#Yes

#Spells can have multiple episodes, but episodes should only have one spell
HESEpisode<-HESEpisode%>%
  ungroup()%>%
  group_by(epikey)%>%
  mutate(spno_n=length(unique(spno)))%>%
  ungroup()
table(HESEpisode$spno_n)
#Yes.

#Limit it to spells that aren't maternity/unfinished
HESEpisode<-subset(HESEpisode, spno %in% HESHosp$spno)
#Just check
length(which(!(HESEpisode$patid %in% Trial$patid)))
length(which(!(HESEpisode$spno %in% HESHosp$spno)))
length(which(!(HESEpisode$patid %in% HESHosp$patid)))

#Are there any class 2 (day case) emergency admissions
table(HESEpisode$classpat, HESEpisode$admimeth)

#Just check regular attenders
CheckDay<-subset(HESEpisode, classpat==3&eorder==1)
length(unique(CheckDay$spno))

CheckNight<-subset(HESEpisode, classpat==4&eorder==1)
length(unique(CheckNight$spno))

#Only keep episodes that are ordinary or day admissions
table(HESEpisode$classpat, useNA="ifany")
HESEpisode<-subset(HESEpisode, classpat==1|classpat==2)

#Only keep episodes where epitype is "general episode" - the rest are maternity or a weird psych one that's rarely used.
table(HESEpisode$epitype, useNA="ifany")
Checkbirth<-subset(HESEpisode, (epitype==2| epitype==3) &eorder==1)
HESEpisode<-subset(HESEpisode, epitype==1)

#Only keep spells that have one episode at least
HESHosp<-subset(HESHosp, spno %in% HESEpisode$spno)

#Merge back to spells and count episodes per spell
EpisodePerSpell<-HESEpisode%>%
  group_by(spno)%>%
  summarise(EpiPerSpell=n(), minEOrder=min(eorder))

length(unique(EpisodePerSpell$spno))

#Only include spells with at least one episode (because I've excluded the episodes)
table(EpisodePerSpell$EpiPerSpell, useNA="ifany")
Spell<-merge(x=HESHosp, y=EpisodePerSpell, by="spno", all.x=FALSE, all.y=TRUE)

#Only include those which have a first episode. If they don't it's because I've already excluded the first episode and so it's not a hospitalisation that I'm interested in
length(which(Spell$minEOrder>1))
table(Spell$EpiPerSpell, useNA="ifany")
Spell<-subset(Spell, minEOrder==1)

#Remove those episodes as well
HESEpisode<-subset(HESEpisode, spno %in% Spell$spno)
Spell<-subset(Spell, spno %in% HESEpisode$spno)

#How many are split between years: very few - so after checking I chose to ignore this.(111 potential)
Spell$DischargeDay<-day(Spell$discharged)
Spell$DischargeMonth<-month(Spell$discharged)
Spell$AdmiDay<-day(Spell$admidate)
Spell$AdmiMonth<-month(Spell$admidate)

Spell<-Spell%>%
  group_by(patid)%>%
  mutate(SpellPerPat=n())

length(which(Spell$DischargeDay==31&Spell$DischargeMonth==3))
PotentialSPlitEpisodesDis<-subset(Spell, Spell$DischargeDay==31&Spell$DischargeMonth==3&SpellPerPat>1)
PotentialSPlitEpisodesDis<-select(PotentialSPlitEpisodesDis, patid, admidate, discharged, admimeth, dismeth, duration)
PotentialSPlitEpisodesAdm<-subset(Spell, Spell$AdmiDay==1&Spell$AdmiMonth==4&SpellPerPat>1)
PotentialSPlitEpisodesAdm<-select(PotentialSPlitEpisodesAdm, patid, admidate, discharged, admimeth, dismeth, duration)

Split<-merge(x=PotentialSPlitEpisodesDis, y=PotentialSPlitEpisodesAdm, by="patid", all.x=FALSE, all.y=FALSE)
Split<-subset(Split, year(admidate.x)==year(admidate.y))

#How many spells per patient do I currently have?
table(Spell$SpellPerPat)

#Pull in all diagnoses
AllDiagA<-read.table("Linkages/Results/Aurum_linked/Final/HES APC/hes_diagnosis_epi_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))
AllDiagG<-read.table("Linkages/Results/Final Gold/HES APC/hes_diagnosis_epi_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))

AllDiagA$spno<-paste0("A", AllDiagA$spno)
AllDiagG$spno<-paste0("G", AllDiagG$spno)

AllDiagA$epikey<-paste0("A", AllDiagA$epikey)
AllDiagG$epikey<-paste0("G", AllDiagG$epikey)

AllDiagA$patid<-paste0(AllDiagA$patid, "-A")
AllDiagG$patid<-paste0(AllDiagG$patid, "-G")

AllDiag<-rbind(AllDiagA, AllDiagG)

AllDiag$ICD<-as.character(AllDiag$ICD)
AllDiag$ICDx<-as.character(AllDiag$ICDx)

#Limit to our patients
AllDiag<-subset(AllDiag, patid %in% Trial$patid)

#Is spell number unique?
AllDiag<-AllDiag%>%
  group_by(spno)%>%
  mutate(patid_n=length(unique(patid)))
table(AllDiag$patid_n)

#Only select those diagnoses where the spell number is in the list of spells
AllDiag<-subset(AllDiag, spno %in% Spell$spno)
length(which(Spell$spno %in% AllDiag$spno))
table(Spell$spno[!(Spell$spno %in% AllDiag$spno)])

#Is epikey unique for each patient
AllDiag<-AllDiag%>%
  ungroup()%>%
  group_by(epikey)%>%
  mutate(patid_n=length(unique(patid)))
table(AllDiag$patid_n)

#Is epikey unique for each spell
AllDiag<-AllDiag%>%
  ungroup()%>%
group_by(epikey)%>%
  mutate(spno_n_n=length(unique(spno)))
table(AllDiag$spno_n_n)

#Only select those for episodes that we have included
AllDiag<-subset(AllDiag, epikey %in% HESEpisode$epikey)

#JustCheck
length(which(!(AllDiag$patid %in% Trial$patid)))
length(which(!(AllDiag$spno %in% Spell$spno)))
length(which(!(AllDiag$patid %in% Spell$patid)))
length(which(!(AllDiag$patid %in% HESEpisode$patid)))
length(which(!(AllDiag$spno %in% HESEpisode$spno)))
length(which(!(AllDiag$epikey %in% HESEpisode$epikey)))

#Split into all, first ep and first ep primary diag
FirstEp<-HESEpisode
FirstEp<-subset(FirstEp, spno %in% Spell$spno)
FirstEp<-subset(FirstEp, eorder==1)

FirstEpDiag<-subset(AllDiag, epikey %in% FirstEp$epikey)

PrimaryDiag<-subset(FirstEpDiag, d_order==1)
length(unique(PrimaryDiag$epikey))

#Check primary diagnosis:
table(PrimaryDiag$ICD)

#Mental health
length(which(startsWith((PrimaryDiag$ICD), "F")))
length(which(startsWith((PrimaryDiag$ICD), "R41")))#disorientation/amnesia
length(which(startsWith((PrimaryDiag$ICD), "R44")))#hallucinations
length(which(startsWith((PrimaryDiag$ICD), "R45")))#Emotional state
length(which(startsWith((PrimaryDiag$ICD), "R46")))#Appearance and behaviour

length(which(startsWith((PrimaryDiag$ICD), "Z00.4")))#General psychiatric examination, not elsewhere classified
length(which(startsWith((PrimaryDiag$ICD), "Z03.2")))#Observation for suspected mental and behavioural disorders
length(which(startsWith((PrimaryDiag$ICD), "Z13.3")))#Special screening examination for mental and behavioural disorders
length(which(startsWith((PrimaryDiag$ICD), "Z50.4")))
#length(which(startsWith((PrimaryDiag$ICD), "Z54.3")))#Exclude unless including planned
#length(which(startsWith((PrimaryDiag$ICD), "Z86.5")))#Exclude as is history but might be worth considering
#length(which(startsWith((PrimaryDiag$ICD), "Z91.4")))#Exclude as is history but might be worth considering
#length(which(startsWith((PrimaryDiag$ICD), "Z09.3")))#Exclude unless including planned
#length(which(startsWith((PrimaryDiag$ICD), "Z13.4")))#Exclude as in childhood
length(which(startsWith((PrimaryDiag$ICD), "Z73")))
table(PrimaryDiag$ICD[startsWith((PrimaryDiag$ICD), "Z73")])

#Self-harm
length(which(startsWith((PrimaryDiag$ICD), "Y1")))
length(which(startsWith((PrimaryDiag$ICD), "Y2")))
length(which(startsWith((PrimaryDiag$ICD), "Y3")))
length(which(startsWith((PrimaryDiag$ICD), "X6")))
length(which(startsWith((PrimaryDiag$ICD), "X7")))
length(which(startsWith((PrimaryDiag$ICD), "X8")))
length(which(startsWith((PrimaryDiag$ICD), "Y87.0")))

#Accidents and injuries and phys health see previous publications

####Code these up####

#Primary diagnosis of first episode
#MH
PrimaryDiag$PrimaryMH<-0
PrimaryDiag$PrimaryMH[startsWith((PrimaryDiag$ICD), "F")]<-1

#Vague MH
PrimaryDiag$PrimaryPossMH<-0
PrimaryDiag$PrimaryPossMH[startsWith((PrimaryDiag$ICD), "R41")|startsWith((PrimaryDiag$ICD), "R44")|startsWith((PrimaryDiag$ICD), "R45")|
                            startsWith((PrimaryDiag$ICD), "R46")|startsWith((PrimaryDiag$ICD), "Z00.4")|startsWith((PrimaryDiag$ICD), "Z03.2")|
                            startsWith((PrimaryDiag$ICD), "Z13.3")|startsWith((PrimaryDiag$ICD), "Z50.4")|startsWith((PrimaryDiag$ICD), "Z73")]<-1

#Self harm
PrimaryDiag$PrimarySH<-0
PrimaryDiag$PrimarySH[startsWith((PrimaryDiag$ICD), "Y1")|startsWith((PrimaryDiag$ICD), "Y2")|startsWith((PrimaryDiag$ICD), "Y3")|
                        startsWith((PrimaryDiag$ICD), "X6")|startsWith((PrimaryDiag$ICD), "X7")|startsWith((PrimaryDiag$ICD), "X8")|
                        startsWith((PrimaryDiag$ICD), "Y87.0")]<-1

#Physical
PrimaryDiag$physical<-0
PrimaryDiag$physical[startsWith((PrimaryDiag$ICD), "A")|startsWith((PrimaryDiag$ICD), "B")|
                       startsWith((PrimaryDiag$ICD), "C")|startsWith((PrimaryDiag$ICD), "D0")|startsWith((PrimaryDiag$ICD), "D1")|
                       startsWith((PrimaryDiag$ICD), "D2")|startsWith((PrimaryDiag$ICD), "D3")|startsWith((PrimaryDiag$ICD), "D4")
                     |startsWith((PrimaryDiag$ICD), "D5")|startsWith((PrimaryDiag$ICD), "D6")|startsWith((PrimaryDiag$ICD), "D7")|
                       startsWith((PrimaryDiag$ICD), "D8")|startsWith((PrimaryDiag$ICD), "E")|startsWith((PrimaryDiag$ICD), "G")
                     |startsWith((PrimaryDiag$ICD), "H0")|startsWith((PrimaryDiag$ICD), "H1")|startsWith((PrimaryDiag$ICD), "H2")|
                       startsWith((PrimaryDiag$ICD), "H3")|startsWith((PrimaryDiag$ICD), "H4")|startsWith((PrimaryDiag$ICD), "H5")|
                       startsWith((PrimaryDiag$ICD), "H6")|startsWith((PrimaryDiag$ICD), "H7")|startsWith((PrimaryDiag$ICD), "H8")|
                       startsWith((PrimaryDiag$ICD), "H9")|startsWith((PrimaryDiag$ICD), "I")|startsWith((PrimaryDiag$ICD), "J")|
                       startsWith((PrimaryDiag$ICD), "K")|startsWith((PrimaryDiag$ICD), "L")|startsWith((PrimaryDiag$ICD), "M")|startsWith((PrimaryDiag$ICD), "N")|
                       startsWith((PrimaryDiag$ICD), "R0")|startsWith((PrimaryDiag$ICD), "R1")|startsWith((PrimaryDiag$ICD), "R20")|startsWith((PrimaryDiag$ICD), "R21")|startsWith((PrimaryDiag$ICD), "R22")|startsWith((PrimaryDiag$ICD), "R23")|
                       startsWith((PrimaryDiag$ICD), "R25")|startsWith((PrimaryDiag$ICD), "R26")|startsWith((PrimaryDiag$ICD), "R27")|startsWith((PrimaryDiag$ICD), "R28")|startsWith((PrimaryDiag$ICD), "R29")|
                       startsWith((PrimaryDiag$ICD), "R3") |startsWith((PrimaryDiag$ICD), "R50")|startsWith((PrimaryDiag$ICD), "R51")|startsWith((PrimaryDiag$ICD), "R52")|
                       startsWith((PrimaryDiag$ICD), "R53")|startsWith((PrimaryDiag$ICD), "R55")|startsWith((PrimaryDiag$ICD), "R56")|startsWith((PrimaryDiag$ICD), "R57")|
                       startsWith((PrimaryDiag$ICD), "R59")|startsWith((PrimaryDiag$ICD), "R64")|startsWith((PrimaryDiag$ICD), "R65")|startsWith((PrimaryDiag$ICD), "R68.0")|
                       startsWith((PrimaryDiag$ICD), "R70")|startsWith((PrimaryDiag$ICD), "R71")|startsWith((PrimaryDiag$ICD), "R72")|startsWith((PrimaryDiag$ICD), "R73")|
                       startsWith((PrimaryDiag$ICD), "R74")|startsWith((PrimaryDiag$ICD), "R75")|startsWith((PrimaryDiag$ICD), "R76")|startsWith((PrimaryDiag$ICD), "R77")|
                       startsWith((PrimaryDiag$ICD), "R79")|startsWith((PrimaryDiag$ICD), "R80")|startsWith((PrimaryDiag$ICD), "R81")|startsWith((PrimaryDiag$ICD), "R82.0")|
                       startsWith((PrimaryDiag$ICD), "R82.1")|startsWith((PrimaryDiag$ICD), "R82.2")|startsWith((PrimaryDiag$ICD), "R82.3")
                     |startsWith((PrimaryDiag$ICD), "R82.4")|startsWith((PrimaryDiag$ICD), "R82.6")|startsWith((PrimaryDiag$ICD), "R82.7")|
                       startsWith((PrimaryDiag$ICD), "R82.8")|startsWith((PrimaryDiag$ICD), "R82.9")|
                       startsWith((PrimaryDiag$ICD), "R83")|startsWith((PrimaryDiag$ICD), "R84")|startsWith((PrimaryDiag$ICD), "R85")|
                       startsWith((PrimaryDiag$ICD), "R86")|startsWith((PrimaryDiag$ICD), "R87")|startsWith((PrimaryDiag$ICD), "R88")
                     |startsWith((PrimaryDiag$ICD), "R89")|
                       startsWith((PrimaryDiag$ICD), "R90")|startsWith((PrimaryDiag$ICD), "R91")|startsWith((PrimaryDiag$ICD), "R92")|
                       startsWith((PrimaryDiag$ICD), "R93")|startsWith((PrimaryDiag$ICD), "R94")|
                       startsWith((PrimaryDiag$ICD), "U0")|startsWith((PrimaryDiag$ICD), "U1")|startsWith((PrimaryDiag$ICD), "U2")|
                       startsWith((PrimaryDiag$ICD), "U3")|startsWith((PrimaryDiag$ICD), "U4")|
                       startsWith((PrimaryDiag$ICD), "U8")|startsWith((PrimaryDiag$ICD), "Z00.0")|startsWith((PrimaryDiag$ICD), "Z00.1")|startsWith((PrimaryDiag$ICD), "Z00.2")|
                       startsWith((PrimaryDiag$ICD), "Z00.3")|startsWith((PrimaryDiag$ICD), "Z00.6")|startsWith((PrimaryDiag$ICD), "Z00.7")|startsWith((PrimaryDiag$ICD), "Z00.8")|
                       startsWith((PrimaryDiag$ICD), "Z01")|startsWith((PrimaryDiag$ICD), "Z02")|startsWith((PrimaryDiag$ICD), "Z03.0")|
                       startsWith((PrimaryDiag$ICD), "Z03.1")|startsWith((PrimaryDiag$ICD), "Z03.3")|startsWith((PrimaryDiag$ICD), "Z03.4")|startsWith((PrimaryDiag$ICD), "Z03.5")|
                       startsWith((PrimaryDiag$ICD), "Z03.8")|startsWith((PrimaryDiag$ICD), "Z03.9")|startsWith((PrimaryDiag$ICD), "Z08")|
                       startsWith((PrimaryDiag$ICD), "Z09.1")|startsWith((PrimaryDiag$ICD), "Z09.2") |startsWith((PrimaryDiag$ICD), "Z10")|
                       startsWith((PrimaryDiag$ICD), "Z11")|startsWith((PrimaryDiag$ICD), "Z12")|startsWith((PrimaryDiag$ICD), "Z13.0")|startsWith((PrimaryDiag$ICD), "Z13.1")|startsWith((PrimaryDiag$ICD), "Z13.2")|
                       startsWith((PrimaryDiag$ICD), "Z13.5")|startsWith((PrimaryDiag$ICD), "Z13.6")|startsWith((PrimaryDiag$ICD), "Z13.8")|startsWith((PrimaryDiag$ICD), "Z13.9")|
                       startsWith((PrimaryDiag$ICD), "Z40")|startsWith((PrimaryDiag$ICD), "Z45")|startsWith((PrimaryDiag$ICD), "Z49")|startsWith((PrimaryDiag$ICD), "Z50")|
                       startsWith((PrimaryDiag$ICD), "Z51.0")|startsWith((PrimaryDiag$ICD), "Z51.1")|startsWith((PrimaryDiag$ICD), "Z51.2")|
                       startsWith((PrimaryDiag$ICD), "Z51.6")|startsWith((PrimaryDiag$ICD), "Z54.1")|startsWith((PrimaryDiag$ICD), "Z54.2")|
                       startsWith((PrimaryDiag$ICD), "Z71.3")|startsWith((PrimaryDiag$ICD), "Z71.7")|startsWith((PrimaryDiag$ICD), "Z80")|
                       startsWith((PrimaryDiag$ICD), "Z85")|startsWith((PrimaryDiag$ICD), "Z86.0")|startsWith((PrimaryDiag$ICD), "Z86.1")|startsWith((PrimaryDiag$ICD), "Z86.2")|
                       startsWith((PrimaryDiag$ICD), "Z86.3")|startsWith((PrimaryDiag$ICD), "Z86.6")|startsWith((PrimaryDiag$ICD), "Z86.7")|
                       startsWith((PrimaryDiag$ICD), "Z87.0")|startsWith((PrimaryDiag$ICD), "Z87.1")|startsWith((PrimaryDiag$ICD), "Z87.2")|
                       startsWith((PrimaryDiag$ICD), "Z87.3")|startsWith((PrimaryDiag$ICD), "Z87.4")|startsWith((PrimaryDiag$ICD), "Z87.6")|startsWith((PrimaryDiag$ICD), "Z87.7")|startsWith((PrimaryDiag$ICD), "Z87.8")|
                       startsWith((PrimaryDiag$ICD), "Z88")|startsWith((PrimaryDiag$ICD), "Z91.0")|startsWith((PrimaryDiag$ICD), "Z94")|startsWith((PrimaryDiag$ICD), "Z95")|startsWith((PrimaryDiag$ICD), "Z2")]<-1

PrimaryDiag$physical[startsWith((PrimaryDiag$ICD), "Z50.2")|startsWith((PrimaryDiag$ICD), "Z50.3")]<-0

#Accidents
PrimaryDiag$accident<-0
PrimaryDiag$accident[startsWith((PrimaryDiag$ICD), "R78")|startsWith((PrimaryDiag$ICD), "S")|startsWith((PrimaryDiag$ICD), "T0")|startsWith((PrimaryDiag$ICD), "T10")|startsWith((PrimaryDiag$ICD), "T11")|
                       startsWith((PrimaryDiag$ICD), "T12")|startsWith((PrimaryDiag$ICD), "T13")|startsWith((PrimaryDiag$ICD), "T14")|
                       startsWith((PrimaryDiag$ICD), "T15")|startsWith((PrimaryDiag$ICD), "T16")|startsWith((PrimaryDiag$ICD), "T17")|
                       startsWith((PrimaryDiag$ICD), "T18")|startsWith((PrimaryDiag$ICD), "T19")|
                       startsWith((PrimaryDiag$ICD), "T2")|startsWith((PrimaryDiag$ICD), "T30")|startsWith((PrimaryDiag$ICD), "T31")|
                       startsWith((PrimaryDiag$ICD), "T32")|
                       startsWith((PrimaryDiag$ICD), "T33")|startsWith((PrimaryDiag$ICD), "T34")|startsWith((PrimaryDiag$ICD), "T35")|
                       startsWith((PrimaryDiag$ICD), "T36")|startsWith((PrimaryDiag$ICD), "T37")|startsWith((PrimaryDiag$ICD), "T38")|
                       startsWith((PrimaryDiag$ICD), "T39")|startsWith((PrimaryDiag$ICD), "T4")|startsWith((PrimaryDiag$ICD), "T5")|
                       startsWith((PrimaryDiag$ICD), "T60")|startsWith((PrimaryDiag$ICD), "T61")|startsWith((PrimaryDiag$ICD), "T62")|
                       startsWith((PrimaryDiag$ICD), "T63")|startsWith((PrimaryDiag$ICD), "T64")|startsWith((PrimaryDiag$ICD), "T65")|
                       startsWith((PrimaryDiag$ICD), "T66")|startsWith((PrimaryDiag$ICD), "T67")|startsWith((PrimaryDiag$ICD), "T68")|
                       startsWith((PrimaryDiag$ICD), "T69")|startsWith((PrimaryDiag$ICD), "T70")|startsWith((PrimaryDiag$ICD), "T71")|
                       startsWith((PrimaryDiag$ICD), "T72")|startsWith((PrimaryDiag$ICD), "T73")|startsWith((PrimaryDiag$ICD), "T74")|
                       startsWith((PrimaryDiag$ICD), "T75")|startsWith((PrimaryDiag$ICD), "T76")|startsWith((PrimaryDiag$ICD), "T77")|startsWith((PrimaryDiag$ICD), "T78")|
                       startsWith((PrimaryDiag$ICD), "T79")|
                       startsWith((PrimaryDiag$ICD), "T9")|
                       startsWith((PrimaryDiag$ICD), "V")|startsWith((PrimaryDiag$ICD), "W")|startsWith((PrimaryDiag$ICD), "X0")|startsWith((PrimaryDiag$ICD), "X1")|
                       startsWith((PrimaryDiag$ICD), "X2")|startsWith((PrimaryDiag$ICD), "X3")|startsWith((PrimaryDiag$ICD), "X4")|startsWith((PrimaryDiag$ICD), "X5")|
                       startsWith((PrimaryDiag$ICD), "X6")|startsWith((PrimaryDiag$ICD), "X7")|startsWith((PrimaryDiag$ICD), "X80")|startsWith((PrimaryDiag$ICD), "X81")|
                       startsWith((PrimaryDiag$ICD), "X82")|startsWith((PrimaryDiag$ICD), "X83")|startsWith((PrimaryDiag$ICD), "X84")|
                       startsWith((PrimaryDiag$ICD), "X85")|startsWith((PrimaryDiag$ICD), "X86")|startsWith((PrimaryDiag$ICD), "X87")|startsWith((PrimaryDiag$ICD), "X88")|
                       startsWith((PrimaryDiag$ICD), "X89")|startsWith((PrimaryDiag$ICD), "X9")|startsWith((PrimaryDiag$ICD), "Y0")|
                       startsWith((PrimaryDiag$ICD), "Y1")|startsWith((PrimaryDiag$ICD), "Y2")|startsWith((PrimaryDiag$ICD), "Y30")|startsWith((PrimaryDiag$ICD), "Y31")|
                       startsWith((PrimaryDiag$ICD), "Y32")|startsWith((PrimaryDiag$ICD), "Y33")|startsWith((PrimaryDiag$ICD), "Y34")|
                       startsWith((PrimaryDiag$ICD), "Y35")|startsWith((PrimaryDiag$ICD), "Y36")|
                       startsWith((PrimaryDiag$ICD), "Y85")|startsWith((PrimaryDiag$ICD), "Y86")|startsWith((PrimaryDiag$ICD), "Y87")|
                       startsWith((PrimaryDiag$ICD), "Y89")|
                       startsWith((PrimaryDiag$ICD), "Y90")|startsWith((PrimaryDiag$ICD), "Y91")|startsWith((PrimaryDiag$ICD), "Z03.6")|
                       startsWith((PrimaryDiag$ICD), "Z04.0")|startsWith((PrimaryDiag$ICD), "Z04.1")|startsWith((PrimaryDiag$ICD), "Z04.2")|startsWith((PrimaryDiag$ICD), "Z04.3")|
                       startsWith((PrimaryDiag$ICD), "Z04.4")|startsWith((PrimaryDiag$ICD), "Z04.5")|startsWith((PrimaryDiag$ICD), "Z04.6")|startsWith((PrimaryDiag$ICD), "Z04.7")|
                       startsWith((PrimaryDiag$ICD), "Z04.8")|startsWith((PrimaryDiag$ICD), "Z04.9")|
                       startsWith((PrimaryDiag$ICD), "Z09.4")|startsWith((PrimaryDiag$ICD), "Z50.2")|startsWith((PrimaryDiag$ICD), "Z50.3")|
                       startsWith((PrimaryDiag$ICD), "Z54.4")|startsWith((PrimaryDiag$ICD), "Z71.4")|startsWith((PrimaryDiag$ICD), "Z71.5")|
                       startsWith((PrimaryDiag$ICD), "Z71.6")|startsWith((PrimaryDiag$ICD), "Z72")|startsWith((PrimaryDiag$ICD), "Z86.4")|
                       startsWith((PrimaryDiag$ICD), "Z91.5")|startsWith((PrimaryDiag$ICD), "Z91.6")]<-1

#Any MH
FirstEpDiag$AnyMH<-0
FirstEpDiag$AnyMH[startsWith((FirstEpDiag$ICD), "F")]<-1

#Any SH
FirstEpDiag$AnySH<-0
FirstEpDiag$AnySH[startsWith((FirstEpDiag$ICD), "Y1")|startsWith((FirstEpDiag$ICD), "Y2")|startsWith((FirstEpDiag$ICD), "Y3")|
                        startsWith((FirstEpDiag$ICD), "X6")|startsWith((FirstEpDiag$ICD), "X7")|startsWith((FirstEpDiag$ICD), "X8")|
                        startsWith((FirstEpDiag$ICD), "Y87.0")]<-1

#Create look ups for each
PrimaryMH<-subset(PrimaryDiag, PrimaryMH==1) #53724
PrimarySH<-subset(PrimaryDiag, PrimarySH==1) #0
PrimaryPossMH<-subset(PrimaryDiag, PrimaryPossMH==1) #3785
Physical<-subset(PrimaryDiag, physical==1) #214157
Accident<-subset(PrimaryDiag, accident==1) #26103

AnyMH<-subset(FirstEpDiag, AnyMH==1) #259451
AnySelfHarm<-subset(FirstEpDiag, AnySH==1) #18127

#Limit the possible MH to those who have a mental health diagnosis at some point that episode
PrimaryPossMH<-subset(PrimaryPossMH, epikey %in% AnyMH$epikey) #2318
PrimaryMH<-rbind(PrimaryMH, PrimaryPossMH) #56042

#Check the codes for self harm with non accident first
Check<-subset(FirstEpDiag, !(epikey %in% Accident$epikey))
Check<-subset(Check, !(epikey %in% PrimaryMH$epikey))
Check<-subset(Check, epikey %in% AnySelfHarm$epikey)
Check<-subset(Check, d_order==1)

PrimarySelfHarm<-subset(AnySelfHarm, epikey %in% Accident$epikey) #17457

#Then remove self-harm from the accident admissions
Accident<-subset(Accident, !(epikey %in% PrimarySelfHarm$epikey))#15668


#Add them to the spells table
Spell$PrimaryMH<-0
Spell$PrimaryMH[Spell$spno %in% PrimaryMH$spno]<-1

Spell$PrimarySH<-0
Spell$PrimarySH[Spell$spno %in% PrimarySelfHarm$spno]<-1

Spell$Accident<-0
Spell$Accident[Spell$spno %in% Accident$spno]<-1

Spell$Physical<-0
Spell$Physical[Spell$spno %in% Physical$spno]<-1

#Look at planned vs. unplanned spells
Spell$Emergency<-1
Spell$Emergency[Spell$admiDet=="Planned hospital transfer"|Spell$admiDet=="Planned"]<-0

table(Spell$Emergency[Spell$PrimaryMH==1])

table(Spell$Emergency[Spell$PrimarySH==1])

table(Spell$Emergency[Spell$Physical==1])

table(Spell$Emergency[Spell$Accident==1])

#Only include physical emergencies
Spell$Physical[Spell$Emergency==0]<-0

#Only include accident emergencies
Spell$Accident[Spell$Emergency==0]<-0


Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date, end)
Spell<-merge(x=Spell, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Only include if before end
Spell<-subset(Spell, admidate<=end & admidate<=as.Date("2019-12-31"))

#Check no overlaps
length(which(Spell$PrimaryMH==1 & Spell$PrimarySH==1))

length(which(Spell$PrimaryMH==1 & Spell$Accident==1))

length(which(Spell$PrimaryMH==1 & Spell$Physical==1))

length(which(Spell$PrimarySH==1 & Spell$Physical==1))

length(which(Spell$PrimarySH==1 & Spell$Accident==1))

length(which(Spell$Accident==1 & Spell$Physical==1))

Spell<-subset(Spell, patid %in% Trial$patid)

Check<-subset(Spell, Spell$Accident==1 & Spell$Physical==1)
Check<-subset(PrimaryDiag, spno %in% Check$spno)

save(Spell, file="VariableExtracts/CPRD2023/MergedObs/Hosps.Rdata")

####Merge back to the main file####
summary(Spell$admidate)#Have already limited to only those before end date

#Bind onto main data: Year before start STATIN
PriorMHStat<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate<=Trial1StatDate & Trial1StatDate-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorMHStat=sum(PrimaryMH))

length(unique(PriorMHStat$patid))
            
PriorSHStat<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate<=Trial1StatDate & Trial1StatDate-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorSHStat=sum(PrimarySH))

PriorPhysicalStat<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate<=Trial1StatDate & Trial1StatDate-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorPhysicalStat=sum(Physical))

PriorAccidentStat<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate<=Trial1StatDate & Trial1StatDate-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorAccidentStat=sum(Accident))

Trial<-merge(x=Trial, y=PriorMHStat, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=PriorSHStat, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=PriorPhysicalStat, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=PriorAccidentStat, by="patid", all.x=TRUE, all.y=TRUE)

#Bind onto main data: Year before start AP
PriorMHAP<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate<=FirstAPTrial3Date & FirstAPTrial3Date-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorMHAP=sum(PrimaryMH))

PriorSHAP<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate<=FirstAPTrial3Date & FirstAPTrial3Date-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorSHAP=sum(PrimarySH))

PriorPhysicalAP<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate<=FirstAPTrial3Date & FirstAPTrial3Date-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorPhysicalAP=sum(Physical))

PriorAccidentAP<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate<=FirstAPTrial3Date & FirstAPTrial3Date-admidate<=365.25)%>%
  group_by(patid)%>%
  summarise(PriorAccidentAP=sum(Accident))

Trial<-merge(x=Trial, y=PriorMHAP, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=PriorSHAP, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=PriorPhysicalAP, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=PriorAccidentAP, by="patid", all.x=TRUE, all.y=TRUE)

####Outcomes####
#Stat 12w
OutcomeMHStat12w<-Spell%>%
subset(PrimaryMH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=84)%>%
  group_by(patid)%>%
  summarise(OutcomeMHStat12w=sum(PrimaryMH))

OutcomeSHStat12w<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=84)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHStat12w=sum(PrimarySH))

OutcomePhysicalStat12w<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=84)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalStat12w=sum(Physical))

OutcomeAccidentStat12w<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=84)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentStat12w=sum(Accident))

Trial<-merge(x=Trial, y=OutcomeMHStat12w, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHStat12w, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalStat12w, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentStat12w, by="patid", all.x=TRUE, all.y=TRUE)

#Stat 6mo
OutcomeMHStat6mo<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=182.625)%>%
  group_by(patid)%>%
  summarise(OutcomeMHStat6mo=sum(PrimaryMH))

OutcomeSHStat6mo<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=182.625)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHStat6mo=sum(PrimarySH))

OutcomePhysicalStat6mo<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=182.625)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalStat6mo=sum(Physical))

OutcomeAccidentStat6mo<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=182.625)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentStat6mo=sum(Accident))

Trial<-merge(x=Trial, y=OutcomeMHStat6mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHStat6mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalStat6mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentStat6mo, by="patid", all.x=TRUE, all.y=TRUE)

#Stat 12mo

OutcomeMHStat12mo<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=365.25)%>%
  group_by(patid)%>%
  summarise(OutcomeMHStat12mo=sum(PrimaryMH))

OutcomeSHStat12mo<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=365.25)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHStat12mo=sum(PrimarySH))

OutcomePhysicalStat12mo<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=365.25)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalStat12mo=sum(Physical))

OutcomeAccidentStat12mo<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=365.25)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentStat12mo=sum(Accident))

Trial<-merge(x=Trial, y=OutcomeMHStat12mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHStat12mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalStat12mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentStat12mo, by="patid", all.x=TRUE, all.y=TRUE)

#Stat 24mo

OutcomeMHStat24mo<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=730.5)%>%
  group_by(patid)%>%
  summarise(OutcomeMHStat24mo=sum(PrimaryMH), OutcomeMHStat24moDate=min(admidate))

OutcomeSHStat24mo<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=730.5)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHStat24mo=sum(PrimarySH), OutcomeHospSHStat24moDate=min(admidate))

OutcomePhysicalStat24mo<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=730.5)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalStat24mo=sum(Physical), OutcomePhsycialStat24moDate=min(admidate))

OutcomeAccidentStat24mo<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>Trial1StatDate & admidate-Trial1StatDate<=730.5)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentStat24mo=sum(Accident), OutcomeAccidentStat24moDate=min(admidate))

Trial<-merge(x=Trial, y=OutcomeMHStat24mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHStat24mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalStat24mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentStat24mo, by="patid", all.x=TRUE, all.y=TRUE)

#Bind onto main data: Year before start AP

#AP 12w
OutcomeMHAP12w<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375 & admidate-FirstAPTrial3Date<=84+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeMHAP12w=sum(PrimaryMH))

OutcomeSHAP12w<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=84+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHAP12w=sum(PrimarySH))

OutcomePhysicalAP12w<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=84+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalAP12w=sum(Physical))

OutcomeAccidentAP12w<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=84+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentAP12w=sum(Accident))

Trial<-merge(x=Trial, y=OutcomeMHAP12w, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHAP12w, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalAP12w, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentAP12w, by="patid", all.x=TRUE, all.y=TRUE)

#AP 6mo
OutcomeMHAP6mo<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=182.625+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeMHAP6mo=sum(PrimaryMH))

OutcomeSHAP6mo<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=182.625+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHAP6mo=sum(PrimarySH))

OutcomePhysicalAP6mo<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=182.625+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalAP6mo=sum(Physical))

OutcomeAccidentAP6mo<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=182.625+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentAP6mo=sum(Accident))

Trial<-merge(x=Trial, y=OutcomeMHAP6mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHAP6mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalAP6mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentAP6mo, by="patid", all.x=TRUE, all.y=TRUE)

#AP 12mo

OutcomeMHAP12mo<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=365.25+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeMHAP12mo=sum(PrimaryMH))

OutcomeSHAP12mo<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=365.25+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHAP12mo=sum(PrimarySH))

OutcomePhysicalAP12mo<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=365.25+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalAP12mo=sum(Physical))

OutcomeAccidentAP12mo<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=365.25+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentAP12mo=sum(Accident))

Trial<-merge(x=Trial, y=OutcomeMHAP12mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHAP12mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalAP12mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentAP12mo, by="patid", all.x=TRUE, all.y=TRUE)

#AP 24mo

OutcomeMHAP24mo<-Spell%>%
  subset(PrimaryMH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=730.5+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeMHAP24mo=sum(PrimaryMH), OutcomeMHAP24moDate=min(admidate))

OutcomeSHAP24mo<-Spell%>%
  subset(PrimarySH==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=730.5+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeHospSHAP24mo=sum(PrimarySH), OutcomeHospSHAP24moDate=min(admidate))

OutcomePhysicalAP24mo<-Spell%>%
  subset(Physical==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=730.5+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomePhysicalAP24mo=sum(Physical), OutcomePhsycialAP24moDate=min(admidate))

OutcomeAccidentAP24mo<-Spell%>%
  subset(Accident==1)%>%
  subset(admidate>FirstAPTrial3Date & admidate-FirstAPTrial3Date>30.4375& admidate-FirstAPTrial3Date<=730.5+30.4375)%>%
  group_by(patid)%>%
  summarise(OutcomeAccidentAP24mo=sum(Accident), OutcomeAccidentAP24moDate=min(admidate))

Trial<-merge(x=Trial, y=OutcomeMHAP24mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeSHAP24mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomePhysicalAP24mo, by="patid", all.x=TRUE, all.y=TRUE)
Trial<-merge(x=Trial, y=OutcomeAccidentAP24mo, by="patid", all.x=TRUE, all.y=TRUE)

save(Trial, file="StatinCPRD/Data/Trial_Complete.rdata")
