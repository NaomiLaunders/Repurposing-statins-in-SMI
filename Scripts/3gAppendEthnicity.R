library(tidyverse)
library(lubridate)
library(tableone)

#Clear environment

rm(list = ls(all.names = TRUE))

load("StatinCPRD/Data/StudyPops.rdata")

#Select just those that are in at least one trial
Trial<-subset(HospAll, !is.na(Trial1Statin)|!is.na(Trial2Statin)|!is.na(APTrial3))
rm(HospAll)

#Load ethnicity observations and limit to our trial patients
load("VariableExtracts/CPRD2023/MergedObs/EthnAll.Rdata")
Ethn<-subset(PatEthnAll, patid %in% Trial$patid)       
rm(PatEthnAll)
Ethn<-subset(Ethn, desc!="interpreter not needed")

#Look at distinct codes and set heirarchy
EthnGroup<-Ethn%>%
  select(desc)%>%
  distinct()%>%
  mutate(Best=case_when(grepl("cypriot", desc, ignore.case=TRUE) ~ 1,
                        grepl("not|unknown", desc, ignore.case=TRUE) ~ 9,
                        grepl("born|birth", desc, ignore.case=TRUE) ~ 2,
                        grepl("language|interpreter|reads|spoken", desc, ignore.case=TRUE) ~ 3,
                        grepl("census|2011|2001|ethn|race|black|white|asian|mixed|cypriot|indian|pakistani|origin|bangladeshi|european|african|irish|chinese", desc, ignore.case=TRUE) ~ 1,
                        TRUE ~ 1))

EthnGroup1<-subset(EthnGroup, Best==1)
EthnGroup1<-EthnGroup1%>%
  mutate(Group=case_when(desc=="other ethnic non-mixed (nmo)"|desc=="n african arab/iranian (nmo)"~ "Other",
                         desc=="e afric asian/indo-carib (nmo)"~ "Black",
                         grepl("white", desc, ignore.case=TRUE) & grepl("black", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("white", desc, ignore.case=TRUE) & grepl("asian", desc, ignore.case=TRUE) ~ "Mixed",
                         grepl("black", desc, ignore.case=TRUE) & grepl("asian", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("chinese", desc, ignore.case=TRUE) & grepl("white", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("afric", desc, ignore.case=TRUE) & grepl("asia", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("afric", desc, ignore.case=TRUE) & grepl("arab", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("afric", desc, ignore.case=TRUE) & grepl("iran", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("black", desc, ignore.case=TRUE) & grepl("iran", desc, ignore.case=TRUE)  ~ "Mixed",
                         grepl("white|northern|irish|cypri|caucasian|european|north american|yugoslav", desc, ignore.case=TRUE) ~ "White",
                         grepl("black|west indian|negroid", desc, ignore.case=TRUE) ~ "Black",
                         grepl("asian|indian|chinese|sri|pak|kashmir|tamil|mauritian|viet|malay|punjabi|muslim|sikh|hindu|filip|japan|buddhist|bangla|oriental|nepali|mongol|far east", desc, ignore.case=TRUE) ~ "Asian",
                         grepl("British", desc, ignore.case=TRUE) ~ "White",
                         grepl("mixed", desc, ignore.case=TRUE) ~ "Mixed",
                         grepl("n afric|north af|non|nec|minor|nos|arab", desc, ignore.case=TRUE) ~ "Other",
                         grepl("africa|carib|nigeria", desc, ignore.case=TRUE) ~ "Black",
                         grepl("ameri|arab|other|iran|sinhalese|somali|kosov|israeli|jewish|nos|moroccan|turkish|yemen|middle eastern", desc, ignore.case=TRUE) ~ "Other",
                         grepl("scot|english|trav|gyp|polish|welsh|albania|greek|croatian|italian|russian|serbian|baltic|kurdish|bosnian|cornish|portu", desc, ignore.case=TRUE) ~ "White",
                         grepl("other", desc, ignore.case=TRUE) ~ "Other",
                         TRUE ~ "Unknown"))

Ethn1<-merge(x=Ethn, y=EthnGroup1, by="desc", all.x=FALSE, all.y=FALSE)
length(which(Ethn$patid %in% Ethn1$patid))
Ethn1<-select(Ethn1, -Best)

#If not take lower tier
Ethn2<-subset(Ethn, !patid %in% Ethn1$patid)
EthnGroup2<-Ethn2%>%
  select(desc)%>%
  distinct()

EthnGroup2<-EthnGroup2%>%
  mutate(Group=case_when(grepl("not|unknown", desc, ignore.case=TRUE) ~ "Unknown",
                         grepl("bengali|urdu|punjabi|panjabi|gujerati", desc, ignore.case=TRUE) ~ "Asian",
                         grepl("arabic|pushto|persian", desc, ignore.case=TRUE) ~ "Other",
                         grepl("russian|gaelic|german|abkhazian|turkish", desc, ignore.case=TRUE) ~ "White",
                         TRUE ~ "DROP"))
EthnGroup2<-subset(EthnGroup2, Group!="DROP")
Ethn2<-merge(x=Ethn2, y=EthnGroup2, by="desc", all.x=FALSE, all.y=FALSE)


####For Alex#####
EthnA<-Ethn1%>%
  mutate(Source = case_when(grepl("A", patid, ignore.case=TRUE) ~ "Aurum",
                            TRUE ~ "Gold"))
EthnA<-subset(EthnA, Source=="Aurum")
EthnA<-select(EthnA, desc, medcode, readcode, Group)
EthnA<-distinct(EthnA)

EthnB<-Ethn2%>%
  mutate(Source = case_when(grepl("A", patid, ignore.case=TRUE) ~ "Aurum",
                            TRUE ~ "Gold"))
EthnB<-subset(EthnB, Source=="Aurum")
EthnB<-select(EthnB, desc, medcode, readcode, Group)
EthnB<-distinct(EthnB)
EthnA$Level<-1
EthnB$Level<-2

EthnAlex<-rbind(EthnA, EthnB)
length(unique(EthnAlex$medcode))
length(unique(EthnAlex$desc))

EthnAlex<-EthnAlex%>%
  group_by(desc)%>%
  mutate(n=n())

Check<-subset(EthnAlex, n>1)
EthnAlex<-select(EthnAlex, -n)

write.table(EthnAlex, "C:/Users/naomi/OneDrive - University College London/Fellowship/Codelists/Alex/Ethnicity_Alex.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".") 

####Back to study####
EthnFinal<-rbind(Ethn1, Ethn2)
####Find unique per patient####
EthnDistinct<-EthnFinal%>%
  select(patid, Group)%>%
  group_by(patid, Group)%>%
  mutate(npergroup=n())%>%
  distinct()%>%
  group_by(patid)%>%
  mutate(ngroup=n())%>%
  ungroup()

####Single####
Single<-EthnDistinct%>%
  subset(ngroup==1)
    
####Check the mis-used "british" term in those with multiple records####
Check<-EthnFinal%>%
  subset(!patid %in% Single$patid)%>%
  select(patid, Group, eventdate, desc)%>%
  mutate(Code=case_when(desc=="british or mixed british - ethnic category 2001 census" ~ 1,
                        TRUE ~ 0))%>%
  group_by(patid)%>%
  mutate(EverBritish=sum(Code, na.rm=TRUE))%>%
  ungroup()

####Remove that term and what happens####
Check<-Check%>%
  subset(desc!="british or mixed british - ethnic category 2001 census")%>%
  select(patid, Group)%>%
  group_by(patid)%>%
  mutate(ngroup=n())%>%
  ungroup()

#Include the ones that are single now the misused british term is removed
Single1<-subset(Check, ngroup==1)

Single<-select(Single, -npergroup)
Single<-rbind(Single, Single1)  

####If still multiples, remove the unknown term####
Unknown<-EthnDistinct%>%
  subset(!patid %in% Single$patid)%>%
  mutate(Code=case_when(Group=="Unknown" ~ 1,
                        TRUE ~ 0))%>%
  group_by(patid)%>%
  mutate(EverUnknown=sum(Code, na.rm=TRUE))%>%
  ungroup()

####Remove that term and what happens####
Unknown<-Unknown%>%
  subset(Group!="Unknown")%>%
  select(patid, Group)%>%
  group_by(patid)%>%
  mutate(ngroup=n())%>%
  ungroup()

Single2<-subset(Unknown, ngroup==1)
Single<-rbind(Single, Single2)  

####Take the most common####
MostCommon<-EthnDistinct%>%
  subset(!patid %in% Single$patid)%>%
  group_by(patid)%>%
  mutate(most=max(npergroup))%>%
  subset(most==npergroup)%>%
  mutate(ngroup=n())%>%
  ungroup()
  
Single3<-MostCommon%>%
  subset(ngroup==1)%>%
  select(-most, -npergroup)

Single<-rbind(Single, Single3)

####Find the few that still are multiples and take the most recent
Multiple<-EthnFinal%>%
  subset(!patid %in% Single$patid)%>%
  select(patid, Group, eventdate, desc)%>%
  group_by(patid)%>%
  mutate(Recent=max(eventdate, na.rm=TRUE))%>%
  subset(Recent==eventdate)%>%
  distinct()%>%
  mutate(ngroup=n())%>%
  ungroup()

Single4<-Multiple%>%
  subset(ngroup==1)%>%
  select(-eventdate, -desc, -Recent)

Single<-rbind(Single, Single4)

#What is left - set those that aren't mixed by hand
Left<-subset(EthnFinal, !patid %in% Single$patid)
Left<-Left%>%
  select(patid, desc, Group)%>%
  mutate(NewGroup=case_when(grepl("Somali|Turkish|Arab|jewish",desc,  ignore.case=TRUE) ~ "Other",
                            grepl("other white",desc,  ignore.case=TRUE) ~ "White",
                            grepl("filipino", desc, ignore.case=TRUE) ~ "Asian", 
                            patid=="5662749021242-A"|patid=="6236176421463-A"~"Asian",
                            TRUE~"DROP"))

Manual<-Left%>%
  subset(NewGroup!="DROP")%>%
  select(patid, NewGroup)%>%
  distinct()%>%
  group_by(patid)%>%
  mutate(ngroup=n())%>%
  ungroup()

Manual<-select(Manual, patid, Group=NewGroup, ngroup)
Check<-subset(Left, ! patid %in% Manual$patid)

Mixed<-Left%>%
  subset(! patid %in% Manual$patid)%>%
  select(patid)%>%
  distinct()%>%
  mutate(ngroup=n(), Group="Mixed")%>%
  select(patid, Group, ngroup)

Single<-rbind(Single, Manual, Mixed)
length(unique(Single$patid))  
length(unique(EthnFinal$patid)) 

Single<-select(Single, patid, ethn=Group)

length(unique(Single$patid))/ length(unique(Trial$patid))*100

####Pull ethnicity for HES records####
HESAurum<-read.table("Linkages/Results/Aurum_linked/Final/HES APC/hes_patient_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character", "gen_hesid" = "character"))
HESGold<-read.table("Linkages/Results/Final Gold/HES APC/hes_patient_21_000729.txt", header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character", "gen_hesid" = "character"))

HESAurum$patid<-paste0(HESAurum$patid, "-A")
HESGold$patid<-paste0(HESGold$patid, "-G")
HospRecord<-rbind(HESGold, HESAurum)
HospRecord<-subset(HospRecord, patid %in% Trial$patid)

#Just double check unique
length(unique(HospRecord$patid))

#Select ethnicity
table(HospRecord$gen_ethnicity)

#Group the multiple Asian/Black terms
HospRecord<-HospRecord%>%
  mutate(gen_ethnicity=case_when(grepl("Bangl|Chinese|Indian|Asian|Pakistani", gen_ethnicity, ignore.case=TRUE) ~ "Asian",
                                 grepl("Bl", gen_ethnicity, ignore.case=TRUE) ~ "Black",
                                 TRUE ~gen_ethnicity ))

table(HospRecord$gen_ethnicity)
HospRecord$gen_ethnicity[HospRecord$gen_ethnicity==""]<-NA

HospRecord<-select(HospRecord, patid, gen_ethnicity)

Ethnicity<-merge(x=Single, y=HospRecord, by="patid", all.x=TRUE, all.y=TRUE)

#Look at whether they match
Ethnicity<-Ethnicity%>%
  mutate(FinalEthnicity=case_when(ethn==gen_ethnicity | is.na(gen_ethnicity) | (gen_ethnicity=="Unknown"& !is.na(ethn)) ~ ethn,
                                  is.na(ethn) | (ethn=="Unknown"& !is.na(gen_ethnicity)) ~ gen_ethnicity,
                                  TRUE ~ "Check"))

length(which(Ethnicity$FinalEthnicity=="Check"))

#Preferentially take CPRD

Ethnicity$FinalEthnicity<-case_when(Ethnicity$FinalEthnicity =="Check" ~ Ethnicity$ethn,
                                    TRUE ~ Ethnicity$FinalEthnicity)

table(Ethnicity$FinalEthnicity, useNA="ifany")
Ethnicity<-select(Ethnicity, patid, ethnicity=FinalEthnicity)

Trial<-merge(x=Trial, y=Ethnicity, by="patid", all.x=TRUE, all.y=FALSE)

table(Trial$ethnicity, useNA="ifany")

save(Trial, file="StatinCPRD/Data/Trial_ethn.rdata")

