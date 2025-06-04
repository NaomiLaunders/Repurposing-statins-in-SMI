####Lipid screening with value####
rm(list = ls(all.names = TRUE))

library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)
library(lubridate)

load("StatinCPRD/Data/Trial_Stat.rdata")
####Aurum####
load("VariableExtracts/CPRD2023/Aurum/LipidAurum.Rdata")
PatLipidAurum<-subset(PatLipidAurum, patid %in% Trial$patid)
numunits <- read.delim("LookUps/202205_Lookups_CPRDAurum/NumUnit.txt")

LipidAurumValue<-merge(x=PatLipidAurum, y=numunits, by="numunitid", all.x=TRUE, all.y=FALSE)
LipidAurumValue<-rename(LipidAurumValue, unit=Description, Term=desc)

#Only select those with values
LipidAurumValue<-subset(LipidAurumValue, !is.na(value)& value!=0)
summary(LipidAurumValue$value)
LipidAurumValue$patdate<-paste0(LipidAurumValue$patid, LipidAurumValue$eventdate)

#If negative make positive

LipidAurumValue$value[LipidAurumValue$value<0]<-LipidAurumValue$value[LipidAurumValue$value<0]*-1

#Check units types

Unit<-LipidAurumValue%>%
  group_by(unit)%>%
  summarise(Count=n())

Term<-LipidAurumValue%>%
  group_by(Term)%>%
  summarise(Count=n())

TermUnit<-LipidAurumValue%>%
  group_by(Term, unit)%>%
  summarise(Count=n())

####Remove units that don't make sense####
LipidAurumValue$NewUnit<-0
LipidAurumValue$NewUnit[LipidAurumValue$unit=="mmol/l"|LipidAurumValue$unit=="mmo1/1"|LipidAurumValue$unit=="mmo1/l"|
                          LipidAurumValue$unit=="mmol/1"|LipidAurumValue$unit=="mmmol/l"|LipidAurumValue$unit=="mmol/L"|
                          LipidAurumValue$unit=="mmol/L (not fasting)"| LipidAurumValue$unit=="MMOL/L." | LipidAurumValue$unit=="mmol/l..."|
                          LipidAurumValue$unit=="mmol/litre"|LipidAurumValue$unit=="mmolL"| LipidAurumValue$unit=="mmolo/l"|LipidAurumValue$unit=="mmols/L"
                        | LipidAurumValue$unit=="mmol/1"|LipidAurumValue$unit=="mm/l"]<-"mmols/L"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="MG/DL"|LipidAurumValue$unit=="mg/100 ml"]<-"MG/DL"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="u/L"]<-"u/L"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="g/L"]<-"g/L"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="% of total."|LipidAurumValue$unit=="%"|LipidAurumValue$unit=="per cent"|LipidAurumValue$unit=="percent"]<-"%"
LipidAurumValue$NewUnit[LipidAurumValue$unit==":1"|LipidAurumValue$unit=="?:1"| LipidAurumValue$unit=="{ratio}"|
                          LipidAurumValue$unit=="ratio"|LipidAurumValue$unit=="total ratio" | LipidAurumValue$unit=="1:1" |LipidAurumValue$unit=="R"|
                          LipidAurumValue$unit== "mmol:mmol" |LipidAurumValue$unit=="mol/mol" | LipidAurumValue$unit=="mmol/mmol" | LipidAurumValue$unit=="1/1"]<-"Ratio"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="#/[tot]"| LipidAurumValue$unit=="fraction"]<-"Fraction"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="umol/L"]<-"umol/l"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="nmol/L"]<-"nmol/l"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="mol/L"]<-"mol/l"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="mg/L"]<-"mg/L"

LipidAurumValue$NewUnit[LipidAurumValue$NewUnit==0]<-"Other"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="(Calculate"]<-"Calculated"
LipidAurumValue$NewUnit[LipidAurumValue$unit=="(Calculated)"]<-"Calculated"

table(LipidAurumValue$NewUnit)
table(LipidAurumValue$NewUnit, LipidAurumValue$unit)
TermCheck<-as.data.frame(table(LipidAurumValue$Term[LipidAurumValue$NewUnit=="Other"],LipidAurumValue$unit[LipidAurumValue$NewUnit=="Other"] ))
TermCheck<-subset(TermCheck, Freq>0)

####Remove those terms that don't make sense####
LipidAurumValue<-subset(LipidAurumValue, !(grepl("dutch lipid", Term, ignore.case=TRUE)))

####Label up terms####

LipidAurumValue <- LipidAurumValue %>%
  mutate(Anyratio= case_when(grepl("ratio", Term) ~ 1, 
                             TRUE ~ 0),
         hdlldlratio = case_when(Term=="hdl : ldl ratio" ~ 1,
                                 TRUE ~ 0),
         ldlhdlratio =    case_when(Term=="plasma ldl/hdl ratio"| Term=="serum ldl/hdl ratio"~ 1, 
                                    TRUE ~ 0),
         tcldlratio = case_when(Term=="serum cholesterol/ldl ratio" ~ 1, 
                                TRUE ~ 0),
         hdlnonhdlratio = case_when(Term=="serum hdl:non-hdl cholesterol ratio" ~ 1, 
                                    TRUE ~ 0),
         hdltriratio = case_when(Term == "serum hdl cholesterol:triglyceride ratio" ~ 1, 
                                 TRUE ~ 0),
         tcvldlratio = case_when(grepl("vldl ratio", Term) ~ 1, #tc vldl  ratio
                                 TRUE ~ 0),
         hdltcratio = case_when(grepl("hdl : total cholesterol ratio", Term) ~ 1, 
                                TRUE ~ 0),
         tchdlratio = case_when(Term=="tc/hdl ratio" | Term == "total cholesterol:hdl ratio" | Term =="serum cholesterol/hdl ratio"|
                                  Term == "plasma cholesterol/hdl ratio" | Term == "cholesterol/hdl ratio" ~ 1, #tc hdl Ratios
                                TRUE ~ 0),
         vldl = case_when(grepl("vldl", Term) & Anyratio == 0 ~ 1, #vldl
                          TRUE ~ 0),
         nonhdl = case_when(grepl("non", Term) & Anyratio == 0 ~ 1, #Nonhdl
                            TRUE ~ 0),
         ldl = case_when(grepl("ldl", Term) & Anyratio == 0 & vldl == 0 ~ 1,  #ldl
                         TRUE ~ 0),
         hdl = case_when(grepl("hdl", Term) & Anyratio == 0 & nonhdl == 0  ~ 1, #hdl
                         TRUE ~ 0),
         triglycerides = case_when(grepl("trigl", Term) & Anyratio==0 ~ 1, #TG
                                   TRUE ~ 0),
         totalcholesterol = case_when(grepl("cholesterol", Term) & ldl == 0 & hdl == 0 & 
                                        triglycerides == 0 & nonhdl == 0 & vldl == 0
                                      & Anyratio ==0 ~ 1, # tc
                                      TRUE ~ 0))
#Check them
Check<-subset(LipidAurumValue, vldl==0&nonhdl==0&ldl==0&hdl==0&triglycerides==0&totalcholesterol==0&Anyratio==0)#These will be set to TC

LipidAurumValue<-subset(LipidAurumValue, !grepl("apolipoprotein", Term, ignore.case=TRUE))

TermCheck<-LipidAurumValue%>%
  group_by(Term)%>%
  summarise(Anyrat=sum(Anyratio), hdlldlratio=sum(hdlldlratio),ldlhdlratio=sum(ldlhdlratio), tcldlratio=sum(tcldlratio),
         hdlnonhdlratio=sum(hdlnonhdlratio), hdltriratio=sum(hdltriratio), tcvldlratio=sum(tcvldlratio), hdltcratio=sum(hdltcratio), tchdlratio=sum(tchdlratio), vldl=sum(vldl),
         nonhdl=sum(nonhdl),ldl=sum(ldl), hdl=sum(hdl), triglycerides=sum(triglycerides), totalcholesterol=sum(totalcholesterol) )

#If they aren't labelled make them total cholesterol
LipidAurumValue<-LipidAurumValue%>%
  mutate(totalcholesterol = case_when(vldl==0&nonhdl==0&ldl==0&hdl==0&triglycerides==0&totalcholesterol==0&Anyratio==0 ~1,
                                      TRUE ~ totalcholesterol))


#Change to long format
LipidAurumLong<-LipidAurumValue%>%
  pivot_longer(13:26, names_to = "Label", values_to = "Count")%>%
  subset(Count>0)%>%
  select(-Count)

table(LipidAurumLong$Label, useNA="ifany")

tapply(LipidAurumLong$value, LipidAurumLong$Label, summary)

rm(Term, TermUnit, LipidAurumValue, PatLipidAurum, Unit, numunits, TermCheck, Check)

####GOLD#####
load("VariableExtracts/CPRD2023/Gold/LipidGoldTest.Rdata")
load("VariableExtracts/CPRD2023/Gold/LipidGoldClinical.Rdata")
PatLipidGold<-subset(PatTest, patid %in% Trial$patid)
PatClin<-subset(PatClin, patid %in% Trial$patid)
rm(PatTest)

entity <- read.delim("LookUps/202303_Lookups_CPRDGold/entity.txt")
Ent<-select(entity, enttype, description)

Additional<-read.table("GOLD/Additional/SMI_GOLD_Extract_Additional_001.txt", header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))

#Only select those with values
Additional$patad<-paste0(Additional$patid, "-G-", Additional$adid)
PatClin$patad<-paste0(PatClin$patid, "-", PatClin$adid)
Check<-subset(Additional, patad %in% PatClin$patad) # No additional details in the clinical table
Check<-merge(x=Check, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)
table(Check$description) #None are useful

#Are there any with a lipid code which arent linked?
Check1<-subset(Additional, enttype==338 | enttype==175 | enttype==177 | enttype==206 | enttype==163 | enttype==202 |enttype==288)
Check1<-subset(Check1, !patad %in% PatLipidGold$patad)#No there aren't

#No additional info in clinical table so drop
rm(Ad1, Ad2, Ad3, Additional, PatClin, Check, Check1)

#Try the other table
LipidGoldValue<-merge(x=PatLipidGold, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)
EntCheck<-subset(entity, enttype %in% LipidGoldValue$enttype)

#Now look at values
opr <- read.delim("LookUps/202303_Lookups_CPRDGold/TXTFILES/OPR.txt")
sum <- read.delim("LookUps/202303_Lookups_CPRDGold/TXTFILES/SUM.txt")

GoldValue<-subset(LipidGoldValue, enttype!=214)
GoldValue<-merge(x=GoldValue, y=opr, by.x="data1", by.y="Code", all.x=TRUE, all.y=FALSE)
table(GoldValue$Operator)
GoldValue<-merge(x=GoldValue, y=sum, by.x="data3", by.y="Code", all.x=TRUE, all.y=FALSE)
table(GoldValue$Specimen.Unit.of.Measure)

GoldValue<-rename(GoldValue, unit=Specimen.Unit.of.Measure, value=data2)
GoldValue<-subset(GoldValue, !is.na(value)& value!=0)

summary(GoldValue$value)

#Patdate
GoldValue$patdate<-paste0(GoldValue$patid, GoldValue$eventdate)

####Remove units that don't make sense####
GoldValue$NewUnit<-0
GoldValue$NewUnit[GoldValue$unit=="mmol/l"|GoldValue$unit=="mmo1/1"|GoldValue$unit=="mmo1/l"|
                    GoldValue$unit=="mmol/1"|GoldValue$unit=="mmmol/l"|GoldValue$unit=="mmol/L"|
                    GoldValue$unit=="mmol/L (not fasting)"| GoldValue$unit=="MMOL/L." | GoldValue$unit=="mmol/l..."|
                    GoldValue$unit=="mmol/litre"|GoldValue$unit=="mmolL"| GoldValue$unit=="mmolo/l"|GoldValue$unit=="mmols/L"
                  | GoldValue$unit=="mmol/1"]<-"mmols/L"
GoldValue$NewUnit[GoldValue$unit=="MG/DL"|GoldValue$unit=="mg/100 ml"|GoldValue$unit=="mg/dl" | GoldValue$unit=="mg/dL" ]<-"MG/DL"
GoldValue$NewUnit[GoldValue$unit=="u/L"| GoldValue$unit=="U/L"]<-"u/L"
GoldValue$NewUnit[GoldValue$unit=="g/L"]<-"g/L"
GoldValue$NewUnit[GoldValue$unit=="% of total."|GoldValue$unit=="%"|GoldValue$unit=="per cent"]<-"%"
GoldValue$NewUnit[GoldValue$unit==":1"|GoldValue$unit=="?:1"| GoldValue$unit=="{ratio}"|GoldValue$unit=="Ratio"|
                    GoldValue$unit=="ratio"|GoldValue$unit=="total ratio" | GoldValue$unit=="1:1" |GoldValue$unit=="L:L" |
                    GoldValue$unit== "mmol:mmol" |GoldValue$unit=="mol/mol" | GoldValue$unit=="mmol/mmol" | GoldValue$unit=="1/1" | GoldValue$unit=="L/L"]<-"Ratio"
GoldValue$NewUnit[GoldValue$unit=="#/[tot]"| GoldValue$unit=="fraction"]<-"Fraction"
GoldValue$NewUnit[GoldValue$unit=="umol/L"]<-"umol/l"
GoldValue$NewUnit[GoldValue$unit=="nmol/L"]<-"nmol/l"
GoldValue$NewUnit[GoldValue$unit=="mg/L"]<-"mg/L"
GoldValue$NewUnit[GoldValue$unit=="mol/L"]<-"mol/L"

GoldValue$NewUnit[GoldValue$NewUnit==0]<-"Other"

table(GoldValue$NewUnit, GoldValue$unit)

####Label up terms####
GoldValue<-rename(GoldValue, term=desc)
table(GoldValue$term, GoldValue$description)

GoldValue <- GoldValue %>%
  mutate(Anyratio= case_when(grepl("ratio", term) ~ 1, 
                             TRUE ~ 0),
         hdlldlratio = case_when(term=="hdl : ldl ratio" ~ 1,
                                 TRUE ~ 0),
         ldlhdlratio =    case_when(term=="plasma ldl/hdl ratio"| term=="serum ldl/hdl ratio"~ 1, 
                                    TRUE ~ 0),
         tcldlratio = case_when(term=="serum cholesterol/ldl ratio" ~ 1, 
                                TRUE ~ 0),
         hdlnonhdlratio = case_when(term=="serum hdl:non-hdl cholesterol ratio" ~ 1, 
                                    TRUE ~ 0),
         hdltriratio = case_when(term == "serum hdl cholesterol:triglyceride ratio" ~ 1, 
                                 TRUE ~ 0),
         tcvldlratio = case_when(grepl("vldl ratio", term) ~ 1, #tc vldl  ratio
                                 TRUE ~ 0),
         hdltcratio = case_when(grepl("hdl : total cholesterol ratio", term) ~ 1, 
                                TRUE ~ 0),
         tchdlratio = case_when(term=="tc/hdl ratio" | term == "total cholesterol:hdl ratio" | term =="serum cholesterol/hdl ratio"|
                                  term == "plasma cholesterol/hdl ratio" | term == "cholesterol/hdl ratio" ~ 1, #tc hdl Ratios
                                TRUE ~ 0),
         vldl = case_when(grepl("vldl", term) & Anyratio == 0 ~ 1, #vldl
                          TRUE ~ 0),
         nonhdl = case_when(grepl("non", term) & Anyratio == 0 ~ 1, #Nonhdl
                            TRUE ~ 0),
         ldl = case_when(grepl("ldl", term) & Anyratio == 0 & vldl == 0 ~ 1,  #ldl
                         TRUE ~ 0),
         hdl = case_when(grepl("hdl", term) & Anyratio == 0 & nonhdl == 0  ~ 1, #hdl
                         TRUE ~ 0),
         triglycerides = case_when(grepl("trigl", term) & Anyratio==0 ~ 1, #TG
                                   TRUE ~ 0),
         totalcholesterol = case_when(grepl("cholesterol", term) & ldl == 0 & hdl == 0 & 
                                        triglycerides == 0 & nonhdl == 0 & vldl == 0
                                      & Anyratio ==0 ~ 1, # tc
                                      TRUE ~ 0))

Check<-subset(GoldValue, vldl==0&nonhdl==0&ldl==0&hdl==0&triglycerides==0&totalcholesterol==0&Anyratio==0)#These will be set to TC

TermCheck<-GoldValue%>%
  group_by(term)%>%
  summarise(Anyrat=sum(Anyratio), hdlldlratio=sum(hdlldlratio),ldlhdlratio=sum(ldlhdlratio), tcldlratio=sum(tcldlratio),
            hdlnonhdlratio=sum(hdlnonhdlratio), hdltriratio=sum(hdltriratio), tcvldlratio=sum(tcvldlratio), hdltcratio=sum(hdltcratio), tchdlratio=sum(tchdlratio), vldl=sum(vldl),
            nonhdl=sum(nonhdl),ldl=sum(ldl), hdl=sum(hdl), triglycerides=sum(triglycerides), totalcholesterol=sum(totalcholesterol) )


GoldValue<-GoldValue%>%
  mutate(totalcholesterol = case_when(vldl==0&nonhdl==0&ldl==0&hdl==0&triglycerides==0&totalcholesterol&Anyratio==0 ~1,
                                      TRUE ~ totalcholesterol))

#Change to long format
GoldValueLong<-GoldValue%>%
  pivot_longer(22:35, names_to = "Label", values_to = "Count")%>%
  subset(Count>0)%>%
  select(-Count)

table(GoldValueLong$Label, useNA="ifany")

tapply(GoldValueLong$value, GoldValueLong$Label, summary)

table(GoldValueLong$value[GoldValueLong$Operator!="="&GoldValueLong$Operator!="Data Not Entered" ],GoldValueLong$Operator[GoldValueLong$Operator!="="&GoldValueLong$Operator!="Data Not Entered" ] )

#Compare to entdescription
table(GoldValueLong$Label, GoldValueLong$description)

rm(LipidGoldValue, opr, sum, tqu, GoldValue, Ent, EntCheck, Check, entity, TermCheck)

####Merge Gold and Aurum and clean####
LipidAurumLong<-select(LipidAurumLong, medcode, patid, eventdate, sysdate, value, Term, unit, patdate, NewUnit, Label)
LipidAurumLong$Operator<-NA
GoldValueLong<-select(GoldValueLong, medcode, patid, eventdate, sysdate, value, Term=term, unit, patdate, NewUnit, Label, Operator)
Lipids<-rbind(GoldValueLong, LipidAurumLong)
Lipids<-distinct(Lipids)
Lipids$patdatelabel<-paste0(Lipids$patid, Lipids$eventdate, Lipids$Label)
length(unique(Lipids$patdatelabel))
length(unique(Lipids$patid))

#Drop medcode  and QOF as likely the only difference for some
Lipids<-select(Lipids, -medcode, -Term)
Lipids<-distinct(Lipids)
length(unique(Lipids$patdatelabel))

save(Lipids, file = "VariableExtracts/CPRD2023/MergedObs/GoldandAurumLipids.Rdata")
####Cleaning####

#Only keep those which we need - take total, and if not tchdlratio if it's present on that day and in range
SelectLipids<-Lipids%>%
  mutate(Evertc = case_when(Label=="totalcholesterol" & value>=1.75 & value<=20 ~ 1,
                            TRUE ~ 0),
         Evertchdl = case_when(Label == "tchdlratio" & value>=1 & value<=70 ~ 1,
                               TRUE ~ 0))%>%
  group_by(patdate)%>%
  mutate(sumEvertc = sum(Evertc), sumEvertchdl= sum(Evertchdl))%>%
  ungroup()%>%
  subset((sumEvertc == 0 & sumEvertchdl==0) | 
           (sumEvertc>0 & Label=="totalcholesterol")|
           (sumEvertc == 0 & sumEvertchdl>0 & Label=="tchdlratio"))

####Find those with multiple records of the same type on the same day####
Multi<-SelectLipids%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())%>%
  subset(SameDay>1)
length(unique(Multi$patdatelabel))

Single<-SelectLipids%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())%>%
  subset(SameDay==1)
length(unique(Single$patdatelabel))

#Find min and max on each day for each label type
Multi<-Multi%>%
  group_by(patdatelabel)%>%
  mutate(maxval=max(value), minval=min(value), ave=mean(value), diff=maxval-minval)%>%
  ungroup%>%
  mutate(InRange = case_when(Label=="hdl" & value<=4.7 & value>=0.3 ~ 1,
                             Label=="ldl" & value<=10.3 & value>=0.26 ~ 1,
                             Label=="nonhdl" & value<=20 & value>=0.26 ~ 1,
                             Label=="triglycerides" & value<=25 & value>=0.10 ~ 1,
                             Label=="totalcholesterol" & value<=20.0 & value>=1.75 ~ 1,
                             Label=="tchdlratio" & value<=70 & value>=1 ~ 1,
                             Label=="ldlhdlratio" & value<=35 & value>=0.05 ~ 1, #calculated limits
                             Label=="hdlnonhdlratio" & value<=19 & value>=0.01 ~ 1,#calculated limits
                             Label=="hdlldlratio" & value<=19 & value>=0.01 ~ 1,
                             TRUE ~ 0))%>%
  group_by(patdatelabel)%>%
  mutate(EverInRange = sum(InRange))

#If the values are the same, preferentially take the right units and take average 
length(which(Multi$diff==0))
length(which(Multi$diff<=0.5))

NewSingle<-subset(Multi, diff==0)
table(NewSingle$NewUnit, NewSingle$Label)

NewSingle<-NewSingle%>%
  mutate(Order = case_when((Label=="hdl"|Label=="ldl"|Label=="nonhdl"|Label=="totalcholesterol" |
                              Label=="triglycerides"| Label=="vldl") & NewUnit=="mmols/L" ~ 3,
                           
                           (Label=="hdl"|Label=="ldl"|Label=="nonhdl"|Label=="totalcholesterol" |
                              Label=="triglycerides"| Label=="vldl") & NewUnit=="MG/DL" ~ 2,
                           
                           (Label=="hdlnonhdlratio" |Label=="tchdlratio"| Label=="tcvldlratio"|Label=="hdlldlratio"|
                              Label=="hdltcratio" | Label=="ldlhdlratio" |Label=="tcldlratio"  ) & NewUnit=="Ratio" ~2,
                           
                           TRUE ~0))

NewSingle<-NewSingle %>%
  group_by(patdatelabel) %>%
  mutate(value=ave) %>%
  arrange(desc(Order)) %>%
  filter(row_number()==1)%>%
  select(-maxval, -minval, -ave, -diff, -Order)
length(unique(NewSingle$patdatelabel))

Single<-rbind(Single, NewSingle)
length(unique(Single$patdatelabel))

#Sort those with real differences
LargeDiff<-subset(Multi, diff>0.5)

#If there is a valid measure in range that day then take that and drop the rest
InRange<-LargeDiff%>%
  subset(InRange==1 | (InRange==0 & EverInRange==0))%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n(), maxval=max(value), minval=min(value), ave=mean(value), diff=maxval-minval)

length(unique(InRange$patdatelabel))
rm(LargeDiff)

#Add those that now have v small difference 
NewSingle<-subset(InRange, diff<=0.5)

NewSingle<-NewSingle%>%
  mutate(Order = case_when((Label=="hdl"|Label=="ldl"|Label=="nonhdl"|Label=="totalcholesterol" |
                              Label=="triglycerides"| Label=="vldl") & NewUnit=="mmols/L" ~ 3,
                           
                           (Label=="hdl"|Label=="ldl"|Label=="nonhdl"|Label=="totalcholesterol" |
                              Label=="triglycerides"| Label=="vldl") & NewUnit=="MG/DL" ~ 2,
                           
                           (Label=="hdlnonhdlratio" |Label=="tchdlratio"| Label=="tcvldlratio"|Label=="hdlldlratio"|
                              Label=="hdltcratio" | Label=="ldlhdlratio" |Label=="tcldlratio"  ) & NewUnit=="Ratio" ~2,
                           
                           TRUE ~0))

NewSingle<-NewSingle %>%
  group_by(patdatelabel) %>%
  mutate(value=ave) %>%
  arrange(desc(Order)) %>%
  filter(row_number()==1)%>%
  select(-maxval, -minval, -ave, -diff, -Order)
length(unique(NewSingle$patdatelabel))

Single<-rbind(NewSingle, Single)
length(unique(Single$patdatelabel))

#What is remaining
Remaining<-subset(SelectLipids, !(patdatelabel %in% Single$patdatelabel))

#Check outliers
table(Remaining$Label)
tapply(Remaining$value, Remaining$Label, summary)

table(Remaining$NewUnit)

Remaining<-Remaining%>%
  group_by(patdatelabel)%>%
  mutate(anycorrect = case_when((Label=="hdl"|Label=="ldl"|Label=="nonhdl"|Label=="totalcholesterol" |
                                   Label=="triglycerides"| Label=="vldl") & NewUnit=="mmols/L" ~ 1,
                                (Label=="tcldlratio" |Label=="tchdlratio"| Label=="tcvldlratio"|Label=="ldlhdlratio" | Label=="hdltcratio"|
                                   Label=="hdlnonhdlratio" | Label=="hdlldlratio") & NewUnit=="Ratio" ~1,
                                TRUE ~ 0),
         sumanycorrect = sum(anycorrect),
         remove = case_when((Label=="hdl"|Label=="ldl"|Label=="nonhdl"|Label=="totalcholesterol" |
                               Label=="triglycerides"| Label=="vldl") & NewUnit != "mmols/L" & sumanycorrect > 0  ~ 1, # if there a record with the correct unit, use that
                            (Label=="tcldlratio" |Label=="tchdlratio"| Label=="tcvldlratio"|Label=="ldlhdlratio" | Label=="hdltcratio"|
                               Label=="hdlnonhdlratio" | Label=="hdlldlratio") & NewUnit != "Ratio" & sumanycorrect > 0 ~ 1,
                            TRUE ~ 0)) %>%
  ungroup() %>%
  filter(remove == 0)%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n(), maxval=max(value), minval=min(value), ave=mean(value), diff=maxval-minval)

Labels<-Remaining %>%
  subset(diff<=0.5)%>%
  group_by(patdatelabel) %>%
  mutate(value=ave) %>%
  filter(row_number()==1)%>%
  select(-maxval, -minval, -ave, -diff, -remove, -anycorrect, -sumanycorrect)

length(unique(Labels$patdatelabel))

#Final few
Remaining<-Remaining%>%
  subset(diff>0.5)

table(Remaining$Label)
table(Remaining$NewUnit, Remaining$Label)

#Look by label
#hdl
summary(Single$value[Single$Label=="hdl"])
hdl<-subset(Remaining, Label=="hdl") # Take minimum unless both under 1, then take average

#If a difference >2 assume one is hdl and one is TC, otherwise take the average
WrongLab1<-hdl%>%
  subset(diff>=2 & (SameDay==2 | (SameDay>2 & value==maxval | value==minval)))%>%
  mutate(Label=case_when(value ==minval ~ "hdl",
                         value ==maxval ~ "totalcholesterol",
                         TRUE ~ Label),
         patdatelabel = paste0(patid, eventdate, Label))%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())
length(unique(WrongLab1$patdatelabel))

hdl<-hdl%>%
  subset(diff<2) %>%
  filter(row_number()==1)%>%
  mutate(value=ave)
length(unique(hdl$patdatelabel))

#hdl:tc
summary(Single$value[Single$Label=="hdltcratio"])
hdltcratio<-subset(Remaining, Label=="hdltcratio") # Take minimum unless both under 1, then take average

#Assume one is hdl and one is TC if difference is bigger than 2, otherwise take the average
WrongLab4<-hdltcratio%>%
  subset(diff>=2 & (SameDay==2 | (SameDay>2 & value==maxval | value==minval)))%>%
  mutate(Label=case_when(value ==minval ~ "hdl",
                         value ==maxval ~ "totalcholesterol",
                         TRUE ~ Label),
         patdatelabel = paste0(patid, eventdate, Label))%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())
length(unique(WrongLab4$patdatelabel))

hdltcratio<-hdltcratio%>%
  subset(diff<2) %>%
  filter(row_number()==1)%>%
  mutate(value=ave)
length(unique(hdltcratio$patdatelabel))

#ldl
summary(Single$value[Single$Label=="ldl"])
ldl<-subset(Remaining, Label=="ldl") # take based on individual id

WrongLab5<-ldl%>%
  subset(diff>=2 & (SameDay==2 | (SameDay>2 & value==maxval | value==minval)))%>%
  mutate(Label=case_when(value ==minval ~ "ldl",
                         value ==maxval ~ "totalcholesterol",
                         TRUE ~ Label),
         patdatelabel = paste0(patid, eventdate, Label))%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())
length(unique(WrongLab5$patdatelabel))

ldl<-ldl%>%
  subset(diff<2) %>%
  filter(row_number()==1)%>%
  mutate(value=ave)
length(unique(ldl$patdatelabel))

#Total:hdl
summary(Single$value[Single$Label=="tchdlratio"])#If >21 diff 
tchdlratio<-subset(Remaining, Label=="tchdlratio")
#Do we have both tc and hdl
WrongLab6<-tchdlratio%>%
  subset((value<=2.5 & maxval>=4) |(value>=4 & minval<=2.5) )%>%
  mutate(Label=case_when(value<=2.5 & maxval>=4 ~ "hdl",
                         value>=4 & minval<=2.5 ~ "totalcholesterol",
                         TRUE ~ Label),
         patdatelabel = paste0(patid, eventdate, Label))

tchdlratio<-subset(tchdlratio, !(patdate %in% WrongLab6$patdate))
tchdlratio<-tchdlratio%>%
  filter(row_number()==1)%>%
  mutate(value = ave)
length(unique(tchdlratio$patdatelabel))

#Total cholesterol
summary(Single$value[Single$Label=="totalcholesterol"])
totalcholesterol<-subset(Remaining, Label=="totalcholesterol")
#Do we have hdl+ldl

totalcholesterol<-totalcholesterol%>%
  filter(row_number()==1)%>%
  mutate(value=case_when(maxval>100 ~ minval,
                         TRUE ~ maxval))

length(unique(totalcholesterol$patdatelabel))

#tryglycerides
summary(Single$value[Single$Label=="triglycerides"])
triglycerides<-subset(Remaining, Label=="triglycerides")

triglycerides<-triglycerides%>%
  filter(row_number()==1)%>%
  mutate(value=ave)

#Don't know so dont include
hdl$calc<-1
hdltcratio$calc<-1
ldl$calc<-1
tchdlratio$calc<-1
totalcholesterol$calc<-1
triglycerides$calc<-1
WrongLab1$calc<-1
WrongLab4$calc<-1
WrongLab5$calc<-1
WrongLab6$calc<-1

#Check have no ratios in the wrong lab tables
table(WrongLab1$Label)
table(WrongLab4$Label)
table(WrongLab5$Label)
table(WrongLab6$Label)

ByLabel<-rbind(hdl, hdltcratio, ldl, tchdlratio, totalcholesterol, triglycerides, WrongLab1, WrongLab4, WrongLab5, WrongLab6)
length(unique(ByLabel$patdatelabel))

####Clean values - UNIQUE####
NewLipids<-rbind(Single, Labels, ByLabel)
length(unique(NewLipids$patdatelabel))
length(unique(NewLipids$patid))
length(unique(SelectLipids$patid))

length(unique(NewLipids$patdate))
length(unique(SelectLipids$patdate))

table(NewLipids$Label)

FinalLipids<-NewLipids%>%
  group_by(patdate)%>%
  select(patid, patdate, eventdate, value, unit, Label)%>%
  pivot_wider(names_from = Label, values_from = c(value, unit))%>%
  mutate(n=n())
length(unique(FinalLipids$patdate))

#Rename
FinalLipids<-rename(FinalLipids, tchol=value_totalcholesterol, hdl=value_hdl, ldl=value_ldl, tri=value_triglycerides, nonhdl=value_nonhdl, tchdlratio=value_tchdlratio,
                    vldl=value_vldl, hdlldlratio = value_hdlldlratio, hdltcratio = value_hdltcratio, hdlnonhdlratio = value_hdlnonhdlratio)

FinalLipids$eventyear<-year(FinalLipids$eventdate)

FinalLipids<-FinalLipids%>%
  ungroup()
save(FinalLipids, file="VariableExtracts/CPRD2023/MergedObs/LipidValues.Rdata")

length(which(FinalLipids$tchol>20))
length(which(is.na(FinalLipids$tchol)))
length(which(is.na(FinalLipids$tchol)&is.na(FinalLipids$tchdlratio)))

rm(ByLabel, GoldValueLong, Labels, LipidAurumLong, Lipids, Multi, NewLipids, NewSingle, Remaining, SelectLipids, Single, tchdlratio, totalcholesterol, WrongLab1, hdl, hdlldlratio, ldl, InRange)
rm(WrongLab4, WrongLab5, WrongLab6, triglycerides, hdltcratio)
rm(Check, Check1, Additional, PatClin, PatLipidGold)

#Only bother cleaning those we need - take total preferentially

#Sort out hdl, ldl, tri and non-hdl first
####Sort out total to components so can calculate####
hdl<-subset(FinalLipids, hdl>=4.7)

ldl<-subset(FinalLipids, ldl>=10.30)

nonhdl<-subset(FinalLipids, nonhdl>=10)

tri<-subset(FinalLipids, tri>=25)

#if tchlol and hdl is also over 20 then probably mg/dl
FinalLipids<-FinalLipids%>%
  mutate(hdl = case_when(hdl>11.6 ~ hdl*0.02586,
                         TRUE ~ hdl),
         ldl = case_when(ldl>10.5~ ldl*0.02586,
                         TRUE ~ ldl),
         nonhdl = case_when(nonhdl>20~ nonhdl*0.02586,
                            TRUE ~ nonhdl),
         tri = case_when(tri>25 ~ tri*0.01129,
                         TRUE ~ tri),
         vldl = case_when(vldl>5.3~ vldl*0.01129,
                          TRUE ~ vldl),
         tchol = case_when(tchol>67~ tchol*0.02586,
                           TRUE ~ tchol))

#If hdl is missing calculate from nonhdl and ldl
FinalLipids<-FinalLipids%>%
  mutate(hdl = case_when((hdl<0.3 | hdl>4.7 | is.na(hdl)) & tchol<=20 & tchol>=1.75 & nonhdl<=20 & nonhdl>=0.26 ~ tchol-nonhdl,
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & tchol<=20 & tchol>=1.75 & ldl<=10.30 & ldl>=0.26 & is.na(nonhdl) & tri<=4.5 & tri>=0.1 ~ tchol-ldl-(tri/2.2),
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & tchol<=20 & tchol>=1.75 & ldl<=10.30 & ldl>=0.26 & is.na(nonhdl) & is.na(tri) & vldl>=0.1 &vldl<2.5 ~ tchol-ldl-vldl,
                         TRUE ~ hdl))
length(which(FinalLipids$hdl<0.3 | FinalLipids$hdl>4.7 | is.na(FinalLipids$hdl)))

#Can we glean anything from any of the other ratios?
Ratios1<-subset(FinalLipids, !is.na(hdlldlratio)& ((hdl<0.3 | hdl>4.7 | is.na(hdl)) | (ldl<0.26 | ldl>10.30 | is.na(ldl))) & !((hdl<0.3 | hdl>4.7 | is.na(hdl)) & (ldl<0.26 | ldl>10.30 | is.na(ldl)))) #7
Ratios2<-subset(FinalLipids, !is.na(hdltcratio)& ((hdl<0.3 | hdl>4.7 | is.na(hdl)) | (tchol<1.75 | tchol>20 | is.na(tchol))) & !((hdl<0.3 | hdl>4.7 | is.na(hdl)) & is.na(tchol))) #8
Ratios3<-subset(FinalLipids, !is.na(hdlnonhdlratio)& ((hdl<0.3 | hdl>4.7 | is.na(hdl)) | (nonhdl>20 | nonhdl<0.26 | is.na(nonhdl))) & !((hdl<0.3 | hdl>4.7 | is.na(hdl)) & (nonhdl>20 | nonhdl<0.26 | is.na(nonhdl)))) #4
#No records where a ratio is present without the right numbers

FinalLipids<-FinalLipids%>%
  mutate(hdl = case_when((hdl<0.3 | hdl>4.7 | is.na(hdl)) & !is.na(hdlldlratio) & ldl<=10.30 & ldl>=0.26 ~ hdlldlratio*ldl,
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & hdltcratio<1 & tchol<=20 & tchol>=1.75 ~ hdltcratio*tchol,
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & !is.na(hdlnonhdlratio) & nonhdl<=20 & nonhdl>=0.26 ~ hdlnonhdlratio*nonhdl,
                          TRUE ~ hdl),
         ldl = case_when((ldl<0.26 | ldl>10.30 | is.na(ldl)) & !is.na(hdlldlratio) & hdl<=4.7 & hdl>=0.3 ~ hdl/hdlldlratio,
                          TRUE ~ ldl),
         calctchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdltcratio<1 & hdl<=4.7 & hdl>=0.3 ~ 1,
                               TRUE ~ 0),
         tchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdltcratio<1 & hdl<=4.7 & hdl>=0.3 ~ hdl/hdltcratio,
                           TRUE ~ tchol),
         nonhdl = case_when((nonhdl>20 | nonhdl<0.26 | is.na(nonhdl)) & !is.na(hdlnonhdlratio) & hdl<=4.7 & hdl>=0.3 ~ hdl/hdlnonhdlratio,
                            TRUE ~ nonhdl),
         tchdlratio = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdltcratio>=1 & hdl<=4.7 & hdl>=0.3 ~ hdltcratio,
                                TRUE ~ tchdlratio), 
         hdltcratio = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdltcratio>=1 & hdl<=4.7 & hdl>=0.3 ~ NA_real_,
                                TRUE ~ hdltcratio))

#Now look at total:hdl ratio         
summary(FinalLipids$tchdlratio[FinalLipids$unit_tchdlratio=="#/[tot]"]) #None
summary(FinalLipids$tchdlratio[FinalLipids$unit_tchdlratio=="%"])
summary(FinalLipids$tchdlratio[FinalLipids$unit_tchdlratio=="fraction"]) #None

TchdlRatio<-subset(FinalLipids, !is.na(tchdlratio))


FinalLipids<-FinalLipids%>%
  mutate(tchdlratio = case_when((tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & tchol<=20 & tchol>=1.75 & hdl>=0.3 & hdl<=4.7 ~ tchol/hdl,
                                (tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & tchol<=20 & tchol>=1.75 & (is.na(hdl)|hdl<0.3 |hdl>4.7) ~   tchol / ((tchdlratio/100)*tchol),#Probably a %
                                (tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & (is.na(tchol)|tchol>20 |tchol<1.75) & hdl>=0.3 & hdl<=4.7 ~ hdl/ ((tchdlratio/100) /hdl),#Probably a %
                                TRUE ~ tchdlratio))

#Also sort too low
FinalLipids<-FinalLipids%>%
  mutate(tchdlratio = case_when(tchdlratio<1 & tchol<=20 & tchol>=1.75 & hdl>=0.3 & hdl<=4.7 ~ tchol/hdl,
                                TRUE ~ tchdlratio))
#How many can we calculate?
FinalLipids<-FinalLipids%>%
  mutate(calctchol= case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 & tri<=25 & tri>=0.1 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & nonhdl<=20 & nonhdl>=0.26 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & ldl<=10.3 & ldl>=0.26 & vldl<=2.26 & vldl>=0.06 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & tchdlratio<=15 & tchdlratio>=1 ~ 1,
                              TRUE ~ 0),
         tchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 & tri<=4.5 & tri>=0.1 ~ hdl+ldl+(tri/2.2),
                           (tchol>20 | is.na(tchol) |tchol<1.75) & nonhdl<=20 & nonhdl>=0.26 ~ hdl+nonhdl,
                           (tchol>20 | is.na(tchol) |tchol<1.75) & ldl<=10.3 & ldl>=0.26 & vldl<=2.26 & vldl>=0.06 ~ hdl+ldl+vldl,
                           (tchol>20 | is.na(tchol) |tchol<1.75) & tchdlratio<=15 & tchdlratio>=1 ~ hdl*tchdlratio,
                           TRUE ~ tchol))

length(which(FinalLipids$calctchol==1))

tcholHigh<-subset(FinalLipids, tchol>20&calctchol==0)
tcholHighCalc<-subset(FinalLipids, tchol>20&calctchol==1)

#If calculated, and tchol>20 and set tchol to null and 
FinalLipids<-FinalLipids%>%
  mutate(tchol= case_when(tchol>20 & tchol<20.5 ~ 20,
                          tchol>20 & calctchol==1 ~ NA_real_,
                          TRUE ~ tchol))
#Units
FinalLipids<-FinalLipids%>%
  mutate(tchol = case_when(tchol>20 & (unit_totalcholesterol=="mg/dL" |unit_totalcholesterol=="MG/DL" |unit_totalcholesterol=="mm/Hg")  ~ tchol*0.02586,
                           TRUE ~ tchol))

tcholHigh<-subset(FinalLipids, tchol>20&calctchol==0)
tcholHighCalc<-subset(FinalLipids, tchol>20&calctchol==1)

#If tchol >67.67 then probably mg/dl, if not divide by 10
FinalLipids<-FinalLipids%>%
  mutate(tchol = case_when(tchol> 67.67 ~ tchol*0.02586,
                           tchol>30 & tchol<=67.67 ~ tchol/10,
                           TRUE ~ tchol))

#If between 20 and 30 set to null
FinalLipids<-FinalLipids%>%
  mutate(tchol = case_when(tchol<=30 & tchol>20~ NA_real_,
                           TRUE ~ tchol))

###Sort out implausibly low####
tcholLow<-subset(FinalLipids, tchol<1.75 & calctchol==0)
tcholLowcalc<-subset(FinalLipids, tchol<1.75 & calctchol==1)

FinalLipids<-FinalLipids%>% #If tchol is less than hdl then hdl is probably tchol!
  mutate(tchol1=case_when(hdl>tchol & hdl>1.75 & (hdl>ldl | is.na(ldl)) ~ hdl,
                          ldl>tchol & ldl>1.75 & (ldl>hdl | is.na(hdl)) ~ ldl,
                          TRUE ~ tchol),
         hdl = case_when(hdl>tchol & hdl>1.75 & (hdl>ldl | is.na(ldl))~ tchol,
                         TRUE ~ hdl),
         ldl = case_when(ldl>tchol & ldl>1.75 & (ldl>hdl | is.na(hdl)) ~ tchol,
                         TRUE ~ ldl))%>%
  select(-tchol)%>%
  rename(tchol = tchol1)

#Otherwise there isn't much we can do, so make them NA
FinalLipids<-FinalLipids%>%
  mutate(tchol= case_when(tchol<1.75 ~ NA_real_,
                          TRUE ~ tchol))

#Now look at all missing ones
tcholmis<-subset(FinalLipids, is.na(tchol))

#Final recalculations

FinalLipids<-FinalLipids%>%
  mutate(hdl = case_when((hdl<0.3 | hdl>4.7 | is.na(hdl)) & !is.na(hdlldlratio) & ldl<=10.30 & ldl>=0.26 ~ hdlldlratio*ldl,
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & hdltcratio<1 & tchol<=20 & tchol>=1.75 ~ hdltcratio*tchol,
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & !is.na(hdlnonhdlratio) & nonhdl<=20 & nonhdl>=0.26 ~ hdlnonhdlratio*nonhdl,
                         TRUE ~ hdl),
         ldl = case_when((ldl<0.26 | ldl>10.30 | is.na(ldl)) & !is.na(hdlldlratio) & hdl<=4.7 & hdl>=0.3 ~ hdl/hdlldlratio,
                         TRUE ~ ldl),
         calctchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdltcratio<1 & hdl<=4.7 & hdl>=0.3 ~ 1,
                               TRUE ~ 0),
         tchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdltcratio<1 & hdl<=4.7 & hdl>=0.3 ~ hdl/hdltcratio,
                           TRUE ~ tchol),
         nonhdl = case_when((nonhdl>20 | nonhdl<0.26 | is.na(nonhdl)) & !is.na(hdlnonhdlratio) & hdl<=4.7 & hdl>=0.3 ~ hdl/hdlnonhdlratio,
                            TRUE ~ nonhdl))

FinalLipids$calctchol<-0
FinalLipids<-FinalLipids%>%
  mutate(calctchol= case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 & tri<=25 & tri>=0.1 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & nonhdl<=20 & nonhdl>=0.26 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & ldl<=10.3 & ldl>=0.26 & vldl<=2.26 & vldl>=0.06 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & tchdlratio<=15 & tchdlratio>=1 ~ 1,
                              TRUE ~ calctchol),
         tchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 & tri<=4.5 & tri>=0.1 ~ hdl+ldl+(tri/2.2),
                           (tchol>20 | is.na(tchol) |tchol<1.75) & nonhdl<=20 & nonhdl>=0.26 ~ hdl+nonhdl,
                           (tchol>20 | is.na(tchol) |tchol<1.75) & ldl<=10.3 & ldl>=0.26 & vldl<=2.26 & vldl>=0.06 ~ hdl+ldl+vldl,
                           (tchol>20 | is.na(tchol) |tchol<1.75) & tchdlratio<=15 & tchdlratio>=1 ~ hdl*tchdlratio,
                           TRUE ~ tchol))
table(FinalLipids$calctchol)
summary(FinalLipids$tchol[FinalLipids$calctchol==1])

#If dont have triglycerides, just add the median/2.2
FinalLipids<-FinalLipids%>%
  mutate(tchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 &is.na(tri) & is.na(vldl) ~ hdl+ldl+0.4,
                           TRUE ~ tchol))

FinalLipids<-FinalLipids%>%
  mutate(tchdlratio = case_when((tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & tchol<=20 & tchol>=1.75 & hdl>=0.3 & hdl<=4.7 ~ tchol/hdl,
                                (tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & tchol<=20 & tchol>=1.75 & (is.na(hdl)|hdl<0.3 |hdl>4.7) ~   tchol / ((tchdlratio/100)*tchol),
                                (tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & (is.na(tchol)|tchol>20 |tchol<1.75) & hdl>=0.3 & hdl<=4.7 ~ hdl/ ((tchdlratio/100) /hdl),
                                TRUE ~ tchdlratio))
#Final hdl
length(which(FinalLipids$hdl<0.3 | FinalLipids$hdl>4.7 | is.na(FinalLipids$hdl)))
FinalLipids<-FinalLipids%>%
  mutate(hdl = case_when((hdl<0.3 | hdl>4.7 | is.na(hdl)) & tchol<=20 & tchol>=1.75 & nonhdl<=20 & nonhdl>=0.26 ~ tchol-nonhdl,
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & tchol<=20 & tchol>=1.75 & ldl<=10.30 & ldl>=0.26 & is.na(nonhdl) & tri<=4.5 & tri>=0.1 ~ tchol-ldl-(tri/2.2),
                         (hdl<0.3 | hdl>4.7 | is.na(hdl)) & tchol<=20 & tchol>=1.75 & ldl<=10.30 & ldl>=0.26 & is.na(nonhdl)& is.na(tri) & vldl>=0.1 &vldl<2.5 ~ tchol-ldl-vldl,
                         TRUE ~ hdl))
#Final nonhdl
length(which(FinalLipids$nonhdl<0.26 | FinalLipids$nonhdl>20 | is.na(FinalLipids$nonhdl)))
FinalLipids<-FinalLipids%>%
  mutate(nonhdl = case_when((nonhdl<0.26 | nonhdl>20 | is.na(nonhdl)) & hdl<=4.4 & hdl>=0.3 & tchol>=1.75 & tchol<=20 ~ tchol-hdl,
                            (nonhdl<0.26 | nonhdl>20 | is.na(nonhdl) & (is.na(hdl)|is.na(tchol)) & tri<=25 & tri>=0.1 & ldl>=0.26 & ldl<=10.3) ~ ldl+(2.2*tri), 
                            TRUE ~ nonhdl))

#Final ldl
length(which(FinalLipids$ldl<10.3 | FinalLipids$ldl>0.26 | is.na(FinalLipids$ldl)))
FinalLipids<-FinalLipids%>%
  mutate(ldl = case_when((ldl<0.26 | ldl>10.3 | is.na(ldl)) & hdl<=4.4 & hdl>=0.3 & tchol>=1.75 & tchol<=20 & tri<=4.5 & tri>=0.1 ~ tchol-hdl-(tri/2.2),
                         (ldl<0.26 | ldl>10.3 | is.na(ldl)) & (is.na(tchol)|is.na(hdl)) &  tri<=4.5 & tri>=0.1 & nonhdl>=0.26 & nonhdl<=20 ~ nonhdl -(tri/2.2),
                         TRUE ~ ldl))

#Final tchdlratio
length(which(FinalLipids$tchdlratio>10 | FinalLipids$tchdlratio<1 | is.na(FinalLipids$tchdlratio)))
FinalLipids<-FinalLipids%>%
  mutate(tchdlratio = case_when((tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & tchol<=20 & tchol>=1.75 & hdl>=0.3 & hdl<=4.7 ~ tchol/hdl,
                                (tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & tchol<=20 & tchol>=1.75 & (is.na(hdl)|hdl<0.3 |hdl>4.7) ~   tchol / ((tchdlratio/100)*tchol),
                                (tchdlratio>10 | tchdlratio<1 | is.na(tchdlratio)) & (is.na(tchol)|tchol>20 |tchol<1.75) & hdl>=0.3 & hdl<=4.7 ~ hdl/ ((tchdlratio/100) /hdl),
                                TRUE ~ tchdlratio))

#Final total cholesterol
length(which(FinalLipids$tchol>20 | is.na(FinalLipids$tchol) |FinalLipids$tchol<1.75))

FinalLipids<-FinalLipids%>%
  mutate(calctchol= case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 & tri<=25 & tri>=0.1 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & nonhdl<=20 & nonhdl>=0.26 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & ldl<=10.3 & ldl>=0.26 & vldl<=2.26 & vldl>=0.06 ~ 1,
                              (tchol>20 | is.na(tchol) |tchol<1.75) & tchdlratio<=15 & tchdlratio>=1 ~ 1,
                              TRUE ~ calctchol),
         tchol = case_when((tchol>20 | is.na(tchol) |tchol<1.75) & hdl<=4.7 & hdl>=0.3 & ldl<=10.30 & ldl>=0.26 & tri<=4.5 & tri>=0.1 ~ hdl+ldl+(tri/2.2),
                           (tchol>20 | is.na(tchol) |tchol<1.75) & nonhdl<=20 & nonhdl>=0.26 ~ hdl+nonhdl,
                           (tchol>20 | is.na(tchol) |tchol<1.75) & ldl<=10.3 & ldl>=0.26 & vldl<=2.26 & vldl>=0.06 ~ hdl+ldl+vldl,
                           (tchol>20 | is.na(tchol) |tchol<1.75) & tchdlratio<=15 & tchdlratio>=1 ~ hdl*tchdlratio,
                           TRUE ~ tchol))

#If everything is blank then remove
FinalLipids<-FinalLipids%>%
  mutate(hdl = case_when (hdl>4.7 | hdl <0.3 ~ NA_real_,
                          TRUE ~ hdl),
         ldl = case_when (ldl>10.3 | ldl <0.26 ~ NA_real_,
                          TRUE ~ ldl),
         nonhdl = case_when (nonhdl>20 | nonhdl <0.26 ~ NA_real_,
                             TRUE ~ nonhdl),
         tri = case_when (tri>25 | tri <0.1 ~ NA_real_,
                          TRUE ~ tri),
         tchol = case_when (tchol>20 | tchol <1.75 ~ NA_real_,
                            TRUE ~ tchol),
         tchdlratio = case_when (tchdlratio>70 | tchdlratio <1 ~ NA_real_,
                                 TRUE ~ tchdlratio),
         hdlldlratio = case_when (hdlldlratio>19 | hdlldlratio <0.01 ~ NA_real_,
                                  TRUE ~ hdlldlratio),
         vldl = case_when (vldl>2.26 | vldl <0.06 ~ NA_real_,
                           TRUE ~ vldl),
         hdltcratio = case_when (hdltcratio>1 | hdltcratio <0.015 ~ NA_real_,
                                 TRUE ~ hdltcratio),
         hdlnonhdlratio = case_when (hdlnonhdlratio>19 | hdlnonhdlratio <0.01 ~ NA_real_,
                                     TRUE ~ hdlnonhdlratio))


FinalLipids<-FinalLipids[c(1:3, 26, 4:25)]

FinalLipids$sum <- rowSums(FinalLipids[,c(4:13)], na.rm=TRUE)
length(which(FinalLipids$sum>0))

FinalLipids <- FinalLipids %>%
  filter(sum != 0) %>%
  select(-sum)

length(which(is.na(FinalLipids$tchol)))

save(FinalLipids, file = "VariableExtracts/CPRD2023/MergedObs/FinalLipids.Rdata")

length(unique(FinalLipids$patid))
length(unique(FinalLipids$patdate))

FinalLipids<-select(FinalLipids, patid, tchol, eventdate)
FinalLipids<-subset(FinalLipids, !is.na(tchol))

####Statin trial####
length(unique(FinalLipids$patid))

Date<-select(Trial, patid, Trial1StatDate, FirstAPTrial3Date, end)
FinalLipids<-merge(x=FinalLipids, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Find closest in the last 3 years
Chol5<-FinalLipids%>%
  subset(eventdate<=Trial1StatDate)%>%
  mutate(Time=Trial1StatDate-eventdate)%>%
  subset(Time<=1095.75)

length(unique(Chol5$patid))

####Calculate most recent value####
Chol5<-Chol5%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)

length(unique(Chol5$patid))/length(which(!is.na(Trial$Trial1StatDate))) #91% complete

Chol5<-select(Chol5, patid, StatChol=tchol, CholDateStat=eventdate)

Chol5<-distinct(Chol5)
Trial<-merge(x=Trial, y=Chol5, by="patid", all.x=TRUE, all.y=FALSE)

####AP trial####

#Find closest in the last 3 years
Chol5<-FinalLipids%>%
  subset(eventdate<=FirstAPTrial3Date)%>%
  mutate(Time=FirstAPTrial3Date-eventdate)%>%
  subset(Time<=1095.75)

length(unique(Chol5$patid))

####Calculate most recent value####
Chol5<-Chol5%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)

length(unique(Chol5$patid))/length(which(!is.na(Trial$FirstAPTrial3Date))) #91% complete

Chol5<-select(Chol5, patid, APChol=tchol, CholDateAP=eventdate)
Chol5<-distinct(Chol5)
Trial<-merge(x=Trial, y=Chol5, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file="StatinCPRD/Data/Trial_Chol.rdata")


