####Create self-harm in CPRD###

rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)

AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Aurum Self harm#####
AurumSH<-AurumMed%>%
  mutate(SH = case_when((Term=="Suicidal deliberate poisoning"| MedCodeId=="5561371000006113"| Term=="Accident & emergency patient group - deliberate self-harm"|
                           grepl("injury undetermined", Term, ignore.case=TRUE)|grepl("self-ab", Term, ignore.case=TRUE)|grepl("injury \\?acci", Term, ignore.case=TRUE)|grepl("injury\\:\\?acci", Term, ignore.case=TRUE)) ~1,
                         (grepl("attendance|cooks|feeding|pharma|abort|employ|help|refer|complain|himself|esteem|cath|well|induced|exam|retain", Term, ignore.case=TRUE)|
                            grepl("screening|self-medi|administration|healing|wash|harmful|hazard|harmo|harma|reduct", Term, ignore.case=TRUE)|
                            grepl("acc |perception|view|minimisation|recognise|reliance|shave|comm|monitor|concept|care|cert|detox|cater|repo|neglect|patient|wheel", Term, ignore.case=TRUE)|
                            grepl("acci|self-man|self man|withdrawal|destruc|defeat|self me|taken|balanc|adh|emuls|honos|cured|compl", Term, ignore.case=TRUE)|
                           grepl("FH|dilat|guide|sampl|limit|bath|clean|move|safe|effic|asses|rating|confid|express|image", Term, ignore.case=TRUE)|
                           grepl("anti-suici|inflat|aware|suffic|iapt|maint|infec|com pharm|rhythm", Term, ignore.case=TRUE)|
                           grepl("H/O", Term, ignore.case=TRUE)|
                            grepl("history|depreciat|worth|stimulative|advocat|urinate|exposing|shower|dry|voice|doubt|consci", Term, ignore.case=TRUE)|
                            grepl("tripped|isolat|myself|yourself|rated|engag|control|scba|feed|understood|anger", Term, ignore.case=TRUE)|
                           grepl("ideation|palliative|self-disch|prevent|avoid|harmfl", Term, ignore.case=TRUE)|
                           grepl("no suic", Term, ignore.case=TRUE)|
                            grepl("thought", Term, ignore.case=TRUE)|
                             grepl("tremor", Term, ignore.case=TRUE)|
                           grepl("relative", Term, ignore.case=TRUE)|
                           grepl("potential", Term, ignore.case=TRUE)|
                            grepl("difficulty", Term, ignore.case=TRUE)|
                           grepl("risk", Term, ignore.case=TRUE)|
                           grepl("abandonment", Term, ignore.case=TRUE)|
                            grepl("suicidal", Term, ignore.case=TRUE)|
                            grepl("score", Term, ignore.case=TRUE)|
                            grepl("in therapy", Term, ignore.case=TRUE)|
                            grepl("during therapy", Term, ignore.case=TRUE)|
                            grepl("female genital", Term, ignore.case=TRUE)|
                            grepl("accidental", Term, ignore.case=TRUE)|
                            grepl("cough", Term, ignore.case=TRUE)|
                            grepl("unintent", Term, ignore.case=TRUE)|
                            (grepl("administer", Term, ignore.case=TRUE) & !(grepl("poison", Term, ignore.case=TRUE))) |
                            grepl("threatening", Term, ignore.case=TRUE)|
                            grepl("plan", Term, ignore.case=TRUE)|
                            grepl("longstanding", Term, ignore.case=TRUE)|
                            grepl("intentional round", Term, ignore.case=TRUE)|
                            grepl("intent regard", Term, ignore.case=TRUE)|
                            grepl("diagnostic intent", Term, ignore.case=TRUE)|
                            grepl("take home", Term, ignore.case=TRUE)|
                            grepl("number", Term, ignore.case=TRUE)|
                            grepl("precautions", Term, ignore.case=TRUE)|
                            grepl("previous", Term, ignore.case=TRUE)|
                            grepl("Intentional weight loss", Term, ignore.case=TRUE)|
                            grepl("treatment intent", Term, ignore.case=TRUE)|
                            grepl("procedure intent", Term, ignore.case=TRUE)|
                            grepl("intention to admit", Term, ignore.case=TRUE)|
                            grepl("treatment - intent", Term, ignore.case=TRUE)|
                            grepl("Deliberate shooting NOS", Term, ignore.case=TRUE)|
                            grepl("paradoxical", Term, ignore.case=TRUE)|
                            grepl("Intentionally shot", Term, ignore.case=TRUE)|
                            grepl("suicide intent", Term, ignore.case=TRUE)|
                            grepl("cash", Term, ignore.case=TRUE)|
                            grepl("\\*\\*", Term, ignore.case=TRUE)|
                            MedCodeId=="1697801000006111"|MedCodeId=="392271000006112"
                            ) ~0,#Remove as not clear
                          (grepl("intent", Term, ignore.case=TRUE) |
                          grepl("self", Term, ignore.case=TRUE) |
                            grepl("harm", Term, ignore.case=TRUE) |
                            grepl("deliberate", Term, ignore.case=TRUE)|
                          grepl("suicid", Term, ignore.case=TRUE)|
                            grepl("overdose", Term, ignore.case=TRUE)|
                            grepl("mutilation", Term, ignore.case=TRUE)|
                            grepl("int slf hrm|undt int|undet int", Term, ignore.case=TRUE)|
                            grepl("injurious", Term, ignore.case=TRUE)) ~1,
                          TRUE ~ 0))%>%
  subset(SH==1)

SarahAurum<-read.table("Codelists/SelfHarm22/Sarah/SH_Aurum.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(medcodeid="character"))
NotInSarah<-subset(AurumSH, !(MedCodeId %in% SarahAurum$medcodeid))
NotInNaomi<-subset(SarahAurum, !(medcodeid %in% AurumSH$MedCodeId))

AurumSH<-AurumSH%>%
  mutate(Group = case_when(grepl("self|deliberate|intentional|mutilation", Term, ignore.case=TRUE) ~ "self harm",
                           grepl("suici", Term, ignore.case=TRUE) ~ "attempted suicide",
                           grepl("overdose|unk|undeter|\\?", Term, ignore.case=TRUE) ~ "unknown intent",
                           TRUE ~ "Check"))
table(AurumSH$Group) 
Check<-subset(AurumSH, Group=="Check")
AurumSH$Group[AurumSH$Group=="Check"]<-"self harm"

####Gold self harm####
GoldSH<-GoldMed%>%
  mutate(SH = case_when((desc=="Suicidal deliberate poisoning"| desc=="Accident & emergency patient group - deliberate self-harm"|
                           grepl("injury undetermined", desc, ignore.case=TRUE)|grepl("self-ab", desc, ignore.case=TRUE)|grepl("injury \\?acci", desc, ignore.case=TRUE)|grepl("injury\\:\\?acci", desc, ignore.case=TRUE)) ~1,
                        (grepl("attendance|cooks|perception|feeding|pharma|abort|employ|help|refer|complain|himself|esteem|cath|well|induced|exam|retain", desc, ignore.case=TRUE)|
                           grepl("screening|self-medi|administration|healing|wash|harmful|hazard|harmo|harma|reduct", desc, ignore.case=TRUE)|
                           grepl("acc |comm|monitor|view|minimisation|recognise|reliance|shave|concept|care|cert|detox|cater|repo|neglect|patient|wheel", desc, ignore.case=TRUE)|
                           grepl("acci|self-man|self man|withdrawal|destruc|defeat|self me|taken|balanc|adh|emuls|honos|cured|compl", desc, ignore.case=TRUE)|
                           grepl("FH|dilat|guide|sampl|limit|bath|clean|move|safe|effic|asses|rating|confid|express|image", desc, ignore.case=TRUE)|
                           grepl("anti-suici|inflat|aware|suffic|iapt|maint|infec|com pharm|rhythm", desc, ignore.case=TRUE)|
                           grepl("H/O", desc, ignore.case=TRUE)|
                           grepl("history|depreciat|worth|stimulative|advocat|urinate|exposing|shower|dry|voice|doubt|consci", desc, ignore.case=TRUE)|
                           grepl("tripped|isolat|myself|yourself|rated|engag|control|scba|feed|understood|anger", desc, ignore.case=TRUE)|
                           grepl("ideation|palliative|self-disch|prevent|avoid|harmfl", desc, ignore.case=TRUE)|
                           grepl("no suic", desc, ignore.case=TRUE)|
                           grepl("thought", desc, ignore.case=TRUE)|
                           grepl("tremor", desc, ignore.case=TRUE)|
                           grepl("relative", desc, ignore.case=TRUE)|
                           grepl("potential", desc, ignore.case=TRUE)|
                           grepl("difficulty", desc, ignore.case=TRUE)|
                           grepl("risk", desc, ignore.case=TRUE)|
                           grepl("abandonment", desc, ignore.case=TRUE)|
                           grepl("suicidal", desc, ignore.case=TRUE)|
                           grepl("score", desc, ignore.case=TRUE)|
                           grepl("in therapy", desc, ignore.case=TRUE)|
                           grepl("during therapy", desc, ignore.case=TRUE)|
                           grepl("female genital", desc, ignore.case=TRUE)|
                           grepl("accidental", desc, ignore.case=TRUE)|
                           grepl("cough", desc, ignore.case=TRUE)|
                           grepl("unintent", desc, ignore.case=TRUE)|
                           (grepl("administer", desc, ignore.case=TRUE) & !(grepl("poison", desc, ignore.case=TRUE))) |
                           grepl("threatening", desc, ignore.case=TRUE)|
                           grepl("plan", desc, ignore.case=TRUE)|
                           grepl("longstanding", desc, ignore.case=TRUE)|
                           grepl("intentional round", desc, ignore.case=TRUE)|
                           grepl("intent regard", desc, ignore.case=TRUE)|
                           grepl("diagnostic intent", desc, ignore.case=TRUE)|
                           grepl("take home", desc, ignore.case=TRUE)|
                           grepl("number", desc, ignore.case=TRUE)|
                           grepl("precautions", desc, ignore.case=TRUE)|
                           grepl("previous", desc, ignore.case=TRUE)|
                           grepl("Intentional weight loss", desc, ignore.case=TRUE)|
                           grepl("treatment intent", desc, ignore.case=TRUE)|
                           grepl("procedure intent", desc, ignore.case=TRUE)|
                           grepl("intention to admit", desc, ignore.case=TRUE)|
                           grepl("treatment - intent", desc, ignore.case=TRUE)|
                           grepl("Deliberate shooting NOS", desc, ignore.case=TRUE)|
                           grepl("paradoxical", desc, ignore.case=TRUE)|
                           grepl("Intentionally shot", desc, ignore.case=TRUE)|
                           grepl("suicide intent", desc, ignore.case=TRUE)|
                           grepl("cash", desc, ignore.case=TRUE)|
                           grepl("\\*\\*", desc, ignore.case=TRUE)|
                           medcode=="39353"
                        ) ~0,#Remove as not clear
                        (grepl("intent", desc, ignore.case=TRUE) |
                           grepl("self", desc, ignore.case=TRUE) |
                           grepl("harm", desc, ignore.case=TRUE) |
                           grepl("deliberate", desc, ignore.case=TRUE)|
                           grepl("suicid", desc, ignore.case=TRUE)|
                           grepl("overdose", desc, ignore.case=TRUE)|
                           grepl("mutilation", desc, ignore.case=TRUE)|
                           grepl("int slf hrm|undt int|undet int", desc, ignore.case=TRUE)|
                           grepl("injurious", desc, ignore.case=TRUE)) ~1,
                        TRUE ~ 0))%>%
  subset(SH==1)

SarahGold<-read.table("Codelists/SelfHarm22/Sarah/SH_Gold.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(medcode="character"))
NotInSarah<-subset(GoldSH, !(medcode %in% SarahGold$medcode))
NotInNaomi<-subset(SarahGold, !(medcode %in% GoldSH$medcode))

GoldSH<-GoldSH%>%
  mutate(Group = case_when(grepl("self|deliberate|intentional|mutilation", desc, ignore.case=TRUE) ~ "self harm",
                           grepl("suici", desc, ignore.case=TRUE) ~ "attempted suicide",
                           grepl("overdose|unk|undet|\\?", desc, ignore.case=TRUE) ~ "unknown intent",
                           grepl("int", desc, ignore.case=TRUE) ~ "self harm",
                           TRUE ~ "Check"))
table(GoldSH$Group) 

####Cross check Gold and Aurum####
GoldSHCheck<-subset(GoldSH, !(desc %in% AurumSH$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldSHCheck$desc) #None to add on term

AurumSHCheck<-subset(AurumSH, !(Term %in% GoldSH$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumSHCheck$Term) #None to add on term

GoldSHCheck<-subset(GoldSH, !(readcode %in% AurumSH$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldSHCheck$readcode) #5 to add on readcode but don't because we don't know if they are right

AurumSHCheck<-subset(AurumSH, !(CleansedReadCode %in% GoldSH$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumSHCheck$CleansedReadCode) #36 to add on readcode

####Save the files####
write.table(AurumSH, file = "Codelists/SelfHarm22/SelfHarmAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

write.table(GoldSH, file = "Codelists/SelfHarm22/SelfHarmGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

