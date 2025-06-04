############Code List for cardiovascular disease and diabetes###############
rm(list = ls(all.names = TRUE))
####Libraries####
library(lubridate)
library(tidyverse)
library(stringr)

####Pull in Old files####
#From multimorbidity studies
GoldElix<-read.table("CodeLists/Charlson/Final lookups/GoldElix.txt", header=TRUE, sep="\t", colClasses=c(V1="character"))
GoldCharlson<-read.table("CodeLists/Charlson/Final lookups/GoldCharlson.txt", header=TRUE, sep="\t", colClasses=c(V1="character"))
AurumElix<-read.table("CodeLists/Charlson/Final lookups/AurumElixAll.txt", header=TRUE, sep="\t", colClasses=c(medcodeid="character"))
AurumCharlson<-read.table("CodeLists/Charlson/Final lookups/AurumChAll.txt", header=TRUE, sep="\t", colClasses=c(medcodeid="character"))

#Look ups
AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Hypertension (as per ICD-10, ####
#Format existing list
HypertensionElixAurum<-subset(AurumElix, startsWith(cat, "Hypertension"))
HypertensionElixAurum<-select(HypertensionElixAurum, medcodeid, cleansedreadcode, term, cat)
HypertensionAurum<-distinct(HypertensionElixAurum)

HypertensionElixGold<-subset(GoldElix, startsWith(cat, "Hypertension"))
HypertensionElixGold<-select(HypertensionElixGold, medcode=V1, readcode, readterm, cat)
HypertensionGold<-distinct(HypertensionElixGold)

####Check for new hypertension codes Aurum####
NewHypAurum<-subset(AurumMed, !(MedCodeId %in% HypertensionAurum$medcodeid))

#Checked codes for blood pressure or bp but none give definite diagnosis
NewHypAurum<-NewHypAurum%>%
  mutate(Hyp = case_when(grepl("without h|not diag|screen|exam|nonspec|preg|child|white|pre-e|maternal|gestational|neonatal|ocular|not required|no h\\/o|encephalopathy|labile|resolved|risk|pulmonary|venous|portal|intracranial|without diagnosis|family|fh|deleted|not to|decide|obstetric|score", Term, ignore.case=TRUE) ~0,
    grepl("hypertens", Term, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(Hyp==1)

HypertensionAurum<-select(HypertensionAurum, MedCodeId=medcodeid, CleansedReadCode=cleansedreadcode, Term=term)
NewHypAurum<-select(NewHypAurum, MedCodeId, CleansedReadCode, Term)
HypertensionAurum<-rbind(HypertensionAurum, NewHypAurum)

####Check for new hypertension codes Gold####
NewHypGold<-subset(GoldMed, !(medcode %in% HypertensionGold$medcode))

#Checked codes for blood pressure or bp but none give definite diagnosis
NewHypGold<-NewHypGold%>%
  mutate(Hyp = case_when(grepl("without h|screen|preg|white|pre-e|maternal|gestational|neonatal|ocular|not required|encephalopathy|resolved|risk|pulmonary|venous|portal|intracranial|family|fh|deleted", desc, ignore.case=TRUE) ~0,
                         grepl("hypertens", desc, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(Hyp==1)

HypertensionGold<-select(HypertensionGold, medcode, readcode, desc=readterm)
NewHypGold<-select(NewHypGold, medcode, readcode, desc)
HypertensionGold<-rbind(HypertensionGold, NewHypGold)

####Compare Gold and Aurum####
HypertensionAurum$Term<-tolower(HypertensionAurum$Term)
HypertensionAurum$Term<-gsub('"', '', HypertensionAurum$Term)
HypertensionGold$desc<-tolower(HypertensionGold$desc)

GoldHypertensionCheck<-subset(HypertensionGold, !(desc %in% HypertensionAurum$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldHypertensionCheck$desc) #None to add

AurumHypertensionCheck<-subset(HypertensionAurum, !(Term %in% HypertensionGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumHypertensionCheck$Term) #None to add on term

GoldHypertensionCheck<-subset(HypertensionGold, !(readcode %in% HypertensionAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldHypertensionCheck$readcode) #None to add

AurumHypertensionCheck<-subset(HypertensionAurum, !(CleansedReadCode %in% HypertensionGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumHypertensionCheck$CleansedReadCode) #5 to add on readcode

HypertensionGold<-rbind(HypertensionGold, AddtoGoldR)

####Save final lists####
write.table(HypertensionAurum, file = "Codelists/CVD22/HypertensionAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(HypertensionGold, file = "Codelists/CVD22/HypertensionGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####MI####
MICharlsonAurum<-subset(AurumCharlson, startsWith(cat, "Myocardial infarction"))
MICharlsonAurum<-select(MICharlsonAurum, medcodeid, cleansedreadcode, term, cat)
MIAurum<-distinct(MICharlsonAurum)

MICharlsonGold<-subset(GoldCharlson, startsWith(cat, "Myocardial infarction"))
MICharlsonGold<-select(MICharlsonGold, medcode=V1, readcode, readterm, cat)
MIGold<-distinct(MICharlsonGold)

#remove ischaemia code
MIAurum<-subset(MIAurum, term!="silent myocardial ischaemia")
MIGold<-subset(MIGold, readterm!="silent myocardial ischaemia")

####Check for new MI codes Aurum####
#Including coronary thrombosis and impending MI, but excluding ischaemia.
NewMIAurum<-subset(AurumMed, !(MedCodeId %in% MIAurum$medcodeid))

#Checked codes for blood pressure or bp but none give definite diagnosis
NewMIAurum<-NewMIAurum%>%
  mutate(MI = case_when(grepl("embol|disorder|choroidal|breast|liver|perfusion|fallopian|cardiomyopathy|omental|water|thyroid|ablation|placenta|stress|study|revascularisation|rheumatoid|lymph|mesenteric|mri|multi|oxygen|antibody|bridge|contusion|finding|of muscle|medulla|ovary|prostate|spleen|degen|basal|brain|biopsy|lung|fixed|problem|hepatic|fibrosis|family|fh|fet|anxiety|anaemic|no myocardial|vein thrombosis|venous thrombosis|suspected|cereb|arteri|dementia|stroke|bowel|intestinal|lacunar|spin|prei|pre-|pulmonary|renal|splenic|thalamic|score", Term, ignore.case=TRUE) ~0,
    Term=="acute infarct"|Term=="infarct"|Term=="infarction"|Term=="old infarct"|Term=="thrombotic infarction" ~ 0,
    grepl("isch", Term, ignore.case=TRUE) & !grepl("infarction", Term, ignore.case=TRUE) ~ 0,
                        grepl("myocardial|infarct|heart attack", Term, ignore.case=TRUE) ~1,
    grepl("coronary&throm", Term, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(MI==1)

MIAurum<-select(MIAurum, MedCodeId=medcodeid, CleansedReadCode=cleansedreadcode, Term=term)
NewMIAurum<-select(NewMIAurum, MedCodeId, CleansedReadCode, Term)
MIAurum<-rbind(MIAurum, NewMIAurum)

####Check for new MI codes Gold####
NewMIGold<-subset(GoldMed, !(medcode %in% MIGold$medcode))

#Check for myocardial infarction codes
NewMIGold<-NewMIGold%>%
  mutate(MI = case_when(grepl("embol|breast|perfusion|class|fallopian|thyroid|placenta|positron|study|lymph|multi|oxygen|antibody|bridge|contusion|of muscle|ovary|prostate|degen|basal|brain|problem|hepatic|fibrosis|family|fh|fet|no myocardial|venous thrombosis|suspected|cereb|arteri|dementia|stroke|bowel|spin|prei|pulmonary|renal|splenic", desc, ignore.case=TRUE) ~0,
    grepl("isch", desc, ignore.case=TRUE) & !grepl("infarction", desc, ignore.case=TRUE) ~ 0,
    grepl("myocardial|infarct|heart attack", desc, ignore.case=TRUE) ~1,
                        grepl("coronary&throm", desc, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(MI==1)

MIGold<-select(MIGold, medcode, readcode, desc=readterm)
NewMIGold<-select(NewMIGold, medcode, readcode, desc)
MIGold<-rbind(MIGold, NewMIGold)

####Compare Gold and Aurum####
MIAurum$Term<-tolower(MIAurum$Term)
MIAurum$Term<-gsub('"', '', MIAurum$Term)
MIGold$desc<-tolower(MIGold$desc)

GoldMICheck<-subset(MIGold, !(desc %in% MIAurum$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldMICheck$desc) #None to add

AurumMICheck<-subset(MIAurum, !(Term %in% MIGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumMICheck$Term) #None to add on term

GoldMICheck<-subset(MIGold, !(readcode %in% MIAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldMICheck$readcode) #None to add

AurumMICheck<-subset(MIAurum, !(CleansedReadCode %in% MIGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumMICheck$CleansedReadCode) #2 to add on readcode

MIGold<-rbind(MIGold, AddtoGoldR)

####Save final lists####
write.table(MIAurum, file = "Codelists/CVD22/MIAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(MIGold, file = "Codelists/CVD22/MIGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Heart failure####
CHFCharlsonAurum<-subset(AurumCharlson, startsWith(cat, "Congestive heart failure"))
CHFCharlsonAurum<-select(CHFCharlsonAurum, medcodeid, cleansedreadcode, term, cat)

CHFCharlsonGold<-subset(GoldCharlson, startsWith(cat, "Congestive heart failure"))
CHFCharlsonGold<-select(CHFCharlsonGold, medcode=V1, readcode, readterm, cat)

CHFElixAurum<-subset(AurumElix, startsWith(cat, "Congestive heart failure"))
CHFElixAurum<-select(CHFElixAurum, medcodeid, cleansedreadcode, term, cat)
CHFAurum<-distinct(rbind(CHFElixAurum, CHFCharlsonAurum))

CHFElixGold<-subset(GoldElix, startsWith(cat, "Congestive heart failure"))
CHFElixGold<-select(CHFElixGold, medcode=V1, readcode, readterm, cat)
CHFGold<-distinct(rbind(CHFElixGold, CHFCharlsonGold))

#Exclude cardiomyopathy and erroneous normal ECG
CHFAurum<-subset(CHFAurum, term!="congestive cardiomyopathy" & term!="echocardiogram shows normal left ventricular function" &term!="heart failure resolved" &term!="echocardiogram shows lvh"&term!="pulmonary oedema - acute")
CHFGold<-subset(CHFGold, readterm!="congestive cardiomyopathy" & readterm!="echocardiogram shows normal left ventricular function" &readterm!="echocardiogram shows lvh"&readterm!="pulmonary oedema - acute")

####Check for new CHF codes Aurum####

NewCHFAurum<-subset(AurumMed, !(MedCodeId %in% CHFAurum$medcodeid))

#Find heart failure codes
NewCHFAurum<-NewCHFAurum%>%
  mutate(CHF = case_when(grepl("non-|problem|screen|renal failure|without|family|fh|suspected|assessment|not to have|status|cha2ds2|chads2|cath|fast|transplant|excluded|resolve|carotid|fibrinogen|no cardiac|no evidence|somatoform", Term, ignore.case=TRUE) ~0,
    grepl("new york|ccf", Term, ignore.case=TRUE) ~1,
    grepl("heart", Term, ignore.case=TRUE) & grepl("failure", Term, ignore.case=TRUE)  ~1,
    grepl("ventric", Term, ignore.case=TRUE) & grepl("failure", Term, ignore.case=TRUE)  ~1,
    grepl("diastolic", Term, ignore.case=TRUE) & grepl("failure", Term, ignore.case=TRUE)  ~1,
    grepl("systolic", Term, ignore.case=TRUE) & grepl("failure", Term, ignore.case=TRUE)  ~1,
    grepl("cardiac", Term, ignore.case=TRUE) & grepl("failure", Term, ignore.case=TRUE)  ~1,
    grepl("heart", Term, ignore.case=TRUE) & grepl("dysfunction", Term, ignore.case=TRUE)  ~1,
    grepl("ventric", Term, ignore.case=TRUE) & grepl("dysfunction", Term, ignore.case=TRUE)  ~1,
    grepl("diastolic", Term, ignore.case=TRUE) & grepl("dysfunction", Term, ignore.case=TRUE)  ~1,
    grepl("systolic", Term, ignore.case=TRUE) & grepl("dysfunction", Term, ignore.case=TRUE)  ~1,
    grepl("cardiac", Term, ignore.case=TRUE) & grepl("dysfunction", Term, ignore.case=TRUE)  ~1,
    grepl("cardiac", Term, ignore.case=TRUE) & grepl("cirrhosis", Term, ignore.case=TRUE)  ~1,
    grepl("cardiac", Term, ignore.case=TRUE) & grepl("insufficiency", Term, ignore.case=TRUE)  ~1,
    grepl("cardiac", Term, ignore.case=TRUE) & grepl("asthma", Term, ignore.case=TRUE)  ~1,
                        TRUE ~ 0))%>%
  subset(CHF==1)

CHFAurum<-select(CHFAurum, MedCodeId=medcodeid, CleansedReadCode=cleansedreadcode, Term=term)
NewCHFAurum<-select(NewCHFAurum, MedCodeId, CleansedReadCode, Term)
CHFAurum<-rbind(CHFAurum, NewCHFAurum)

####Check for new CHF codes Gold####
NewCHFGold<-subset(GoldMed, !(medcode %in% CHFGold$medcode))

#Checked codes for blood pressure or bp but none give definite diagnosis
NewCHFGold<-NewCHFGold%>%
  mutate(CHF = case_when(grepl("screen|renal failure|without|suspected|cath|transplant|excluded|resolve|no evidence", desc, ignore.case=TRUE) ~0,
                         grepl("new york|ccf", desc, ignore.case=TRUE) ~1,
                         grepl("heart", desc, ignore.case=TRUE) & grepl("failure", desc, ignore.case=TRUE)  ~1,
                         grepl("ventric", desc, ignore.case=TRUE) & grepl("failure", desc, ignore.case=TRUE)  ~1,
                         grepl("diastolic", desc, ignore.case=TRUE) & grepl("failure", desc, ignore.case=TRUE)  ~1,
                         grepl("systolic", desc, ignore.case=TRUE) & grepl("failure", desc, ignore.case=TRUE)  ~1,
                         grepl("cardiac", desc, ignore.case=TRUE) & grepl("failure", desc, ignore.case=TRUE)  ~1,
                         grepl("heart", desc, ignore.case=TRUE) & grepl("dysfunction", desc, ignore.case=TRUE)  ~1,
                         grepl("ventric", desc, ignore.case=TRUE) & grepl("dysfunction", desc, ignore.case=TRUE)  ~1,
                         grepl("diastolic", desc, ignore.case=TRUE) & grepl("dysfunction", desc, ignore.case=TRUE)  ~1,
                         grepl("systolic", desc, ignore.case=TRUE) & grepl("dysfunction", desc, ignore.case=TRUE)  ~1,
                         grepl("cardiac", desc, ignore.case=TRUE) & grepl("dysfunction", desc, ignore.case=TRUE)  ~1,
                         grepl("cardiac", desc, ignore.case=TRUE) & grepl("cirrhosis", desc, ignore.case=TRUE)  ~1,
                         grepl("cardiac", desc, ignore.case=TRUE) & grepl("insufficiency", desc, ignore.case=TRUE)  ~1,
                         grepl("cardiac", desc, ignore.case=TRUE) & grepl("asthma", desc, ignore.case=TRUE)  ~1,
                         TRUE ~ 0))%>%
  subset(CHF==1)

CHFGold<-select(CHFGold, medcode, readcode, desc=readterm)
NewCHFGold<-select(NewCHFGold, medcode, readcode, desc)
CHFGold<-rbind(CHFGold, NewCHFGold)

####Compare Gold and Aurum####
CHFAurum$Term<-tolower(CHFAurum$Term)
CHFAurum$Term<-gsub('"', '', CHFAurum$Term)
CHFGold$desc<-tolower(CHFGold$desc)

GoldCHFCheck<-subset(CHFGold, !(desc %in% CHFAurum$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldCHFCheck$desc) #None to add

AurumCHFCheck<-subset(CHFAurum, !(Term %in% CHFGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumCHFCheck$Term) #None to add on term

GoldCHFCheck<-subset(CHFGold, !(readcode %in% CHFAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldCHFCheck$readcode) #None to add

AurumCHFCheck<-subset(CHFAurum, !(CleansedReadCode %in% CHFGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumCHFCheck$CleansedReadCode) #0 to add on readcode

####Save final lists####
write.table(CHFAurum, file = "Codelists/CVD22/CHFAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(CHFGold, file = "Codelists/CVD22/CHFGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Cerebrovascular disease####
CerebCharlsonAurum<-subset(AurumCharlson, startsWith(cat, "Cerebrovascular disease"))
CerebCharlsonAurum<-select(CerebCharlsonAurum, medcodeid, cleansedreadcode, term, cat)
CerebAurum<-distinct(CerebCharlsonAurum)

CerebCharlsonGold<-subset(GoldCharlson, startsWith(cat, "Cerebrovascular disease"))
CerebCharlsonGold<-select(CerebCharlsonGold, medcode=V1, readcode, readterm, cat)
CerebGold<-distinct(CerebCharlsonGold)

#Exclude scores and newborn codes, suspected and transient
CerebGold<-CerebGold%>%
  mutate(select=case_when(readterm=="hyperten, abnorm renal/liver funct, stroke, bled score" ~ 0,
                          grepl("fet|suspect|infantile", readterm, ignore.case=TRUE)~0,
                          grepl("transient", readterm, ignore.case=TRUE) & !grepl("stroke", readterm, ignore.case=TRUE)~0,
                          TRUE~1))%>%
  subset(select==1)%>%
  select(-select)

CerebAurum<-CerebAurum%>%
  mutate(select=case_when(term=="hyperten, abnorm renal/liver funct, stroke, bled score" ~ 0,
                          grepl("fet|suspect|infantile", term, ignore.case=TRUE)~0,
                          grepl("transient", term, ignore.case=TRUE) & !grepl("stroke", term, ignore.case=TRUE)~0,
                          TRUE~1))%>%
  subset(select==1)%>%
  select(-select)
####Check for new Cereb codes Aurum####

NewCerebAurum<-subset(AurumMed, !(MedCodeId %in% CerebAurum$medcodeid))

#Include non traumatic haemorrhage but not haematoma, exclude TIA as not in ICD-10
NewCerebAurum<-NewCerebAurum%>%
  mutate(Cereb = case_when(grepl("evacuation|scrotal|breast|retinocochleocerebral|perinatal|ocular|myocardial|contracture|muscle|dysgenesis|conus|cvag|cerebri|toxo|distension|ventriculomegaly|white|spinal|lupus|pressure|vessel|steal|meningioma|metastases|ectomy|herniation|birth|fungus|gigantism|plasty|function|cryptococcosis|concussion|atonia|akinetopsia|anoxia|dystrophy|stent|cauda equina|spinal cord|rhinocerebral|sunstroke|structure|tuberculosis|anastomosis|naevus|shunt|cyst|degeneration|column|cervicocranial|agenesis|thalassaemia|adrenocortical|plasia|ataxia|stein|prevention|graphy|angiog|angiopl|anom|reconstruction|test|compression|brain syndrome|lymphoma|repair|resection|reversible|neoplasm|family|fh|no h|palsy|tumour|fluid|risk|glioma|lipidoses|irrita|mri|cytoma|malaria|scan|oedema|cerebral arteritis|lethal|score|contusion|ophth|oculocerebrorenal|embolectomy|carcinoma|neonatal|fet|newborn|lesion|transection|atrophy|ca cerebrum|exophthalmos|dominance|globus|hypothalamic|thrombasthenia|heat|glucocerebrosidosis|lactic|prematurity|lenz|bypass|melkersson|meningocele|suspect|entire|adverse reaction|adhesions|\\[so\\]|inj|^traum|[[:space:]]traum|abscess|cerebellopontine|capsular|caudal|retinal|carotid sinus|acuity|level", Term, ignore.case=TRUE) ~0,
    Term=="vertebral artery aneurysm"|Term=="cerebral artery"|Term=="anterior cerebral artery"|Term=="cerebellum"|Term=="left cerebral hemisphere"|Term=="left posterior cerebral artery"|Term=="left middle cerebral artery"|Term=="branch of middle cerebral artery" | Term=="middle cerebral artery" | Term=="right middle cerebral artery"|Term=="right cerebral hemisphere"|Term=="posterior cerebral artery"|Term=="posterior cerebellar lobe fissure" ~ 0,
                           grepl("operation|embolisation", Term, ignore.case=TRUE) & grepl("aneurysm", Term, ignore.case=TRUE) & grepl("isch|stroke|cva|cereb|subarachnoid|carotid|berry|bas|subdural|vertebr|brain|cortical|capsul|thal|caud|putamen|globus|pallidusa|lobar|tentor|cranial|medul|intraventric|pontine", Term, ignore.case=TRUE)~1,
                           grepl("operation|embolisation", Term, ignore.case=TRUE) ~ 0,
                           grepl("transient", Term, ignore.case=TRUE) & !grepl("stroke", Term, ignore.case=TRUE) ~ 0,
    grepl("malformation", Term, ignore.case=TRUE) & !grepl("hemorrhage", Term, ignore.case=TRUE) ~ 0, 
    grepl("congenital", Term, ignore.case=TRUE) & !grepl("aneurysm", Term, ignore.case=TRUE) ~ 0, 
    grepl("dissection", Term, ignore.case=TRUE) & !grepl("aneurysm|stroke", Term, ignore.case=TRUE) ~ 0, 
    grepl("carotid", Term, ignore.case=TRUE) & grepl("aneurysm", Term, ignore.case=TRUE)  ~0,
    grepl("[[:punct:]]traum", Term, ignore.case=TRUE) & !grepl("non\\-traum", Term, ignore.case=TRUE) ~ 0, 
    grepl("cerebellar", Term, ignore.case=TRUE) & !grepl("stroke|heamorrha|hemorrha|haemorrha|accident", Term, ignore.case=TRUE)  ~0,
    grepl("stroke|cva|cereb", Term, ignore.case=TRUE) ~1,
    grepl("haematoma|hematoma", Term, ignore.case=TRUE) & grepl("non\\-traum|nontraum", Term, ignore.case=TRUE) ~ 1,
    grepl("isch|arachnoid|carotid|berry|bas|subdural|vertebr|brain|cortical|capsul|thal|caud|putamen|globus|pallidusa|lobar|tentor|cranial|medul|intraventric|pontine", Term, ignore.case=TRUE) & grepl("aneurysm|attack|heamorrha|hemorrha|haemorrha|occlusion|infarction|syndrome", Term, ignore.case=TRUE)  ~1,
                                   TRUE ~ 0))%>%
  subset(Cereb==1)

CerebAurum<-select(CerebAurum, MedCodeId=medcodeid, CleansedReadCode=cleansedreadcode, Term=term)
NewCerebAurum<-select(NewCerebAurum, MedCodeId, CleansedReadCode, Term)
CerebAurum<-rbind(CerebAurum, NewCerebAurum)

####Check for new Cereb codes Gold####
NewCerebGold<-subset(GoldMed, !(medcode %in% CerebGold$medcode))

#Find new cerebrovascular codes
NewCerebGold<-NewCerebGold%>%
  mutate(Cereb = case_when(grepl("scrotal|breast|testicular|evacuation|transnt|scpe|mitochond|metastasis|scre|retinocochleocerebral|perinatal|muscle|cerebri|spinal|lupus|vessel|meningioma|ectomy|birth|fungus|gigantism|function|cryptococcosis|concussion|atonia|akinetopsia|anoxia|dystrophy|stent|cauda equina|spinal cord|rhinocerebral|sunstroke|tuberculosis|anastomosis|naevus|shunt|cyst|degeneration|column|cervicocranial|agenesis|adrenocortical|plasia|ataxia|stein|prevention|graphy|anom|compression|brain syndrome|repair|reversible|neoplasm|family|fh|palsy|tumour|fluid|risk|glioma|lipidoses|irrita|malaria|scan|oedema|cerebral arteritis|lethal|score|contusion|ophth|oculocerebrorenal|embolectomy|carcinoma|neonatal|fet|newborn|lesion|atrophy|exophthalmos|thrombasthenia|heat|lenz|meningocele|suspect|adhesions|\\[so\\]|inj|^traum|[[:space:]]traum|abscess|cerebellopontine|retinal|carotid sinus|level", desc, ignore.case=TRUE) ~0,
    grepl("operation|embolisation", desc, ignore.case=TRUE) & grepl("aneurysm", desc, ignore.case=TRUE) & grepl("isch|stroke|cva|cereb|subarachnoid|carotid|berry|bas|subdural|vertebr|brain|cortical|capsul|thal|caud|putamen|globus|pallidusa|lobar|tentor|cranial|medul|intraventric|pontine", desc, ignore.case=TRUE)~1,
                           grepl("operation|embolisation", desc, ignore.case=TRUE) ~ 0,
                           grepl("transient", desc, ignore.case=TRUE) & !grepl("stroke", desc, ignore.case=TRUE) ~ 0,
                           grepl("malformation", desc, ignore.case=TRUE) & !grepl("hemorrhage", desc, ignore.case=TRUE) ~ 0, 
                           grepl("congenital", desc, ignore.case=TRUE) & !grepl("aneurysm", desc, ignore.case=TRUE) ~ 0, 
                           grepl("dissection", desc, ignore.case=TRUE) & !grepl("aneurysm|stroke", desc, ignore.case=TRUE) ~ 0, 
                           grepl("carotid", desc, ignore.case=TRUE) & grepl("aneurysm", desc, ignore.case=TRUE)  ~0,
    grepl("[[:punct:]]traum", desc, ignore.case=TRUE) & !grepl("non\\-traum", desc, ignore.case=TRUE) ~ 0,                        
    grepl("cerebellar", desc, ignore.case=TRUE) & !grepl("stroke|heamorrha|hemorrha|haemorrha|haematoma|accident", desc, ignore.case=TRUE)  ~0,
    grepl("haematoma|hematoma", desc, ignore.case=TRUE) & grepl("non\\-traum|nontraum", desc, ignore.case=TRUE) ~ 1,                       
    grepl("stroke|cva|cereb", desc, ignore.case=TRUE) ~1,
                           grepl("isch|arachnoid|carotid|berry|bas|subdural|vertebr|brain|cortical|capsul|thal|caud|putamen|globus|pallidusa|lobar|tentor|cranial|medul|intraventric|pontine", desc, ignore.case=TRUE) & grepl("aneurysm|attack|heamorrha|hemorrha|haemorrha|occlusion|infarction|syndrome", desc, ignore.case=TRUE)  ~1,
                           TRUE ~ 0))%>%
  subset(Cereb==1)

CerebGold<-select(CerebGold, medcode, readcode, desc=readterm)
NewCerebGold<-select(NewCerebGold, medcode, readcode, desc)
CerebGold<-rbind(CerebGold, NewCerebGold)

####Compare Gold and Aurum####
CerebAurum$Term<-tolower(CerebAurum$Term)
CerebAurum$Term<-gsub('"', '', CerebAurum$Term)
CerebGold$desc<-tolower(CerebGold$desc)

GoldCerebCheck<-subset(CerebGold, !(desc %in% CerebAurum$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldCerebCheck$desc) #None to add

AurumCerebCheck<-subset(CerebAurum, !(Term %in% CerebGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumCerebCheck$Term) #None to add on term

GoldCerebCheck<-subset(CerebGold, !(readcode %in% CerebAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldCerebCheck$readcode) #None to add

AurumCerebCheck<-subset(CerebAurum, !(CleansedReadCode %in% CerebGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumCerebCheck$CleansedReadCode) #0 to add on readcode

AddtoGoldR<-subset(AddtoGoldR, grepl("F", readcode, ignore.case=TRUE))
CerebGold<-rbind(CerebGold, AddtoGoldR)
####Save final lists####
write.table(CerebAurum, file = "Codelists/CVD22/CerebAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(CerebGold, file = "Codelists/CVD22/CerebGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Diabetes####
#Format existing list
DiabCharlsonAurum<-subset(AurumCharlson, startsWith(cat, "Diabetes"))
DiabCharlsonAurum<-select(DiabCharlsonAurum, medcodeid, cleansedreadcode, term, cat)

DiabCharlsonGold<-subset(GoldCharlson, startsWith(cat, "Diabetes"))
DiabCharlsonGold<-select(DiabCharlsonGold, medcode=V1, readcode, readterm, cat)

DiabElixAurum<-subset(AurumElix, startsWith(cat, "Diabetes"))
DiabElixAurum<-select(DiabElixAurum, medcodeid, cleansedreadcode, term, cat)
DiabAurum<-distinct(rbind(DiabElixAurum, DiabCharlsonAurum))

DiabElixGold<-subset(GoldElix, startsWith(cat, "Diabetes"))
DiabElixGold<-select(DiabElixGold, medcode=V1, readcode, readterm, cat)
DiabGold<-distinct(rbind(DiabElixGold, DiabCharlsonGold))

#Llimit
DiabGold<-DiabGold%>%
  mutate(select=case_when(grepl("haemoglobin a1c|fundoscopy|family|high risk review", readterm, ignore.case=TRUE)~0,
                          TRUE~1))%>%
  subset(select==1)%>%
  select(-select)

DiabAurum<-DiabAurum%>%
  mutate(select=case_when(grepl("haemoglobin a1c|fundoscopy|family|high risk review", term, ignore.case=TRUE)~ 0,
                          TRUE~1))%>%
  subset(select==1)%>%
  select(-select)

####Check for new Diab codes Aurum####

NewDiabAurum<-subset(AurumMed, !(MedCodeId %in% DiabAurum$medcodeid))

#Find new diab codes
NewDiabAurum<-NewDiabAurum%>%
  mutate(Diab = case_when(grepl("nondiabetic|risk of diabetes|high risk review|diabetes-deafness|hba1 level|high risk of|hyperproinsulinaemia|insulin like|receptor abnormality|hba1c level|hyperinsulina|fundoscopy|hyperinsulinaemia|relative|diabetic mother syndrome|diabetes mellitus screening|resolved|insulin resistance|insulin autoimmune syndrome|measurement|insulin level|antibody level|test|hypoinsulinaemia|maternal diabetes syndrome|no h\\/o: diabetes mellitus|poisoning|hyperinsulinism|qdiabetes|remediable|urine screening|risk engine|transient|suspected|walking away|non\\-diabetic|shock therapy|insulin\\-like|pro\\-insulin|non\\-diabetes|decide|insulinoma|diabetes mellitus excluded|not to have|at risk of|prevention|risk calc|risk score|electroshock|blood pressure|insipidus|adverse reaction|infant|stroke|dmard|dmo|dmd|dmek|dmsa|family history|fh|screening for|diabetes scre|prediab|pre-diab", Term, ignore.case=TRUE)  ~0,
    grepl("insulin|diab", Term, ignore.case=TRUE)  ~1,
    grepl("[[:space:]]dm|[[:punct:]]dm", Term, ignore.case=TRUE)  ~1,
    startsWith(Term, "dm") ~1,
                           TRUE ~ 0))%>%
  subset(Diab==1)

DiabAurum<-select(DiabAurum, MedCodeId=medcodeid, CleansedReadCode=cleansedreadcode, Term=term)
NewDiabAurum<-select(NewDiabAurum, MedCodeId, CleansedReadCode, Term)
DiabAurum<-rbind(DiabAurum, NewDiabAurum)

#Sort out categories
DiabAurum<-DiabAurum%>%
  mutate(cat= case_when(grepl("neonate|secondary endocrine diabetes|renal diabetes|renal cysts and diabetes syndrome|protein-deficient|neonatal diabetes mellitus|maternally inherited|insulinopenic|due to cystic fibrosis|cystic fibrosis related|other specified diabete|secondary diab|bronze|pancreatic|induced|steroid|maturity|malnut|caused by|lipoatrophic|lada|atypical|genetic|autosomal|hormonal", Term, ignore.case=TRUE)  ~"Other",
                        grepl("no longer diabetic|no longer categorised as diabetic|xpert|x\\-pert|remission|help-diabetes|insulin-resistant diabetes|ascend|non-insulin depend|insulin refused|insulin therapy declined|insulin treatment stopped|niddm|non-insulin-dependent|ins-treat non-ins-dep|on diet only|oral|diabetes education and self managemen|desmond|jamaica|type ii|type 2|adult|diet controlled", Term, ignore.case=TRUE)  ~"Type 2",
                        grepl("insulin dependent|iddm|insulin-dependent|dose adjustment for normal eating|dafne|juvenile|type i|type 1|insulin receptor", Term, ignore.case=TRUE)  ~"Type 1",
                        grepl("gestational|arising in pregnancy", Term, ignore.case=TRUE) ~ "Def gestational",
                        grepl("preg\\.|maternal diabetes|antenatal clinic|puerperium|pregnan|mother", Term, ignore.case=TRUE) &!grepl("pre\\-|requirement", Term, ignore.case=TRUE)  ~"Poss gestational",
                        TRUE ~ "Unknown"))
#Check categories
Other<-subset(DiabAurum, cat=="Other")
Type1<-subset(DiabAurum, cat=="Type 1")
Type2<-subset(DiabAurum, cat=="Type 2")
Posspreg<-subset(DiabAurum, cat=="Poss gestational")
Defpreg<-subset(DiabAurum, cat=="Def gestational")
Unknown<-subset(DiabAurum, cat=="Unknown")

####Check for new Diab codes Gold####
NewDiabGold<-subset(GoldMed, !(medcode %in% DiabGold$medcode))

#Find new Diab codes
NewDiabGold<-NewDiabGold%>%
  mutate(Diab = case_when(grepl("serum insulin|evaluation|diabetes mellitus screen|nondiab|risk of diabetes|fundoscopy|relative|resolved|insulin resistance|insulinoma|insulin autoimmune syndrome|insulin level|antibody level|test|hypoinsulinaemia|poisoning|hyperinsulinism|qdiabetes|urine screening|suspected|non\\-diabet|shock therapy|insulin\\-like|pro\\-insulin|diabetes mellitus excluded|at risk of|prevention|risk score|electroshock|insipidus|adverse reaction|infant|stroke|dmard|dmo|dmsa|family history|fh|screening for|diabetes scre|prediab|pre-diab", desc, ignore.case=TRUE)  ~0,
grepl("insulin|diab", desc, ignore.case=TRUE)  ~1,
         grepl("[[:space:]]dm|[[:punct:]]dm", desc, ignore.case=TRUE)  ~1,
         startsWith(desc, "dm") ~1,
         TRUE ~ 0))%>%
  subset(Diab==1)

DiabGold<-select(DiabGold, medcode, readcode, desc=readterm)
NewDiabGold<-select(NewDiabGold, medcode, readcode, desc)
DiabGold<-rbind(DiabGold, NewDiabGold)

#Sort out categories
DiabGold<-DiabGold%>%
  mutate(cat= case_when(grepl("neonate|secondary endocrine diabetes|renal diabetes|renal cysts and diabetes syndrome|protein-deficient|neonatal diabetes mellitus|maternally inherited|insulinopenic|due to cystic fibrosis|cystic fibrosis related|other specified diabete|secondary diab|bronze|pancreatic|induced|steroid|maturity|malnut|caused by|lipoatrophic|lada|atypical|genetic|autosomal|hormonal", desc, ignore.case=TRUE)  ~"Other",
                        grepl("no longer diabetic|no longer categorised as diabetic|xpert|x\\-pert|remission|help-diabetes|insulin-resistant diabetes|ascend|non-insulin depend|insulin refused|insulin therapy declined|insulin treatment stopped|niddm|non-insulin-dependent|ins-treat non-ins-dep|on diet only|oral|diabetes education and self managemen|desmond|jamaica|type ii|type 2|adult|diet controlled", desc, ignore.case=TRUE)  ~"Type 2",
                        grepl("insulin dependent|iddm|insulin-dependent|dose adjustment for normal eating|dafne|juvenile|type i|type 1|insulin receptor", desc, ignore.case=TRUE)  ~"Type 1",
                        grepl("gestational|arising in pregnancy", desc, ignore.case=TRUE) ~ "Def gestational",
                        grepl("preg\\.|maternal diabetes|antenatal clinic|puerperium|pregnan|mother", desc, ignore.case=TRUE) &!grepl("pre\\-|requirement", desc, ignore.case=TRUE)  ~"Poss gestational",
                        TRUE ~ "Unknown"))
#Check categories
Other<-subset(DiabGold, cat=="Other")
Type1<-subset(DiabGold, cat=="Type 1")
Type2<-subset(DiabGold, cat=="Type 2")
Posspreg<-subset(DiabGold, cat=="Poss gestational")
Defpreg<-subset(DiabGold, cat=="Def gestational")
Unknown<-subset(DiabGold, cat=="Unknown")

####Compare Gold and Aurum####
DiabAurum$Term<-tolower(DiabAurum$Term)
DiabAurum$Term<-gsub('"', '', DiabAurum$Term)
DiabGold$desc<-tolower(DiabGold$desc)

GoldDiabCheck<-subset(DiabGold, !(desc %in% DiabAurum$Term))
AddtoAurumT<-subset(AurumMed, Term %in% GoldDiabCheck$desc) #Ignore - is there really.

AurumDiabCheck<-subset(DiabAurum, !(Term %in% DiabGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumDiabCheck$Term) #None to add on term

GoldDiabCheck<-subset(DiabGold, !(readcode %in% DiabAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldDiabCheck$readcode) #Ignore - is there really.

AurumDiabCheck<-subset(DiabAurum, !(CleansedReadCode %in% DiabGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumDiabCheck$CleansedReadCode) #0 to add on readcode

AddtoGoldR<-subset(AddtoGoldR, grepl("F", readcode, ignore.case=TRUE))
DiabGold<-rbind(DiabGold, AddtoGoldR)

####Save final lists####
write.table(DiabAurum, file = "Codelists/CVD22/DiabAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(DiabGold, file = "Codelists/CVD22/DiabGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

#Cross check diabetes with Annies for CCB study
DiabAurum<-read.table("Codelists/CVD22/DiabAurum.txt", header=TRUE, sep="\t", colClasses=c(MedCodeId="character"))
DiabGold<-read.table("Codelists/CVD22/DiabGold.txt", header=TRUE, sep="\t", colClasses=c(medcode="character"))
DiabAnnie<-read.table("Codelists/CVD22/ANNIE_ALLDIABETESCODES.txt", quote = "", header=TRUE, sep="\t", colClasses=c(medcode.G="character", medcodeA="character" ))

DiabNotInc<-subset(DiabAnnie, !(readcode %in% DiabAurum$CleansedReadCode) & !(readcode %in% DiabGold$readcode))
DiabNotInc<-subset(DiabNotInc, !(medcode.G %in% DiabGold$medcode))
DiabNotInc$readterm<-tolower(DiabNotInc$readterm)
DiabNotInc<-subset(DiabNotInc, !(readterm %in% DiabGold$desc) & !(readterm %in% DiabGold$Term))

GoldAdd<-subset(GoldMed, medcode %in% DiabNotInc$medcode.G)
AurumAdd<-subset(AurumMed, OriginalReadCode %in% DiabNotInc$readcode | CleansedReadCode %in% DiabNotInc$readcode )

write.table(GoldAdd, file = "Codelists/CVD22/AnnieDiabCheckGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(AurumAdd, file = "Codelists/CVD22/AnnieDiabCheckAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")


