#Clear environment

rm(list = ls(all.names = TRUE))

#~~~Libraries
library(tidyverse)
library(lubridate)
library(haven)

####Load product dictionaries####
AurumProd<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISProductDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(ProdCodeId="character"))
GoldProd<-read.table("LookUps/202303_Lookups_CPRDGOLD/product.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(prodcode="character"))

FirstGenList<-as.list(c("benperidol",  "Chlorpromazine",  "Droperidol",  "Flupenthix",  "flupentixol",  "fluphenaz", 
                        "fluphenazine",  "fluspirilene",  "hadol",  "haloperidol",  "levomepromazine",  "loxapine",  
                        "oxypertine",  "pericyazine",  "Perphenazine",  "pimozide",  "pipotiazine", 
                        "promazine",  "sulpiride",  "thioridazine",  "trifluoperazine",  "Trifluperidol",  
                        "Pipothiazine", "zuclopenthix",  "zuclopenthixol",  "prochlorperazine",  "Chlorprothixene",  
                        "Methotrimeprazine", "Thioproperazine", "Thiopropazate"))


SecondGenList<-as.list(c("Amisulpride",  "Aripiprazole",  "Clozapine",  "Olanzapine",  "Quetiapine",  
                         "Remoxipride",  "Risperidone",  "Sertindole",  "Sertindone",  "Zotepine", 
                         "Asenapine",  "Cariprazine",  "Lurasidone", "Paliperidone", "Melperone", "Ziprasidone", "Clotiapine"))

BPMed<-as.list(c("valproate", "lithium", "lamotrigine"))

####AP search#####
AurumAP<-AurumProd%>%
  mutate(Gen = case_when(grepl(paste(SecondGenList, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~2,
                        grepl(paste(SecondGenList, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~2,
                        grepl(paste(SecondGenList, collapse='|'), ProductName, ignore.case=TRUE) ~2,
                        grepl(paste(FirstGenList, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~1,
                        grepl(paste(FirstGenList, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~1,
                        grepl(paste(FirstGenList, collapse='|'), ProductName, ignore.case=TRUE) ~1,
                        grepl(paste(BPMed, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~3,
                        grepl(paste(BPMed, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~3,
                        grepl(paste(BPMed, collapse='|'), ProductName, ignore.case=TRUE) ~3,
                        BNFChapter=="4020100"~ 4,
                            TRUE ~ 0))%>%
  mutate(Gen = case_when(ProdCodeId=="5109741000033110" ~ 1,
                         TRUE ~ Gen),
         DrugSubstanceName = case_when(ProdCodeId=="5109741000033110" ~ "Sulpiride",
                                       TRUE ~ DrugSubstanceName))%>%
  subset(Gen>0)%>%
  mutate(AP = case_when(DrugSubstanceName!="" ~ word(DrugSubstanceName, 1),
                       TRUE ~ word(Term.from.EMIS, 1)))
         
table(AurumAP$AP)
AurumAP<-subset(AurumAP, AurumAP$AP!="Amitriptyline")
AurumAP$AP[AurumAP$AP=="Sodium"]<-"Valproate"
AurumAP<-subset(AurumAP, AP!="Levacetylmethadol")
AurumAP$AP[AurumAP$AP=="*Lithium"]<-"Lithium"
AurumAP$AP[AurumAP$AP=="Pipothiazine"]<-"Pipotiazine"

table(AurumAP$DrugSubstanceName)

AurumAP<-subset(AurumAP, DrugSubstanceName!="Lithium succinate/ Zinc sulfate")
AurumAP<-subset(AurumAP, DrugSubstanceName!="Fluphenazine hydrochloride/ Nortriptyline hydrochloride")

table(AurumAP$Gen)

AurumAPLab1<-unique(as.list((word(AurumAP$ProductName, 1)), 
                              (word(AurumAP$Term.from.EMIS, 1)), 
                              (word(AurumAP$DrugSubstanceName, 1))))

AurumAPLab1<-subset(AurumAPLab1, AurumAPLab1!= "" & AurumAPLab1!="Sodium")
AurumAPLab1<-append(AurumAPLab1, "sodium valproate")
AurumAP1<-subset(AurumProd, !(ProdCodeId %in% AurumAP$ProdCodeId))
AurumAP1<-subset(AurumAP1, (grepl(paste(AurumAPLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumAPLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumAPLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))


AurumAP1<-subset(AurumAP1, !(grepl("Amitriptyline|ChloraPrep|Nortriptyline|NovoRapid|Tranquilyn|Allegron", Term.from.EMIS, ignore.case=TRUE)))
AurumAP1<-subset(AurumAP1, !(grepl("Amitriptyline|Triclofos|Treprostinil|Volanesorsen|Integrin|ChloraPrep|Nortriptyline|NovoRapid|Tranquilyn|Allegron|warfarin", DrugSubstanceName, ignore.case=TRUE)))
AurumAP1<-subset(AurumAP1, !grepl("Lithium succinate", Term.from.EMIS, ignore.case=TRUE) & !grepl("Lithium succinate", DrugSubstanceName, ignore.case=TRUE) & DrugSubstanceName!="Amitriptyline hydrochloride")

AurumAP1$DrugSubstanceName[grepl("Integrin", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Oxypertine"
AurumAP1$DrugSubstanceName[grepl("Depixol", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Flupentixol"
AurumAP1$DrugSubstanceName[grepl("Dozic Liquid|haldol|Serenace Elixir", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Haloperidol"
AurumAP1$DrugSubstanceName[grepl("Fentazin", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Perphenazine"
AurumAP1$DrugSubstanceName[grepl("Largactil", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Chlorpromazine"
AurumAP1$DrugSubstanceName[grepl("Melleril", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Thioridazine"
AurumAP1$DrugSubstanceName[grepl("Moditen Enanthate", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Fluphenazine"
AurumAP1$DrugSubstanceName[grepl("Neulactil", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Pericyazine"
AurumAP1$DrugSubstanceName[grepl("Orap Tablets", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Pimozide"
AurumAP1$DrugSubstanceName[grepl("Seroquel", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Quetiapine"
AurumAP1$DrugSubstanceName[grepl("Stelazine", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Trifluoperazine"
AurumAP1$DrugSubstanceName[grepl("Stemetil", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Prochlorperazine"
AurumAP1$DrugSubstanceName[grepl("Xeplion", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Paliperidone"
AurumAP1$DrugSubstanceName[grepl("Li-liquid|lithium|Camcolit", AurumAP1$Term.from.EMIS, ignore.case=TRUE)]<-"Lithium"
AurumAP1$DrugSubstanceName[grepl("valpro", AurumAP1$Term.from.EMIS, ignore.case=TRUE)|grepl("valpro", AurumAP1$DrugSubstanceName, ignore.case=TRUE)]<-"Valproate"

AurumAP1<-AurumAP1%>%
  mutate(Gen = case_when(grepl(paste(SecondGenList, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), ProductName, ignore.case=TRUE) ~2,
                         grepl(paste(FirstGenList, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), ProductName, ignore.case=TRUE) ~1,
                         grepl(paste(BPMed, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), ProductName, ignore.case=TRUE) ~3,
                         BNFChapter=="4020100"~ 4,
                         TRUE ~ 0))%>%
  mutate(AP = case_when(DrugSubstanceName!="" ~ word(DrugSubstanceName, 1),
                        TRUE ~ word(Term.from.EMIS, 1)))

table(AurumAP1$Gen)

AurumAP<-rbind(AurumAP, AurumAP1)

AurumAPLab2<-unique(as.list((word(AurumAP$ProductName, 1)), 
                            (word(AurumAP$Term.from.EMIS, 1)), 
                            (word(AurumAP$DrugSubstanceName, 1))))
AurumAPLab2<-subset(AurumAPLab2, !(AurumAPLab2 %in% AurumAPLab1))

AurumAPLab2<-subset(AurumAPLab2, AurumAPLab2!= "" & AurumAPLab2!="Sodium")

AurumAP2<-subset(AurumProd, !(ProdCodeId %in% AurumAP$ProdCodeId))
AurumAP2<-subset(AurumAP2, (grepl(paste(AurumAPLab2, collapse='|'), ProductName, ignore.case=TRUE))|
                   (grepl(paste(AurumAPLab2, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                   (grepl(paste(AurumAPLab2, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))
#None to add
Check<-subset(AurumAP, grepl("\\/", DrugSubstanceName))

Check<-subset(AurumAP, grepl("\\/", Term.from.EMIS))
table(AurumAP$AP)
AurumAP$AP[AurumAP$AP=="Flupenthixol"]<-"Flupentixol"

AurumAP<-subset(AurumAP, Term.from.EMIS!="Lithium Succinate Ointment")

#Sort out injectable valproate as this is epilepsy
table(AurumAP$RouteOfAdministration[AurumAP$AP=="Valproate"])
AurumAP<-subset(AurumAP, !(RouteOfAdministration=="Intravenous" & AP=="Valproate"))
Val<-subset(AurumAP, AP=="Valproate")
table(AurumAP$Gen)

write.table(AurumAP, file = "Codelists/APs22/APAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Gold####

GoldAP<-GoldProd%>%
  mutate(Gen = case_when(grepl(paste(SecondGenList, collapse='|'), drugsubstance, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), bnfchapter, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), productname, ignore.case=TRUE) ~2,
                         grepl(paste(FirstGenList, collapse='|'), drugsubstance, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), bnfchapter, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), productname, ignore.case=TRUE) ~1,
                         grepl(paste(BPMed, collapse='|'), drugsubstance, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), bnfchapter, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), productname, ignore.case=TRUE) ~3,
                         grepl("^0402|\\/0402", bnfcode)~ 4,
                         TRUE ~ 0))%>%
  mutate(Gen = case_when(prodcode=="31159" ~ 1,
                         TRUE ~ Gen),
         drugsubstance = case_when(prodcode=="31159" ~ "Thiopropazate",
                                       TRUE ~ drugsubstance))%>%
  subset(Gen>0)%>%
  mutate(AP = case_when(drugsubstance!="" ~ word(drugsubstance, 1),
                        TRUE ~ word(productname, 1)))

table(GoldAP$AP)
table(GoldAP$Gen)
GoldAP<-subset(GoldAP, GoldAP$AP!="Amitriptyline")
GoldAP$AP[GoldAP$AP=="Sodium"]<-"Valproate"
GoldAP<-subset(GoldAP, !(grepl("Lithium succinate", drugsubstance, ignore.case=TRUE)))
GoldAP$AP[GoldAP$AP=="CHLORPROMAZINE"]<-"Chlorpromazine"
GoldAP$AP[GoldAP$AP=="DROPERIDOL"]<-"Droperidol"
GoldAP$AP[GoldAP$AP=="ZUCLOPENTHIXOL"]<-"Zuclopenthixol"
GoldAP$AP[GoldAP$AP=="TRIFLUPERIDOL"]<-"Trifluperidol"
GoldAP$AP[GoldAP$AP=="TRIFLUOPERAZINE"]<-"Trifluoperazine"
GoldAP$AP[GoldAP$AP=="FLUPHENAZINE"]<-"Fluphenazine"
GoldAP$AP[GoldAP$AP=="FLUSPIRILENE"]<-"Fluspirilene"
GoldAP$AP[GoldAP$AP=="HALOPERIDOL"]<-"Haloperidol"
GoldAP$AP[GoldAP$AP=="OLANZAPINE"]<-"Olanzapine"
GoldAP$AP[GoldAP$AP=="PERICYAZINE"]<-"Pericyazine"
GoldAP$AP[GoldAP$AP=="PERPHENAZINE"]<-"Perphenazine"
GoldAP$AP[GoldAP$AP=="PROCHLORPERAZINE"]<-"Prochlorperazine"
GoldAP$AP[GoldAP$AP=="PROMAZINE"]<-"Promazine"
GoldAP$AP[GoldAP$AP=="SULPIRIDE"]<-"Sulpiride"
GoldAP$AP[GoldAP$AP=="THIORIDAZINE"]<-"Thioridazine"
GoldAP$AP[GoldAP$AP=="LITHIUM"]<-"Lithium"
GoldAP$AP[GoldAP$AP=="SOD"]<-"Valproate"
GoldAP$AP[GoldAP$AP=="LAMICTAL"]<-"Lamotrigine"
GoldAP$AP[GoldAP$AP=="THIOPROPERAZINE"]<-"Thioproperazine"
GoldAP$AP[GoldAP$AP=="THIOPROPAZATE"]<-"Thiopropazate"
GoldAP$AP[GoldAP$AP=="Pipothiazine"]<-"Pipotiazine"
GoldAP$drugsubstance[grepl("Xeplion", GoldAP$productname, ignore.case=TRUE)]<-"Paliperidone"
GoldAP$AP[grepl("Xeplion", GoldAP$productname, ignore.case=TRUE)]<-"Paliperidone"

table(GoldAP$Gen)

GoldAP<-subset(GoldAP, !(grepl("Nortriptyline", drugsubstance, ignore.case=TRUE)))
GoldAP<-subset(GoldAP, !(grepl("Isopropamide", drugsubstance, ignore.case=TRUE)))
GoldAP<-subset(GoldAP, !(grepl("Tranylcypromine", drugsubstance, ignore.case=TRUE)))

GoldAP<-subset(GoldAP, drugsubstance!="Lithium chloride" & AP!="MORPHINE,COCAINE" & AP!="Fentanyl" & AP!="Droperidol/Fentanyl" & AP!="Carbamazepine")

table(GoldAP$AP)
table(GoldAP$Gen)

Check<-subset(GoldAP, grepl("\\/", drugsubstance))

GoldAP$Gen[GoldAP$Gen==4]<-2
GoldAPLab1<-unique(as.list((word(GoldAP$productname, 1)), 
                            (word(GoldAP$bnfchapter, 1)), 
                            (word(GoldAP$drugsubstance, 1))))

GoldAPLab1<-subset(GoldAPLab1, GoldAPLab1!= "" & GoldAPLab1!="Sodium" & GoldAPLab1!="SOD")
GoldAPLab1<-append(GoldAPLab1, "sodium valproate")
GoldAP1<-subset(GoldProd, !(prodcode %in% GoldAP$prodcode))
GoldAP1<-subset(GoldAP1, (grepl(paste(GoldAPLab1, collapse='|'), productname, ignore.case=TRUE))|
                   (grepl(paste(GoldAPLab1, collapse='|'), bnfchapter, ignore.case=TRUE))|
                   (grepl(paste(GoldAPLab1, collapse='|'), drugsubstance, ignore.case=TRUE)))

GoldAP1<-subset(GoldAP1, !(grepl("Amitriptyline|ChloraPrep|Nortriptyline|NovoRapid|Tranquilyn|Allegron", productname, ignore.case=TRUE)))
GoldAP1<-subset(GoldAP1, !(grepl("Amitriptyline|Triclofos|Treprostinil|Volanesorsen|ChloraPrep|Nortriptyline|NovoRapid|Tranquilyn|Allegron|warfarin", drugsubstance, ignore.case=TRUE)))
GoldAP1<-subset(GoldAP1, !grepl("Lithium succinate|tegretol", productname, ignore.case=TRUE) & !grepl("Lithium succinate", drugsubstance, ignore.case=TRUE) & !grepl("Amitriptyline hydrochloride", drugsubstance, ignore.case=TRUE))
GoldAP1<-subset(GoldAP1, productname!="Vertigon 25mg tablets (Manx Healthcare Ltd)" & drugsubstance!="Isopropamide-Trifluoperazine"& drugsubstance!="Lithium chloride" & productname!="MORPHINE,COCAINE & CHLORPROMAZINE MIX" & drugsubstance!="Fentanyl Citrate/Droperidol")

GoldAP1$drugsubstance[grepl("Depixol|Fluanxol", GoldAP1$productname, ignore.case=TRUE)]<-"Flupentixol"
GoldAP1$drugsubstance[grepl("Dozic Liquid|haldol|Serenace", GoldAP1$productname, ignore.case=TRUE)]<-"Haloperidol"
GoldAP1$drugsubstance[grepl("Fentazin", GoldAP1$productname, ignore.case=TRUE)]<-"Perphenazine"
GoldAP1$drugsubstance[grepl("Largactil", GoldAP1$productname, ignore.case=TRUE)]<-"Chlorpromazine"
GoldAP1$drugsubstance[grepl("Melleril", GoldAP1$productname, ignore.case=TRUE)]<-"Thioridazine"
GoldAP1$drugsubstance[grepl("Moditen Enanthate|modecate", GoldAP1$productname, ignore.case=TRUE)]<-"Fluphenazine"
GoldAP1$drugsubstance[grepl("Neulactil", GoldAP1$productname, ignore.case=TRUE)]<-"Pericyazine"
GoldAP1$drugsubstance[grepl("Orap Tablets", GoldAP1$productname, ignore.case=TRUE)]<-"Pimozide"
GoldAP1$drugsubstance[grepl("Seroquel", GoldAP1$productname, ignore.case=TRUE)]<-"Quetiapine"
GoldAP1$drugsubstance[grepl("Stelazine", GoldAP1$productname, ignore.case=TRUE)]<-"Trifluoperazine"
GoldAP1$drugsubstance[grepl("Stemetil", GoldAP1$productname, ignore.case=TRUE)]<-"Prochlorperazine"
GoldAP1$drugsubstance[grepl("Xeplion", GoldAP1$productname, ignore.case=TRUE)]<-"Paliperidone"
GoldAP1$drugsubstance[grepl("Li-liquid|lithium|priadel", GoldAP1$productname, ignore.case=TRUE)]<-"Lithium"
GoldAP1$drugsubstance[grepl("valpro|epilim", GoldAP1$productname, ignore.case=TRUE)|grepl("valpro", GoldAP1$drugsubstance, ignore.case=TRUE)]<-"Valproate"
GoldAP1$drugsubstance[grepl("risperdal", GoldAP1$productname, ignore.case=TRUE)]<-"Risperidone"

GoldAP1<-GoldAP1%>%
  mutate(Gen = case_when(grepl(paste(SecondGenList, collapse='|'), drugsubstance, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), bnfchapter, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), productname, ignore.case=TRUE) ~2,
                         grepl(paste(FirstGenList, collapse='|'), drugsubstance, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), bnfchapter, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), productname, ignore.case=TRUE) ~1,
                         grepl(paste(BPMed, collapse='|'), drugsubstance, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), bnfchapter, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), productname, ignore.case=TRUE) ~3,
                         TRUE ~ 0))%>%
  mutate(AP = case_when(drugsubstance!="" ~ word(drugsubstance, 1),
                        TRUE ~ word(productname, 1)))

table(GoldAP1$AP)
GoldAP1<-subset(GoldAP1, AP!="Isopropamide" & AP!="Loratadine" & AP!="Tranylcypromine"& AP!="Droperidol/Fentanyl")

Check<-subset(GoldAP1, grepl("\\/", drugsubstance))

GoldAP1<-subset(GoldAP1, !(grepl("Tranylcypromine", drugsubstance, ignore.case=TRUE)))

GoldAP<-rbind(GoldAP, GoldAP1)

GoldAPLab2<-unique(as.list((word(GoldAP$productname, 1)), 
                            (word(GoldAP$bnfchapter, 1)), 
                            (word(GoldAP$drugsubstance, 1))))

GoldAPLab2<-lapply(GoldAPLab2, tolower)
GoldAPLab1<-lapply(GoldAPLab1, tolower)

GoldAPLab2<-subset(GoldAPLab2, !(GoldAPLab2 %in% GoldAPLab1))

GoldAPLab2<-subset(GoldAPLab2, GoldAPLab2!= "" & GoldAPLab2!="sodium" & GoldAPLab2!="sod" &GoldAPLab2!="risperdal(3")
GoldAPLab2<-append(GoldAPLab2, "risperdal")

GoldAP2<-subset(GoldProd, !(prodcode %in% GoldAP$prodcode))
GoldAP2<-subset(GoldAP2, (grepl(paste(GoldAPLab2, collapse='|'), productname, ignore.case=TRUE))|
                   (grepl(paste(GoldAPLab2, collapse='|'), bnfchapter, ignore.case=TRUE))|
                   (grepl(paste(GoldAPLab2, collapse='|'), drugsubstance, ignore.case=TRUE)))
GoldAP2<-subset(GoldAP2, !(grepl("sterile", GoldAP2$productname, ignore.case=TRUE)) & !(grepl("sterile", GoldAP2$bnfchapter, ignore.case=TRUE)))

#None to add

#Sort out injectables
table(GoldAP$route[GoldAP$AP=="Valproate"])
GoldAP<-subset(GoldAP, !(route=="Intravenous" & AP=="Valproate"))
Val<-subset(GoldAP, AP=="Valproate")

table(GoldAP$Gen)

write.table(GoldAP, file = "Codelists/APs22/APGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Are gold and aurum the same####
table(GoldAP$AP[!(GoldAP$AP %in% AurumAP$AP)])
table(AurumAP$AP[!(AurumAP$AP %in% GoldAP$AP)])

GoldAPLab3<-unique(as.list((word(GoldAP$productname, 1)), 
                           (word(GoldAP$bnfchapter, 1)), 
                           (word(GoldAP$drugsubstance, 1))))

GoldAPLab3<-subset(GoldAPLab3, GoldAPLab3!= "" & GoldAPLab3!="Sodium" & GoldAPLab3!="SOD" &GoldAPLab3!="RISPERDAL(3")
GoldAPLab3<-append(GoldAPLab3, "RISPERDAL")

AurumAPLab3<-unique(as.list((word(AurumAP$ProductName, 1)), 
                            (word(AurumAP$Term.from.EMIS, 1)), 
                            (word(AurumAP$DrugSubstanceName, 1))))

AurumAPLab3<-subset(AurumAPLab3, AurumAPLab3!= "" & AurumAPLab3!="Sodium")

AurumAPLab3<-lapply(AurumAPLab3, tolower)
GoldAPLab3<-lapply(GoldAPLab3, tolower)

GoldAPLab4<-subset(GoldAPLab3, !(GoldAPLab3 %in% AurumAPLab3))
AurumAPLab4<-subset(AurumAPLab3, !(AurumAPLab3 %in% GoldAPLab3))

####Look up gold vs aurum####
GoldAP3<-subset(GoldProd, !(prodcode %in% GoldAP$prodcode))
GoldAP3<-subset(GoldAP3, (grepl(paste(AurumAPLab4, collapse='|'), productname, ignore.case=TRUE))|
                  (grepl(paste(AurumAPLab4, collapse='|'), bnfchapter, ignore.case=TRUE))|
                  (grepl(paste(AurumAPLab4, collapse='|'), drugsubstance, ignore.case=TRUE)))

AurumAP3<-subset(AurumProd, !(ProdCodeId %in% AurumAP$ProdCodeId))
AurumAP3<-subset(AurumAP3, (grepl(paste(GoldAPLab4, collapse='|'), ProductName, ignore.case=TRUE))|
                   (grepl(paste(GoldAPLab4, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                   (grepl(paste(GoldAPLab4, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumAP3<-subset(AurumAP3, !(DrugSubstanceName=="Sodium valproate" & RouteOfAdministration=="Intravenous"))

AurumAP3$DrugSubstanceName[grepl("Chloractil", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Chlorpromazine"
AurumAP3$DrugSubstanceName[grepl("Droleptan", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Droperidol"
AurumAP3$DrugSubstanceName[grepl("Parstelin", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Trifluoperazine-Tranylcypromine"
AurumAP3$DrugSubstanceName[grepl("Redeptin", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Fluspirilene"
AurumAP3$DrugSubstanceName[grepl("roxiam", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Remoxipride"
AurumAP3$DrugSubstanceName[grepl("sparine", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Promazine"
AurumAP3$DrugSubstanceName[grepl("Triperidol", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Trifluperidol"
AurumAP3$DrugSubstanceName[grepl("Sulparex", AurumAP3$Term.from.EMIS, ignore.case=TRUE)]<-"Amisulpride"

AurumAP3<-subset(AurumAP3, !(grepl("phasal tabl|Sulphasalazine|Tranylcypromine|vertigon|Efalith", AurumAP3$Term.from.EMIS, ignore.case=TRUE)))

AurumAP3<-AurumAP3%>%
  mutate(Gen = case_when(grepl(paste(SecondGenList, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~2,
                         grepl(paste(SecondGenList, collapse='|'), ProductName, ignore.case=TRUE) ~2,
                         grepl(paste(FirstGenList, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~1,
                         grepl(paste(FirstGenList, collapse='|'), ProductName, ignore.case=TRUE) ~1,
                         grepl(paste(BPMed, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~3,
                         grepl(paste(BPMed, collapse='|'), ProductName, ignore.case=TRUE) ~3,
                         BNFChapter=="4020100"~ 4,
                         TRUE ~ 0))%>%
  mutate(AP = case_when(DrugSubstanceName!="" ~ word(DrugSubstanceName, 1),
                        TRUE ~ word(Term.from.EMIS, 1)))

AurumAP<-rbind(AurumAP, AurumAP3)

####Compare to Alvins
AlvinAurum<-read.table("R:/Alvin/Code lists/Antipsychotics/CPRD-2023/antipsychotics_AURUM_010823.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(prodcodeid="character"))
AlvinGold<-read.table("R:/Alvin/Code lists/Antipsychotics/CPRD-2023/antipsychotics_GOLD_010823.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".",colClasses = c(prodcode="character"))

NotInNaomiAurum<-subset(AlvinAurum, !(prodcodeid %in% AurumAP$ProdCodeId))
NotInAlvinAurum<-subset(AurumAP, !(ProdCodeId %in% AlvinAurum$prodcodeid))
NotInAlvinAurum<-subset(NotInAlvinAurum, AP!="Lithium" & AP!="Valproate" & AP!="Lamotrigine")

NotInNaomiGold<-subset(AlvinGold, !(prodcode %in% GoldAP$prodcode))
NotInAlvinGold<-subset(GoldAP, !(prodcode %in% AlvinGold$prodcode))
NotInAlvinGold<-subset(NotInAlvinGold, AP!="Lithium" & AP!="Valproate" & AP!="Lamotrigine")

write.table(AurumAP, file = "Codelists/APs22/APAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(GoldAP, file = "Codelists/APs22/APGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

write.table(NotInNaomiAurum, file = "Codelists/APs22/NotInNaomiAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)
write.table(NotInAlvinAurum, file = "Codelists/APs22/NotInAlvinAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)
write.table(NotInNaomiGold, file = "Codelists/APs22/NotInNaomiGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)
write.table(NotInAlvinGold, file = "Codelists/APs22/NotInAlvinGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)



table(GoldAP$Gen)
table(AurumAP$Gen)

####Check medical dictionaries####
AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)

GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
GoldMed$desc<-tolower(GoldMed$desc)

AurumAPMed<-AurumMed%>%
  mutate(AP = case_when(grepl(paste(SecondGenList, collapse='|'), Term, ignore.case=TRUE) ~1,
         grepl(paste(FirstGenList, collapse='|'), Term, ignore.case=TRUE) ~1,
         grepl(paste(BPMed, collapse='|'), Term, ignore.case=TRUE) ~1,
         grepl("antipsychotic", Term, ignore.case=TRUE) ~1,
         grepl("anti-psychotic", Term, ignore.case=TRUE) ~1,
         TRUE ~ 0))%>%
  subset(AP!=0)

AurumAPMed<-subset(AurumAPMed, !(grepl("amitriptyline|carbonicum|lithium succinate|nortriptyline|Lithium Heparin|epilepsy|levacetylmethadol|neuroleptic", Term, ignore.case=TRUE)))


AurumAPMed<-AurumAPMed%>%
  mutate(AP = case_when(grepl("inject", Term, ignore.case=TRUE)~"EvidenceOfInjectables",
                        grepl("lithium", Term, ignore.case=TRUE)~"EvidenceOfBP",
                        grepl("valproate", Term, ignore.case=TRUE)~"EvidenceOfBP",
                        grepl("lamotrigine", Term, ignore.case=TRUE)~"EvidenceOfBP",
                        TRUE ~ "EvidenceOfAP"))

AurumAPMed<-subset(AurumAPMed, Term!="lithium fumes")
                        
GoldAPMed<-GoldMed%>%
  mutate(AP = case_when(grepl(paste(SecondGenList, collapse='|'), desc, ignore.case=TRUE) ~1,
                        grepl(paste(FirstGenList, collapse='|'), desc, ignore.case=TRUE) ~1,
                        grepl(paste(BPMed, collapse='|'), desc, ignore.case=TRUE) ~1,
                        grepl("antipsychotic", desc, ignore.case=TRUE) ~1,
                        grepl("anti-psychotic", desc, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  subset(AP!=0)

GoldAPMed<-subset(GoldAPMed, !(grepl("amitriptyline|carbonicum|lithium succinate|nortriptyline|Lithium Heparin|epilepsy|levacetylmethadol|neuroleptic", desc, ignore.case=TRUE)))


GoldAPMed<-GoldAPMed%>%
  mutate(AP = case_when(grepl("inject", desc, ignore.case=TRUE)~"EvidenceOfInjectables",
                        grepl("lithium", desc, ignore.case=TRUE)~"EvidenceOfBP",
                        grepl("valproate", desc, ignore.case=TRUE)~"EvidenceOfBP",
                        grepl("lamotrigine", desc, ignore.case=TRUE)~"EvidenceOfBP",
                        TRUE ~ "EvidenceOfAP"))

#Compare lists
GoldStatCheck<-subset(GoldAPMed, !(desc %in% AurumAPMed$Term))
AddtoAurum<-subset(AurumMed, Term %in% GoldStatCheck$desc) #None to add on term

AurumStatCheck<-subset(AurumAPMed, !(Term %in% GoldAPMed$desc))
AddtoGold<-subset(GoldMed, desc %in% AurumStatCheck$Term) #None to add on term

GoldStatCheck<-subset(GoldAPMed, !(readcode %in% AurumAPMed$CleansedReadCode))
AddtoAurum<-subset(AurumMed, CleansedReadCode %in% GoldStatCheck$readcode) #None to add on readcode

AurumStatCheck<-subset(AurumAPMed, !(CleansedReadCode %in% GoldAPMed$readcode) & CleansedReadCode!="")
AddtoGold<-subset(GoldMed, readcode %in% AurumStatCheck$CleansedReadCode) #One to add on readcode
AddtoGold$AP<-"Evidence of BP"

GoldAPMed<-rbind(GoldAPMed, AddtoGold)

GoldAPMed<-subset(GoldAPMed, !grepl("fetal", desc, ignore.case=TRUE))
AurumAPMed<-subset(AurumAPMed, !grepl("fetal", Term, ignore.case=TRUE))

write.table(AurumAPMed, file = "Codelists/APs22/APAurumMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(GoldAPMed, file = "Codelists/APs22/APGoldMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Compare to Alvins
AlvinAurum<-read.table("R:/Alvin/Code lists/Antipsychotics/CPRD-2023/antipsychotics_AURUM_medical_010823.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(medcodeid="character"))
AlvinGold<-read.table("R:/Alvin/Code lists/Antipsychotics/CPRD-2023/antipsychotics_GOLD_medical_010823.txt", header=TRUE, fill=TRUE, sep="\t", dec = ".",colClasses = c(medcode="character"))

NotInNaomiAurum<-subset(AlvinAurum, !(medcodeid %in% AurumAPMed$MedCodeId))
NotInAlvinAurum<-subset(AurumAPMed, !(MedCodeId %in% AlvinAurum$medcodeid))
NotInAlvinAurum<-subset(NotInAlvinAurum, AP=="EvidenceOfAP")

NotInNaomiGold<-subset(AlvinGold, !(medcode %in% GoldAPMed$medcode))
NotInAlvinGold<-subset(GoldAPMed, !(medcode %in% AlvinGold$medcode))
NotInAlvinGold<-subset(NotInAlvinGold, AP=="EvidenceOfAP")

write.table(NotInNaomiAurum, file = "Codelists/APs22/NotInNaomiAurumMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)
write.table(NotInAlvinAurum, file = "Codelists/APs22/NotInAlvinAurumMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)
write.table(NotInNaomiGold, file = "Codelists/APs22/NotInNaomiGoldMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)
write.table(NotInAlvinGold, file = "Codelists/APs22/NotInAlvinGoldMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)

