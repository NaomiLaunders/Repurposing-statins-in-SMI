#Clear environment

rm(list = ls(all.names = TRUE))

#~~~Libraries
library(tidyverse)
library(lubridate)
library(haven)

####Load product dictionaries####
AurumProd<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISProductDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(ProdCodeId="character"))
GoldProd<-read.table("LookUps/202303_Lookups_CPRDGOLD/product.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(prodcode="character"))

#Load Annie's list#
AllAd<-read.table("Codelists/Antidepressants22/AntidepressantsFromAnnie.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(productcode="character"))
GoldAd<-subset(AllAd, database=="Gold")
AurumAd<-subset(AllAd, database=="Aurum")

AllADLab<-unique(as.list((word(AllAd$productname, 1)),
                            (word(AllAd$drugsubstance, 1))))

####AD search#####
NewAurumAD<-AurumProd%>%
  subset(!ProdCodeId %in% AurumAd$productcode)%>%
  mutate(AD = case_when(grepl("Nutrineal|Vamin|syrup|Acidophilus|decanoate|Enanthate|Fentazin|HealthAid|Lamberts|Norval|Trifluoperazine", Term.from.EMIS, ignore.case=TRUE)~0,
                        DrugSubstanceName=="Fluphenazine hydrochloride" | DrugSubstanceName=="Fluphenazine decanoate"| DrugSubstanceName=="Perphenazine"|
                          DrugSubstanceName=="Trifluoperazine"|DrugSubstanceName=="Trifluoperazine hydrochloride"~ 0,
                        ProdCodeId=="1052041000033118" | ProdCodeId=="1065341000033119" ~ 0,
    grepl(paste(AllADLab, collapse='|'), DrugSubstanceName, ignore.case=TRUE) ~1,
                        grepl(paste(AllADLab, collapse='|'), Term.from.EMIS, ignore.case=TRUE) ~1,
                        grepl(paste(AllADLab, collapse='|'), ProductName, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  
  subset(AD==1)

NewAurumAD$AD<-NewAurumAD$DrugSubstanceName
NewAurumAD$AD[grepl("Amitriptyline|Domical|Elavil|Tryptizol", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Amitriptyline"
NewAurumAD$AD[grepl("Amoxapine", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Amoxapine"
NewAurumAD$AD[grepl("Anafranil", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Clomipramine"
NewAurumAD$AD[grepl("Nortriptyline|aventyl", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Nortriptyline"
NewAurumAD$AD[grepl("Optimax", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Tryptophan"
NewAurumAD$AD[grepl("Asendis", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Amoxapine"
NewAurumAD$AD[grepl("Dutonin|Nefazodone Hydrochloride", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Nefazodone"
NewAurumAD$AD[grepl("Tofranil", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Imipramine"
NewAurumAD$AD[grepl("Bolvidon", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Bolvidon"
NewAurumAD$AD[grepl("Clomipramine Hydrochloride", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Clomipramine"
NewAurumAD$AD[grepl("Domical ", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Clomipramine"
NewAurumAD$AD[grepl("Fluoxetine Hydrochloride", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Fluoxetine"
NewAurumAD$AD[grepl("Limbitrol", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Amitriptyline/chlordiazepoxide"
NewAurumAD$AD[grepl("Marplan", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Isocarboxazid"
NewAurumAD$AD[grepl("Optimax|pacitron", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Tryptophan"
NewAurumAD$AD[grepl("Parnate", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Tranylcypromine"
NewAurumAD$AD[grepl("Parstelin", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"trifluoperazine/tranylcypromine"
NewAurumAD$AD[grepl("Paxoran", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Citalopram"
NewAurumAD$AD[grepl("Praminil", NewAurumAD$Term.from.EMIS, ignore.case=TRUE)]<-"Imipramine"

#Check BNF
table(NewAurumAD$BNFChapter)

NewAurumAD<-select(NewAurumAD, productname=ProductName, formulation=Formulation, route=RouteOfAdministration, drugsubstance=DrugSubstanceName, strength=SubstanceStrength, productcode=ProdCodeId, BNFcode=BNFChapter, Term=Term.from.EMIS, AD)
AurumAD<-select(AurumAd, -database)
AurumAD$Term<-NA
AurumAD$AD<-AurumAD$drugsubstance

AurumAD<-rbind(AurumAD, NewAurumAD)

AurumADLab1<-unique(as.list((word(AurumAD$productname, 1)), 
                              (word(AurumAD$Term, 1)), 
                              (word(AurumAD$AD, 1))))

AurumADLab1<-subset(AurumADLab1, !(AurumADLab1 %in% AllADLab)) # Only Majoven and Dutor that havent been searched

AurumADLab1<-c("Majoven", "Dutor")
AurumAD1<-subset(AurumProd, !(ProdCodeId %in% AurumAD$productcode))
AurumAD1<-subset(AurumAD1, (grepl(paste(AurumADLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumADLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumADLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))
#None to add

####Gold####
NewGoldAD<-GoldProd%>%
  subset(!prodcode %in% GoldAd$productcode)%>%
  mutate(AD = case_when(grepl("stelabid|Stelazine|infus|intrafus|Amino|Nutrineal|Vamin|syrup|Glamin|Acidophilus|decanoate|Enanthate|Fentazin|HealthAid|Lamberts|Norval|Trifluoperazine|Nephramine", productname, ignore.case=TRUE)~0,
                        drugsubstance=="Fluphenazine hydrochloride" | drugsubstance=="Fluphenazine decanoate"| drugsubstance=="Perphenazine"|
                          drugsubstance=="Fluphenazine Decanoate"| drugsubstance=="Trifluoperazine"|drugsubstance=="Trifluoperazine hydrochloride"~ 0,
                        prodcode=="24107"|prodcode=="20061"| prodcode=="21353"~ 0,
                        grepl(paste(AllADLab, collapse='|'), drugsubstance, ignore.case=TRUE) ~1,
                        grepl(paste(AllADLab, collapse='|'), productname, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  
  subset(AD==1)

NewGoldAD$AD<-NewGoldAD$drugsubstance
NewGoldAD$AD[grepl("Amitriptyline|Domical|Elavil|Tryptizol", NewGoldAD$productname, ignore.case=TRUE)]<-"Amitriptyline"
NewGoldAD$AD[grepl("Amoxapine", NewGoldAD$productname, ignore.case=TRUE)]<-"Amoxapine"
NewGoldAD$AD[grepl("Anafranil|CLOMIPRAMINE", NewGoldAD$productname, ignore.case=TRUE)]<-"Clomipramine"
NewGoldAD$AD[grepl("Nortriptyline|aventyl", NewGoldAD$productname, ignore.case=TRUE)]<-"Nortriptyline"
NewGoldAD$AD[grepl("Optimax", NewGoldAD$productname, ignore.case=TRUE)]<-"Tryptophan"
NewGoldAD$AD[grepl("Asendis", NewGoldAD$productname, ignore.case=TRUE)]<-"Amoxapine"
NewGoldAD$AD[grepl("Dutonin|Nefazodone Hydrochloride", NewGoldAD$productname, ignore.case=TRUE)]<-"Nefazodone"
NewGoldAD$AD[grepl("Tofranil", NewGoldAD$productname, ignore.case=TRUE)]<-"Imipramine"
NewGoldAD$AD[grepl("Bolvidon", NewGoldAD$productname, ignore.case=TRUE)]<-"Bolvidon"
NewGoldAD$AD[grepl("Clomipramine Hydrochloride", NewGoldAD$productname, ignore.case=TRUE)]<-"Clomipramine"
NewGoldAD$AD[grepl("Domical ", NewGoldAD$productname, ignore.case=TRUE)]<-"Clomipramine"
NewGoldAD$AD[grepl("Fluoxetine Hydrochloride|Fluoxetine", NewGoldAD$productname, ignore.case=TRUE)]<-"Fluoxetine"
NewGoldAD$AD[grepl("Limbitrol", NewGoldAD$productname, ignore.case=TRUE)]<-"Amitriptyline/chlordiazepoxide"
NewGoldAD$AD[grepl("Marplan", NewGoldAD$productname, ignore.case=TRUE)]<-"Isocarboxazid"
NewGoldAD$AD[grepl("Optimax|pacitron|Tryptophan", NewGoldAD$productname, ignore.case=TRUE)]<-"Tryptophan"
NewGoldAD$AD[grepl("Parnate", NewGoldAD$productname, ignore.case=TRUE)]<-"Tranylcypromine"
NewGoldAD$AD[grepl("Parstelin", NewGoldAD$productname, ignore.case=TRUE)]<-"Tranylcypromine/trifluoperazine"
NewGoldAD$AD[grepl("Paxoran", NewGoldAD$productname, ignore.case=TRUE)]<-"Citalopram"
NewGoldAD$AD[grepl("Praminil|IMIPRAMINE", NewGoldAD$productname, ignore.case=TRUE)]<-"Imipramine"
NewGoldAD$AD[grepl("doxepin|SINEQUAN", NewGoldAD$productname, ignore.case=TRUE)]<-"Doxepin"
NewGoldAD$AD[grepl("Duloxetine ", NewGoldAD$productname, ignore.case=TRUE)]<-"Duloxetine"
NewGoldAD$AD[grepl("Faverin", NewGoldAD$productname, ignore.case=TRUE)]<-"Fluvoxamine"
NewGoldAD$AD[grepl("LOFEPRAMINE", NewGoldAD$productname, ignore.case=TRUE)]<-"Lofepramine"
NewGoldAD$AD[grepl("MAPROTILINE", NewGoldAD$productname, ignore.case=TRUE)]<-"Maprotiline"
NewGoldAD$AD[grepl("Mirtazapine", NewGoldAD$productname, ignore.case=TRUE)]<-"Mirtazapine"
NewGoldAD$AD[grepl("PROTHIADEN", NewGoldAD$productname, ignore.case=TRUE)]<-"Dosulepin"
NewGoldAD$AD[grepl("Sertraline", NewGoldAD$productname, ignore.case=TRUE)]<-"Sertraline"
NewGoldAD$AD[grepl("SURMONTIL", NewGoldAD$productname, ignore.case=TRUE)]<-"Trimipramine"
NewGoldAD$AD[grepl("Trazodone", NewGoldAD$productname, ignore.case=TRUE)]<-"Trazodone"
NewGoldAD$AD[grepl("Venlafaxine|Winfex", NewGoldAD$productname, ignore.case=TRUE)]<-"Venlafaxine"

#Check  BNF
table(NewGoldAD$bnfcode)
table(NewGoldAD$bnfchapter)


GoldAD<-select(GoldAd, -database)
GoldAD$AD<-GoldAD$drugsubstance
NewGoldAD<-select(NewGoldAD, productname, formulation, route, drugsubstance, strength, productcode=prodcode, BNFcode=bnfcode, AD)

GoldAD<-rbind(GoldAD, NewGoldAD)

GoldADLab1<-unique(as.list((word(GoldAD$productname, 1)),
                            (word(GoldAD$AD, 1))))

GoldADLab1<-tolower(GoldADLab1)
AllADLab<-tolower(AllADLab)

GoldADLab1<-subset(GoldADLab1, !(GoldADLab1 %in% AllADLab)) # Only Majoven and l-tryptophan that havent been searched

GoldADLab1<-c("Majoven", "l-tryptophan")
GoldAD1<-subset(GoldProd, !(prodcode %in% GoldAD$productcode))
GoldAD1<-subset(GoldAD1, (grepl(paste(GoldADLab1, collapse='|'), productname, ignore.case=TRUE))|
                   (grepl(paste(GoldADLab1, collapse='|'), drugsubstance, ignore.case=TRUE)))
#None to add

#Standardise drug substance#
GoldAD$AD<-tolower(GoldAD$AD)
AurumAD$AD<-tolower(AurumAD$AD)

table(GoldAD$AD)
table(AurumAD$AD)

GoldAD$AD<-str_replace(GoldAD$AD, "\\/", "_")
GoldAD$AD<-str_replace(GoldAD$AD, "hydrochloride", "")
GoldAD$AD<-str_replace(GoldAD$AD, "hydrobromide", "")
GoldAD$AD<-str_replace(GoldAD$AD, "maleate", "")
GoldAD$AD<-str_replace(GoldAD$AD, " ", "")
GoldAD$AD<-str_replace(GoldAD$AD, "sulfate", "")
GoldAD$AD<-str_replace(GoldAD$AD, "sulphate", "")
GoldAD$AD<-str_replace(GoldAD$AD, "oxalate", "")

AurumAD$AD<-str_replace(AurumAD$AD, "\\/", "_")
AurumAD$AD<-str_replace(AurumAD$AD, "hydrochloride", "")
AurumAD$AD<-str_replace(AurumAD$AD, "hydrobromide", "")
AurumAD$AD<-str_replace(AurumAD$AD, "maleate", "")
AurumAD$AD<-str_replace(AurumAD$AD, "sulfate", "")
AurumAD$AD<-str_replace(AurumAD$AD, "sulphate", "")
AurumAD$AD<-str_replace(AurumAD$AD, "oxalate", "")
AurumAD$AD<-str_replace(AurumAD$AD, " ", "")

table(GoldAD$AD)
table(AurumAD$AD)

####Are gold and aurum the same####
AurumAD$AD[AurumAD$AD=="fluphenazine/nortriptyline"]<-"nortriptyline/fluphenazine"
GoldAD$AD[GoldAD$AD=="fluphenazine/nortriptyline"]<-"nortriptyline/fluphenazine"

table(GoldAD$AD[!(GoldAD$AD %in% AurumAD$AD)])
table(AurumAD$AD[!(AurumAD$AD %in% GoldAD$AD)])

write.table(AurumAD, file = "Codelists/Antidepressants22/ADAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(GoldAD, file = "Codelists/Antidepressants22/ADGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Check medical dictionaries####
AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)

GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
GoldMed$desc<-tolower(GoldMed$desc)

AurumADMed<-AurumMed%>%
  mutate(AD = case_when(grepl(paste(AllADLab, collapse='|'), Term, ignore.case=TRUE) ~1,
         grepl(paste(AurumADLab1, collapse='|'), Term, ignore.case=TRUE) ~1,
         grepl(paste(GoldADLab1, collapse='|'), Term, ignore.case=TRUE) ~1,
         grepl("antidepres", Term, ignore.case=TRUE) ~1,
         grepl("anti-depres", Term, ignore.case=TRUE) ~1,
         TRUE ~ 0))%>%
  subset(AD!=0)

AurumADMed<-subset(AurumADMed, !(grepl("fluphenazine|trifluoperazine|perphenazine|serum tryptophan|plasma tryptophan|urine tryptophan", Term, ignore.case=TRUE)))

GoldADMed<-GoldMed%>%
  mutate(AD = case_when(grepl(paste(AllADLab, collapse='|'), desc, ignore.case=TRUE) ~1,
                        grepl(paste(AurumADLab1, collapse='|'), desc, ignore.case=TRUE) ~1,
                        grepl(paste(GoldADLab1, collapse='|'), desc, ignore.case=TRUE) ~1,
                        grepl("antidepres", desc, ignore.case=TRUE) ~1,
                        grepl("anti-depres", desc, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  subset(AD!=0)

GoldADMed<-subset(GoldADMed, !(grepl("fluphenazine|trifluoperazine|perphenazine|serum tryptophan|plasma tryptophan|urine tryptophan", desc, ignore.case=TRUE)))

#Compare lists
GoldStatCheck<-subset(GoldADMed, !(desc %in% AurumADMed$Term))
AddtoAurum<-subset(AurumMed, Term %in% GoldStatCheck$desc) #None to add on term

AurumStatCheck<-subset(AurumADMed, !(Term %in% GoldADMed$desc))
AddtoGold<-subset(GoldMed, desc %in% AurumStatCheck$Term) #None to add on term

GoldStatCheck<-subset(GoldADMed, !(readcode %in% AurumADMed$CleansedReadCode))
AddtoAurum<-subset(AurumMed, CleansedReadCode %in% GoldStatCheck$readcode) #None to add on readcode

AurumStatCheck<-subset(AurumADMed, !(CleansedReadCode %in% GoldADMed$readcode) & CleansedReadCode!="")
AddtoGold<-subset(GoldMed, readcode %in% AurumStatCheck$CleansedReadCode) #One to add on readcode

write.table(AurumADMed, file = "Codelists/Antidepressants22/ADAurumMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(GoldADMed, file = "Codelists/Antidepressants22/ADGoldMed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
