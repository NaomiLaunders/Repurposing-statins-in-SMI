#Clear environment

rm(list = ls(all.names = TRUE))


#~~~Libraries
library(tidyverse)
library(lubridate)

####Load product dictionaries####
AurumProd<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISProductDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(ProdCodeId="character"))
GoldProd<-read.table("LookUps/202303_Lookups_CPRDGOLD/product.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(prodcode="character"))

####Statin search#####
AurumStat<-AurumProd%>%
  mutate(Statin = case_when(grepl("nystatin|cilastatin|ecostatin", ProductName, ignore.case=TRUE) |
                              grepl("nystatin|cilastatin|ecostatin", DrugSubstanceName, ignore.case=TRUE) | 
                              grepl("nystatin|cilastatin|ecostatin", Term.from.EMIS, ignore.case=TRUE) ~ 0,
                            grepl("statin", ProductName, ignore.case=TRUE) | 
                              grepl("statin", DrugSubstanceName, ignore.case=TRUE) | 
                              grepl("statin", Term.from.EMIS, ignore.case=TRUE) |
                              BNFChapter=="2120000"~ 1,
                            TRUE ~ 0))%>%
  subset(Statin==1)%>%
  mutate(Statin = case_when(grepl("atorvastatin", ProductName, ignore.case=TRUE)  |
                       grepl("atorvastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("atorvastatin", Term.from.EMIS, ignore.case=TRUE) ~ "Atorvastatin",
                       grepl("fluvastatin", ProductName, ignore.case=TRUE) | 
                         grepl("fluvastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("fluvastatin", Term.from.EMIS, ignore.case=TRUE) ~ "fluvastatin",
                       grepl("lovastatin", ProductName, ignore.case=TRUE) | 
                         grepl("lovastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("lovastatin", Term.from.EMIS, ignore.case=TRUE) ~ "Lovastatin",
                       grepl("pitavastatin", ProductName, ignore.case=TRUE) | 
                         grepl("pitavastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("pitavastatin", Term.from.EMIS, ignore.case=TRUE) ~ "Pitavastatin",
                       grepl("simvastatin", ProductName, ignore.case=TRUE) | 
                         grepl("simvastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("simvastatin", Term.from.EMIS, ignore.case=TRUE) ~ "Simvastatin",
                       grepl("pravastatin", ProductName, ignore.case=TRUE) | 
                         grepl("pravastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("pravastatin", Term.from.EMIS, ignore.case=TRUE) ~ "Pravastatin",
                       grepl("rosuvastatin", ProductName, ignore.case=TRUE) | 
                         grepl("rosuvastatin", DrugSubstanceName, ignore.case=TRUE) | grepl("rosuvastatin", Term.from.EMIS, ignore.case=TRUE) ~ "Rosuvastatin",
                       TRUE ~ "Other"))
table(AurumStat$Statin)         
Check<-subset(AurumStat, Statin=="Other")         
AurumStat<-subset(AurumStat, Statin!="Other" | (Statin=="Other" & grepl("Cerivastatin", DrugSubstanceName, ignore.case=TRUE)))

AurumStatLab1<-unique(as.list((word(AurumStat$ProductName, 1)), 
                             (word(AurumStat$Term.from.EMIS, 1)), 
                             (word(AurumStat$DrugSubstanceName, 1))))

AurumStatLab1<-subset(AurumStatLab1, AurumStatLab1!= "")
AurumStat1<-subset(AurumProd, !(ProdCodeId %in% AurumStat$ProdCodeId))
AurumStat1<-subset(AurumStat1, (grepl(paste(AurumStatLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumStatLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumStatLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

#None of interest

Check<-subset(AurumStat, grepl("\\/", DrugSubstanceName)|grepl("\\/", Term.from.EMIS)|grepl("\\/", ProductName) )

AurumStat$Statin[AurumStat$DrugSubstanceName=="Ezetimibe/ Simvastatin"]<-"Other"
AurumStat$Statin[AurumStat$DrugSubstanceName=="Fenofibrate/ Simvastatin"]<-"Other"
AurumStat<-subset(AurumStat, DrugSubstanceName!="Cholesterol/ Simvastatin")

write.table(AurumStat, file = "Codelists/Statin22/StatinAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

GoldStat<-GoldProd%>%
  mutate(Statin = case_when(grepl("nystatin|cilastatin|ecostatin", productname, ignore.case=TRUE) | grepl("nystatin|cilastatin|ecostatin", bnfchapter, ignore.case=TRUE) |
                              grepl("nystatin|cilastatin|ecostatin", drugsubstance, ignore.case=TRUE) ~ 0,
                            grepl("statin", productname, ignore.case=TRUE) | grepl("statin", bnfchapter, ignore.case=TRUE) |
                              grepl("statin", drugsubstance, ignore.case=TRUE)|
                              grepl("0212", bnfchapter) ~ 1,
                            TRUE ~ 0))%>%
  subset(Statin==1)%>%
  mutate(Statin = case_when(grepl("atorvastatin", productname, ignore.case=TRUE) | grepl("atorvastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("atorvastatin", drugsubstance, ignore.case=TRUE) ~ "Atorvastatin",
                            grepl("fluvastatin", productname, ignore.case=TRUE) | grepl("fluvastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("fluvastatin", drugsubstance, ignore.case=TRUE)  ~ "fluvastatin",
                            grepl("lovastatin", productname, ignore.case=TRUE) | grepl("lovastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("lovastatin", drugsubstance, ignore.case=TRUE)  ~ "Lovastatin",
                            grepl("pitavastatin", productname, ignore.case=TRUE) | grepl("pitavastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("pitavastatin", drugsubstance, ignore.case=TRUE)  ~ "Pitavastatin",
                            grepl("simvastatin", productname, ignore.case=TRUE) | grepl("simvastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("simvastatin", drugsubstance, ignore.case=TRUE)  ~ "Simvastatin",
                            grepl("pravastatin", productname, ignore.case=TRUE) | grepl("pravastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("pravastatin", drugsubstance, ignore.case=TRUE)  ~ "Pravastatin",
                            grepl("rosuvastatin", productname, ignore.case=TRUE) | grepl("rosuvastatin", bnfchapter, ignore.case=TRUE) |
                              grepl("rosuvastatin", drugsubstance, ignore.case=TRUE)  ~ "Rosuvastatin",
                            TRUE ~ "Other"))
table(GoldStat$Statin)     
Check<-subset(GoldStat, Statin=="Other")

GoldStat<-subset(GoldStat, Statin!="Other" | (Statin=="Other" & drugsubstance=="Cerivastatin sodium"))

GoldStatLab1<-unique(as.list((word(GoldStat$productname, 1)), 
                              (word(GoldStat$drugsubstance, 1)), 
                              (word(GoldStat$bnfchapter, 1))))

GoldStatLab1<-subset(GoldStatLab1, GoldStatLab1!= "")
GoldStat1<-subset(GoldProd, !(prodcode %in% GoldStat$prodcode))
GoldStat1<-subset(GoldStat1, (grepl(paste(GoldStatLab1, collapse='|'), productname, ignore.case=TRUE))|
                     (grepl(paste(GoldStatLab1, collapse='|'), drugsubstance, ignore.case=TRUE))|
                     (grepl(paste(GoldStatLab1, collapse='|'), bnfchapter, ignore.case=TRUE)))

#Nothing to add

Check<-subset(GoldStat, grepl("\\/", drugsubstance)|grepl("\\/", bnfchapter)|grepl("\\/", productname) )

GoldStat$Statin[GoldStat$drugsubstance=="Simvastatin/Ezetimibe"]<-"Other"
GoldStat$Statin[GoldStat$drugsubstance=="Simvastatin/Fenofibrate"]<-"Other"

write.table(GoldStat, file = "Codelists/Statin22/StatinGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

####Compare gold and aurum####

GoldStatLab2<-unique(as.list((word(GoldStat$productname, 1)), 
                           (word(GoldStat$bnfchapter, 1)), 
                           (word(GoldStat$drugsubstance, 1))))

AurumStatLab2<-unique(as.list((word(AurumStat$ProductName, 1)), 
                            (word(AurumStat$Term.from.EMIS, 1)), 
                            (word(AurumStat$DrugSubstanceName, 1))))

AurumStatLab2<-lapply(AurumStatLab2, tolower)
GoldStatLab2<-lapply(GoldStatLab2, tolower)
AurumStatLab2<-unique(AurumStatLab2)
GoldStatLab2<-unique(GoldStatLab2)

GoldStatLab3<-subset(GoldStatLab2, !(GoldStatLab2 %in% AurumStatLab2))
AurumStatLab3<-subset(AurumStatLab2, !(AurumStatLab2 %in% GoldStatLab2))

#None found
GoldStat2<-subset(GoldProd, !(prodcode %in% GoldStat$prodcode))
GoldStat2<-subset(GoldStat2, (grepl(paste(AurumStatLab3, collapse='|'), productname, ignore.case=TRUE))|
                  (grepl(paste(AurumStatLab3, collapse='|'), bnfchapter, ignore.case=TRUE))|
                  (grepl(paste(AurumStatLab3, collapse='|'), drugsubstance, ignore.case=TRUE)))

#None found
AurumStat2<-subset(AurumProd, !(ProdCodeId %in% AurumStat$ProdCodeId))
AurumStat2<-subset(AurumStat2, (grepl(paste(GoldStatLab3, collapse='|'), ProductName, ignore.case=TRUE))|
                   (grepl(paste(GoldStatLab3, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                   (grepl(paste(GoldStatLab3, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

####Medcodes suggestive of statins####
AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)


AurumStat<-AurumMed%>%
  mutate(Statin = case_when(grepl("nystatin|cilastatin|ecostatin", Term, ignore.case=TRUE) ~ 0,
                            grepl("statin", Term, ignore.case=TRUE) ~ 1,
                            grepl("statin", Term, ignore.case=TRUE) ~ 1,
                            grepl("statin", Term, ignore.case=TRUE) ~ 1,
                            
                            TRUE ~ 0))%>%
  subset(Statin==1)%>%
  mutate(Statin = case_when(grepl("atorvastatin", Term, ignore.case=TRUE) ~ "Atorvastatin",
                            grepl("fluvastatin", Term, ignore.case=TRUE)  ~ "fluvastatin",
                            grepl("lovastatin", Term, ignore.case=TRUE) ~ "Lovastatin",
                            grepl("pitavastatin", Term, ignore.case=TRUE) ~ "Pitavastatin",
                            grepl("simvastatin", Term, ignore.case=TRUE) ~ "Simvastatin",
                            grepl("pravastatin", Term, ignore.case=TRUE) ~ "Pravastatin",
                            grepl("rosuvastatin", Term, ignore.case=TRUE) ~ "Rosuvastatin",
                            TRUE ~ "Other"))
table(AurumStat$Statin)         
Check<-subset(AurumStat, Statin=="Other")         
AurumStat<-subset(AurumStat, !(grepl("sandostatin|cystatin c|Pancreastatin|somatostatin|declined|not tolerated|offer|not indicated|contraindicated", Term, ignore.case=TRUE)))       
AurumStat<-subset(AurumStat, Term!="Adverse reaction to Cholesterol And Simvastatin")

AurumStat<-select(AurumStat, -Statin)

####Gold medcodes####
GoldStat<-GoldMed%>%
  mutate(Statin = case_when(grepl("nystatin|cilastatin|ecostatin", desc, ignore.case=TRUE) ~ 0,
                            grepl("statin", desc, ignore.case=TRUE) ~ 1,
                            grepl("statin", desc, ignore.case=TRUE) ~ 1,
                            
                            TRUE ~ 0))%>%
  subset(Statin==1)%>%
  mutate(Statin = case_when(grepl("atorvastatin", desc, ignore.case=TRUE) ~ "Atorvastatin",
                            grepl("fluvastatin", desc, ignore.case=TRUE)  ~ "fluvastatin",
                            grepl("lovastatin", desc, ignore.case=TRUE) ~ "Lovastatin",
                            grepl("pitavastatin", desc, ignore.case=TRUE) ~ "Pitavastatin",
                            grepl("simvastatin", desc, ignore.case=TRUE) ~ "Simvastatin",
                            grepl("pravastatin", desc, ignore.case=TRUE) ~ "Pravastatin",
                            grepl("rosuvastatin", desc, ignore.case=TRUE) ~ "Rosuvastatin",
                            TRUE ~ "Other"))
table(GoldStat$Statin)         
Check<-subset(GoldStat, Statin=="Other")         
GoldStat<-subset(GoldStat, !(grepl("cystatin c|somatostatin|declined|not tolerated|offer|not indicated|contraindicated", desc, ignore.case=TRUE)))       
GoldStat<-select(GoldStat, -Statin)

####Compare gold and aurum####
GoldStatCheck<-subset(GoldStat, !(desc %in% AurumStat$Term))
AddtoAurum<-subset(AurumMed, Term %in% GoldStatCheck$desc) #None to add on term

AurumStatCheck<-subset(AurumStat, !(Term %in% GoldStat$desc))
AddtoGold<-subset(GoldMed, desc %in% AurumStatCheck$Term) #None to add on term

GoldStatCheck<-subset(GoldStat, !(readcode %in% AurumStat$CleansedReadCode))
AddtoAurum<-subset(AurumMed, CleansedReadCode %in% GoldStatCheck$readcode) #one to add on readcode

AurumStatCheck<-subset(AurumStat, !(CleansedReadCode %in% GoldStat$readcode) & CleansedReadCode!="")
AddtoGold<-subset(GoldMed, readcode %in% AurumStatCheck$CleansedReadCode) #None to add on readcode

AurumStat<-rbind(AurumStat, AddtoAurum)

#Save files
write.table(GoldStat, file = "Codelists/Statin22/StatinGoldmed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")

write.table(AurumStat, file = "Codelists/Statin22/StatinAurummed.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
