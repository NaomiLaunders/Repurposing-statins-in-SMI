rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(lubridate)
library(tableone)

####Load trials####
load("StatinCPRD/Data/Trial1.rdata")
load("StatinCPRD/Data/Trial2.rdata")
load("StatinCPRD/Data/Trial3.rdata")

####Basic descriptives####
#Trial 1
names(Trial1)

Factors<-c("Active1", "gender", "Died2yr1", "region", "ethnicity", "StatBMICat", "Trial1Statin", "DiabStat",  "HypertensionStat",
           "MIStat", "CerebrovascularStat", "CHFStat", "SMIDiagStat", "StatDose1", "SSRIStat", "TCAStat", "OtherADStat", "DyslipStat", "YearStat", "APStatTrial1", "StatDose1Miss", 
           "StatBMIValMiss", "PriorMHBinStat","PriorSHBinStat","PriorPhysicalBinStat","PriorAccidentBinStat", "PriorGPBinStat", "PriorGPSHBinStat", "StatCholMiss", "PatIMD", "PracIMD")

Trial1[,Factors]<-lapply(Trial1[,Factors], factor)

#Table 1 - Cohort basics
MyVars<-c(names(Trial1))
MyVars<-c(MyVars[c(1, 6:44, 46, 51:53, 88)])

Table1<-CreateTableOne(vars=MyVars,  data=Trial1, includeNA = TRUE)
print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table1Exp <- print(Table1, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table1Exp, file = "StatinCPRD/Outputs/Trial1Basic.csv")

#Trial 2
names(Trial1)

Factors<-c("Active2","gender", "Died2yr1", "region", "ethnicity", "StatBMICat", "Trial2Statin", "DiabStat",  "HypertensionStat",
           "MIStat", "CerebrovascularStat", "CHFStat", "SMIDiagStat", "SSRIStat", "TCAStat", "OtherADStat", "DyslipStat", "YearStat", "APStatTrial2", "StatDose1Miss", 
           "StatBMIValMiss", "PriorMHBinStat","PriorSHBinStat", "StatDose1", "PriorPhysicalBinStat","PriorAccidentBinStat", "PriorGPBinStat", "PriorGPSHBinStat", "StatCholMiss", "PatIMD", "PracIMD")

Trial2[,Factors]<-lapply(Trial2[,Factors], factor)

#Table 2 - Cohort basics
MyVars<-c(names(Trial2))
MyVars<-c(MyVars[c(1, 6:44, 46, 51:53, 88)])

Table2<-CreateTableOne(vars=MyVars,  data=Trial2, includeNA = TRUE)
print(Table2, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table2Exp <- print(Table2, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table2Exp, file = "StatinCPRD/Outputs/Trial2Basic.csv")

#Trial 3
names(Trial3)

Factors<-c("Active3","gender", "Died2yr3", "region", "ethnicity", "APBMICat", "LastStatin", "DiabAP",  "HypertensionAP",
           "MIAP", "CerebrovascularAP", "CHFAP", "SMIDiagAP","StatDoseTrial3", "SSRIAP", "TCAAP", "OtherADAP", "DyslipAP", "YearAP", "PriorGPBinAP", "PriorGPSHBinAP", "StatDose3Miss", 
           "APBMIValMiss", "PriorMHBinAP","PriorSHBinAP","PriorPhysicalBinAP","PriorAccidentBinAP", "APCholMiss", "PatIMD", "PracIMD")

Trial3[,Factors]<-lapply(Trial3[,Factors], factor)

#Table 3 - Cohort basics
MyVars<-c(names(Trial3))
MyVars<-c(MyVars[c(6:46, 53:55)])

Table3<-CreateTableOne(vars=MyVars,  data=Trial3, includeNA = TRUE)
print(Table3, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table3Exp <- print(Table3, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table3Exp, file = "StatinCPRD/Outputs/Trial3Basic.csv")

save(Trial1, file="StatinCPRD/Data/Trial1.rdata")
save(Trial2, file="StatinCPRD/Data/Trial2.rdata")
save(Trial3, file="StatinCPRD/Data/Trial3.rdata")

length(which(Trial2$RegTimeStat<0.5 & Trial2$Trial2Statin=="Prav"))
length(which(Trial2$RegTimeStat<0.5))
