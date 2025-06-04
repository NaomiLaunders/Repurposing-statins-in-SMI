rm(list = ls(all.names = TRUE))
####Libraries####
library(tidyverse)
library(lubridate)
library(tableone)
library(forestplot)

#Trial1

#Load the results
load("StatinCPRD/Outputs/Trial1Adj.Rdata")

Trial1<-AdjResult
#Forest plot for Trial 1 - 12 month
Trial1<-subset(Trial1, term=="Active11")
Trial1$Group<-factor(Trial1$Group, level=c("MH", "SH", "Accident", "Phys"))
Trial1$TimePoint<-factor(Trial1$TimePoint, level=c("3", "6", "12", "24"))

Trial1<-Trial1%>%
  arrange(Group, TimePoint)

Plot1<-Trial1%>%
  arrange(Group, TimePoint)%>%
  mutate(Hosp=lapply(Group, \(x) case_when(x=="Accident"  ~ "Accident/injury",
                                        x=="MH"  ~ "Psychiatric",
                                        x=="SH" ~ "Self-harm",
                                        x=="Phys" ~ "Physical",
                                        TRUE ~ "")),
         Time=lapply(TimePoint, \(x) case_when(x=="3" ~ "3 months",
                                               x=="6" ~ "6 months",
                                               x=="12" ~ "12 months",
                                               x=="24" ~ "24 months",
                                        TRUE ~ "None")))
Label<-select(Plot1, Hosp, Time)
Label$Hosp[Label$Time=="6 months"]<-"admissions"
Label$Hosp[Label$Time=="12 months"|Label$Time=="24 months"]<-""
                                        
forestplot(labeltext = c(Label), boxsize=0.1, line.margin=0.1,xticks.digits = 1,ci.vertices=TRUE,
           hrzl_lines = list("1" = gpar(lty=2), "5" = gpar(lty=2), "9" = gpar(lty=2),"13" = gpar(lty=2)), 
           mean = Trial1$estimate,
           lower = Trial1$`2.5 %`,
           upper = Trial1$`97.5 %`,
           zero=1, xlog=TRUE, lwd.ci=gpar(lwd=2), xlab="Adjusted hazard ratio", 
           clip = c(0.1,3),
           xticks = c(log(0.5), log(1), log(2), log(4)),
           txt_gp = fpTxtGp(ticks=gpar(fontFamily="", fontsize=8, cex=1.5), label = gpar(fontFamily = "Arial", fontsize=8,cex=1.5), legend=gpar(fontFamily = "Arial", fontsize=8,cex=1.5), xlab=gpar(fontFamily = "Arial", fontsize=8,cex=1.5)))%>%
  fp_set_zebra_style("#f9f9f9")

#Trial 2
load("StatinCPRD/Outputs/Trial2Adj.Rdata")
Trial2<-AdjResult
#Forest plot for Trial 1 - 12 month
Trial2<-subset(Trial2, term=="Active21")
Trial2$Group<-factor(Trial2$Group, level=c("MH", "SH", "Accident", "Phys"))
Trial2$TimePoint<-factor(Trial2$TimePoint, level=c("3", "6", "12", "24"))
Trial2<-subset(Trial2, !(Group=="SH" & TimePoint=="3")&!(Group=="Accident" & (TimePoint=="3"|TimePoint=="6")))

Trial2<-Trial2%>%
  arrange(Group, TimePoint)

Plot2<-Trial2%>%
  arrange(Group, TimePoint)%>%
  mutate(Hosp=lapply(Group, \(x) case_when(x=="Accident"  ~ "Accident/injury",
                                           x=="MH"  ~ "Psychiatric",
                                           x=="SH" ~ "Self-harm",
                                           x=="Phys" ~ "Physical",
                                           TRUE ~ "")),
         Time=lapply(TimePoint, \(x) case_when(x=="3" ~ "3 months",
                                               x=="6" ~ "6 months",
                                               x=="12" ~ "12 months",
                                               x=="24" ~ "24 months",
                                               TRUE ~ "None")))
Label<-select(Plot2, Hosp, Time)
Label$Hosp[(Label$Time=="12 months" & Label$Hosp=="Self-harm")|(Label$Time=="24 months" & Label$Hosp=="Accident/injury")|(Label$Time=="6 months" & (Label$Hosp=="Psychiatric"|Label$Hosp=="Physical"))]<-"admissions"
Label$Hosp[(Label$Time=="24 months" & Label$Hosp=="Self-harm")|(Label$Time=="12 months" & (Label$Hosp=="Psychiatric"|Label$Hosp=="Physical"))|(Label$Time=="24 months" & (Label$Hosp=="Psychiatric"|Label$Hosp=="Physical"))]<-""

forestplot(labeltext = c(Label), boxsize=0.1, line.margin=0.1,xticks.digits = 1,ci.vertices=TRUE,
           hrzl_lines = list("1" = gpar(lty=2), "5" = gpar(lty=2), "8" = gpar(lty=2),"10" = gpar(lty=2)), 
           mean = Trial2$estimate,
           lower = Trial2$`2.5 %`,
           upper = Trial2$`97.5 %`,
           zero=1, xlog=TRUE, lwd.ci=gpar(lwd=2), xlab="Adjusted hazard ratio", 
           clip = c(0.1,3),
           xticks = c(log(0.1),log(0.2), log(0.4), log(0.7), log(1),log(2),log(4), log(8)),
           txt_gp = fpTxtGp(ticks=gpar(fontFamily="", fontsize=8, cex=1.5), label = gpar(fontFamily = "Arial", fontsize=8,cex=1.5), legend=gpar(fontFamily = "Arial", fontsize=8,cex=1.5), xlab=gpar(fontFamily = "Arial", fontsize=8,cex=1.5)))%>%
  fp_set_zebra_style("#f9f9f9")


#Trial 3
load("StatinCPRD/Outputs/Trial3Adj.Rdata") 

Trial3<-AdjResult
#Forest plot for Trial 1 - 12 month
Trial3<-subset(Trial3, term=="Active31")
Trial3$Group<-factor(Trial3$Group, level=c("MH", "SH", "Accident", "Phys"))
Trial3$TimePoint<-factor(Trial3$TimePoint, level=c("3", "6", "12", "24"))

Trial3<-Trial3%>%
  arrange(Group, TimePoint)

Plot3<-Trial3%>%
  arrange(Group, TimePoint)%>%
  mutate(Hosp=lapply(Group, \(x) case_when(x=="Accident"  ~ "Accident/injury",
                                           x=="MH"  ~ "Psychiatric",
                                           x=="SH" ~ "Self-harm",
                                           x=="Phys" ~ "Physical",
                                           TRUE ~ "")),
         Time=lapply(TimePoint, \(x) case_when(x=="3" ~ "3 months",
                                               x=="6" ~ "6 months",
                                               x=="12" ~ "12 months",
                                               x=="24" ~ "24 months",
                                               TRUE ~ "None")))
Label<-select(Plot3, Hosp, Time)
Label$Hosp[Label$Time=="6 months"]<-"admissions"
Label$Hosp[Label$Time=="12 months"|Label$Time=="24 months"]<-""

forestplot(labeltext = c(Label), boxsize=0.1, line.margin=0.1,xticks.digits = 1,ci.vertices=TRUE,
           hrzl_lines = list("1" = gpar(lty=2), "5" = gpar(lty=2), "9" = gpar(lty=2),"13" = gpar(lty=2)), 
           mean = Trial3$estimate,
           lower = Trial3$`2.5 %`,
           upper = Trial3$`97.5 %`,
           zero=1, xlog=TRUE, lwd.ci=gpar(lwd=2), xlab="Adjusted hazard ratio", 
           clip = c(0.1,3),
           xticks = c(log(0.5), log(1),log(2),log(4)),
           txt_gp = fpTxtGp(ticks=gpar(fontFamily="", fontsize=8, cex=1.5), label = gpar(fontFamily = "Arial", fontsize=8,cex=1.5), legend=gpar(fontFamily = "Arial", fontsize=8,cex=1.5), xlab=gpar(fontFamily = "Arial", fontsize=8,cex=1.5)))%>%
  fp_set_zebra_style("#f9f9f9")

####Trial 3 stratified analysis####
load("StatinCPRD/Outputs/Trial3_PhysStrat.Rdata")
load("StatinCPRD/Outputs/Trial3_MHStrat.Rdata")
MHStrat$Group<-"Psychiatric"
PhysStrat$Group<-"Physical health"
Strat<-rbind(MHStrat, PhysStrat)

Strat<-subset(Strat, term=="Active31")
Strat$Group<-factor(Strat$Group, level=c("Psychiatric", "Physical health"))
Strat$Diag<-factor(Strat$Diag, level=c("Schizophrenia", "Bipolar disorder", "Other psychoses", "Unknown"))

Strat<-Strat%>%
  arrange(Group, Diag)

PlotStrat<-Strat%>%
  arrange(Group, Diag)%>%
  mutate(Hosp=lapply(Group, \(x) case_when(x=="Psychiatric"  ~ "Psychiatric",
                                           x=="Physical health"  ~ "Physical health",
                                         TRUE ~ "")),
         Diag=lapply(Diag, \(x) case_when(x=="3" ~ "3 months",
                                               x=="Schizophrenia" ~ "Schizophrenia",
                                               x=="Bipolar disorder" ~ "Bipolar disorder",
                                               x=="Other psychoses" ~ "Other psychoses",
                                          x=="Unknown" ~ "Undetermined",
                                               TRUE ~ "None")))
Label<-select(PlotStrat, Hosp, Diag)
Label$Hosp[Label$Diag=="Bipolar disorder"|Label$Diag=="Other psychoses"|Label$Diag=="Undetermined"]<-""

forestplot(labeltext = c(Label), boxsize=0.1, line.margin=0.1,xticks.digits = 1,ci.vertices=TRUE,
           hrzl_lines = list("1" = gpar(lty=2), "5" = gpar(lty=2), "9" = gpar(lty=2)), 
           mean = Strat$estimate,
           lower = Strat$`2.5 %`,
           upper = Strat$`97.5 %`,
           zero=1, xlog=TRUE, lwd.ci=gpar(lwd=2), xlab="Adjusted hazard ratio", 
           clip = c(0.1,3),
           xticks = c(log(0.2), log(0.5), log(1),log(2)),
           txt_gp = fpTxtGp(ticks=gpar(fontFamily="", fontsize=8, cex=1.5), label = gpar(fontFamily = "Arial", fontsize=8,cex=1.5), legend=gpar(fontFamily = "Arial", fontsize=8,cex=1.5), xlab=gpar(fontFamily = "Arial", fontsize=8,cex=1.5)))%>%
  fp_set_zebra_style("#f9f9f9")

 