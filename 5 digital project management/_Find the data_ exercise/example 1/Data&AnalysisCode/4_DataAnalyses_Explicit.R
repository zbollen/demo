############### DATA ANALYSIS PART 1: Analysis Explicit ##############
## Last update: 30/10/2020 ##
## Author: Simone Mattavelli ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc)
library(BayesFactor)
library(car)
library(effects)
library(lsr)
library(psy)
library(heplots)
library(reshape2)
library(psych)
library(afex)
library(lsmeans)

##### 1. Read in final data-file and exclude those who did not hear the sound ####

Data<-read.table("Data2_IAT.txt")
Data<-subset(Data, subject != 226 & subject != 389 & subject != 797 & subject != 356 & subject != 26 & subject != 330)
describe(Data) #190 pps
colnames(Data)

##### 2. US negative expectancy ratings #######

#create scores

Data$exp_neg_CSneg<-ifelse(Data$CSneg==1,Data$Expect_1neg,-1000)
Data$exp_neg_CSneg<-ifelse(Data$CSneg==2,Data$Expect_2neg,Data$exp_neg_CSneg)

Data$exp_neg_CSpos<-ifelse(Data$CSneg==2,Data$Expect_1neg,-1000)
Data$exp_neg_CSpos<-ifelse(Data$CSneg==1,Data$Expect_2neg,Data$exp_neg_CSpos)

#t-tests

t.test(Data$exp_neg_CSneg,Data$exp_neg_CSpos, paired=T) #11.78, p<.001
cohensD(Data$exp_neg_CSneg-Data$exp_neg_CSpos) #.83
mean(Data$exp_neg_CSneg)
mean(Data$exp_neg_CSpos)
sd(Data$exp_neg_CSneg)
sd(Data$exp_neg_CSpos)

#descriptives
labels(Data)
Dataa<-Data[c(1,19,30:31)]
Dataco<-Dataa

#mixed anova preparation

Dataco<-melt(Dataco, id.vars=c("subject","valence"))

Dataco$CStype<-ifelse(Dataco$variable=='exp_neg_CSneg',1,1000)
Dataco$CStype<-ifelse(Dataco$variable=='exp_neg_CSpos',2,Dataco$CStype)
Dataco$CStype

Dataco$valence<-factor(Dataco$valence)
table(Dataco$valence,Dataco$subject) #bsfactor
table(Dataco$valence)/2 #92 vs 98

Dataco$CStype<-factor(Dataco$CStype)
table(Dataco$CStype,Dataco$subject) #wsfactor


#mixed anova
fit<-aov_car(value ~ valence*CStype + Error(subject/(CStype)),data = Dataco, anova_table = list(es = "pes"))

knitr::kable(nice(fit))

##Main effect of CStype
emmeans(fit, ~CStype) #p<.001
cstype<-emmeans(fit, ~CStype)
pairs (cstype)
exp(ttest.tstat(t=11.76, n1=190, rscale = 0.707)[['bf']]) # BF1>10.000


#interaction VALENCE*cstype
Interaction1<-emmeans(fit, ~CStype|valence) #p.80
Interaction1
pairs(Interaction1)

d<-3.23/(0.386*(sqrt(92)))
d #0.87
d<-3.09/(0.374*(sqrt(98)))
d #0.83



##### 3. US positive expectancy ratings #######

#create scores

Data$exp_pos_CSneg<-ifelse(Data$CSneg==1,Data$Expect_1pos,-1000)
Data$exp_pos_CSneg<-ifelse(Data$CSneg==2,Data$Expect_2pos,Data$exp_pos_CSneg)

Data$exp_pos_CSpos<-ifelse(Data$CSneg==2,Data$Expect_1pos,-1000)
Data$exp_pos_CSpos<-ifelse(Data$CSneg==1,Data$Expect_2pos,Data$exp_pos_CSpos)

#t-tests

t.test(Data$exp_pos_CSneg,Data$exp_pos_CSpos, paired=T) #-13.06, p<.001
cohensD(Data$exp_pos_CSneg-Data$exp_pos_CSpos) #.95
mean(Data$exp_pos_CSneg)
mean(Data$exp_pos_CSpos)
sd(Data$exp_pos_CSneg)
sd(Data$exp_pos_CSpos)

#descriptives
labels(Data)
Dataa<-Data[c(1,19,32:33)]
Dataco<-Dataa

#mixed anova preparation

Dataco<-melt(Dataco, id.vars=c("subject", "valence"))

Dataco$CStype<-ifelse(Dataco$variable=='exp_pos_CSneg',1,1000)
Dataco$CStype<-ifelse(Dataco$variable=='exp_pos_CSpos',2,Dataco$CStype)

Dataco$valence<-factor(Dataco$valence)
table(Dataco$valence,Dataco$subject) #bsfactor
table(Dataco$valence)/2 #92 vs 98

Dataco$CStype<-factor(Dataco$CStype)
table(Dataco$CStype,Dataco$subject) #wsfactor

#mixed anova
fit<-aov_car(value ~ valence*CStype + Error(subject/(CStype)),data = Dataco, anova_table = list(es = "pes"))

knitr::kable(nice(fit))

##Main effect of CStype
emmeans(fit, ~CStype) #p<.001
cstype<-emmeans(fit, ~CStype)
pairs (cstype)
exp(ttest.tstat(t=-13.001, n1=190, rscale = 0.707)[['bf']]) # BF1>10.000

#interaction Iatorder*cstype
Interaction1<-emmeans(fit, ~CStype|valence) #p.66
Interaction1
pairs(Interaction1)

d<--3.66/(0.419*(sqrt(92)))
d #-0.91
d<--3.92/(0.406*(sqrt(98)))
d #-0.98

###CORRELATIONS###

Data$ExpNegDiff<-Data$exp_neg_CSneg-Data$exp_neg_CSpos
Data$ExpPosDiff<-Data$exp_pos_CSpos-Data$exp_pos_CSneg

cor.test(Data$ExpNegDiff,Data$ExpPosDiff) # .49


##### 4. CS Liking ratings #######

#create scores

Data$Liking1_CSneg<-ifelse(Data$CSneg==1,Data$Liking_1,-1000)
Data$Liking1_CSneg<-ifelse(Data$CSneg==2,Data$Liking_2,Data$Liking1_CSneg)

Data$Liking1_CSpos<-ifelse(Data$CSneg==2,Data$Liking_1,-1000)
Data$Liking1_CSpos<-ifelse(Data$CSneg==1,Data$Liking_2,Data$Liking1_CSpos)

#t-tests

t.test(Data$Liking1_CSneg,Data$Liking1_CSpos, paired=T) #-2.93, p.004
mean(Data$Liking1_CSneg)
mean(Data$Liking1_CSpos)
sd(Data$Liking1_CSneg)
sd(Data$Liking1_CSpos)
cohensD(Data$Liking1_CSneg-Data$Liking1_CSpos) #0.21

#correlations

cor.test(Data$Liking1_CSneg,Data$exp_neg_CSneg) #-0.11
cor.test(Data$Liking1_CSneg,Data$exp_pos_CSneg) # 0.06
cor.test(Data$Liking1_CSpos,Data$exp_neg_CSpos) #-0.14
cor.test(Data$Liking1_CSpos,Data$exp_pos_CSpos) # 0.25

#descriptives
labels(Data)
Dataa<-Data[c(1,19,36:37)]
Dataco<-Dataa

#mixed anova preparation

Dataco<-melt(Dataco, id.vars=c("subject", "valence"))

Dataco$CStype<-ifelse(Dataco$variable=='Liking1_CSneg',1,1000)
Dataco$CStype<-ifelse(Dataco$variable=='Liking1_CSpos',2,Dataco$CStype)


Dataco$valence<-factor(Dataco$valence)
table(Dataco$valence,Dataco$subject) #bsfactor
table(Dataco$valence)/2 #92 vs 98

Dataco$CStype<-factor(Dataco$CStype)
table(Dataco$CStype,Dataco$subject) #wsfactor

#mixed anova
fit<-aov_car(value ~ valence * CStype + Error(subject/(CStype)),data = Dataco, anova_table = list(es = "pes"))

knitr::kable(nice(fit))

##Main effect of CStype
emmeans(fit, ~CStype) #p.003
cstype<-emmeans(fit, ~CStype)
pairs (cstype)
exp(ttest.tstat(t=-2.89, n1=190, rscale = 0.707)[['bf']]) # BF1= 4.55


#interaction valence*cstype
Interaction1<-emmeans(fit, ~CStype|valence) #p.01
Interaction1
pairs(Interaction1)

d<--.174/(0.533*(sqrt(92)))
d #-0.03
d<--1.969/(0.516*(sqrt(98)))
d #-0.39


##run separate t-test##

dataValence1<-subset(Data, Data$valence == 1)

t.test(dataValence1$Liking1_CSneg,dataValence1$Liking1_CSpos, paired=T) #-.53, p.60
mean(dataValence1$Liking1_CSneg)
mean(dataValence1$Liking1_CSpos)
sd(dataValence1$Liking1_CSneg)
sd(dataValence1$Liking1_CSpos)
cohensD(dataValence1$Liking1_CSneg-dataValence1$Liking1_CSpos) #0.05


dataValence2<-subset(Data, Data$valence == 2)

t.test(dataValence2$Liking1_CSneg,dataValence2$Liking1_CSpos, paired=T) #-3.24, p.002
mean(dataValence2$Liking1_CSneg)
mean(dataValence2$Liking1_CSpos)
sd(dataValence2$Liking1_CSneg)
sd(dataValence2$Liking1_CSpos)
cohensD(dataValence2$Liking1_CSneg-dataValence2$Liking1_CSpos) #0.33


##### 5. US evaluation #######

#t-tests

t.test(Data$US1val,Data$US2val, paired=T) #-2.93, p.004
mean(Data$US1val)
mean(Data$US2val)
sd(Data$US1val)
sd(Data$US2val)
cohensD(Data$US1val-Data$US2val) #0.21


#mixed anova preparation

#descriptives
labels(Data)
Dataa<-Data[c(1,19,14:15)]
Dataco<-Dataa

Dataco<-melt(Dataco, id.vars=c("subject",  "valence"))

Dataco$UStype<-ifelse(Dataco$variable=='US1val',1,1000)
Dataco$UStype<-ifelse(Dataco$variable=='US2val',2,Dataco$UStype)


Dataco$valence<-factor(Dataco$valence)
table(Dataco$valence,Dataco$subject) #bsfactor
table(Dataco$valence)/2 #92 vs 98

Dataco$UStype<-factor(Dataco$UStype)
table(Dataco$UStype,Dataco$subject) #wsfactor

#mixed anova
fit<-aov_car(value ~ valence * UStype + Error(subject/(UStype)),data = Dataco, anova_table = list(es = "pes"))

knitr::kable(nice(fit))

#interaction valence*cstype
Interaction1<-emmeans(fit, ~UStype|valence) #p.19
Interaction1
pairs(Interaction1)

d<--6.22/(0.332*(sqrt(92)))
d #-1.95
d<--5.61/(0.321*(sqrt(98)))
d #-1.76

fit<- aov(Data$US1val ~ valence, data=Data)
summary(fit)

fit<- aov(Data$US2val ~ valence, data=Data)
summary(fit)

write.table(Data, "DataFINAL.txt", sep="\t") 

