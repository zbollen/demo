############### DATA ANALYSIS PART 2: Analysis IAT ##############
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
library(afex)
library(reshape2)
library(lsmeans)

##### 1. Read in final data-file ####

Data<-read.table("DataFINAL.txt")
describe(Data) #190 pps
table(Data$instructionclarity)
table(Data$instructionbelief)

##### 2. ANOVA IAT  #######

# calculate reliability

cor.test(Data$IAT1_odd, Data$IAT1_even) 
(2*0.744027 )/(1+0.744027 ) #Spearman-Brown corrected r=0.85

#create scores
t.test(Data$IAT1)
t.test(Data$lat_incomp-Data$lat_comp) 
cor.test(Data$IAT1,Data$lat_incomp-Data$lat_comp)
Data$IAT2<-Data$lat_incomp-Data$lat_comp #IAT2: pref Morag

Data$IAT<-ifelse(Data$CSneg==1,Data$IAT1,Data$IAT1*-1) #IAT score pref CSneg

# t-test
t.test(Data$IAT) #p = .88: M= 0.005


cohensD(Data$Liking1_CSneg-Data$Liking1_CSpos)
t.test(Data$Liking1_CSneg-Data$Liking1_CSpos) #p.003
sd(Data$IAT)
cohensD(Data$IAT) #0.01

#correlate with explicit scores

Data$Exp_neg<-Data$exp_neg_CSneg-Data$exp_neg_CSpos

Data$Exp_pos<-Data$exp_pos_CSneg-Data$exp_pos_CSpos

Data$Liking<-Data$Liking1_CSneg-Data$Liking1_CSpos

Data$USeval<-Data$US1val-Data$US2val

cor.test(Data$IAT,Data$Liking) #0.19
cor.test(Data$IAT,Data$Exp_neg) #0.11
cor.test(Data$IAT,Data$Exp_pos) #-0.08
cor.test(Data$IAT,Data$USeval) #-0.02
cor.test(Data$Liking, Data$Exp_neg) #-0.30
cor.test(Data$Liking, Data$Exp_pos) #0.30
cor.test(Data$Liking, Data$USeval) #0.23
hist(Data$IAT)
hist(Data$Liking)

# factorize variables

Data$IATorder<-factor(Data$IATorder)
Data$valence<-factor(Data$valence)
Data$CSneg<-factor(Data$CSneg)

# ANOVA
fit1 <- lm(IAT~CSneg, data=Data,
          contrasts = list(CSneg="contr.sum"))
summary(fit1)
Anova(fit1, type="III") 
etasq(fit1, anova = TRUE)


# ANOVA with IAT ORDER 
fit <- lm(IAT~IATorder, data=Data,
             contrasts = list(IATorder="contr.sum"))
summary(fit)
Anova(fit, type="III") 
etasq(fit, anova = TRUE)

# ANOVA with  VALENCE
fit <- lm(IAT~valence, data=Data,
          contrasts = list(valence="contr.sum"))
summary(fit)
Anova(fit, type="III") 
etasq(fit, anova = TRUE)

# ANOVA with  CSNEG*IATorder*VALENCE
fit <- lm(IAT~CSneg*IATorder*valence, data=Data,
          contrasts = list(valence="contr.sum", IATorder="contr.sum", CSneg="contr.sum"))
summary(fit)
Anova(fit, type="III") 
etasq(fit, anova = TRUE)



Data
dataValence1 <- Data[which(Data$valence ==1),]
# t-test
t.test(dataValence1$IAT, alternative = "greater") #p = .15: M = 0.07
cohensD(dataValence1$IAT) #0.15

dataValence2 <- Data[which(Data$valence ==2),]
# t-test
t.test(dataValence2$IAT, alternative = "less") #p = .31: M = -0.05
cohensD(dataValence2$IAT) #0.10


t.test(Data$IAT ~ Data$valence, alternative="greater")
cohensD(Data$IAT ~ Data$valence)


##### 2. ANOVA compare IAT  and expl #######

#mixed anova preparation

labels(Data)
Data$ziat<-Data$IAT/sd(Data$IAT)
Data$zexp<-Data$Liking/sd(Data$Liking)
colnames(Data)
Dataco<-Data[c(1,17, 18,19,44,45)]

Dataco<-melt(Dataco, id.vars=c("subject","IATorder","CSneg", "valence"))

Dataco$Meastype<-ifelse(Dataco$variable=='ziat',1,2)

Dataco$IATorder<-factor(Dataco$IATorder)
table(Dataco$IATorder,Dataco$subject) #bsfactor
table(Dataco$IATorder)/2 #103 vs 87

Dataco$CSneg<-factor(Dataco$CSneg)
table(Dataco$CSneg,Dataco$subject) #bsfactor
table(Dataco$CSneg)/2 #105 vs 85

Dataco$valence<-factor(Dataco$valence)
table(Dataco$valence,Dataco$subject) #bsfactor
table(Dataco$valence)/2 #92 vs 98


Dataco$Meastype<-factor(Dataco$Meastype)
table(Dataco$Meastype,Dataco$subject) #wsfactor

#mixed anova
fit<-aov_car(value ~ IATorder*CSneg*valence * Meastype + Error(subject/(Meastype)),data = Dataco, anova_table = list(es = "pes"))

knitr::kable(nice(fit))
emmeans(fit, ~Meastype) 
