############### Meta_Analysis - Study 1-4 ##############
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
library(afex)

##### 1. Read in final data-file ####

Data1<-read.table("DataFINAL1.txt")
Data2<-read.table("DataFINAL2.txt")
Data3<-read.table("DataFINAL3.txt")
colnames(Data3)
Data3$nrUSs
Data3a<-subset(Data3, Data3$nrUSs == 1)
Data3b<-subset(Data3, Data3$nrUSs == 2)
Data4<-read.table("DataFINAL7.txt")
Data4<-subset(Data4, Data4$valence == 1)
variable.names(Data1)
variable.names(Data2)
variable.names(Data3a)
variable.names(Data3b)
variable.names(Data4)
Data1<-Data1[,c(1,24,25)]
names(Data1)[2] <- "Liking1_CSneg"
names(Data1)[3] <- "Liking1_CSpos"
Data1$Exp<-1
Data1$USnr<-1

Data2<-Data2[,c(1,26,27)]
names(Data2)[2] <- "Liking1_CSneg"
names(Data2)[3] <- "Liking1_CSpos"
Data2$subject<-Data2$subject+500
Data2$Exp<-2
Data2$USnr<-1

Data3a<-Data3a[,c(1,31,32)]

names(Data3a)[2] <- "Liking1_CSneg"
names(Data3a)[3] <- "Liking1_CSpos"
Data3a$subject<-Data3a$subject+5000
Data3a$Exp<-3
Data3a$USnr<-1

Data3b<-Data3b[,c(1,31,32)]
names(Data3b)[2] <- "Liking1_CSneg"
names(Data3b)[3] <- "Liking1_CSpos"
Data3b$subject<-Data3b$subject+5000
Data3b$Exp<-3
Data3b$USnr<-2

Data4<-Data4[,c(1,36,37)]
Data4$subject<-Data4$subject+50000
Data4$Exp<-4
Data4$USnr<-2

Data<-rbind(Data1,Data2,Data3a, Data3b, Data4)


describe(Data) #742 pps

##### T-TEST LIKING #######

t.test(Data$Liking1_CSneg, Data$Liking1_CSpos, paired = TRUE) #-4.84, p<.001
cohensD(Data$Liking1_CSneg-Data$Liking1_CSpos) #.18
ttestBF(Data$Liking1_CSneg-Data$Liking1_CSpos) 


Data$explicit<-Data$Liking1_CSneg-Data$Liking1_CSpos

anova<- aov(Data$explicit ~ Data$Exp)
summary(anova)




####IAT###

##### T-TEST IAT  #######


Data1<-read.table("DataFINAL1.txt")
Data2<-read.table("DataFINAL2.txt")
Data3<-read.table("DataFINAL3.txt")
Data3a<-subset(Data3, Data3$nrUSs == 1)
Data3b<-subset(Data3, Data3$nrUSs == 2)
Data4<-read.table("DataFINAL7.txt")
Data4<-subset(Data4, Data4$valence == 1)
variable.names(Data1)
names(Data1)[18] <- "CSneg"
names(Data1)[24] <- "LikingCSneg"
names(Data1)[25] <- "LikingCSpos"
variable.names(Data2)
names(Data2)[18] <- "CSneg"
names(Data2)[26] <- "LikingCSneg"
names(Data2)[27] <- "LikingCSpos"
variable.names(Data3a)
names(Data3a)[20] <- "CSneg"
names(Data3a)[31] <- "LikingCSneg"
names(Data3a)[32] <- "LikingCSpos"
variable.names(Data3b)
names(Data3b)[20] <- "CSneg"
names(Data3b)[31] <- "LikingCSneg"
names(Data3b)[32] <- "LikingCSpos"
variable.names(Data4)
names(Data4)[36] <- "LikingCSneg"
names(Data4)[37] <- "LikingCSpos"

Data1<-Data1[,c(1,2,18,24,25)]
Data1$Exp<-1
Data1$USnr<-1

Data2<-Data2[,c(1,2, 18, 26,27)]
Data2$subject<-Data2$subject+500
Data2$Exp<-2
Data2$USnr<-1

Data3a<-Data3a[,c(1,2, 20, 31, 32)]
Data3a$subject<-Data3a$subject+5000
Data3a$Exp<-3
Data3a$USnr<-1

Data3b<-Data3b[,c(1,2, 20, 31, 32)]
Data3b$subject<-Data3b$subject+50000
Data3b$Exp<-3
Data3b$USnr<-2

Data4<-Data4[,c(1,2,18, 36, 37)]
Data4$subject<-Data4$subject+500000
Data4$Exp<-4
Data4$USnr<-2

Data<-rbind(Data1,Data2,Data3a, Data3b, Data4)


#create scores
Data$IAT<-ifelse(Data$CSneg==1,Data$IAT1,Data$IAT1*-1)

# t-test
t.test(Data$IAT) #p < .001
sd(Data$IAT)    # .40
cohensD(Data$IAT)    #.18
ttestBF(Data$IAT)   #6199.24


anova2<- aov(Data$IAT ~ Data$Exp)
summary(anova2)

#correlate with explicit scores

Data$explicit<-Data$LikingCSneg-Data$LikingCSpos

cor.test(Data$IAT,Data$explicit)
result <- correlationBF(Data$IAT, Data$explicit)
result
