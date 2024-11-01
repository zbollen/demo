############### DATA PREPARATION PART 1: Data Cleaning ##############
## Last update: 30/10/2020 ##
## Author: Simone Mattavelli ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc)
library (reshape2)

##### 1. Read in Raw Data : first ratings ####

#read in explicit rating data
Data1<-read.table("ratings2.iqdat", header=TRUE)
head(Data1)
Data1<-Data1[c(4,7,9,11)]
colnames(Data1)=c("subject", "Liking_2","Expect_2neg","Expect_2pos")
Data1
Data2<-read.table("ratings1.iqdat", header=TRUE)
Data2<-Data2[c(4,7,9,11)]
colnames(Data2)=c("subject", "Liking_1","Expect_1neg","Expect_1pos")
Data2
head(Data2)

Data<-merge(Data1,Data2, by='subject')
describe(Data) # 197 pps - 7 variables 
table(Data$subject) #1 line/pp
colnames(Data)
##### 2. Add exploratory questions ####

#read in data
Data1<-read.table("exploratory.iqdat", header=TRUE)
head(Data1)
Data1<-Data1[c(4,7,9,11,13)]
colnames(Data1)=c("subject","instructionclarity","instructionbelief","US1val","US2val")
Data<-merge(Data,Data1, by='subject')

##### 3. Add condition ####

Data$Group<-Data$subject%%8

Data$IATorder<-ifelse(Data$Group%%2==1, 1,2) # 1= good+Morag first

Data$CSneg<-ifelse(Data$Group%%4==1|Data$Group%%4==2,1, 2) #1 = Morag followed by unpleasant

Data$valence<-ifelse(Data$Group%%8==1|Data$Group%%8==2|Data$Group%%8==3|Data$Group%%8==4, 1, 2) # 1= no valence

table(Data$IATorder,Data$CSneg, Data$valence)
colnames(Data)


##### 4. Add memory 1 ####

Data1<-read.csv("Memory1.csv", header=TRUE, sep=';')
colnames(Data)
Data1<-Data1[c(4,7,9)]
Data<-merge(Data,Data1, by='subject')

Data$concorr<-ifelse((Data$memory1_response=='8/10 times followed by the FIRST sound')&(Data$valence==1),1,0)
Data$concorr<-ifelse((Data$memory1_response=='8/10 times followed by the UNPLEASANT sound')&(Data$valence==2),1,0)

Data$concorr<-ifelse((Data$memory2_response=='Always followed by the SECOND sound')&(Data$valence==1),Data$concorr+1,Data$concorr)
Data$concorr<-ifelse((Data$memory2_response=='Always followed by the PLEASANT sound')&(Data$valence==2),Data$concorr+1,Data$concorr)

table(Data$concorr) #170 correct
colnames(Data)


##### 5. Add memory check at the end ####

Data1<-read.csv("Memory2.csv", header=TRUE, sep=';')
Data1
Data1<-Data1[c(4,7,9)]
Data<-merge(Data,Data1, by='subject')

Data$concorr2<-ifelse((Data$memory1_2_response=='8/10 times followed by the FIRST sound')&(Data$valence==1),1,0)
Data$concorr2<-ifelse((Data$memory1_2_response=='8/10 times followed by the UNPLEASANT sound')&(Data$valence==2),1,0)

Data$concorr2<-ifelse((Data$memory2_2_response=='Always followed by the SECOND sound')&(Data$valence==1),Data$concorr2+1,Data$concorr2)
Data$concorr2<-ifelse((Data$memory2_2_response=='Always followed by the PLEASANT sound')&(Data$valence==2),Data$concorr2+1,Data$concorr2)

table(Data$concorr2) #182 correct
colnames(Data)

##### 6 add final exploratory #######

Data1<-read.csv("Final_question.csv", header=TRUE, sep=';')
Data1
colnames(Data1)
Data1<-Data1[c(4,7,9)]
Data<-merge(Data,Data1, by='subject')
Data <- Data[-c(44, 135, 101), ]
Data
##### 7 write away final data file #######

write.table(Data, "Data1.txt", sep="\t") 

