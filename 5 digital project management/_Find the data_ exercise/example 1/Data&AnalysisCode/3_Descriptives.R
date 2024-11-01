############### DATA DESCRIPTIVES ##############
## Last update: 30/10/2020 ##
## Author: Simone Mattavelli ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc) 

##### 1. Read in final data-file ####

Data<-read.table("Data2_IAT.txt")
describe(Data) 

##### 2. Check counterbalancing ####

# conditions
table(Data$Group) 
table(Data$IATorder,Data$CSneg, Data$valence)

##### 3. Descriptives #####

Data2<-read.csv("demographics.csv", header=TRUE, sep=';')
Data2<-Data2[(Data2$subject %in% Data$subject),]
describe(Data2) #163 pps

## Age

Data2a<-Data2[Data2$trialcode=="age",]
Data2a<-Data2a[order(Data2a$subject),]
describe(Data2a) #163 pps

Data2a$response<-as.numeric(as.character(Data2a$response))
describe(Data2a$response)
mean(Data2a$response, na.rm=T) #33.13 years
sd(Data2a$response, na.rm=T) #sd=12.76

## Sex

Data2a<-Data2[Data2$trialcode=="sex",]
Data2a<-Data2a[order(Data2a$subject),]
describe(Data2a) #163 pps

Data2a$response<-as.numeric(Data2a$response)
table(Data2a$response) # F:84, M:79

