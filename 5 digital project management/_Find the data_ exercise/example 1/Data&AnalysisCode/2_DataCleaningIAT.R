############### DATA PREPARATION PART 2: Data Cleaning IAT ##############
## Last update: 30/10/2020 ##
## Author: Simone Mattavelli ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc)
library (reshape2)

##### 1. Read in Raw Data : standard IAT ####

#read in IAT data
Data<-read.csv("iat.csv", header=TRUE, sep=';')
describe(Data) # 196 pps ! 15 vars 35365 lines 
table(Data$subject,Data$blocknum)
table(Data$trialcode,Data$stimulusitem1)

#only critical IAT blocks
Data$blocknumber<-ifelse(Data$blocknum>4,Data$blocknum-1,Data$blocknum)
Data$blocknumber<-ifelse(Data$blocknumber==8,Data$blocknumber-1,Data$blocknumber)
Data<-Data[Data$blocknumber>2&Data$blocknumber<8,]
Data<-Data[Data$blocknumber>5|Data$blocknumber<5,] #only blocks 3,4,6,7

#re-number trials
Data$trialnumber<-ifelse(Data$blocknumber==3|Data$blocknumber==6,Data$trialnum-1,Data$trialnum)

#only keep important vars: subject, block, trial, blockcode, target, correct, lat
Data<-Data[c(3,16,17,9,10,4)] 
head(Data)
colnames(Data)=c("subject", "blocknumber", "trialnumber","correct", "latency","startblock")

#exclude pps with uncompleted IATs (<60+60=120 trials)
table(Data$subject, Data$blocknumber)
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b1<-b[b$n<120|b$n>120,]
Data<-Data[!(Data$subject %in% b1$number),]
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b # 20 pps (=0 pps excluded)

#read in full data file

Data2<-read.table("Data1.txt",header=T)
Data2<-Data2[Data2$subject %in% Data$subject,]
Data<-Data[Data$subject %in% Data2$subject,]
describe(Data) #20 pps: 0 incomplete!  
write.table(Data2, "Data2_1.txt", sep="\t") 

##### 2. add IAT exclusion variable #######

#discard data if errorrates >.30: more than 36
table(Data$subject,Data$correct)

ar<-table(Data$subject,Data$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>36,]
Data<-Data[!(Data$subject %in% c2$Var1),]
describe(Data)  #so 20 pps ! error excluded pps: 0!

#discard data if errorrates >.40 for any block: more than 8
D1<-Data[Data$blocknumber==3,]
ar<-table(D1$subject,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>8,]
Data<-Data[!(Data$subject %in% c2$Var1),]

D1<-Data[Data$blocknumber==4,]
ar<-table(D1$subject,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>16,]
Data<-Data[!(Data$subject %in% c2$Var1),]

D1<-Data[Data$blocknumber==6,]
ar<-table(D1$subject,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>8,]
Data<-Data[!(Data$subject %in% c2$Var1),]

D1<-Data[Data$blocknumber==7,]
ar<-table(D1$subject,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>16,]
Data<-Data[!(Data$subject %in% c2$Var1),]

describe(Data)  ##so 20 pps ! block error excluded pps: 0!

# discard data if fast trials: >10% (faster than 400 ms: more than 12)
Dataex<-Data[Data$latency<400,]
ar<-table(Dataex$subject)
c1<-as.data.frame(ar)
c2<-c1[c1$Freq>12,]
Data<-Data[!(Data$subject %in% c2$Var1),]

describe(Data)  #so 20 pps ! fast trial excluded pp: 0!

##### 3. compute IAT D2 scores #######

#latencies
Dataa<-Data[Data$startblock=='compatibletest1'|Data$startblock=='compatibletest2',]
Dataa2<-Data[Data$startblock=='incompatibletest1'|Data$startblock=='incompatibletest2',]

ar<-tapply(Dataa$latency, Dataa$subject, mean)
c1<-as.data.frame(ar)
c1$subject<-rownames(c1)
c1$lat_comp<-c1$ar
c1<-c1[c(2,3)] 

Data2<-merge(Data2, c1, by="subject")

ar2<-tapply(Dataa2$latency, Dataa2$subject, mean)
c1<-as.data.frame(ar2)
c1$subject<-rownames(c1)
c1$lat_incomp<-c1$ar
c1<-c1[c(2,3)] 

Data2<-merge(Data2, c1, by="subject")

#run script to calculate D2 IAT score
VAR=c(1,2,3,4,5)
exclude=c()
lower.bound=0
Data<- Data[order(Data$subject),]
summary(Data)
source("maarten_IATscript.txt")
out <- IAT(Data, VAR, exclude, lower.bound)
summary(out)

# calculate D2score
Data1<- out$score$Subject
Data1<-cbind(Data1, out$score$D2)
colnames(Data1)=c("subject", "IAT1")
head(Data1)

# calculate D2score for odd-numbered trials
Datao<- Data[Data$trialnumber%%2==1,]
table(Datao$trialnumber)
out<-IAT(Datao,VAR,exclude,lower.bound)
Data1<-cbind(Data1, out$score$D2)
colnames(Data1)=c("subject", "IAT1","IAT1_odd")

# calculate D2score for even-numbered trials
Datae<- Data[Data$trialnumber%%2==0,]
Datae<- Datae[order(Datae$subject),]
out<-IAT(Datae,VAR,exclude,lower.bound)
Data1<-data.frame(Data1, out$score$D2)
colnames(Data1)=c("subject", "IAT1","IAT1_odd","IAT1_even")

# calculate reliability
cor.test(Data1$IAT1_odd, Data1$IAT1_even) #r=0.74

# calculate first_block variable indicating which block first (e.g. compatible)
Data<-Data[Data$blocknumber==3&Data$trialnumber==1,]
Data$startblock<-factor(Data$startblock)
Data1<-cbind.data.frame(Data1, Data$startblock)
colnames(Data1)=c("subject", "IAT1","IAT1_odd","IAT1_even","IAT1_block1")

##### 4. calculate IAT scores indicating preference for morag over struan #######
table(Data1$IAT1_block1)
Data1$IAT1<-ifelse(Data1$IAT1_block1=='compatibletest1',Data1$IAT1,Data1$IAT1*-1)
t.test(Data1$IAT1) # pref morag over struan: t=2.57, p=.011
describe(Data1) #20 pps

Data<-merge(Data1,Data2, by='subject')
describe(Data) #20 pps

##### 5. write away final data file #######

write.table(Data, "Data2_IAT.txt", sep="\t") 

