#upload functions and packeges
source("C:\\yba.funcs.R")

#The data's folder (directory)
dir = 'C:\\all'
# where to save merged files
out1 = 'C:\\out'

###################
###################
#demo
###################
path <- paste(dir, "demographics_19_12_04.csv", sep="\\")
demo <- read.csv(path)
head(demo)

demos <- demo[, c("subject","sex_response", "age_response" )] 

#Get rid of deuplicates
demosdup1 <- demos[duplicated(demos[,c('subject')]),]
demosdup1$subject
#none
#save this file
write.csv(demos,file=paste(out1, "demos.csv", sep="\\"))

#############
#read EC condition
##############
path <- paste(dir, "ec_training_w_n_c_l_19_12_04.csv", sep="\\")
nif_w <- read.csv(path)

nif_w1 <- filter(nif_w, blocknum ==1 & trialnum ==1)
nif_w1$condition <- "niff_w"
nif_w1 <- nif_w1[, c("subject", "condition")]

#Get rid of deuplicates
nif_w1_D <- nif_w1[duplicated(nif_w1[,c('subject')]),]
nif_w1_D$subject
#no


path <- paste(dir, "ec_training_c_n_w_l_19_12_04.csv", sep="\\")
lup_w <- read.csv(path)

lup_w1 <- filter(lup_w, blocknum ==1 & trialnum ==1)
lup_w1$condition <- "luup_w"
lup_w1 <- lup_w1[, c("subject", "condition")]

#Get rid of deuplicates
lup_w1_D <- lup_w1[duplicated(lup_w1[,c('subject')]),]
lup_w1_D$subject
#421

#merge all EC files
cond <- full_join(nif_w1, lup_w1, by = c("subject", "condition"))
#save this file
write.csv(cond,file=paste(out1, "cond.csv", sep="\\"))
#I will delete the duplicated in the csv (could be the same person that started the task twice will delete if is duplicate on DV)

##################
#explicit stereotype
#################
path <- paste(dir, "explicit_survey_19_12_04.csv", sep="\\")
toexpst <- read.csv(path)
head(toexpst)
#recode to numeric. positive scores indicate a stronger link between Niffites and Warm/Luupites and competent
table(toexpst$warm_response)

toexpst$warm_s <- ifelse(toexpst$warm_response %like% "1=",7,
                              ifelse(toexpst$warm_response %like% "2",6,
                                     ifelse(toexpst$warm_response %like% "3",5,
                                            ifelse(toexpst$warm_response %like% "4=",4,
                                                   ifelse(toexpst$warm_response %like% "5",3,
                                                          ifelse(toexpst$warm_response %like% "6",2,
                                                                 ifelse(toexpst$warm_response %like% "7=",1, NA)))))))

table(toexpst$competent_response)

toexpst$comp_s <- ifelse(toexpst$competent_response %like% "1=",1,
                         ifelse(toexpst$competent_response %like% "2",2,
                                ifelse(toexpst$competent_response %like% "3",3,
                                       ifelse(toexpst$competent_response %like% "4=",4,
                                              ifelse(toexpst$competent_response %like% "5",5,
                                                     ifelse(toexpst$competent_response %like% "6",6,
                                                            ifelse(toexpst$competent_response %like% "7=",7, NA)))))))

#recode with -3 to +3 

toexpst$warm_s1 <- ifelse(toexpst$warm_response %like% "1=",3,
                          ifelse(toexpst$warm_response %like% "2",2,
                                 ifelse(toexpst$warm_response %like% "3",1,
                                        ifelse(toexpst$warm_response %like% "4=",0,
                                               ifelse(toexpst$warm_response %like% "5",-1,
                                                      ifelse(toexpst$warm_response %like% "6",-2,
                                                             ifelse(toexpst$warm_response %like% "7=",-3, NA)))))))


toexpst$comp_s1 <- ifelse(toexpst$competent_response %like% "1=",-3,
                         ifelse(toexpst$competent_response %like% "2",-2,
                                ifelse(toexpst$competent_response %like% "3",-1,
                                       ifelse(toexpst$competent_response %like% "4=",0,
                                              ifelse(toexpst$competent_response %like% "5",1,
                                                     ifelse(toexpst$competent_response %like% "6",2,
                                                            ifelse(toexpst$competent_response %like% "7=",3, NA)))))))

#create one score by average the two items
toexpst$stExplicit<- rowMeans(toexpst[,c('warm_s', 'comp_s')], na.rm=TRUE)
toexpst$stExplicit1<- rowMeans(toexpst[,c('warm_s1', 'comp_s1')], na.rm=TRUE)
#keep what we need
expst <- toexpst[, c("subject", "warm_s", 'comp_s', 'stExplicit', "warm_s1", 'comp_s1', 'stExplicit1')]


#Get rid of deuplicates
expstdup1 <- expst[duplicated(expst[,c('subject')]),]
expstdup1$subject
#no
#expst$ok <- ifelse(expst$subject %in% c(45, 213),0,1)
#expst <- filter(expst, ok == 1)


#save this file
write.csv(expst,file=paste(out1, "expst.csv", sep="\\"))


###################
#explicit valence ratings 
###################

#learn about order
path <- paste(dir, "valence_ratings_19_12_04.csv", sep="\\")
toval <- read.csv(path)
head(toval)

#recode the var names to shorter names
toval$Comp_pn <- toval$Competent_PN_Positive_Negative_response
toval$Comp_gb <- toval$Competent_GB_Good_Bad_response

toval$Warm_pn <- toval$Warm_PN_Positive_Negative_response
toval$Warm_gb <- toval$Warm_GB_Good_Bad_response


#test if numeric
class(toval$Comp_pn)
class(toval$Comp_gb)
class(toval$Warm_pn)
class(toval$Warm_gb)

#compute mean evaluation score
#recode rating
toval$expComp<- rowMeans(toval[,c('Comp_gb', 'Comp_pn')], na.rm=TRUE)
toval$expWarm <- rowMeans(toval[,c('Warm_gb', 'Warm_pn')], na.rm=TRUE)
#compute valExplicit <- to reflect more positivty of warm
toval$valExplicit <- toval$expWarm-toval$expComp

#Get rid of deuplicates
tovaldup1 <- toval[duplicated(toval[,c('subject')]),]
tovaldup1$subject
#no

#toval$ok <- ifelse(toval$subject %in% 45,0,1)
#toval <- filter(toval, ok == 1)

#keep only the relevant vars
valence <- toval[, c("subject", "Comp_pn", "Comp_gb", "Warm_pn", "Warm_gb", "expComp", "expWarm", "valExplicit" )] 

#save this file
write.csv(valence,file=paste(out1, "val.csv", sep="\\"))


#################
#Memory
#############
path <- paste(dir, "memory_19_12_04.csv", sep="\\")
tomem <- read.csv(path)
head(tomem)


#Get rid of deuplicates
tomemdup1 <- tomem[duplicated(tomem[,c('subject')]),]
tomemdup1$subject
#no

#toval$ok <- ifelse(toval$subject %in% c(45, 213),0,1)
#toval <- filter(toval, ok == 1)

#keep only the relevant vars
mem <- tomem[, c("subject", "mem_c_response", "mem_w_response" )] 

#save this file
write.csv(mem,file=paste(out1, "mem.csv", sep="\\"))

###########################################################
###########################################################
##IAT valence
#I will recode the IAT such that a positive score indicate - preference for warm
###########################################################
###########################################################

###########################################################
#w_g_c_b
###########################################################
path <- paste(dir, "iat_w_g_c_b_19_12_04.csv", sep="\\")
toiat1 <- read.csv(path)
head(toiat1)
#Use Martan script
#creacte a data set that it need after combining the four data sets 
#keep only relavent trials
iatRaw1 <- subset(toiat1, blocknum %in% c(3, 5, 7, 9))
#Create blockName based on the pairing condition.
iatRaw1$blockName[iatRaw1$blocknum == 3] <- "3"
iatRaw1$blockName[iatRaw1$blocknum == 5] <- "4"
iatRaw1$blockName[iatRaw1$blocknum == 7] <- "6"
iatRaw1$blockName[iatRaw1$blocknum == 9] <- "7"

table(iatRaw1$blockName)
length(unique(iatRaw1$subject))

#keep only what we need 
Data1 <- iatRaw1[,c("subject", "blockName", "trialnum", "correct", "latency")]
#rename according to what the code need
colnames(Data1)=c("subject", "blocknumber", "trialnumber","correct", "latency")

##########################################################
#iat_c_g_w_b_19_05_17
###########################################################
path <- paste(dir, "iat_c_g_w_b_19_12_04.csv", sep="\\")
toiat2 <- read.csv(path)
head(toiat2)

#keep only relavent trials
iatRaw2 <- subset(toiat2, blocknum %in% c(3, 5, 7, 9))
#Create blockName based on the pairing condition.
iatRaw2$blockName[iatRaw2$blocknum == 3] <- "6"
iatRaw2$blockName[iatRaw2$blocknum == 5] <- "7"
iatRaw2$blockName[iatRaw2$blocknum == 7] <- "3"
iatRaw2$blockName[iatRaw2$blocknum == 9] <- "4"

table(iatRaw2$blockName)
length(unique(iatRaw2$subject))

#keep only what we need 
Data2 <- iatRaw2[,c("subject", "blockName", "trialnum", "correct", "latency")]
#rename according to what the code need
colnames(Data2)=c("subject", "blocknumber", "trialnumber","correct", "latency")

#merge the four files
Data <- full_join(Data1, Data2, by = c("subject", "blocknumber", "trialnumber","correct", "latency"))


## Regularize trial numbers
table(Data$trialnumber,Data$blocknumber)
#need to fix blocks 2 and 6 
Data<-Data[order(Data$subject,Data$trialnumber),]
Data$trialnumber<- ifelse(Data$blocknumber==3, Data$trialnumber-1, 
                           ifelse(Data$blocknumber==6, Data$trialnumber-1, Data$trialnumber))

table(Data$trialnumber,Data$blocknumber)

## Exclude pps with icomplete IATs (<20+40+20+40=120 trials)
table(Data$subject, Data$blocknumber)
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b1<-b[b$n<120|b$n>120,]
Data<-Data[!(Data$subject %in% b1$number),]
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b # 198 pps (2 excluded)

##### 3. IAT data exclusions #######
## Discard data if fast trials: >10% (faster than 300 ms: more than 12)
Dataex<-Data[Data$latency<300,]
ar<-table(Dataex$subject)
c1<-as.data.frame(ar)
c2<-c1[c1$Freq>12,]
Data<-Data[!(Data$subject %in% c2$Var1),]
describe(Data)  # 189 pps ! fast trial excluded pps: 9

## Run script to calculate D2 IAT score for IAT1
VAR=c(1,2,3,4,5)
exclude=c()
lower.bound=0
Data<- Data[order(Data$subject),]
summary(Data)
source("C:\\maarten_IATscript.txt")
out <- IAT(Data, VAR, exclude, lower.bound)
summary(out)

## Check plot
plot(out, d="D2")
plot(out$score$D2)

## Calculate D2 score
DataD2<- NA
DataD2<- out$score$Subject
DataD2<-cbind.data.frame(DataD2, out$score$D2)
colnames(DataD2)=c("subject", "valIAT")
head(DataD2)

## Calculate D2 score for odd-numbered trials
Datao<- Data[Data$trialnumber%%2==1,]
table(Datao$trialnumber)
outo<-IAT(Datao,VAR,exclude,lower.bound)
summary(outo)
DataD2o<- NA
DataD2o<- outo$score$Subject
DataD2o<-cbind.data.frame(DataD2o, outo$score$D2)
colnames(DataD2o)=c("subject", "valIATodd")

## Calculate D2 score for even-numbered trials
Datae<- Data[Data$trialnumber%%2==0,]
table(Datae$trialnumber)
oute<-IAT(Datae,VAR,exclude,lower.bound)
summary(oute)
DataD2e<- NA
DataD2e<- oute$score$Subject
DataD2e<-cbind.data.frame(DataD2e, oute$score$D2)
colnames(DataD2e)=c("subject", "valIATeven")

## Calculate reliability
cor.test(DataD2e$valIATeven, DataD2o$valIATodd) #r=0.71

#save togther and with session_id
valIATall <- merge(DataD2, DataD2o, by='subject', all=T)
valIATall <- merge(valIATall, DataD2e, by='subject', all=T)

write.csv(valIATall,file=paste(out1, "valIAT.csv", sep="\\"))


#code block order iat valence
iatv1 <- filter(toiat1,(blocknum==1 & trialnum==2))
iatv1$block_order <- "compf"
iatv1 <- iatv1[, c("subject", "block_order")] 

iatv2 <- filter(toiat2,(blocknum==1 & trialnum==2))
iatv2$block_order <- "incompf"
iatv2 <- iatv2[, c("subject", "block_order")] 


#merge the four files
iatvord <- full_join(iatv1, iatv2, by = c("subject", "block_order"))
write.csv(iatvord,file=paste(out1, "iatvord.csv", sep="\\"))


###########################################################
###########################################################
##IAT stereotypes
#I will recode the IAT such that a positive score indicate - Niff + warm
###########################################################
###########################################################

###########################################################
#w_g_c_b
###########################################################
path <- paste(dir, "iat_w_n_c_l_19_12_04.csv", sep="\\")
toiats1 <- read.csv(path)
head(toiats1)
#Use Martan script
#creacte a data set that it need after combining the four data sets 
#keep only relavent trials
iatsRaw1 <- subset(toiats1, blocknum %in% c(3, 5, 7, 9))
#Create blockName based on the pairing condition.
iatsRaw1$blockName[iatsRaw1$blocknum == 3] <- "3"
iatsRaw1$blockName[iatsRaw1$blocknum == 5] <- "4"
iatsRaw1$blockName[iatsRaw1$blocknum == 7] <- "6"
iatsRaw1$blockName[iatsRaw1$blocknum == 9] <- "7"

table(iatsRaw1$blockName)
length(unique(iatsRaw1$subject))

#keep only what we need 
Data1s <- iatsRaw1[,c("subject", "blockName", "trialnum", "correct", "latency")]
#rename according to what the code need
colnames(Data1s)=c("subject", "blocknumber", "trialnumber","correct", "latency")

##########################################################
#iat_c_g_w_b
###########################################################
path <- paste(dir, "iat_c_n_w_l_19_12_04.csv", sep="\\")
toiats2 <- read.csv(path)
head(toiats2)

#keep only relavent trials
iatsRaw2 <- subset(toiats2, blocknum %in% c(3, 5, 7, 9))
#Create blockName based on the pairing condition.
iatsRaw2$blockName[iatsRaw2$blocknum == 3] <- "6"
iatsRaw2$blockName[iatsRaw2$blocknum == 5] <- "7"
iatsRaw2$blockName[iatsRaw2$blocknum == 7] <- "3"
iatsRaw2$blockName[iatsRaw2$blocknum == 9] <- "4"

table(iatsRaw2$blockName)
length(unique(iatsRaw2$subject))

#keep only what we need 
Data2s <- iatsRaw2[,c("subject", "blockName", "trialnum", "correct", "latency")]
#rename according to what the code need
colnames(Data2s)=c("subject", "blocknumber", "trialnumber","correct", "latency")

#merge the four files
Datas <- full_join(Data1s, Data2s, by = c("subject", "blocknumber", "trialnumber","correct", "latency"))


## Regularize trial numbers
table(Datas$trialnumber,Datas$blocknumber)
#need to fix blocks 2 and 6 
Datas<-Datas[order(Datas$subject,Datas$trialnumber),]
Datas$trialnumber<- ifelse(Datas$blocknumber==3, Datas$trialnumber-1, 
                          ifelse(Datas$blocknumber==6, Datas$trialnumber-1, Datas$trialnumber))

table(Datas$trialnumber,Datas$blocknumber)

## Exclude pps with icomplete IATs (<20+40+20+40=120 trials)
table(Datas$subject, Datas$blocknumber)
as <- rle(sort(Datas$subject))
bs <- data.frame(number=as$values, n=as$lengths)
bs1<-bs[bs$n<120|bs$n>120,]
Datas<-Datas[!(Datas$subject %in% bs1$number),]
as <- rle(sort(Datas$subject))
bs <- data.frame(number=as$values, n=as$lengths)
bs # 200 pps 

##### 3. IAT data exclusions #######
## Discard data if fast trials: >10% (faster than 300 ms: more than 12)
Datasex<-Datas[Datas$latency<300,]
ars<-table(Datasex$subject)
c1s<-as.data.frame(ars)
c2s<-c1s[c1s$Freq>12,]
Datas<-Datas[!(Datas$subject %in% c2s$Var1),]
describe(Datas)  # 191 pps ! fast trial excluded pps: 9

## Run script to calculate D2 IAT score for IAT1
VAR=c(1,2,3,4,5)
exclude=c()
lower.bound=0
Datas<- Datas[order(Datas$subject),]
summary(Datas)
source("C:\\maarten_IATscript.txt")
outs <- IAT(Datas, VAR, exclude, lower.bound)
summary(outs)

## Check plot
plot(outs, d="D2")
plot(outs$score$D2)

## Calculate D2 score
DatasD2<- NA
DatasD2<- outs$score$Subject
DatasD2<-cbind.data.frame(DatasD2, outs$score$D2)
colnames(DatasD2)=c("subject", "steIAT")
head(DatasD2)

## Calculate D2 score for odd-numbered trials
Dataso<- Datas[Datas$trialnumber%%2==1,]
table(Dataso$trialnumber)
outos<-IAT(Dataso,VAR,exclude,lower.bound)
summary(outos)
DatasD2o<- NA
DatasD2o<- outos$score$Subject
DatasD2o<-cbind.data.frame(DatasD2o, outos$score$D2)
colnames(DatasD2o)=c("subject", "steIATodd")

## Calculate D2 score for even-numbered trials
Datase<- Datas[Datas$trialnumber%%2==0,]
table(Datase$trialnumber)
outes<-IAT(Datase,VAR,exclude,lower.bound)
summary(outes)
DatasD2e<- NA
DatasD2e<- outes$score$Subject
DatasD2e<-cbind.data.frame(DatasD2e, outes$score$D2)
colnames(DatasD2e)=c("subject", "steIATeven")

## Calculate reliability
cor.test(DatasD2e$steIATeven, DatasD2o$steIATodd) #r=0.79

#save togther and with session_id
steIATall <- merge(DatasD2, DatasD2o, by='subject', all=T)
steIATall <- merge(steIATall, DatasD2e, by='subject', all=T)

write.csv(steIATall,file=paste(out1, "steIAT.csv", sep="\\"))


#code block order iat valence
iats1 <- filter(toiats1,(blocknum==1 & trialnum==2))
iats1$sblock_order <- "compf"
iats1 <- iats1[, c("subject", "sblock_order")] 

iats2 <- filter(toiats2,(blocknum==1 & trialnum==2))
iats2$sblock_order <- "incompf"
iats2 <- iats2[, c("subject", "sblock_order")] 


#merge the four files
iatsord <- full_join(iats1, iats2, by = c("subject", "sblock_order"))
write.csv(iatsord,file=paste(out1, "iatsord.csv", sep="\\"))



#merge all files
#We read the datasets again from the file.
demos <- read.csv(paste(out1, "demos.csv", sep="\\"))
val <-read.csv(paste(out1, "val.csv", sep="\\"))
steIAT <-read.csv(paste(out1, "steIAT.csv", sep="\\"))
valIAT <-read.csv(paste(out1, "valIAT.csv", sep="\\"))
iatsord <-read.csv(paste(out1, "iatsord.csv", sep="\\"))
iatvord <-read.csv(paste(out1, "iatvord.csv", sep="\\"))
expst <-read.csv(paste(out1, "expst.csv", sep="\\"))
cond <-read.csv(paste(out1, "cond.csv", sep="\\"))
mem <-read.csv(paste(out1, "mem.csv", sep="\\"))

allds <- merge(demos, val, by='subject', all=T)
allds <- merge(allds, steIAT, by='subject', all=T)
allds <- merge(allds, valIAT, by='subject', all=T)
allds <- merge(allds, iatsord, by='subject', all=T)
allds <- merge(allds, iatvord, by='subject', all=T)
allds <- merge(allds, expst, by='subject', all=T)
allds <- merge(allds, cond, by='subject', all=T)
allds <- merge(allds, mem, by='subject', all=T)

#recode vars acording to condition
names(allds)



#who to exclude?
#exclude my tries
allds$ok <- ifelse(allds$subject < 2,0, 1)
##finished the study (has valExplicit)
allds$finish <- rowSums(!is.na(allds['valExplicit']))
table(allds$finish)
allds$finishok <- ifelse(allds$finish==1, T, F)

#complete all explicit measures 

allds$nrspexp <- rowSums(!is.na(allds[,c('stExplicit', 'valExplicit')]))
table(allds$nrspexp)
allds$expok <- ifelse(allds$nrspexp==2, T, F)

#IATs ok

allds$nrspiat <- rowSums(!is.na(allds[,c('steIAT', "valIAT")]))
table(allds$nrspiat)
allds$iatok <- ifelse(allds$nrspiat==2, T, F)

#how many
table(allds$ok)#213 started the study
allds1 <- filter(allds, ok ==1)
table(allds1$finishok)#197 completed the study
allds2 <- filter(allds1, finishok ==T)
table(allds2$expok)#197 answers all measures 
allds3 <- filter(allds2, expok ==T)
table(allds3$iatok)#185 IATs ok
allok <- filter(allds3, iatok ==T)

#save this file

write.csv(allok,file=paste(out1, "allok.csv", sep="\\"))
