#upload functions and packeges
source("C:\\yba.funcs.R")

#The data's folder (directory)
dir = 'C:\\all'
# where to save merged files
out1 = 'C:\\out'

allok<- read.csv(paste(out1, "allok.csv", sep="\\"), stringsAsFactors = FALSE)


#####accurate memory sample######
head(allok)
table(allok$mem_w_response)
table(allok$mem_c_response)
with(allok, table(mem_w_response, condition))

table(allok$condition)

allok$warm_ok <- ifelse(allok$condition == 'niff_w' & allok$mem_w_response == "they were paired with Niffites names", 1, 
                        ifelse(allok$condition == 'luup_w' & allok$mem_w_response == "they were paired with Luupites names", 1 , 0))

table(allok$warm_ok)

with(allok, table(mem_c_response, condition))

allok$comp_ok <- ifelse(allok$condition == 'luup_w' & allok$mem_c_response == "they were paired with Niffites names", 1, 
                        ifelse(allok$condition == 'niff_w' & allok$mem_c_response == "they were paired with Luupites names", 1 , 0))



table(allok$comp_ok)

allok$cont_ok <- ifelse(allok$warm_ok == 1 & allok$comp_ok == 1, 1, 0)
table(allok$cont_ok)
with(allok, table(cont_ok, condition))
memok <- filter(allok, cont_ok==1)

write.csv(memok,file=paste(out1, "memok.csv", sep="\\"))

memok<- read.csv(paste(out1, "memok.csv", sep="\\"))
##############
#demo
##############
#N =186
mean(allok$age_response)#32.44
sd(allok$age_response)#11.52
table(allok$sex_response)#51% female 



######################
#ANALYSIS
#################
#internal concictency of the IAT
#stereotypes
r_s <- cor(allok$steIATeven, allok$steIATodd) #r=0.91
(2 * r_s) / (1 + r_s) 
#Evaluative
r_s <- cor(allok$valIATeven, allok$valIATodd) #r=0.81
(2 * r_s) / (1 + r_s) 

#means
mysumBy(steIAT ~ condition, dt = allok, bindTables = T)

#ANOVA
#first scale the valIAT and factor the condition and IAT block order
allok$assignment<-factor(allok$condition)
allok$sIATorder<-factor(allok$sblock_order)
allok$valenceIAT <- scale(allok$valIAT)

imp <- aov_car(steIAT ~ assignment + sIATorder + valenceIAT+ Error(subject),factorize=FALSE, data=allok)
imp

#90% CI for assignment effect
library(MBESS)
ci.pvaf(F.value=18.37, df.1=1, df.2=182, N=186, conf.level=.90)

#does the results look the same without controling for valence?
impno <- aov_car(steIAT ~ assignment + sIATorder+ Error(subject),factorize=FALSE, data=allok)
impno
#yes

####accurate sample
#internal consistency
#stereotypes
r_s <- cor(memok$steIATeven, memok$steIATodd) #r=0.92
(2 * r_s) / (1 + r_s) 
#Evaluative
r_s <- cor(memok$valIATeven, memok$valIATodd) #r=0.77
(2 * r_s) / (1 + r_s) 

#means
mysumBy(steIAT ~ condition, dt = memok, bindTables = T)

#ANOVA
memok$assignment<-factor(memok$condition)
memok$sIATorder<-factor(memok$sblock_order)
memok$valenceIAT <- scale(memok$valIAT)

imp1 <- aov_car(steIAT ~ assignment + sIATorder + valenceIAT+ Error(subject),factorize=FALSE, data=memok)
imp1

#90% CI for assignment effect
ci.pvaf(F.value=33.38, df.1=1, df.2=63, N=67, conf.level=.90)

#does the results look the same without controling for valence?
imp1no <- aov_car(steIAT ~ assignment + sIATorder+ Error(subject),factorize=FALSE, data=memok)
imp1no
#yes

##############
#Self-report
#############
#internal consistency
#stereotyping
cronbach(allok[,c('warm_s1', 'comp_s1')])#0.91
#evaluation
cronbach(allok[,c('Comp_gb', 'Comp_pn')])#0.92
cronbach(allok[,c('Warm_gb', 'Warm_pn')])#0.92


#means
mysumBy(stExplicit1 ~ condition, dt = allok, bindTables = T)

allok$valenceExplicit <- scale(allok$valExplicit)

explicit <- aov_car(stExplicit1 ~ assignment + sIATorder +valenceExplicit + Error(subject), factorize=FALSE, data=allok)
explicit

#90% CI for assignment effect
ci.pvaf(F.value=32.34, df.1=1, df.2=182, N=186, conf.level=.90)

#does the results look the same without controling for valence?
explicitno <- aov_car(stExplicit1 ~ assignment + sIATorder +Error(subject), factorize=FALSE, data=allok)
explicitno
#yes

#accurate sub saple 
#internal consistency
#stereotyping
cronbach(memok[,c('warm_s1', 'comp_s1')])#0.93
#evaluation
cronbach(memok[,c('Comp_gb', 'Comp_pn')])#0.93
cronbach(memok[,c('Warm_gb', 'Warm_pn')])#0.92

mysumBy(stExplicit1 ~ condition, dt = memok, bindTables = T)

memok$valenceExplicit <- scale(memok$valExplicit)

explicit1 <- aov_car(stExplicit1 ~ assignment + sIATorder +valenceExplicit + Error(subject), factorize=FALSE, data=memok)
explicit1

#90% CI for assignment effect
ci.pvaf(F.value=120.79, df.1=1, df.2=63, N=67, conf.level=.90)

#does the results look the same without controling for valence?
explicit1no <- aov_car(stExplicit1 ~ assignment + sIATorder +Error(subject), factorize=FALSE, data=memok)
explicit1no
#yes

#Direct comparison between automatic and self-reported effects
#correlation between implicit an explicit
cor.test(allok$stExplicit, allok$steIAT)

zallok <- allok
zallok$imp <- zallok$steIAT
zallok$exp <- zallok$stExplicit1

zallok$zimp <- (zallok$imp-mean(zallok$imp))/sd(zallok$imp)
zallok$zexp <- (zallok$exp-mean(zallok$exp))/sd(zallok$exp)

mysumBy(zexp + zimp ~ condition, dt = zallok, bindTables = T)

toANOVA  <-  zallok[,c("subject", "condition", "zexp", "zimp")]
ANOVAok <- tidyr::gather(toANOVA , "measure", "rate", 3:4)

ANOVAzscore <- ezANOVA(data=ANOVAok, dv=rate, wid=subject, within=.(measure), between=.(condition), type=3, detailed = T, return_aov=T)
anova_out(ANOVAzscore )

#90% CI for the interaction 
Lims <- conf.limits.ncf(F.value =0.98
                        , conf.level = 0.90, df.1 <- 1, df.2 <- 184)
Lower.lim <- Lims$Lower.Limit/(Lims$Lower.Limit + df.1 + df.2 + 1)
Upper.lim <- Lims$Upper.Limit/(Lims$Upper.Limit + df.1 + df.2 + 1)
Lower.lim
Upper.lim

#effect of condition for self-report meausre
exp <- filter(ANOVAok, measure == "zexp")
ANOVAzexp <- ezANOVA(data=exp, dv=rate, wid=subject, between=.(condition), type=3, detailed = T, return_aov=T)
anova_out(ANOVAzexp)
#effect of condition for the IAT
imp <- filter(ANOVAok, measure == "zimp")
ANOVAzimp <- ezANOVA(data=imp, dv=rate, wid=subject, between=.(condition), type=3, detailed = T, return_aov=T)
anova_out(ANOVAzimp)

#save data set (to compute BF in JASP)
write.csv(zallok,file=paste(out1, "zallok.csv", sep="\\"))



