
########
### project2
#####
library(plyr)
?ToothGrowth
boxplot(ToothGrowth$len~ToothGrowth$supp,col=c("orange","green"),main="Tooth Length by Supplement",ylab="Tooth Length (mm)",xlab="Supplement (type)")
boxplot(ToothGrowth$len~ToothGrowth$dose,col=c("lightblue","blue","darkblue"),main="Tooth Length by Dose",ylab="Tooth Length (mm)",xlab="Dose (mg)")
boxplot(len ~ interaction(supp,dose), data=ToothGrowth,col=c("yellow","lightgreen","orange","green","salmon","darkgreen"),main="Tooth Length by Dose and Supplement",ylab="Tooth Length (mm)",xlab="Dose (mg) & Supplement (type) Interaction")

summary(ToothGrowth)
mean(ToothGrowth$len)
sd(ToothGrowth$len)
var(ToothGrowth$len)

TGsuppMean=ddply(ToothGrowth,"supp",summarize, avg=mean(len))
TGsuppSD=ddply(ToothGrowth,"supp",summarize, StDev=sd(len))

TGdoseMean=ddply(ToothGrowth,"dose",summarize, avg=mean(len))
TGdoseSD=ddply(ToothGrowth,"dose",summarize, StDev=sd(len))

TGsuppdoseMean=ddply(ToothGrowth,c("supp","dose"),summarize, avg=mean(len))
TGsuppdoseSD=ddply(ToothGrowth,c("supp","dose"),summarize, StDev=sd(len))


# T test for Supplement type
VC=subset(ToothGrowth, supp=="VC")
OJ=subset(ToothGrowth,supp=="OJ")
round(t.test(VC$len,OJ$len)$p.value,4)

# T test for doses
dose0.5=subset(ToothGrowth,dose==0.5)
dose2=subset(ToothGrowth,dose==2.0)
t.test(dose0.5$len,dose2$len)$p.value

dose0.5=subset(ToothGrowth,dose==0.5)
dose1=subset(ToothGrowth,dose==1.0)
t.test(dose0.5$len,dose1$len)

t.test(dose1$len,dose2$len)

# T test for supplement and dose
VCdose.5=subset(ToothGrowth,dose==0.5 & supp=="VC")
OJdose.5=subset(ToothGrowth,dose==0.5 & supp=="OJ")
t.test(VCdose.5$len,OJdose.5$len)

VCdose1=subset(ToothGrowth,dose==1.0 & supp=="VC")
OJdose1=subset(ToothGrowth,dose==1.0 & supp=="OJ")
t.test(VCdose1$len,OJdose1$len)

VCdose2=subset(ToothGrowth,dose==2.0 & supp=="VC")
OJdose2=subset(ToothGrowth,dose==2.0 & supp=="OJ")
t.test(VCdose2$len,OJdose2$len)


####
subdata=ToothGrowth[ToothGrowth$supp %in% c("VC","OJ"),]
y=subdata$len
group=as.character(subdata$supp)
testStat=function(w,g)mean(w[g=="VC"])-mean(w[g=="OJ"])
observedStat=testStat(y,group)
permut=sapply(1:10000,function(i) testStat(y,sample(group)))
observedStat
mean(permut<observedStat)

hist(permut)
abline(v=observedStat)
d=density(permut)
plot(d,col="red",main="Bootstrap Density of Permutations of Length")
polygon(d, col="salmon", border="red") 
abline(v=observedStat)
sd(permut)
quantile(permut,c(.025,.975))


#####

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,ylab="tooth length (mm)",
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
coplot(len ~ supp | dose, data = ToothGrowth, panel = panel.smooth,ylab="tooth length (mm)",
       xlab = "ToothGrowth data: length vs supplement, given dose")

# F-test (ANOVA) for dose
fitDose=lm(ToothGrowth$len~ToothGrowth$dose)
anova(fitDose)
# F-test (ANOVA) for Supplement type
fitSupp=lm(ToothGrowth$len~ToothGrowth$supp)
anova(fitSupp)
# F-test (ANOVA) for Supplement type and dose
fitSuppDose=lm(ToothGrowth$len~ToothGrowth$supp+ToothGrowth$dose)
anova(fitSuppDose)