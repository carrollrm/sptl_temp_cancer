library(tidyr)
library(stringr)
library(tidyverse)
setwd("C:/Users/twili/OneDrive/Documents/Classes/MAT 596")
################################
######## Data Cleaning #########
################################

#### reading in the data ####
#multimodels
Sbrcacrcaprca = read.csv("code/Results/modelsbrcacrcaprca.csv", header = T)
Sbrcalbca = read.csv("code/Results/modelsbrcalbca.csv", header = T)
STbrcacrcaprca = read.csv("code/Results/modelstbrcacrcaprca.csv", header = T)
STbrcalbca =read.csv("code/Results/modelstbrcalbca.csv", header = T)

#univariate spatial modlels
Sbrcacrcaprcauniv = read.csv("univarResults/modelsbrcacrcaprcauniv.csv", header = T)
Sbrcalbcauniv = read.csv("univarResults/modelsbrcalbcauniv.csv", header = T)
#univariate spatial temporal models
STbrcacrcaprcauniv = read.csv("univarResults/modelstbrcacrcaprcauniv.csv", header = T)
STbrcalbcauniv = read.csv("univarResults/modelstbrcalbcauniv.csv", header = T)

#changepoint models
STCPbrcacrcaprca = read.csv("code/Results/modelSTCPbrcacrcaprca.csv", header = T)
#STCP2brcacrcaprca = read.csv("code/Results/modelSTCP2brcacrcaprca.csv", header = T)
STCPbrcalbca = read.csv("code/Results/modelSTCPbrcalbca.csv", header = T)
STCP2brcalbca = read.csv("code/Results/modelST2CPbrcalbca.csv", header = T)

####mu and gam values for 3 cancer models####

#create a function to extract variables
#start = start of string (1), stop = end of string (length of string)
#string = string you want to match
clean = function(dataset, string, start, stop){
  match = substring(as.character(dataset$X), start, stop)
  new = dataset %>% filter(str_detect(match, string))
  return(new[,2])
}

#Sbrcacrcaprca model (Spatial)
U_sm3 = clean(Sbrcacrcaprca, "u", 1, 1)
#STbrcacrcaprca model (Spatial Temporal)
U_stm3 = clean(STbrcacrcaprca, "u", 1, 1)
gam1_stm3 = c(0,clean(STbrcacrcaprca, "gam1", 1, 4))
gam2_stm3 = c(0,clean(STbrcacrcaprca, "gam2", 1, 4))
gam3_stm3 = c(0,clean(STbrcacrcaprca, "gam3", 1, 4))
#sbrcacrcaprcauniv (univariate Spatial)
U1_sunivm3 = clean(Sbrcacrcaprcauniv, "u", 1, 1)
U2_sunivm3 = clean(Sbrcacrcaprcauniv, "u2", 1, 2)
U3_sunivm3 = clean(Sbrcacrcaprcauniv, "u3", 1, 2)
#stbrcacrcaprcauniv (Univariate Spatial Temporal)
U1_stunivm3 = clean(STbrcacrcaprcauniv, "u", 1, 1)
U2_stunivm3 = clean(STbrcacrcaprcauniv, "u2", 1, 2)
U3_stunivm3 = clean(STbrcacrcaprcauniv, "u3", 1, 2)
gam1_stunivm3 = c(0,clean(STbrcacrcaprcauniv, "gam1", 1, 4))
gam2_stunivm3 = c(0,clean(STbrcacrcaprcauniv, "gam2", 1, 4))
gam3_stunivm3 = c(0,clean(STbrcacrcaprcauniv, "gam3", 1, 4))
#STCPbrcacrcaprca model (Spatial Temporal Change point)
U_stcpm3 = clean(STCPbrcacrcaprca, "u", 1, 1)
gamA1_stcpm3 = c(0,clean(STCPbrcacrcaprca, "gamA1", 1, 5))
gamA2_stcpm3 = c(0,clean(STCPbrcacrcaprca, "gamA2", 1, 5))
gamA3_stcpm3 = c(0,clean(STCPbrcacrcaprca, "gamA3", 1, 5))
gamB1_stcpm3 = clean(STCPbrcacrcaprca, "gamB1", 1, 5)
gamB3_stcpm3 = clean(STCPbrcacrcaprca, "gamB3", 1, 5)
cp1_stcpm3 = clean(STCPbrcacrcaprca, "cp1", 1, 3)
cp3_stcpm3 = clean(STCPbrcacrcaprca, "cp3", 1, 3)

#### mu and gam values for 2 cancer models ####
#Sbrcalbca (Spatial)
U_sm2 = clean(Sbrcalbca, "u", 1, 1)
#STbrcalbca (Spatial Temporal)
U_stm2 = clean(STbrcalbca, "u", 1, 1)
gam1_stm2 = c(0,clean(STbrcalbca, "gam1", 1, 4))
gam2_stm2 = c(0,clean(STbrcalbca, "gam2", 1, 4))
#Sbrcalbcauniv (univariate spatial)
U1_sunivm2 = clean(Sbrcalbcauniv, "u", 1, 1)
U2_sunivm2 = clean(Sbrcalbcauniv, "u", 1, 1)
#stbrcalbcauniv (univaritate spaital temporal)
U1_stunivm2 = clean(STbrcalbcauniv, "u", 1, 1)
U2_stunivm2 = clean(STbrcalbcauniv, "u", 1, 1)
gam1_stunivm2 = c(0,clean(STbrcalbcauniv, "gam1", 1, 4))
gam2_stunivm2 = c(0,clean(STbrcalbcauniv, "gam2", 1, 4))
#STCPbrcalbprca model (Spatial Temporal Change point)
U_stcpm2 = clean(STCPbrcalbca, "u", 1, 1)
gamA1_stcpm2 = c(0,clean(STCPbrcalbca, "gamA1", 1, 5))
gamA2_stcpm2 = c(0,clean(STCPbrcalbca, "gamA2", 1, 5))
gamB1_stcpm2 = clean(STCPbrcalbca, "gamB1", 1, 5)
gamB2_stcpm2 = clean(STCPbrcalbca, "gamB2", 1, 5)
cp1_stcpm2 =clean(STCPbrcalbca, "cp1", 1, 3)
cp2_stcpm2 =clean(STCPbrcalbca, "cp2", 1, 3)
#STCP2brcalbca model (Spatial Temporal 2 Change point)
U_stcp2m2 = clean(STCP2brcalbca, "u", 1, 1)
gamA1_stcp2m2 = c(0,clean(STCP2brcalbca, "gamA1", 1, 5))
gamA2_stcp2m2 = c(0,clean(STCP2brcalbca, "gamA2", 1, 5))
gamB1_stcp2m2 = clean(STCP2brcalbca, "gamB1", 1, 5)
gamB2_stcp2m2 = clean(STCP2brcalbca, "gamB2", 1, 5)
cpBr1_stcp2m2 = clean(STCP2brcalbca, "cpA1", 1, 5)
cpBr2_stcp2m2 = clean(STCP2brcalbca, "cpA2", 1, 5)
cpLb1_stcp2m2 = clean(STCP2brcalbca, "cpB1", 1, 5)
cpLb2_stcp2m2 = clean(STCP2brcalbca, "cpB2", 1, 5)

#### Collect DIC table values ####
#DIC for 2 cancer models 
DIC_sm2 = data.frame(tail(Sbrcalbca,3)[, 1:4],rep("Spatial", 3))
colnames(DIC_sm2) = c("Cancer", "DIC", "PD", "D-Bar", "model")
DIC_stm2 = data.frame(tail(STbrcalbca,3)[, 1:4], rep("Spatial Temporal", 3))
colnames(DIC_stm2) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_sunivm2 = data.frame(tail(Sbrcalbcauniv,3)[, 1:4],rep("Spatial Univ", 3))
DIC_sunivm2[,1] = as.factor(c("BrCa", "LbCa", "Total"))
colnames(DIC_sunivm2) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_stunivm2 = data.frame(tail(STbrcalbcauniv,3)[, 1:4],rep("Spatial Temporal Univ", 3))
colnames(DIC_stunivm2) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_stcpm2 = data.frame(tail(STCPbrcalbca,3)[, 1:4],rep("Spatial Temporal Change Point", 3))
colnames(DIC_stcpm2) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_stcp2m2 = data.frame(tail(STCP2brcalbca,3)[, 1:4],rep("Spatial Temporal 2 Change Points", 3))
colnames(DIC_stcp2m2) = c("Cancer", "DIC", "PD", "D-Bar","model")

DIC_m2 = rbind(DIC_sm2, DIC_stm2, DIC_sunivm2, DIC_stunivm2, DIC_stcpm2, DIC_stcp2m2)

cancers = as.factor(c("BrCa","LbCa", "Total"))
DIC_m2 = split(DIC_m2, cancers)
DIC_m2byca = rbind(DIC_m2$BrCa, DIC_m2$LbCa, DIC_m2$Total)

#write.csv(DIC_m2byca, file = "code/Results/2cancerDIC.csv")

#DIC for 3 cancer models
DIC_sm3 = data.frame(tail(Sbrcacrcaprca,4)[, 1:4],rep("Spatial", 4))
colnames(DIC_sm3) = c("Cancer", "DIC", "PD", "D-Bar", "model")
DIC_stm3 = data.frame(tail(STbrcacrcaprca,4)[, 1:4], rep("Spatial Temporal", 4))
colnames(DIC_stm3) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_sunivm3 = data.frame(tail(Sbrcacrcaprcauniv,4)[, 1:4],rep("Spatial Univ", 4))
DIC_sunivm3[,1] = as.factor(c("BrCa", "CrCa", "PrCa", "Total"))
colnames(DIC_sunivm3) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_stunivm3 = data.frame(tail(STbrcacrcaprcauniv,4)[, 1:4],rep("Spatial Temporal Univ", 4))
colnames(DIC_stunivm3) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_stcpm3 = data.frame(tail(STCPbrcacrcaprca,4)[, 1:4],rep("Spatial Temporal Change Point", 4))
colnames(DIC_stcpm3) = c("Cancer", "DIC", "PD", "D-Bar","model")
DIC_m3 = rbind(DIC_sm3, DIC_stm3, DIC_sunivm3, DIC_stunivm3, DIC_stcpm3)

cancers = as.factor(c("BrCa","CrCa","PrCa", "Total"))
DIC_m3 = split(DIC_m3, cancers)
DIC_m3byca = rbind(DIC_m3$BrCa, DIC_m3$CrCa, DIC_m3$PrCa, DIC_m3$Total)

#write.csv(DIC_m3byca, file = "code/Results/3cancerDIC.csv")




#### Plots ####
#### mapping ####
library(devtools) 
#install_github("carrollrm/fillmap")
library(fillmap)

#spatial plots
library(maptools)
LAmap=readShapePoly("data/GISinforLA/LAcounty.shp") 
setwd("~/Classes/MAT 596/code/Results/plots")
png("LAmap.png")
plot(LAmap,axes=T)
dev.off()

plot(LAmap,axes=T)

#library(corrplot)
# smat=cbind(U1_m2univ, U2_m2univ)
# cormat=round(cor(smat),2)
# cormatqa
# colnames(cormat)=rownames(cormat)=c('Spatial Univ 1', 'Spatial Univ 2')
# corrplot(cormat,method="color",tl.col="black",addCoef.col = "black")



#create a mapping function
mapping = function(u_set, name){
  u_set=u_set-mean(u_set)
  interv = length(u_set)/4
  cuts = c(min(u_set), sort(u_set)[interv], sort(u_set)[interv*2], 
           sort(u_set)[interv*3], max(u_set))
  fillmap(LAmap,name,u_set,n.col=4,main.line=-4,main.cex=1.5,leg.cex=1.2,
          leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")
}

#county maps for BrLB
png("SpatialBrLB.png", width = 800)
par(mfrow=c(2,4),mar=c(0,0,0,0))
mapping(U1_sunivm2, "Spatial Univariate \n Breast")
mapping(U2_sunivm2, "Spatial Univariate \n Lung and Bronchus")
mapping(U1_stunivm2, "Spatial-Temp Univariate \n Breast")
mapping(U2_stunivm2, "Spatial-Temp Univariate \n Lung and Bronchus")
mapping(U_sm2, "Multivariate Spatial \n Breast and Lung and Bronchus")
mapping(U_stm2, "Multivariate Spatial-Temp \n Breast and Lung and Bronchus")
mapping(U_stcpm2, "Multivariate Spatial-Temp 1 CP \n Breast and Lung and Bronchus")
mapping(U_stcp2m2, "Multivariate Spatial-Temp 2 CP \n Breast and Lung and Bronchus")
dev.off()
#county maps for BrCrPr
png("SpatialBrCrPr.png",height = 900, width = 800)
par(mfrow=c(3,3),mar=c(0,0,0,0))
mapping(U1_sunivm3, "Spatial Univariate \n Breast")
mapping(U2_sunivm3, "Spatial Univariate \n Colorectal")
mapping(U3_sunivm3, "Spatial Univariate \n Prostate")
mapping(U1_stunivm3, "Spatial-Temp Univariate \n Breast")
mapping(U2_stunivm3, "Spatial-Temp Univariate \n Colorectal")
mapping(U3_stunivm3, "Spatial-Temp Univariate \n Prostate")
mapping(U_sm3, "Multiariate Spatial \n Breast, Colorectal, Prostate")
mapping(U_stm3, "Multiariate Spatial-Temp \n Breast, Colorectal, Prostate")
mapping(U_stcpm3, "Multivariate Spatial-Temp 1 CP \n Breast, Colorectal, Prostate")
dev.off()



####temporal overall from best line plots####
#### two cancer models ####
# 2 cancer spatio-temporal univarite effect #
png("UniTempBrLB.png")
plot(2000:2014,gam1_stunivm2[c(1:14,14)],type="s", ylab="Temporal Random Effect",xlab="Diagnosis Year",
     main = "Univariate Temporal Effect for  BrCaLBCa",ylim=c(-1,2.25))
lines(2000:2014,gam2_stunivm2[c(1:14,14)],type="s",col = "darkorange")
legend("bottomright", legend = c("BrCa", "LbCa"), lty = c(1,1),col = c(1, "darkorange"), bty = 'n')
dev.off()

# 2 cancer spatio-temporal effect #
png("tempBrLB.png")
plot(2000:2014,gam1_stm2[c(1:14,14)],type="s", ylab="Temporal Random Effect",xlab="Diagnosis Year",
     main = "Multivariate Temporal Effect for BrCaLBCa",ylim=c(-1,2.25))
lines(2000:2014,gam2_stm2[c(1:14,14)],type="s",col = "darkorange")
legend("bottomright", legend = c("BrCa", "LbCa"), lty = c(1,1),col = c(1, "darkorange"), bty = 'n')
dev.off()

# 2 cancer single cp 1 #
png("tempBrLB1cp.png")
##breast
cp1_stcpm2YR=trunc(cp1_stcpm2/12)+2000
cp1_stcpm2MNTH=trunc(cp1_stcpm2-(cp1_stcpm2YR-2000)*12)
plot(c(2000:cp1_stcpm2YR,cp1_stcpm2YR+cp1_stcpm2MNTH/12,(cp1_stcpm2YR+1):2014),
     gamA1_stcpm2[c(1:(cp1_stcpm2YR-1999),(cp1_stcpm2YR-1999):14,14)]+
       c(rep(0,cp1_stcpm2YR-1999), gamB1_stcpm2[rep(1,16-(cp1_stcpm2YR-1999))]),
     ylim=c(-1,2.25),#lwd=3,
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year",
     main = "Multivariate Temporal Effect for BrCaLBCa with 1 CP.")
#temporal
lines(2000:2014,gamA1_stcpm2[c(1:14,14)],type="s",lty=2)#,lwd=3)
#event effect
lines(c(2000,cp1_stcpm2YR+cp1_stcpm2MNTH/12,2014),
      c(0,gamB1_stcpm2,gamB1_stcpm2),type="s",lty=4)

##lung and bronchus
cp2_stcpm2YR=trunc(cp2_stcpm2/12)+2000
cp2_stcpm2MNTH=trunc(cp2_stcpm2-(cp2_stcpm2YR-2000)*12)
lines(c(2000:cp2_stcpm2YR,cp2_stcpm2YR+cp2_stcpm2MNTH/12,(cp2_stcpm2YR+1):2014),
      gamA2_stcpm2[c(1:(cp2_stcpm2YR-1999),(cp2_stcpm2YR-1999):14,14)]+
        c(rep(0,cp2_stcpm2YR-1999), gamB2_stcpm2[rep(1,16-(cp2_stcpm2YR-1999))]),
      col = "darkorange",type="s")#,lwd=3)
#temporal
lines(2000:2014,gamA2_stcpm2[c(1:14,14)],type="s",lty=2, col = "darkorange")#,lwd=3)
#event effect
lines(c(2000,cp2_stcpm2YR+cp2_stcpm2MNTH/12,2014),
      c(0,gamB2_stcpm2,gamB2_stcpm2),type="s",lty=4,col="darkorange")
legend("bottomleft",legend=c("BrCa Overall", "BrCa Temporal", "BrCa Event Effect", 
                              "LbCa Overall", "LbCa Temporal", "LbCa Event Effect"),
        lty=c(1,2,4,1,2,4),col=c(1,1,1,"darkorange","darkorange", "darkorange"),bty='n',cex=.6)#,lwd=3)

dev.off()
#just event effects
plot(c(2000,cp2_stcpm2YR+cp2_stcpm2MNTH/12,2014),
     c(0,gamB2_stcpm2,gamB2_stcpm2),type="s",lty=4,col="darkorange", 
     main = "Event Effects for Breast and Lung and Bronchus Cancer",
     ylim=c(-1,0.5),#lwd=3,
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(c(2000,cp1_stcpm2YR+cp1_stcpm2MNTH/12,2014),
      c(0,gamB1_stcpm2,gamB1_stcpm2),type="s",lty=4)
legend("bottomleft", legend = c("BrCa", "LbCa"), lty = c(4, 4), col = c(1, "darkorange"), bty = 'n')

#2 cancer double cp 2 #
png("tempBrLB2cp.png")
##breast 
cpBr1_stcp2m2YR=trunc(cpBr1_stcp2m2/12)+2000
cpBr1_stcp2m2MNTH=trunc(cpBr1_stcp2m2-(cpBr1_stcp2m2YR-2000)*12)
cpBr2_stcp2m2YR=trunc(cpBr2_stcp2m2/12)+2000
cpBr2_stcp2m2MNTH=trunc(cpBr2_stcp2m2-(cpBr2_stcp2m2YR-2000)*12)
# overall temporal
plot(c(2000:cpBr1_stcp2m2YR,cpBr1_stcp2m2YR+cpBr1_stcp2m2MNTH/12,
        (cpBr1_stcp2m2YR+1):cpBr2_stcp2m2YR,cpBr2_stcp2m2YR+cpBr2_stcp2m2MNTH/12,
        (cpBr2_stcp2m2YR+1):2014),
      gamA1_stcp2m2[c(1:(cpBr1_stcp2m2YR-1999),(cpBr1_stcp2m2YR-1999):(cpBr2_stcp2m2YR-1999), (cpBr2_stcp2m2YR-1999):14,14)]+
        c(rep(0,cpBr1_stcp2m2YR-1999),rep(gamB1_stcp2m2,(cpBr2_stcp2m2YR-cpBr1_stcp2m2YR+1)), rep(0, 2014-cpBr2_stcp2m2YR+1)),
      cex.lab=1,cex.axis=1.2,ylim=c(-1,2.25),#lwd=3, 
     main = 'Multivariate Temporal Effect for BrCaLBCa with 2 CP',
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year", col = 1)
#temporal
lines(2000:2014,gamA1_stcp2m2[c(1:14,14)],type="s",lty=2,col= 1)
# event effect
lines(c(2000,cpBr1_stcp2m2YR+cpBr1_stcp2m2MNTH/12,cpBr2_stcp2m2YR+cpBr2_stcp2m2MNTH/12,2014),
     c(0,gamB1_stcp2m2,0,0),type="s", lty = 4, col=1)

##lung and Bronchus 
par(mfrow=c(1,1),mar=c(5,5,2,2))
cpLb1_stcp2m2YR=trunc(cpLb1_stcp2m2/12)+2000
cpLb1_stcp2m2MNTH=trunc(cpLb1_stcp2m2-(cpLb1_stcp2m2YR-2000)*12)
cpLb2_stcp2m2YR=trunc(cpLb2_stcp2m2/12)+2000
cpLb2_stcp2m2MNTH=trunc(cpLb2_stcp2m2-(cpLb2_stcp2m2YR-2000)*12)
# overall temporal
lines(c(2000:cpLb1_stcp2m2YR,cpLb1_stcp2m2YR+cpLb1_stcp2m2MNTH/12,
       (cpLb1_stcp2m2YR+1):cpLb2_stcp2m2YR,cpLb2_stcp2m2YR+cpLb2_stcp2m2MNTH/12,
       (cpLb2_stcp2m2YR+1):2014),
     gamA2_stcp2m2[c(1:(cpLb1_stcp2m2YR-1999),(cpLb1_stcp2m2YR-1999):(cpLb2_stcp2m2YR-1999), (cpLb2_stcp2m2YR-1999):14,14)]+
       c(rep(0,cpLb1_stcp2m2YR-1999),rep(gamB2_stcp2m2,(cpLb2_stcp2m2YR-cpLb1_stcp2m2YR+1)), rep(0, 2014-cpLb2_stcp2m2YR+1)),
     type="s", col = "darkorange")
     #cex.lab=1,cex.axis=1.2,ylim=range(-0.25: 2.25),#lwd=3,ylab="Temporal Random Effect",xlab="Diagnosis Year",
     
#temporal
lines(2000:2014,gamA2_stcp2m2[c(1:14,14)],type="s",lty=2,col= "darkorange")
# event effect
lines(c(2000,cpLb1_stcp2m2YR+cpLb1_stcp2m2MNTH/12,cpLb2_stcp2m2YR+cpLb2_stcp2m2MNTH/12,2014),
      c(0,gamB2_stcp2m2,0,0),type="s",lty = 4, col="darkorange")

legend("bottomleft",legend=c("BrCa Overall", "BrCa Temporal", "BrCa Event Effect", 
                             "LbCa Overall", "LbCa Temporal", "LbCa Event Effect"),
       lty=c(1,2,4,1,2,4),col=c(1,1,1,"darkorange","darkorange", "darkorange"),bty='n',cex=.6)
dev.off()

#just event effect
plot(c(2000,cpBr1_stcp2m2YR+cpBr1_stcp2m2MNTH/12,cpBr2_stcp2m2YR+cpBr2_stcp2m2MNTH/12,2014),
      c(0,gamB1_stcp2m2,0,0),type="s", col=1,
      main = "Event Effects for Breast and Lung and Bronchus Cancer", ylim=c(-0.5,0.5),#lwd=3,
ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(c(2000,cpLb1_stcp2m2YR+cpLb1_stcp2m2MNTH/12,cpLb2_stcp2m2YR+cpLb2_stcp2m2MNTH/12,2014),
      c(0,gamB2_stcp2m2,0,0),type="s", col="darkorange")

#### 3 cancer models ####
# 3 cancer spatio-temporal univarite effect #
png("UniTempBrCrPr.png")
plot(2000:2014,gam3_stunivm3[c(1:14,14)],type="s", ylab="Temporal Random Effect",xlab="Diagnosis Year",
     main = "Univariate Temporal Effect for BrCaCrCaPrCa",
     ylim = c(-1,2.25), col = "forestgreen")
lines(2000:2014,gam2_stunivm3[c(1:14,14)],type="s",col = "cornflowerblue")
lines(2000:2014,gam1_stunivm3[c(1:14,14)],type="s",col = "indianred1")
legend("bottomright", legend = c("BrCa", "CrCa", "PrCa"), lty = c(1,1, 1),
       col = c("indianred1", "cornflowerblue","forestgreen"), bty = 'n')

dev.off()
# 3 cancer spatio-temporal effect #
png("tempBrCrPr.png")
plot(2000:2014,gam3_stm3[c(1:14,14)],type="s", ylab="Temporal Random Effect",xlab="Diagnosis Year",
     main = "Multivariate Temporal Effect for BrCaCrCaPrCa",
     ylim = c(-1,2.25), col = "forestgreen")
lines(2000:2014,gam2_stm3[c(1:14,14)],type="s",col = "cornflowerblue")
lines(2000:2014,gam1_stm3[c(1:14,14)],type="s",col = "indianred1")
legend("bottomright", legend = c("BrCa", "CrCa", "PrCa"), lty = c(1,1),
       col = c("indianred1", "cornflowerblue","forestgreen"), bty = 'n')

dev.off()
# 3 cancer single cp 1 #
##breast
png("tempBrCrPr1cp.png")
cp1_stcpm3YR=trunc(cp1_stcpm3/12)+2000
cp1_stcpm3MNTH=trunc(cp1_stcpm3-(cp1_stcpm3YR-2000)*12)
plot(c(2000:cp1_stcpm3YR,cp1_stcpm3YR+cp1_stcpm3MNTH/12,(cp1_stcpm3YR+1):2014),
     gamA1_stcpm3[c(1:(cp1_stcpm3YR-1999),(cp1_stcpm3YR-1999):14,14)]+
       c(rep(0,cp1_stcpm3YR-1999), gamB1_stcpm3[rep(1,16-(cp1_stcpm3YR-1999))]),
     ylim=c(-1,2.25),col = "indianred1",#lwd=3,
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year",
     main = "Mulitvariate Temporal Effect for BrCaCrCaPrCa with 1 CP")
#temporal
lines(2000:2014,gamA1_stcpm3[c(1:14,14)],type="s",lty=2, col = "indianred1")#,lwd=3)
#event effect
lines(c(2000,cp1_stcpm3YR+cp1_stcpm3MNTH/12,2014),
      c(0,gamB1_stcpm3,gamB1_stcpm3),type="s",lty=4, col = "blueviolet")

##Colorectal shared cp with Breast
lines(c(2000:cp1_stcpm3YR,cp1_stcpm3YR+cp1_stcpm3MNTH/12,(cp1_stcpm3YR+1):2014),
      gamA2_stcpm3[c(1:(cp1_stcpm3YR-1999),(cp1_stcpm3YR-1999):14,14)]+
        c(rep(0,cp1_stcpm3YR-1999), gamB1_stcpm3[rep(1,16-(cp1_stcpm3YR-1999))]),
      col = "cornflowerblue", type = "s")#,lwd=3)
#temporal
lines(2000:2014,gamA2_stcpm3[c(1:14,14)],type="s",lty=2, col = "cornflowerblue")#,lwd=3)


##Prostate
cp3_stcpm3YR=trunc(cp3_stcpm3/12)+2000
cp3_stcpm3MNTH=trunc(cp3_stcpm3-(cp3_stcpm3YR-2000)*12)
lines(c(2000:cp3_stcpm3YR,cp3_stcpm3YR+cp3_stcpm3MNTH/12,(cp3_stcpm3YR+1):2014),
      gamA3_stcpm3[c(1:(cp3_stcpm3YR-1999),(cp3_stcpm3YR-1999):14,14)]+
        c(rep(0,cp3_stcpm3YR-1999), gamB3_stcpm3[rep(1,16-(cp3_stcpm3YR-1999))]),
      col = "forestgreen", type = "s")#,lwd=3)
#temporal
lines(2000:2014,gamA3_stcpm3[c(1:14,14)],type="s",lty=2, col = "forestgreen")#,lwd=3)
#event effect
lines(c(2000,cp3_stcpm3YR+cp3_stcpm3MNTH/12,2014),
      c(0,gamB3_stcpm3,gamB3_stcpm3),type="s",lty=4,col="forestgreen")
legend("bottomleft",legend=c("BrCa","CrCa", "PrCa","Br&CrCa",
                             "Overall", "Temporal", "Event Effect"),
       lty=c(1,1,1,1,2,4),col=c("indianred1","cornflowerblue",
                                "forestgreen","blueviolet",1, 1, 1),bty='n',cex=.6)#,lwd=3)
dev.off() 

#just event effects
plot(c(2000,cp1_stcpm3YR+cp1_stcpm3MNTH/12,2014),
     c(0,gamB1_stcpm3,gamB1_stcpm3),type="s",lty=1,col=2, 
     main = "Change Points for Shared BrCa/CaCr and PrCa",
     ylim=c(-1,0.5),#lwd=3,
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(c(2000,cp3_stcpm3YR+cp3_stcpm3MNTH/12,2014),
      c(0,gamB3_stcpm3,gamB3_stcpm3),type="s",lty=1,col=6)
legend("bottomleft", legend = c("BrCa and CaCr", "PrCa"), lty = c(1,1), col = c(2,6),
       cex = 0.75,bty = 'n')
dev.off()

