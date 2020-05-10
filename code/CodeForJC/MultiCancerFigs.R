#lbca
uT1lbca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\lung and bronchus\\modelT1lbca.csv")[2:65,2]  
gamT1lbca=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\lung and bronchus\\modelT1lbca.csv")[c(80:92,94),2])
cpT1lbca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\lung and bronchus\\modelT1lbca.csv")[96,2]
#prostate
uTprca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\Prostate\\modelT1prca.csv")[2:65,2]  
gamTprca=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\Prostate\\modelTprca.csv")[79:91,2])
#breast
uT1brca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\breast\\modelT1brca.csv")[2:65,2]  
gamT1brca=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\breast\\modelT1brca.csv")[c(83:95,97),2])  
cpT1brca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\breast\\modelT1brca.csv")[99,2]
#crc
uTcrc=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\crc\\modelTcrca.csv")[2:65,2]  
gamTcrc=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\crc\\modelTcrca.csv")[81:93,2])
#leuk
uTleuk=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\leukemia\\rerun\\modelTleuk.csv")[2:65,2]  
gamTleuk=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\leukemia\\rerun\\modelTleuk.csv")[74:86,2])
#ovca
uS1ovca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\ovarian\\modelSovR.csv")[2:65,2]
gamT1ovca=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\ovarian\\modelT1ovR.csv")[c(10:22,24),2])
cpT1ovca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\ovarian\\modelT1ovR.csv")[26,2]


#spatial plots
library(maptools)
LAmap=readShapePoly("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\LAcounty.shp")
plot(LAmap,axes=T)

library(corrplot)
smat=cbind(uT1lbca,uTprca,uT1brca,uTcrc,uTleuk,uS1ovca)
cormat=round(cor(smat),2)
cormat
colnames(cormat)=rownames(cormat)=c("LBCa","PrCa","BrCa","CrCa","Leuk","OvCa")
corrplot(cormat,method="color",tl.col="black",addCoef.col = "black")

par(mfrow=c(2,3),mar=c(0,0,0,0))
uT1lbca=uT1lbca-mean(uT1lbca)
cuts=c(min(uT1lbca),sort(uT1lbca)[16],sort(uT1lbca)[32],
       sort(uT1lbca)[48],max(uT1lbca))
fillmap(LAmap,"Lung and Bronchus",uT1lbca,n.col=4,main.line=-1.5,main.cex=1.5,leg.cex=1.2,
        leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")
uTprca=uTprca-mean(uTprca)
cuts=c(min(uTprca),sort(uTprca)[16],sort(uTprca)[32],
       sort(uTprca)[48],max(uTprca))
fillmap(LAmap,"Prostate",uTprca,n.col=4,main.line=-1.5,main.cex=1.5,leg.cex=1.2,
        leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")
uT1brca=uT1brca-mean(uT1brca)
cuts=c(min(uT1brca),sort(uT1brca)[16],sort(uT1brca)[32],
       sort(uT1brca)[48],max(uT1brca))
fillmap(LAmap,"Breast",uT1brca,n.col=4,main.line=-1.5,main.cex=1.5,leg.cex=1.2,
        leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")
uTcrc=uTcrc-mean(uTcrc)
cuts=c(min(uTcrc),sort(uTcrc)[16],sort(uTcrc)[32],
       sort(uTcrc)[48],max(uTcrc))
fillmap(LAmap,"Colorectal",uTcrc,n.col=4,main.line=-1.5,main.cex=1.5,leg.cex=1.2,
        leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")
uTleuk=uTleuk-mean(uTleuk)
cuts=c(min(uTleuk),sort(uTleuk)[16],sort(uTleuk)[32],
       sort(uTleuk)[48],max(uTleuk))
fillmap(LAmap,"Leukemia",uTleuk,n.col=4,main.line=-1.5,main.cex=1.5,leg.cex=1.2,
        leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")
uS1ovca=uS1ovca-mean(uS1ovca)
cuts=c(min(uS1ovca),sort(uS1ovca)[16],sort(uS1ovca)[32],
       sort(uS1ovca)[48],max(uS1ovca))
fillmap(LAmap,"Ovarian",uS1ovca,n.col=4,main.line=-1.5,main.cex=1.5,leg.cex=1.2,
        leg.loc=list(x=-90.9,y=32.9),cuts=cuts,bk="c")

#sec assessment spatial
library(INLA)
xtrVars=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\LAcountyORDER.csv")[,c(5:20,30:34)]
#TS
groc=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\groceries.csv")[,c(16,17,19)]
hospqual=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\HospQualLA.csv")[,4:19]
#ER
plts=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\pollutantsLA.csv")[-65,-c(1,2)]
#spatial plot of spatial risk factors for secondary assessment
names=c("Total Hospitals",
        "ACS Program","Medicare Certification",
        "Health Factors Score","Median Income \n in thousands of dollars",
        "Total Number of Groceries",
        "% African American","% High Education","% Medicaid Eligible",
        "% Urban Population","% Farmland","% Poverty",
        "% Agriculture, Forestry, Fishing,\nHunting, or Mining Work")
xtrVars[,13]=xtrVars[,13]*100
xtrVars[which(is.na(xtrVars[,6])),6]=0
for (i in c(1:6,9:11,13,17)){
  test=assessmap(u=uS1ovca,x=(xtrVars[,i]-mean(xtrVars[,i]))/sd(xtrVars[,i]),
                 graph="C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\LAINLA.txt")
  #if(test[3,2]*test[3,3]>0){
  print(names(xtrVars[,c(i,i)])[1])
  print(test)
  #}
}

#ER - pollutants
for (i in 1:148){
  # i=1
  test=assessmap(u=uS1ovca,x=(plts[,i]-mean(plts[,i]))/sd(plts[,i]),
                 graph="C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\LAINLA.txt")
  if(test[3,2]*test[3,3]>0&test[3,2]<0){
    print(names(plts[,c(i,i)])[1])
    print(test)
    
  }
  print(i)
}

#TS - groceries and quality of healthcare
for (i in 1:3){
  test=assessmap(u=uT1lbca,x=(groc[,i]-mean(groc[,i]))/sd(groc[,i]),graph="C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\LAINLA.txt")
  print(names(groc[,c(i,i)])[1])
  print(test)
}
for (i in c(3,5,9:10,14,16)){
  test=assessmap(u=uS1ovca,x=(hospqual[,i]-mean(hospqual[,i]))/sd(hospqual[,i]),graph="C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Data\\LAINLA.txt")
  print(names(hospqual[,c(i,i)])[1])
  print(test)
}



#temporal overall from best
layout(m=matrix(1:2,nrow=2,ncol=1),heights=c(.9,.1))
par(mar=c(5,5,1,1))
cpT1lbcaYR=trunc(cpT1lbca/12)+2000
cpT1lbcaMNTH=trunc(cpT1lbca-(cpT1lbcaYR-2000)*12)
plot(c(2000:cpT1lbcaYR,cpT1lbcaYR+cpT1lbcaMNTH/12,(cpT1lbcaYR+1):2014),
     gamT1lbca[c(1:(cpT1lbcaYR-1999),(cpT1lbcaYR-1999):14,14)]+
       gamT1lbca[c(rep(1,(cpT1lbcaYR-1999)),rep(15,16-(cpT1lbcaYR-1999)))],
     ylim=c(-1,1.6),#lwd=3,
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(2000:2014,gamTprca[c(1:14,14)],type="s",lty=2)#,lwd=3)
cpT1brcaYR=trunc(cpT1brca/12)+2000
cpT1brcaMNTH=trunc(cpT1brca-(cpT1brcaYR-2000)*12)
lines(c(2000:cpT1brcaYR,cpT1brcaYR+cpT1brcaMNTH/12,(cpT1brcaYR+1):2014),
     gamT1brca[c(1:(cpT1brcaYR-1999),(cpT1brcaYR-1999):14,14)]+
       gamT1brca[c(rep(1,(cpT1brcaYR-1999)),rep(15,16-(cpT1brcaYR-1999)))],
     lty=3,type="s")#,lwd=3)
lines(2000:2014,gamTcrc[c(1:14,14)],type="s",lty=4)#,lwd=3)
lines(2000:2014,gamTleuk[c(1:14,14)],type="s",lty=5)#,lwd=3)
cpT1ovcaYR=trunc(cpT1ovca/12)+2000
cpT1ovcaMNTH=trunc(cpT1ovca-(cpT1ovcaYR-2000)*12)
lines(c(2000:cpT1ovcaYR,cpT1ovcaYR+cpT1ovcaMNTH/12,(cpT1ovcaYR+1):2014),
     gamT1ovca[c(1:(cpT1ovcaYR-1999),(cpT1ovcaYR-1999):14,14)]+
       gamT1ovca[c(rep(1,(cpT1ovcaYR-2000)),rep(15,16-(cpT1ovcaYR-2000)))],
     lty=6,type="s")#,lwd=3)
abline(v=2005+9/12)
#abline(v=2010+2/12)
par(mar=rep(0,4))
plot(0,type='n',axes=FALSE)
legend("center",c("LBCa","PrCa","BrCa","CrCa","Leuk","OvCa"),lty=1:6,bty='n',horiz=TRUE)

#temporal overall from best
#for poster
library(RColorBrewer)
#display.brewer.pal(n = 7, name = 'YlGnBu')
colnms=brewer.pal(7,'YlGnBu')
layout(m=matrix(1:2,nrow=2,ncol=1),heights=c(.9,.1))
par(mar=c(5,5,1,1))
cpT1lbcaYR=trunc(cpT1lbca/12)+2000
cpT1lbcaMNTH=trunc(cpT1lbca-(cpT1lbcaYR-2000)*12)
plot(c(2000:cpT1lbcaYR,cpT1lbcaYR+cpT1lbcaMNTH/12,(cpT1lbcaYR+1):2014),
     gamT1lbca[c(1:(cpT1lbcaYR-1999),(cpT1lbcaYR-1999):14,14)]+
       gamT1lbca[c(rep(1,(cpT1lbcaYR-1999)),rep(15,16-(cpT1lbcaYR-1999)))],
     ylim=c(-1,1.6),lwd=3,col=colnms[2],
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(2000:2014,gamTprca[c(1:14,14)],type="s",col=colnms[3],lwd=3)
cpT1brcaYR=trunc(cpT1brca/12)+2000
cpT1brcaMNTH=trunc(cpT1brca-(cpT1brcaYR-2000)*12)
lines(c(2000:cpT1brcaYR,cpT1brcaYR+cpT1brcaMNTH/12,(cpT1brcaYR+1):2014),
      gamT1brca[c(1:(cpT1brcaYR-1999),(cpT1brcaYR-1999):14,14)]+
        gamT1brca[c(rep(1,(cpT1brcaYR-1999)),rep(15,16-(cpT1brcaYR-1999)))],
      col=colnms[4],type="s",lwd=3)
lines(2000:2014,gamTcrc[c(1:14,14)],type="s",col=colnms[5],lwd=3)
lines(2000:2014,gamTleuk[c(1:14,14)],type="s",col=colnms[6],lwd=3)
cpT1ovcaYR=trunc(cpT1ovca/12)+2000
cpT1ovcaMNTH=trunc(cpT1ovca-(cpT1ovcaYR-2000)*12)
lines(c(2000:cpT1ovcaYR,cpT1ovcaYR+cpT1ovcaMNTH/12,(cpT1ovcaYR+1):2014),
      gamT1ovca[c(1:(cpT1ovcaYR-1999),(cpT1ovcaYR-1999):14,14)]+
        gamT1ovca[c(rep(1,(cpT1ovcaYR-2000)),rep(15,16-(cpT1ovcaYR-2000)))],
      col=colnms[7],type="s",lwd=3)
abline(v=2005+9/12)
abline(v=2010+2/12)
par(mar=rep(0,4))
plot(0,type='n',axes=FALSE)
legend("center",c("LBCa","PrCa","BrCa","CrCa","Leuk","OvCa"),col=colnms[2:7],lwd=3,
       bty='n',horiz=TRUE,cex=1.5)


#Individual plots with breakdown (where needed)
par(mfrow=c(2,3))
plot(c(2000:cpT1lbcaYR,cpT1lbcaYR+cpT1lbcaMNTH/12,(cpT1lbcaYR+1):2014),
     gamT1lbca[c(1:(cpT1lbcaYR-1999),(cpT1lbcaYR-1999):14,14)]+
       gamT1lbca[c(rep(1,(cpT1lbcaYR-1999)),rep(15,16-(cpT1lbcaYR-1999)))],
     ylim=c(-1.05,.5),main="LBCa",
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(c(2000,cpT1lbcaYR+cpT1lbcaMNTH/12,2014),
      c(0,gamT1lbca[15],gamT1lbca[15]),type="s",lty=2,col="red")
lines(2000:2014,gamT1lbca[c(1:14,14)],type="s",lty=3,col="blue")
plot(2000:2014,gamTprca[c(1:14,14)],type="s",col=1,main="PrCa",
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
plot(c(2000:cpT1brcaYR,cpT1brcaYR+cpT1brcaMNTH/12,(cpT1brcaYR+1):2014),
     gamT1brca[c(1:(cpT1brcaYR-1999),(cpT1brcaYR-1999):14,14)]+
       gamT1brca[c(rep(1,(cpT1brcaYR-1999)),rep(15,16-(cpT1brcaYR-1999)))],
     ylim=c(-.01,.75),main="BrCa",
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(c(2000,cpT1brcaYR+cpT1brcaMNTH/12,2014),
      c(0,gamT1brca[15],gamT1brca[15]),type="s",lty=2,col="red")
lines(2000:2014,gamT1brca[c(1:14,14)],type="s",lty=3,col="blue")
plot(2000:2014,gamTcrc[c(1:14,14)],type="s",col=1,main="CrCa",
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
plot(2000:2014,gamTleuk[c(1:14,14)],type="s",col=1,main="Leuk",
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
plot(c(2000:cpT1ovcaYR,cpT1ovcaYR+cpT1ovcaMNTH/12,(cpT1ovcaYR+1):2014),
     gamT1ovca[c(1:(cpT1ovcaYR-1999),(cpT1ovcaYR-1999):14,14)]+
       gamT1ovca[c(rep(1,(cpT1ovcaYR-1999)),rep(15,16-(cpT1ovcaYR-1999)))],
     main="OvCa",
     type="s",ylab="Temporal Random Effect",xlab="Diagnosis Year")
lines(c(2000,cpT1ovcaYR+cpT1ovcaMNTH/12,2014),
      c(0,gamT1ovca[15],gamT1ovca[15]),type="s",lty=2,col="red")
lines(2000:2014,gamT1ovca[c(1:14,14)],type="s",lty=3,col="blue")


#T2 inf plots (where available)
gam12T2prcainf=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\Prostate\\modelT2prcainf.csv")[c(79:91,93),2])
cpT2prcainf=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\Prostate\\modelT2prcainf.csv")[95:96,2]
gam12T2brcainf=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\breast\\modelT2brcainf.csv")[c(83:95,97),2])
cpT2brcainf=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\breast\\modelT2brcainf.csv")[99:100,2]
gam12T2crcinf=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\crc\\modelT2crcainf.csv")[c(81:93,95),2])
cpT2crcinf=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\crc\\modelT2crcainf.csv")[97:98,2]
gam12T2ovcainf=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\ovarian\\modelT2ovinfR.csv")[c(10:22,24),2])
cpT2ovcainf=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\ovarian\\modelT2ovinfR.csv")[26:27,2]

layout(as.matrix(1:3),heights=c(.45,.45,.1))
#T1 plots
plot(c(2000,cpT1lbcaYR+cpT1lbcaMNTH/12,2014),
     c(0,gamT1lbca[15],gamT1lbca[15]),
     type="s",col=1,
     ylim=c(-2,.7),main="Single Change Point Event Effect",
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
gamT1prca=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\Prostate\\modelT1prca.csv")[c(79:91,93),2])
cpT1prca=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\Prostate\\modelT1prca.csv")[95,2]
cpT1prcaYR=trunc(cpT1prca/12)+2000
cpT1prcaMNTH=trunc(cpT1prca-(cpT1prcaYR-2000)*12)
lines(c(2000,cpT1prcaYR+cpT1prcaMNTH/12,2014),
      c(0,gamT1prca[15],gamT1prca[15]),
      type="s",col=2)
lines(c(2000,cpT1brcaYR+cpT1brcaMNTH/12,2014),
      c(0,gamT1brca[15],gamT1brca[15]),
      type="s",col=3)
gamT1crc=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\crc\\modelT1crca.csv")[c(81:93,95),2])
cpT1crc=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\crc\\modelT1crca.csv")[97,2]
cpT1crcYR=trunc(cpT1crc/12)+2000
cpT1crcMNTH=trunc(cpT1crc-(cpT1crcYR-2000)*12)
lines(c(2000,cpT1crcYR+cpT1crcMNTH/12,2014),
      c(0,gamT1crc[15],gamT1crc[15]),
      type="s",col=4)
gamT1leuk=c(0,read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\leukemia\\rerun\\modelT1leuk.csv")[c(81:93,95),2])
cpT1leuk=read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\Student Projects\\Spring 19\\Results\\leukemia\\rerun\\modelT1leuk.csv")[97,2]
cpT1leukYR=trunc(cpT1leuk/12)+2000
cpT1leukMNTH=trunc(cpT1leuk-(cpT1leukYR-2000)*12)
lines(c(2000,cpT1leukYR+cpT1leukMNTH/12,2014),
      c(0,gamT1leuk[15],gamT1leuk[15]),
      type="s",col=5)
lines(c(2000,cpT1ovcaYR+cpT1ovcaMNTH/12,2014),
      c(0,gamT1ovca[15],gamT1ovca[15]),
      type="s",col=6)

cp1T2prcaYR=trunc(cpT2prcainf[1]/12)+2000
cp1T2prcaMNTH=trunc(cpT2prcainf[1]-(cp1T2prcaYR-2000)*12)
cp2T2prcaYR=trunc(cpT2prcainf[2]/12)+2000
cp2T2prcaMNTH=trunc(cpT2prcainf[2]-(cp2T2prcaYR-2000)*12)
plot(c(2000,cp1T2prcaYR+cp1T2prcaMNTH/12,cp2T2prcaYR+cp2T2prcaMNTH/12,2014),
     c(0,gam12T2prcainf[15],0,0),type="s",col=2,ylim=c(-.1,.115),
     main="Double Change Point Event Effect",
     ylab="Temporal Random Effect",xlab="Diagnosis Year")
cp1T2brcaYR=trunc(cpT2brcainf[1]/12)+2000
cp1T2brcaMNTH=trunc(cpT2brcainf[1]-(cp1T2brcaYR-2000)*12)
cp2T2brcaYR=trunc(cpT2brcainf[2]/12)+2000
cp2T2brcaMNTH=trunc(cpT2brcainf[2]-(cp2T2brcaYR-2000)*12)
lines(c(2000,cp1T2brcaYR+cp1T2brcaMNTH/12,cp2T2brcaYR+cp2T2brcaMNTH/12,2014),
     c(0,gam12T2brcainf[15],0,0),type="s",col=3)
cp1T2crcYR=trunc(cpT2crcinf[1]/12)+2000
cp1T2crcMNTH=trunc(cpT2crcinf[1]-(cp1T2crcYR-2000)*12)
cp2T2crcYR=trunc(cpT2crcinf[2]/12)+2000
cp2T2crcMNTH=trunc(cpT2crcinf[2]-(cp2T2crcYR-2000)*12)
lines(c(2000,cp1T2crcYR+cp1T2crcMNTH/12,cp2T2crcYR+cp2T2crcMNTH/12,2014),
     c(0,gam12T2crcinf[15],0,0),type="s",col=4)
cp1T2ovcaYR=trunc(cpT2ovcainf[1]/12)+2000
cp1T2ovcaMNTH=trunc(cpT2ovcainf[1]-(cp1T2ovcaYR-2000)*12)
cp2T2ovcaYR=trunc(cpT2ovcainf[2]/12)+2000
cp2T2ovcaMNTH=trunc(cpT2ovcainf[2]-(cp2T2ovcaYR-2000)*12)
lines(c(2000,cp1T2ovcaYR+cp1T2ovcaMNTH/12,cp2T2ovcaYR+cp2T2ovcaMNTH/12,2014),
     c(0,gam12T2ovcainf[15],0,0),type="s",col=6)

par(mar=rep(0,4))
plot(0,type='n',axes=FALSE)
legend("center",c("LBCa","PrCa","BrCa","CrCa","Leuk","OvCa"),col=1:6,lty=1,bty='n',horiz=TRUE)


#slope calcs
LBCaovrall=gamT1lbca[c(1:(cpT1lbcaYR-1999),(cpT1lbcaYR-1999):14,14)]+
  gamT1lbca[c(rep(1,(cpT1lbcaYR-1999)),rep(15,16-(cpT1lbcaYR-1999)))]
LBCaovrall[14]/14
LBCaovrall[6]/6
(LBCaovrall[10]-LBCaovrall[6])/5
(LBCaovrall[14]-LBCaovrall[10])/6

PrCaovrall=gamTprca[c(1:14,14)]
PrCaovrall[14]/14
PrCaovrall[7]/7
(PrCaovrall[10]-PrCaovrall[7])/4
(PrCaovrall[14]-PrCaovrall[10])/6

BrCaovrall=gamT1brca[c(1:(cpT1brcaYR-1999),(cpT1brcaYR-1999):14,14)]+
  gamT1brca[c(rep(1,(cpT1brcaYR-1999)),rep(15,16-(cpT1brcaYR-1999)))]
BrCaovrall[14]/14
BrCaovrall[7]/7
(BrCaovrall[10]-BrCaovrall[7])/4
(BrCaovrall[16]-BrCaovrall[10])/6


CrCaovrall=gamTcrc[c(1:14,14)]
CrCaovrall[14]/14
CrCaovrall[6]/6
(CrCaovrall[10]-CrCaovrall[6])/5
(CrCaovrall[14]-CrCaovrall[10])/6

Leukovrall=gamTleuk[c(1:14,14)]
Leukovrall[14]/14
Leukovrall[6]/6
(Leukovrall[10]-Leukovrall[6])/5
(Leukovrall[14]-Leukovrall[10])/6

OvCaovrall=gamT1ovca[c(1:(cpT1ovcaYR-1999),(cpT1ovcaYR-1999):14,14)]+
  gamT1ovca[c(rep(1,(cpT1ovcaYR-2000)),rep(15,16-(cpT1ovcaYR-2000)))]
OvCaovrall[14]/14
OvCaovrall[6]/6
(OvCaovrall[9]-OvCaovrall[6])/4
(OvCaovrall[14]-OvCaovrall[10])/6

