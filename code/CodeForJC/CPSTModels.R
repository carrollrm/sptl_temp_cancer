library(R2WinBUGS)
setwd("~/Classes/MAT 596")

#data
databrca=read.csv("data/CancerData/BrCadat.csv")
datalbca=read.csv("data/CancerData/LBCaDat.csv")
datacrca=read.csv("data/CancerData/CRCaDat.csv")
dataprca=read.csv("data/CancerData/PrCaDat.csv")

###### 1 Change point ######

#brca+lbca spatial temporal model with 1 Change Point
tick1=(databrca$yr-1)*12+databrca$mnth
tick2=(datalbca$yr-1)*12+datalbca$mnth
AFTdata=list(spt1=databrca$spt,yr1=databrca$yr,
             timeF1=databrca$timeF,indic1=databrca$indic,
             x11=databrca$x1,x12=databrca$x2,x13=databrca$x3,
             x14=databrca$x4,x15=databrca$x5,x16=databrca$x6,
             x17=databrca$x7,x18=databrca$x8,x19=databrca$x9,
             x110=databrca$x10,x111=databrca$x11,x112=databrca$x12,
             x113=databrca$x13,x114=databrca$x14,x115=databrca$x15,
             tick1=tick1,a=60,b=168,
             #predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
             #x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
             #x9=marital stat unkn,x10=eppr -+,x11=erpr +-,x12=erpr ++,
             #x13=erpr unkn,x14=radition yes, x15=radiation unkn
             spt2=datalbca$spt,yr2=datalbca$yr,
             timeF2=datalbca$timeF,indic2=datalbca$indic,
             x21=datalbca$x1,x22=datalbca$x2,x23=datalbca$x3,
             x24=datalbca$x4,x25=datalbca$x5,x26=datalbca$x6,
             x27=datalbca$x7,x28=datalbca$x8,x29=datalbca$x9,
             x210=datalbca$x10,x211=datalbca$x11,x212=datalbca$x12,
             tick2=tick2,a=60,b=168)
#predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=grade unkn
#x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
#x9=marital stat unkn,x10=radition yes, x11=radiation unkn, x12=gender male
init=list(beta1=rep(0,16),beta2=rep(0,13),sdu=5,u=rep(0,64),
          gamA1=c(NA,rep(0,13)),gamA2=c(NA,rep(0,13)),sdgamA1=5,sdgamA2=5,
          sigma1=1,sigma2=1,gamB1=c(NA,0),sdgamB1=5,cp1=90,gamB2=c(NA,0),sdgamB2=5,cp2=90)
inits=list(init,init)
modelAFT=bugs(data=AFTdata,inits=inits,
              c('deviance','sigma1','sigma2',
                'u','sdu',
                'gamA1','gamA2','sdgamA1','sdgamA2',
                'beta1','beta2','D1','D2', 
                'gamB1','sdgamB1','cp1','gamB2','sdgamB2','cp2'),
              bugs.directory="C:/Users/twili/OneDrive/Documents/WinBUGS14",
              n.chains=2,n.iter=20,n.burnin=10,debug=T,
              model.file="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/CodeForJC/STCPbrcalbca.txt")
write.csv(modelAFT$summary,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstCPbrcalbca.csv")
gof=matrix(0,nrow=3,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","LBCa","Total")
gof[1:2,3]=modelAFT$summary[126:127,1]#D1,D2 means DOUBLE CHECK THESE ROWS!
gof[1:2,2]=.5*modelAFT$summary[126:127,2]^2#D1, D2 sd's squared 
gof[1:2,1]=gof[1:2,3]+gof[1:2,2]
gof[3,]=gof[1,]+gof[2,]
write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstCPbrcalbca.csv",col.names=F,append=T,sep=",")


#brca+crca+prca spatial temporal model with 1 Change Point
tick1=(databrca$yr-1)*12+databrca$mnth
tick2=(datacrca$yr-1)*12+datacrca$mnth
tick3=(dataprca$yr-1)*12+dataprca$mnth
AFTdata=list(spt1=databrca$spt,yr1=databrca$yr,
             timeF1=databrca$timeF,indic1=databrca$indic,
             x11=databrca$x1,x12=databrca$x2,x13=databrca$x3,
             x14=databrca$x4,x15=databrca$x5,x16=databrca$x6,
             x17=databrca$x7,x18=databrca$x8,x19=databrca$x9,
             x110=databrca$x10,x111=databrca$x11,x112=databrca$x12,
             x113=databrca$x13,x114=databrca$x14,x115=databrca$x15,
             tick1=tick1,a=60,b=168,
             #predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
             #x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
             #x9=marital stat unkn,x10=eppr -+,x11=erpr +-,x12=erpr ++,
             #x13=erpr unkn,x14=radition yes, x15=radiation unkn
             spt2=datacrca$spt,yr2=datacrca$yr,
             timeF2=datacrca$timeF,indic2=datacrca$indic,
             x21=datacrca$x1,x22=datacrca$x2,x23=datacrca$x3,
             x24=datacrca$x4,x25=datacrca$x5,x26=datacrca$x6,
             x27=datacrca$x7,x28=datacrca$x8,x29=datacrca$x9,
             x210=datacrca$x10,x211=datacrca$x11,x212=datacrca$x12,
             x213=datacrca$x13,
             tick2=tick2,
             #predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
             #x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
             #x9=marital stat unkn,x10=radition yes, x11=radiation unkn,
             #x12=gender male, x13=previous tumor yes
             spt3=dataprca$spt,yr3=dataprca$yr,
             timeF3=dataprca$timeF,indic3=dataprca$indic,
             x31=dataprca$x1,x32=dataprca$x2,x33=dataprca$x3,
             x34=dataprca$x4,x35=dataprca$x5,x36=dataprca$x6,
             x37=dataprca$x7,x38=dataprca$x8,x39=dataprca$x9,
             x310=dataprca$x10,x311=dataprca$x11,
             tick3=tick3)
#predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
#x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
#x9=marital stat unkn,x10=radition yes, x11=radiation unkn
init=list(beta1=rep(0,16),beta2=rep(0,14),beta3=rep(0,12),
          gamA1=c(NA,rep(0,13)),gamA2=c(NA,rep(0,13)),gamA3=c(NA,rep(0,13)),sdgamA1=5,sdgamA2=5,sdgamA3=5,
          sdu=5,u=rep(0,64),
          sigma1=1,sigma2=1,sigma3=1,
          gamB1=c(NA,0),sdgamB1=5,gamB3=c(NA,0),sdgamB3=5,
          cp1=90,cp3=90)
inits=list(init,init)
modelAFT=bugs(data=AFTdata,inits=inits,
              c('deviance','sigma1','sigma2','sigma3','u','sdu',
                'gamA1','gamA2','gamA3','sdgamA1','sdgamA2','sdgamA3',
                'beta1','beta2','beta3','D1','D2','D3',
                'gamB1','gamB3','sdgamB1','sdgamB3',
                'cp1','cp3'),
              bugs.directory="C:/Users/twili/OneDrive/Documents/WinBUGS14",
              n.chains=2,n.iter=15000,n.burnin=13000,debug=T,
              model.file="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/CodeForJC/STCPbrcacrcapraca.txt")
write.csv(modelAFT$summary,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelSTCPbrcacrcapraca.csv")
gof=matrix(0,nrow=4,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","CrCa","PrCa","Total")
gof[1:3,3]=modelAFT$summary[154:156,1]#D1,D2 means DOUBLE CHECK THESE ROWS!
gof[1:3,2]=.5*modelAFT$summary[154:156,2]^2#D1, D2 sd's squared * .5
gof[1:3,1]=gof[1:3,3]+gof[1:3,2]
gof[4,]=gof[1,]+gof[2,]+gof[3,]
write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstCPbrcacrcaprca.csv",col.names=F,append=T,sep=",")

###########################################

###### 2 Change Points #####

#brca+lbca spatial temporal model with 2 Change Points
tick1=(databrca$yr-1)*12+databrca$mnth
tick2=(datalbca$yr-1)*12+datalbca$mnth
AFTdata=list(spt1=databrca$spt,yr1=databrca$yr,
             timeF1=databrca$timeF,indic1=databrca$indic,
             x11=databrca$x1,x12=databrca$x2,x13=databrca$x3,
             x14=databrca$x4,x15=databrca$x5,x16=databrca$x6,
             x17=databrca$x7,x18=databrca$x8,x19=databrca$x9,
             x110=databrca$x10,x111=databrca$x11,x112=databrca$x12,
             x113=databrca$x13,x114=databrca$x14,x115=databrca$x15,
             tick1=tick1,a=60,b=168,c=128,
             #predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
             #x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
             #x9=marital stat unkn,x10=eppr -+,x11=erpr +-,x12=erpr ++,
             #x13=erpr unkn,x14=radition yes, x15=radiation unkn
             spt2=datalbca$spt,yr2=datalbca$yr,
             timeF2=datalbca$timeF,indic2=datalbca$indic,
             x21=datalbca$x1,x22=datalbca$x2,x23=datalbca$x3,
             x24=datalbca$x4,x25=datalbca$x5,x26=datalbca$x6,
             x27=datalbca$x7,x28=datalbca$x8,x29=datalbca$x9,
             x210=datalbca$x10,x211=datalbca$x11,x212=datalbca$x12,
             tick2=tick2,a=60,b=168,c=128)
#predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=grade unkn
#x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
#x9=marital stat unkn,x10=radition yes, x11=radiation unkn, x12=gender male
init=list(beta1=rep(0,16),beta2=rep(0,13),sdu=5,u=rep(0,64),
          gamA1=c(NA,rep(0,13)),gamA2=c(NA,rep(0,13)),sdgamA1=5,sdgamA2=5,
          sigma1=1,sigma2=1,gamB1=c(NA,0),sdgamB1=5,cpA1=90,gamB2=c(NA,0),sdgamB2=5,cpB1=90)
inits=list(init,init)
modelAFT=bugs(data=AFTdata,inits=inits,
              c('deviance','sigma1','sigma2',
                'u','sdu',
                'gamA1','gamA2','sdgamA1','sdgamA2',
                'beta1','beta2','D1','D2', 
                'gamB1','sdgamB1','cpA1','gamB2','sdgamB2','cpB1', 'cpA2', 'cpB2'),
              bugs.directory="C:/Users/twili/OneDrive/Documents/WinBUGS14",
              n.chains=2,n.iter=15000,n.burnin=13000,debug=T,
              model.file="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/CodeForJC/ST2CPbrcalbca.txt")
write.csv(modelAFT$summary,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelST2CPbrcalbca.csv")
gof=matrix(0,nrow=3,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","LBCa","Total")
gof[1:2,3]=modelAFT$summary[126:127,1]#D1,D2 means DOUBLE CHECK THESE ROWS!
gof[1:2,2]=.5*modelAFT$summary[126:127,2]^2#D1, D2 sd's squared 
gof[1:2,1]=gof[1:2,3]+gof[1:2,2]
gof[3,]=gof[1,]+gof[2,]
write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelST2CPbrcalbca.csv",col.names=F,append=T,sep=",")


#brca+crca+prca spatial temporal model with 2 Change Points
tick1=(databrca$yr-1)*12+databrca$mnth
tick2=(datacrca$yr-1)*12+datacrca$mnth
tick3=(dataprca$yr-1)*12+dataprca$mnth
AFTdata=list(spt1=databrca$spt,yr1=databrca$yr,
             timeF1=databrca$timeF,indic1=databrca$indic,
             x11=databrca$x1,x12=databrca$x2,x13=databrca$x3,
             x14=databrca$x4,x15=databrca$x5,x16=databrca$x6,
             x17=databrca$x7,x18=databrca$x8,x19=databrca$x9,
             x110=databrca$x10,x111=databrca$x11,x112=databrca$x12,
             x113=databrca$x13,x114=databrca$x14,x115=databrca$x15,
             tick1=tick1,a=60,b=168,c=128,
             #predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
             #x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
             #x9=marital stat unkn,x10=eppr -+,x11=erpr +-,x12=erpr ++,
             #x13=erpr unkn,x14=radition yes, x15=radiation unkn
             spt2=datacrca$spt,yr2=datacrca$yr,
             timeF2=datacrca$timeF,indic2=datacrca$indic,
             x21=datacrca$x1,x22=datacrca$x2,x23=datacrca$x3,
             x24=datacrca$x4,x25=datacrca$x5,x26=datacrca$x6,
             x27=datacrca$x7,x28=datacrca$x8,x29=datacrca$x9,
             x210=datacrca$x10,x211=datacrca$x11,x212=datacrca$x12,
             x213=datacrca$x13,
             tick2=tick2,a=60,b=168,c=128,
             #predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
             #x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
             #x9=marital stat unkn,x10=radition yes, x11=radiation unkn,
             #x12=gender male, x13=previous tumor yes
             spt3=dataprca$spt,yr3=dataprca$yr,
             timeF3=dataprca$timeF,indic3=dataprca$indic,
             x31=dataprca$x1,x32=dataprca$x2,x33=dataprca$x3,
             x34=dataprca$x4,x35=dataprca$x5,x36=dataprca$x6,
             x37=dataprca$x7,x38=dataprca$x8,x39=dataprca$x9,
             x310=dataprca$x10,x311=dataprca$x11,
             tick3=tick3,a=60,b=168,c=128)
#predictor order: x1=agedx,x2=race (AA), x3=grade high,x4=igrade unkn
#x5=surg yes,x6=surg unkn, x7=married,x8=prev married,
#x9=marital stat unkn,x10=radition yes, x11=radiation unkn
init=list(beta1=rep(0,16),beta2=rep(0,14),beta3=rep(0,12),
          gamA1=c(NA,rep(0,13)),gamA2=c(NA,rep(0,13)),gamA3=c(NA,rep(0,13)),
          sdgamA1=5,sdgamA2=5,sdgamA3=5,
          sdu=5,u=rep(0,64),
          sigma1=1,sigma2=1,sigma3=1,
          gamB1=c(NA,0),sdgamB1=5,gamB2=c(NA,0),sdgamB2=5,
          cpA1=90,cpB1=90, cpA2=135,cpB2=135)
inits=list(init,init)
modelAFT=bugs(data=AFTdata,inits=inits,
              c('deviance','sigma1','sigma2','sigma3','u','sdu',
                'gamA1','gamA2','gamA3','sdgamA1','sdgamA2','sdgamA3',
                'beta1','beta2','beta3','D1','D2','D3','gamB1','gamB2','sdgamB1','sdgamB2',
                'cpA1','cpB1','cpA2', 'cpB2'),
              bugs.directory="C:/Users/twili/OneDrive/Documents/WinBUGS14",
              n.chains=2,n.iter=10,n.burnin=10,debug=T,
              model.file="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/CodeForJC/ST2CPbrcacrcaprca.txt")
write.csv(modelAFT$summary,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelST2CPbrcacrcaprca.csv")
gof=matrix(0,nrow=4,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","CrCa","PrCa","Total")
gof[1:3,3]=modelAFT$summary[154:156,1]#D1,D2 means DOUBLE CHECK THESE ROWS!
gof[1:3,2]=.5*modelAFT$summary[154:156,2]^2#D1, D2 sd's squared * .5
gof[1:3,1]=gof[1:3,3]+gof[1:3,2]
gof[4,]=gof[1,]+gof[2,]+gof[3,]
write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelST2CPbrcacrcaprca.csv",col.names=F,append=T,sep=",")


