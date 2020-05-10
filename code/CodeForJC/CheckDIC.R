
### sbracalbca bottom calculations
modelAFT = read.csv(file ='C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelsbrcalbca.csv')
gof=matrix(0,nrow=3,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","LBCa","Total")
gof[1:2,3]=modelAFT[98:99,2]#D1,D2 means
# sd of D2 is Zero, issue in calculation
gof[1:2,2]=.5*modelAFT[98:99,3]^2#D1, D2 sd's squared 
gof[1:2,1]=gof[1:2,3]+gof[1:2,2]
gof[3,]=gof[1,]+gof[2,]
#write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelsbrcalbca.csv",col.names=F,append=T,sep=",")

### need to rerun both temporal models ###


### sbrcacrcaprca bottom calculations
modelAFT = read.csv(file ="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelsbrcacrcaprca.csv")
gof=matrix(0,nrow=4,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","CrCa","PrCa","Total")
gof[1:3,3]=modelAFT[112:114,2]#D1,D2 means DOUBLE CHECK THESE ROWS
gof[1:3,2]=.5*modelAFT[112:114,3]^2#D1, D2 sd's squared 
gof[1:3,1]=gof[1:3,3]+gof[1:3,2]
gof[4,]=gof[1,]+gof[2,]+gof[3,]
write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelsbrcacrcaprca.csv",col.names=F,append=T,sep=",")


### stbrcalbca
modelAFT = read.csv(file = 'C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstbrcalbca.csv')
gof=matrix(0,nrow=3,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","LBCa","Total")
gof[1:2,3]=modelAFT[126:127,2]#D1,D2 means DOUBLE CHECK THESE ROWS!
gof[1:2,2]=.5*modelAFT[126:127,3]^2#D1, D2 sd's squared 
gof[1:2,1]=gof[1:2,3]+gof[1:2,2]
gof[3,]=gof[1,]+gof[2,]
#write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstbrcalbca.csv",col.names=F,append=T,sep=",")



### stbrcacrcaprca bottom calculations
modelAFT = read.csv(file ="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstbrcacrcaprca.csv")
gof=matrix(0,nrow=4,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","CrCa","PrCa","Total")
gof[1:3,3]=modelAFT[112:114,2]#D1,D2 means DOUBLE CHECK THESE ROWS
gof[1:3,2]=.5*modelAFT[112:114,3]^2#D1, D2 sd's squared 
gof[1:3,1]=gof[1:3,3]+gof[1:3,2]
gof[4,]=gof[1,]+gof[2,]+gof[3,]
#write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelstbrcacrcaprca.csv",col.names=F,append=T,sep=",")

### STCPbrcalbca bottom calculations
modelAFT = read.csv(file ="C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelSTCPbrcalbca.csv")
gof=matrix(0,nrow=3,ncol=3)
colnames(gof)=c("DIC","pD","Dbar")
rownames(gof)=c("BrCa","LbCa","Total")
gof[1:2,3]=modelAFT[126:127,2]#D1,D2 means DOUBLE CHECK THESE ROWS
gof[1:2,2]=.5*modelAFT[126:127,3]^2#D1, D2 sd's squared 
gof[1:2,1]=gof[1:2,3]+gof[1:2,2]
gof[3,]=gof[1,]+gof[2,]
write.table(gof,"C:/Users/twili/OneDrive/Documents/Classes/MAT 596/code/Results/modelSTCPbrcalbca.csv",col.names=F,append=T,sep=",")



