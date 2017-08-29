require("abind")
require(sampling)
require(dataBeef)
data(beef)
N=nrow(beef)
attach(beef)
beef$r<-lm(y~x,data=beef)$residuals

varsrs<-function(n,Y=y){(1-n/N)*var(Y)/n}
varsrsest<-function(ys){n<-length(ys);(1-n/N)*var(ys)/n}
nrep<-1000
samplesizes<-c(10,50);#you can put the values of n you want here.
HT.expected<-sapply(beef,mean)
HT.expected<-sapply(samplesizes,function(x){HT.expected})
HT.expected["r",]<-HT.expected["y",]
HT.var<-sapply(beef,function(x){sapply(samplesizes,varsrs,Y=x)})
HT.moments<-abind(t(HT.expected),HT.var,along=3)
dimnames(HT.moments)[]<-list(samplesizes,names(beef),c("E","V"))



samplematrix<-array(sapply(c(10,50),function(x){
  replicate(nrep,(function(){srswor(x,N)==1})())}),c(N,nrep,length(samplesizes)))
dimnames(samplematrix)[2:3]<-list(1:nrep,samplesizes)
meansvarestimates<-function(y,Samplematrix=samplematrix){
  AA<-apply(Samplematrix,2:3,function(x){ys<-y[x];c(m=mean(ys),sigma=varsrsest(ys))}) 
}
estimates<-aperm(array(sapply(beef,meansvarestimates),c(2,nrep,length(samplesizes),ncol(beef))),c(1,3,2,4))
dimnames(estimates)<-list(c("E","V"),samplesizes,NULL,names(beef))
estimates["E",,,"r"]<-(estimates["E",,,"y"]/estimates["E",,,"x"])*HT.moments[,"x","E"]
names(dimnames(estimates))<-c("Statistic","Sample size","Iteration number","Parameter")
estimates<-abind(estimates,
                 estimates["E",,,]-qnorm(0.975)*sqrt(estimates["V",,,]),
                 estimates["E",,,]+qnorm(0.975)*sqrt(estimates["V",,,]),along=1)
dimnames(estimates)[[1]][3:4]<-c("CILB","CIUB")
coverage<-plyr::aaply(estimates[3:4,,,],c(1,2,3),function(x){x-HT.expected[,1]})
coverage<-plyr::aaply(coverage,2:4,function(x){x[1]<0&x[2]>0})
coverage<-plyr::aaply(coverage,c(1,3),mean)
names(dimnames(coverage))[2]<-"Estimate"

coverage