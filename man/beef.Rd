\name{beef}
\alias{beef}
\docType{data}
\title{Beef data}
\description{
  
}
\usage{data(beef)}
\format{
 This table contains the data of the paper  "An Error-Components Model for Prediction of County Crop Areas Using Survey and Satellite Data" by 
   George E. Battese, Rachel M. Harter and Wayne A. Fuller (Vol. 83, No. 401 (Mar., 1988), pp. 28-36)
  \describe{
    \name{beef}
    \item{x}{X variable} 
    \item{y}{Y variable}
  }
}
\source{
  Chambers and Dorfman, 1983 JASA}
\examples{
require("abind")
require(sampling)
data(beef)
N=nrow(beef)
attach(beef)
beef$r<-lm(y~x,data=beef)$residuals

varsrs<-function(n,Y=y){(1-n/N)*var(Y)/n}
varsrsest<-function(ys){n<-length(ys);(1-n/N)*var(ys)/n}
nrep<-1000
samplesizes<-c(10,50);
HT.expected<-sapply(beef,mean)
HT.expected<-sapply(samplesizes,function(x){HT.expected})
HT.expected["r",]<-HT.expected["y",]
HT.var<-sapply(beef,function(x){sapply(samplesizes,varsrs,Y=x)})
HT.moments<-abind(t(HT.expected),HT.var,along=3)
dimnames(HT.moments)[]<-list(samplesizes,names(beef),c("E","V"))



samplematrix<-array(sapply(c(10,50),function(x){replicate(nrep,(function(){srswor(x,N)==1})())}),c(N,nrep,length(samplesizes)))
dimnames(samplematrix)[2:3]<-list(1:nrep,samplesizes)
meansvarestimates<-function(y,Samplematrix=samplematrix){
  AA<-apply(Samplematrix,2:3,function(x){ys<-y[x];c(m=mean(ys),sigma=varsrsest(ys))}) 
}
estimates<-aperm(array(sapply(beef,meansvarestimates),c(2,nrep,length(samplesizes),ncol(beef))),c(1,3,2,4))
dimnames(estimates)<-list(c("E","V"),samplesizes,NULL,names(beef))
estimates["E",,,"r"]<-(estimates["E",,,"y"]/estimates["E",,,"x"])*HT.moments[,"x","E"]

#####boxplots for average
texte<-sapply(
  list(c("x","E","mean",100,3700),
       c("y","E","mean",0,300000),
       c("x","V","variance",0,1500000),
       c("y","V","variance",0,15000000000),
       c("r","E","variance",0,300000),
       c("r","V","variance",0,15000000000)),
  function(l){
    paste(paste0('
                 par(mfrow=c(2,1),oma=c(2,2,0,0),mar=c(0,0,0,0),mgp=c(2,1,0))
                 layout(matrix(1:2, 2, 1, byrow = TRUE),heights=c(.1,.9))
                 plot.new()
                 legend( 0,1,legend="True ',l[3],'",  col = "red",lwd=3,bty="n")
                 boxplot(t(estimates["',l[2],'",,,"',l[1],'"]), main="", ylab="",xlab="Sample size", ylim=c(',l[4],', ',l[5],'),bty="n")
                 segments(x0=.5 ,x1=1.5,y0=HT.moments["10","',l[1],'","',l[2],'"], col="red", lwd=2)
                 segments(x0=1.5,x1=2.5,y0=HT.moments["50","',l[1],'","',l[2],'"], col="red", lwd=2)
                 '),collapse=";")
  })



  par(mfrow=c(3,1),oma=c(2,0,1,0),
               mar=c(0,6,1,0),
               mgp=c(4,1,0))
            layout(matrix(1:3, 3, 1, byrow = TRUE), 
            heights=c(.06,.44,.5))
yax<-c("Mean","Variance")

plot.new()
   legend( 0,1,legend="True value",  col = "red",lwd=3,bty="n")
              
     sapply(1:2,function(i){
XX <-data.frame(
x=c(aperm(estimates[i,,,2:3],c(1,3,2))),
est=as.factor(rep(c("$n=10$: HT ","$n=50$: HT","$n=10$: Ratio","$n=50$: Ratio"),nrep)))
boxplot(x~est ,data=XX,
       main="",
       freq=FALSE,
       ylab=yax[i],
       xlab="",
  xaxt=if(is.element(i,2)){"s"}else{"n"})
 segments(x0=.5 ,x1=1.5,y0=HT.moments["10","y",c("E","V")[i]], col="red", lwd=2)
 segments(x0=1.5 ,x1=2.5,y0=HT.moments["10","r",c("E","V")[i]], col="red", lwd=2)
 segments(x0=2.5,x1=3.5,y0=HT.moments["50","y",c("E","V")[i]], col="red", lwd=2)
 segments(x0=3.5,x1=4.5,y0=HT.moments["50","r",c("E","V")[i]], col="red", lwd=2)  
})

par(mfrow=c(2,2),oma=c(0,0,0,0),
               mar=c(4,2,0,0),
               mgp=c(3,1,0))
            layout(matrix(1:4, 2, 2, byrow = TRUE), 
            heights=c(.15,.9))
yax<-c("Mean","Variance")

plot.new()
   legend( 0,1,legend="True value",  col = "red",lwd=3,bty="n")
plot.new()              
     sapply(1:2,function(i){
XX <-data.frame(
x=c(aperm(estimates[i,,,2:3],c(1,3,2))),
est=as.factor(rep(c("$n=10$: HT ","$n=50$: HT","$n=10$: Ratio","$n=50$: Ratio"),nrep)))
boxplot(x~est ,data=XX,
       main="",
       freq=FALSE,
       ylab="",
      xlab=yax[i],
  #xaxt="s",
  yaxt="s"
)
 segments(x0=.5 ,x1=1.5,y0=HT.moments["10","y",c("E","V")[i]], col="red", lwd=2)
 segments(x0=1.5 ,x1=2.5,y0=HT.moments["10","r",c("E","V")[i]], col="red", lwd=2)
 segments(x0=2.5,x1=3.5,y0=HT.moments["50","y",c("E","V")[i]], col="red", lwd=2)
 segments(x0=3.5,x1=4.5,y0=HT.moments["50","r",c("E","V")[i]], col="red", lwd=2)  
})



}
\keyword{datasets}
  