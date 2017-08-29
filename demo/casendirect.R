library(dataCASEN)
library(survey)
X<-casen2009[casen2009$O==1,]
X$POBRE=X$CORTE!="No pobre"
X<-X[order(X$REGION,X$ZONA),]
N<-nrow(X)
X$N<-N
Nh<-data.frame(table(X$REGION))
names(Nh)<-c("REGION","Nh")
X<-merge(X,Nh,by="REGION")
n<-2000
X$STRATA2<-paste0(X$REGION,X$ZONA)
Nh2<-data.frame(table(X$STRATA2))
names(Nh2)<-c("STRATA2","Nh2")
X<-merge(X,Nh2,by="STRATA2")


est<-function(Design){
  estimate<-svyby(formula=~POBRE,by=~REGION,design=do.call(svydesign,c(Design$args,list(data=X[Design$draw(),]))),FUN=svymean)[c("REGION","POBRETRUE","se.POBRETRUE")]
}

listofdesigns<-
  list(
    Design1=list(draw=function(){sample(N,n)},args=list(ids=~0,fpc=~N)),
    Design2a=list(draw=function(){stratsample(X$REGION,(n*c(table(X$REGION))/N))},args=list(ids=~1,strata=~REGION,fpc=~Nh)),
    Design2b=list(draw=function(){s1<-stratsample(X$REGION,(1820*c(table(X$REGION))/N))
    s2<-(1:N)[-s1][stratsample(X$REGION[-s1],0**c(table(X$REGION))+round(180/nrow(Nh)))];c(s1,s2)},args=list(ids=~1,strata=~REGION,fpc=~Nh)),
    Design3a=list(draw=function(){stratsample(X$STRATA2,(n*c(table(X$STRATA2))/N))},args=list(ids=~1,strata=~STRATA2,fpc=~Nh2)),
    Design3b=list(draw=function(){s1<-stratsample(X$STRATA2,(1820*c(table(X$STRATA2))/N))
                  s2<-(1:N)[-s1][stratsample(X$STRATA2[-s1],0**c(table(X$STRATA2))+round(180/nrow(Nh2)))];c(s1,s2)},args=list(ids=~1,strata=~STRATA2,fpc=~Nh2)))
set.seed(1)
allreplicates<-plyr::ldply(listofdesigns,function(x){plyr::ddply(data.frame(Replicate=1:100),~Replicate,function(y){est(x)})})
allreplicates2<-reshape2::melt(allreplicates,id.vars=1:3)
levels(allreplicates2$variable)<-c("Estimate","Standard error")

library(ggplot2)
ggplot(data = allreplicates2[allreplicates2$variable=="Estimate" ,], aes(x=.id, y=value,fill=.id))+
  geom_boxplot()+
  facet_grid(~REGION)

