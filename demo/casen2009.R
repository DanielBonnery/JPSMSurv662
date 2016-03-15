data(casen2009)
casen2009<-as.data.frame(Casen2009)
levels(casen2009$REGION)<-c("Tarapac",                         
                            "Antofagasta"                         ,
                            "Atacama"                             ,
                            "Coquimbo"                            ,
                            "Valparaeso"                       ,
                            "Libertador Bernardo O'Higgins"    ,
                            "Maule"                               ,
                            "Bio Bio"                       ,
                            "La Araucana"                     ,
                            "Los Lagos"                           ,
                            "Aysen"                            ,
                            "Magallanes Y La Antartica Chilena",
                            "Region Metropolitana"             ,
                            "Los Rios"                            ,
                            "Arica y Parinacota")
library(sampling)
#metro=casen2009$REGION==levels(casen2009$REGION)[13]
set.seed(1)
N<-length(unique(casen2009$FOLIO))
householdlevel<-unique(casen2009[c("FOLIO","CORTE","REGION","COMUNA")])
#remove duplicates
counts<-table(householdlevel$FOLIO)
toremove<-names(counts[counts>1])
householdlevel<-householdlevel[!is.element(householdlevel$FOLIO,toremove),]
householdlevel$POBRE<-is.element(householdlevel$CORTE,levels(householdlevel$CORTE)[1:2])
s=sample(unique(householdlevel$FOLIO),2000)
n<-length(s)
pikls<-matrix(n*(n-1)/(N*(N-1)),n,n)
diag(pikls)<-n/N
sh<-is.element(householdlevel$FOLIO,s)
attach(householdlevel[sh,])
hatpi<-table(POBRE,REGION)[2,]/apply(table(POBRE,REGION),2,sum)
pi<-aggregate(householdlevel$POBRE,mean,by=list(householdlevel$REGION))$x
ni<-table(REGION)
Ni<-table(householdlevel$REGION)
expni<-Ni*n/N
hatVariance<-(1-ni/Ni)*(hatpi*(1-hatpi))/(ni-1)
Variance<-(1-ni/Ni)*(pi*(1-pi))/(expni-1)
estimates<-rbind(hatpi,
                 hatVariance,
                 "Sample Size"=table(householdlevel[sh,]$REGION),
                 rbind(hatpi,hatpi)+matrix(c(-1,1),2,1)%*%sqrt(hatVariance)*qnorm(.975))
dimnames(estimates)<-list(c("Poverty Rate","Variance","Size","CI, LB","CI, UB"),levels(REGION))
truevalues<-rbind("Poverty Rate"=pi,
                  "Variance"=Variance, 
                  "Size"=Ni,
                  "CI, LB"=pi-qnorm(0.975)*sqrt(Variance),
                  "CI, UB"=pi+qnorm(0.975)*sqrt(Variance))
ord<-order(Variance)

plot(c(1,length(pi)),range(estimates[4:5,]),type='n',xlab='Region',ylab='Poverty rate');
points(pi[ord],pch=19);
points(hatpi[ord],col="red");
segments(x0 = 1:15,x1=1:15,y0=estimates[4,ord],y1=estimates[5,ord])

