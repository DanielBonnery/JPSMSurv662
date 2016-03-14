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
variables<-model.matrix(~POBRE*REGION-POBRE-REGION-1)
vars<-sapply(1:15,function(i){
  vartaylor_ratio(Ys =variables[,2*i],Xs=variables[,2*i-1],pikls =pikls)})
vars<-matrix(unlist(vars),2,15)
vars<-rbind(vars,vars[c(1,1),]+matrix(c(-1,1),2,1)%*%sqrt(vars[2,])*qnorm(.975))
dimnames(vars)<-list(c("Estimated Poverty Rate","Variance estimate","CI, lower bound","CI, upper bound"),levels(REGION))
vars<-rbind("True Poverty Rate"=aggregate(householdlevel$POBRE,                                          mean,by=list(householdlevel$REGION))$x,
            "Sample Size"=table(householdlevel[sh,]$REGION),
            "Population Size"=table(householdlevel$REGION),
            vars)
ord<-order(vars[4,])

  plot(c(1,15),range(vars[c(1,6:7),]),type='n',xlab='Region',ylab='Poverty rate');
points(vars[1,ord],pch=19);
points(vars[4,ord]);
segments(x0 = 1:15,x1=1:15,y0=vars[6,ord],y1=vars[7,ord])

  