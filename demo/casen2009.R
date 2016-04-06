require(dataCASEN)
data(casen2009)
set.seed(1)
N<-length(unique(casen2009$FOLIO))
library(survey)
householdlevel<-casen2009[casen2009$O==1,]
householdlevel$POBRE<-householdlevel$CORTE!="No pobre"
#Use of sampling package
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


#Use of survey package

data(casen2009)
library(survey)
casen2009$POBRE<-is.element(casen2009$CORTE,levels(casen2009$CORTE)[1:2])
casen2009.design<-
  svydesign(id=~SEGMENTO+FOLIO,
            strata=~ESTRATO,
            data=casen2009,
            weights=~EXPR)
options(survey.lonely.psu = "adjust")
povertyratepercomuna<-svyby(~POBRE,~COMUNA,casen2009.design,svymean, vartype=c("se","cv","var"))
povertyratepercomuna$L<-povertyratepercomuna$POBRETRUE-qnorm(.975)*povertyratepercomuna$se.POBRETRUE
povertyratepercomuna$U<-povertyratepercomuna$POBRETRUE+qnorm(.975)*povertyratepercomuna$se.POBRETRUE
povertyratepercomuna<-povertyratepercomuna[order(povertyratepercomuna$POBRETRUE),]
povertyratepercomuna$COMUNA<-factor(povertyratepercomuna$COMUNA,povertyratepercomuna$COMUNA)
library(ggplot2)
plot1<-ggplot(povertyratepercomuna, aes(x = COMUNA, y = POBRETRUE)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))
