require(dataCASEN)
data(casen2009)
library(survey)
householdlevel<-casen2009[casen2009$O==1,]
householdlevel$POBRE<-householdlevel$CORTE!="No pobre"
householdlevel.design<-
  svydesign(id=~SEGMENTO,
            strata=~ESTRATO,
            data=householdlevel,
            weights=~EXPR)
options(survey.lonely.psu = "adjust")
povertyrateperregion<-svyby(~POBRE,~REGION,householdlevel.design,svymean, vartype=c("se","cv","var"))
povertyrateperregion$L<-povertyrateperregion$POBRETRUE-qnorm(.975)*povertyrateperregion$se.POBRETRUE
povertyrateperregion$U<-povertyrateperregion$POBRETRUE+qnorm(.975)*povertyrateperregion$se.POBRETRUE
povertyrateperregion<-povertyrateperregion[order(povertyrateperregion$POBRETRUE),]
povertyrateperregion$REGION<-factor(povertyrateperregion$REGION,povertyrateperregion$REGION)
library(ggplot2)
plot1<-ggplot(povertyrateperregion, aes(x = REGION, y = POBRETRUE)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))
attach(householdlevel)
sapply(levels(REGION),function(x){
  sum(EXPR*POBRE*NUMPER*(REGION==x))/sum(EXPR*NUMPER*(REGION==x))
})
