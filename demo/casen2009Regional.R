data(casen2009)
library(survey)
householdlevel<-unique(casen2009[c("ESTRATO",
"SEGMENTO","EXPR","FOLIO","CORTE","REGION","COMUNA","NUMPER")])
#remove duplicates
counts<-table(householdlevel$FOLIO)
toremove<-names(counts[counts>1])
householdlevel<-householdlevel[!is.element(householdlevel$FOLIO,toremove),]
householdlevel$POBRE<-is.element(householdlevel$CORTE,levels(householdlevel$CORTE)[1:2])
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
