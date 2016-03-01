data(baseball);attach(baseball)
library("sae")
resultML<-mseFH(hat.P~x1,hat.P*(1-hat.P)/45,method="ML")
resultML