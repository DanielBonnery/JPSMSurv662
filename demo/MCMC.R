require("R2jags")
require("plyr")
require("lattice")
require("abind")
library("dataBaseball")
data(baseball,package="dataBaseball")
attach(baseball)
m<-45
#1. Run jags
fits<-lapply(list(
  "Uniform prior"=list(model="A~dunif(1.0E-5,1.0E5)",inits=list(A=1)),
  "Gelman prior"=list(model="A<-(x^2)/2
                      x~dunif(1.0E-9,1.0E9)",inits=list(x=1)),
  "Gamma prior"=list(model="IA~dgamma(1.0E-5,1.0E-5)
                     A<-1/IA",inits=list(IA=1))),
  function(x){
    textmodel<-paste0("model{
                      for(i in 1:n) {
                      Y[i] ~ dnorm(theta[i] , 1/(psi[i])^2)
                      mu[i] <- beta[1] + beta[2]*X[i]
                      theta[i]~dnorm(mu[i] , 1/A)}
                      for( i in 1:2){
                      beta[i] ~ dnorm(0,1.0E-6)}
                      ", 
                      x$model,"}")
    jags(
      model.file=textConnection(textmodel),
      data=list(n=nrow(baseball),
                Y=hat.P,
                X=x1,
                psi=hat.P*(1-hat.P)/m),
      inits=list(c(list(beta=c(1,1)),
                   x$inits)),
      n.chains=1,
      parameters.to.save=c('beta','A','theta'),
      n.burnin = 1000, n.iter=20000)})  

#2. Reorder data 
beta.1<-adply(do.call(abind,c(lapply(fits,function(x){x$BUGSoutput$sims.array[,,c("A","beta[2]",paste0("theta[1]"))]}),list(along=3))),c(1,3),.id=list("obs","model"))
beta.1$model<-ordered(beta.1$model)
levels(beta.1$model)<-c("Uniform prior","Gelman prior","Gamma prior")

#3. Produce graphs
densityplot(~beta.1$"beta[2]"|beta.1$model,
            xlab="$\\beta_1$",
            par.settings=list(strip.background=list(col="blue")),
            par.strip.text=list(col="white",font=2),
            layout = c(1,3),
            panel=function(x){
              panel.densityplot(x)
              panel.abline(v=mean(x),col=couleur2)})
densityplot(~beta.1$"A"|beta.1$model,
            xlab="$A$",
            par.settings=list(strip.background=list(col="blue")),
            par.strip.text=list(col="white",font=2),
            layout = c(1,3),
            panel=function(x){
              panel.densityplot(x)
              panel.abline(v=mean(x),col=couleur2)})
densityplot(~beta.1[,paste0("theta[1]")]|beta.1$model,
            xlab="$\\theta_i$, county with smallest s.d.",
            par.settings=list(strip.background=list(col="blue")),
            par.strip.text=list(col="white",font=2),
            layout = c(1,3),
            panel=function(x){
              panel.densityplot(x)
              panel.abline(v=mean(x),col=couleur2)})

table1<-adply(do.call(abind,c(lapply(fits,function(x){x$BUGSoutput$summary[c("A","beta[2]","theta[1]"),c(1,2,5,3,7)]}),list(along=3))),c(3,1),.id=list("Model","Parameter"))
names(table1)<-c("Model","Parameter","Mean","s.d.","median","CI-LB","CI-UB" )
table1$Parameter<-rep(c("$A$","$\\beta_1$","Clemente"),each=3)
table1
