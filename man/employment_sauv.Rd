\name{employment}
\alias{employment}
\docType{data}
\title{employment}
\description{
  
}
\usage{data(employment)}
\format{
  \describe{
    \name{employment}
    \item{id}{id}
    \item{state}{state}
    \item{type}{type}
    \item{samplingType}{samplingType}
    \item{unitType}{unitType}
    \item{itemcode}{itemcode}
    \item{ftemp02}{ftemp02}
    \item{ftpay02}{ftpay02}
    \item{ptemp02}{ptemp02}
    \item{ptpay02}{ptpay02}
    \item{pthours02}{pthours02}
    \item{ftemp07}{ftemp07}
    \item{ftpay07}{ftpay07}
    \item{ptemp07}{ptemp07}
    \item{ptpay07}{ptpay07}
    \item{pthours07}{pthours07}
    \item{in02}{in02}
    \item{in07}{in07}
    \item{inSample}{inSample}
    \item{finalWeight}{finalWeight}
    \item{size}{size}
  }
}
\source{
}
\examples{
#0. libraries
library("sqldf")
library(ipfp)
#1. load data
data(employment)
#2. delete observations with samplingtype=0 in02=0, in07=0
attach(employment)
dat<-employment[samplingType!=0&
                  in02!=0&
                  in07!=0&
                  !is.na(ptpay02)&
                  !is.na(ptpay07),-c(5)]
detach(employment)
dat$inSample[is.na(dat$inSample)]<-0
attach(dat)
#2. create small area by state and item code
states<-data.frame(state=levels(state)[sort(unique(state))])
itemcodes<-data.frame(itemcode=levels(itemcode)[sort(unique(itemcode))])
count02<-sqldf("select * from (
               select distinct a.state, a.itemcode, sum(a.ftemp02+a.ptemp02) as margin from dat a group by a.state,a.itemcode
               union all
               select * from (select distinct  b.state, c.itemcode,0 as margin from states b,itemcodes c 
               except select distinct d.state, d.itemcode, 0 as margin from dat d))
               order by state, itemcode
               ")

count07<-sqldf("select * from (
               select distinct a.state, a.itemcode, sum(a.ftemp07+a.ptemp07) as margin from dat a group by a.state,a.itemcode
               union all
               select * from (select distinct  b.state, c.itemcode,0 as margin from states b,itemcodes c 
               except select distinct d.state, d.itemcode, 0 as margin from dat d))
               order by state, itemcode
               ")


#3. create the vector of constraints
Sample<-dat[inSample==1,]

direct07<-sqldf("select * from (
                select distinct a.state, a.itemcode, sum(a.finalWeight*( a.ftemp07+a.ptemp07)) as margin from Sample a group by a.state, a.itemcode
                union all
                select * from (select distinct  b.state, c.itemcode,0 as margin from states b,itemcodes c 
                except select distinct d.state, d.itemcode, 0 as margin from Sample d))
                order by state, itemcode
                ")
marginitem<-sqldf("select * from (
                  select distinct a.itemcode as code, sum(a.finalweight*(a.ftemp07+a.ptemp07)) as margin from Sample a group by itemcode
                  union all
                  select * from (select distinct b.itemcode as code, 0 as margin from itemcodes b 
                  except select c.itemcode as code, 0 as margin from Sample c))
                  group by code order by code
                  ")
marginstate<-sqldf("select distinct state as code,   sum(finalweight*(ftemp07+ptemp07)) as margin from Sample group by state order by state")
margin<-sqldf("select distinct 'intercept' as code, sum(finalweight*(ftemp07+ptemp07)) as margin from Sample")
y<-rbind(margin,marginstate[-1,],marginitem[-1,])[,2]
A<-t(model.matrix(~state+itemcode,data=count02))
total
x0=count02$margin
spree<-ipfp(y=y, A=A, x0=x0, tol = .Machine$double.eps, maxit = 1000,verbose = FALSE, full = FALSE)

length(spree)

REspree <-(spree-count07$margin)/count07$margin
REdirect<-(direct07$margin-count07$margin)/count07$margin
hist(REspree)
hist(REdirect)
boxplot(cbind(Direct=REdirect, Spree=REspree))
plot(REspree,REdirect)
plot(spree,x0,pch=19,cex=.5,col="blue")
abline(0,1,lwd=2)

}
\keyword{datasets}
