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
    \item{id}{Id}
    \item{state}{State}
    \item{type}{State ("0"), County ("1"), Township ("2"), Municipality ("3"), Special district ("4"), School district ("5")}
    \item{samplingType}{Type with levels "2" and "3" collapsed: level "3" corresponds to townships and Municipalities}
    \item{itemcode}{(Item code. "000": summary, 
001= Airport
007= Space Research and Technology (Federal),
005= Correction,
006= National Defense and International Relations(Federal),
044= Streets and Highways,
050= Housing and Community Development (Local),
052= Local Libraries,
059= Natural Resources,
061= Parks and Recreation,
012= Elementary and Secondary - Instruction,
112= Elementary and Secondary - Other Total,
014= Postal Service (Federal),
016= Higher Education - Other,
018= Higher Education - Instructional,
062= Police Protection - Officers,
162= Police-Other,
129= Welfare,
080= Sewerage,
071= Other Education (State) 081 Solid 072, Social Insurance Administration (State),
073= Financial Administration,
074= Firefighters,
124= Fire - Other,
081= Waste Management,
087= Water Transport and Terminals,
089= Other and Unallocable,
090= Liquor Stores (State),
091= Water Supply,
075= Judicial and Legal,
079= Other Government Administration,
032= Health,
092= Electric Power,
093= Gas Supply,
094= Transit)
}
    \item{ftemp12}{ftemp12}
    \item{ftpay12}{ftpay12}
    \item{ptemp12}{ptemp12}
    \item{ptpay12}{ptpay12}
    \item{pthours12}{pthours12}
    \item{ftemp12}{ftemp12}
    \item{ftpay12}{ftpay12}
    \item{ptemp12}{ptemp12}
    \item{ptpay12}{ptpay12}
    \item{pthours12}{pthours12}
    \item{insample}{inSample}
    \item{weight}{finalWeight}
  }
}
\source{

}
\examples{
require("ipfp")
attach(employment)
#2. create small area by state and item code
states<-data.frame(state=levels(state)[sort(unique(state))])
itemcodes<-data.frame(itemcode=levels(itemcode)[sort(unique(itemcode))])
count07<-sqldf("select * from (
               select distinct a.state, a.itemcode, sum(a.ftemp07+a.ptemp07) as margin from employment a group by a.state,a.itemcode
               union all
               select * from (select distinct  b.state, c.itemcode,0 as margin from states b,itemcodes c 
               except select distinct d.state, d.itemcode, 0 as margin from employment d))
               order by state, itemcode
               ")

count12<-sqldf("select * from (
               select distinct a.state, a.itemcode, sum(a.ftemp12+a.ptemp12) as margin from employment a group by a.state,a.itemcode
               union all
               select * from (select distinct  b.state, c.itemcode,0 as margin from states b,itemcodes c 
               except select distinct d.state, d.itemcode, 0 as margin from employment d))
               order by state, itemcode
               ")


#3. create the vector of constraints
Sample<-employment[insample,]

direct12<-sqldf("select * from (
                select distinct a.state, a.itemcode, sum(a.weight*( a.ftemp12+a.ptemp12)) as margin from Sample a group by a.state, a.itemcode
                union all
                select * from (select distinct  b.state, c.itemcode,0 as margin from states b,itemcodes c 
                except select distinct d.state, d.itemcode, 0 as margin from Sample d))
                order by state, itemcode
                ")
marginitem<-sqldf("select * from (
                  select distinct a.itemcode as code, sum(a.weight*(a.ftemp12+a.ptemp12)) as margin from Sample a group by itemcode
                  union all
                  select * from (select distinct b.itemcode as code, 0 as margin from itemcodes b 
                  except select c.itemcode as code, 0 as margin from Sample c))
                  group by code order by code
                  ")
marginstate<-sqldf("select distinct state as code,   sum(weight*(ftemp12+ptemp12)) as margin from Sample group by state order by state")
margin<-sqldf("select distinct 'intercept' as code, sum(weight*(ftemp12+ptemp12)) as margin from Sample")
y<-rbind(margin,marginstate[-1,],marginitem[-1,])[,2]
A<-t(model.matrix(~state+itemcode,data=count07))

x0=count07$margin
spree<-ipfp(y=y, A=A, x0=x0, tol = .Machine$double.eps, maxit = 1000,verbose = FALSE, full = FALSE)
image(-A)
length(spree)

REspree <-(spree-count12$margin)/count12$margin
REdirect<-(direct12$margin-count12$margin)/count12$margin
hist(REspree)
hist(REdirect)
boxplot(cbind(Direct=REdirect, Spree=REspree))
plot(REspree,REdirect)
plot(spree,x0,pch=19,cex=.5,col="blue")
abline(0,1,lwd=2)

}
\keyword{datasets}
