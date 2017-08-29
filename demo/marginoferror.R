npoints<-100
n<-c(10,100,1000)
p=rep(seq(0,1,length.out=npoints),length(n))
n<-rep(n,each=npoints)
library(ggplot2)
plot1<-ggplot(data = data.frame(p,n,y=qnorm(0.975)*sqrt(p*(1-p)/n),Size=factor(n)),
              aes(x=p, y=y, group=Size, colour = Size))+
  geom_line()
