data(lfstate)
attach(lfstate[order(lfstate$ST,lfstate$Year,lfstate$month),])
vardir<-diag(StdCPSUn^2)
so
sae2::eblupDyn(CPSun~CPSpop+Cntwoer,
              D=53,
              T=288,
              vardir)
