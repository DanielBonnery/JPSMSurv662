library(Isi2015Sae)
data(baseball);attach(baseball)
n = nrow(baseball)
# variable of interest, y
hat.P.bar = mean(baseball$hat.P)
#estimate of shi
shi_i = hat.P.bar*(1-hat.P.bar)/45
#PR-method
#OLS of beta
beta_ols = lm(hat.P~x1, data = baseball)$coeff
#design matrix
x_mtx = cbind(1, baseball$x1)#calculate h_jj
h_jj_mid = matrix(apply(sapply(1:n, function(i) x_mtx[i,]%*%t(x_mtx[i,])), 1, sum), 2, 2)
h_jj = sapply(1:n, function(i)t(x_mtx[i,])%*%solve(h_jj_mid)%*%x_mtx[i,])
#calcualte A tilde
A_tld = 1/(n-1)*(sum((hat.P-x_mtx%*%beta_ols)^2)-sum(shi_i*(1-h_jj)))
#estimate of A
A.hat.pr = max(0, A_tld)
#estimate of B
B.hat.pr = shi_i/(shi_i+A.hat.pr)
#EBLUPs
ELUP.pr = (1-B.hat.pr)*hat.P + B.hat.pr*x_mtx%*%beta_ols
#ASD
ASD.pr = sum((ELUP.pr - baseball$P)^2)
#adjusted REML
#calculate sum of residual square
resq = sum((hat.P - x_mtx%*%beta_ols)^2)
#adjusted REML of A
A.hat.reml1 = (-(6*shi_i-resq/2) + sqrt((6*shi_i-resq/2)^2+28*shi_i^2))/14
A.hat.reml2 = (-(4*shi_i-resq/2) + sqrt((4*shi_i-resq/2)^2+48*shi_i^2))/12
#estimate of B
B.hat.reml1 = shi_i/(shi_i+A.hat.reml1)
B.hat.reml2 = shi_i/(shi_i+A.hat.reml2)
#EBLUPs
ELUP.reml1 = (1-B.hat.reml1)*hat.P + B.hat.reml1*x_mtx%*%beta_ols
ELUP.reml2 = (1-B.hat.reml2)*hat.P + B.hat.reml2*x_mtx%*%beta_ols
#ASD
ASD.reml1 = sum((ELUP.reml1 - baseball$P)^2)
ASD.reml2 = sum((ELUP.reml2 - baseball$P)^2)
#plot EBLUPs versus true values
plot.data = data.frame(Estimate = rep(c("PR", "REML1", "REML2"), each = 18),
                       EBLUP = c(ELUP.pr, ELUP.reml1, ELUP.reml2),
                       P = rep(baseball$P, 3))
#library("ggplot")
#p = ggplot(plot.data, aes(P, EBLUP))+ geom_abline(intercept = 0, slope = 1)
#p+geom_point(aes(colour = factor(Estimate)))beta_A.hat = beta_ols
#LR_A = det(t(x_mtx)%*%x_mtx)^(-1/2)*exp(-1/2*sum((hat.P - x_mtx%*%beta_A.hat)^2))
#mse_PR
#clacualte g1, g2, and g3
g1i = function(A){A*shi_i/(A+shi_i)}
g2i = function(A){
  p1 = shi_i^2/(A+shi_i)^2
  p2.mid = matrix(apply(sapply(1:n, function(i) x_mtx[i,]%*%t(x_mtx[i,])/(A+shi_i)), 1, sum), 2, 2)
  p2 = t(x_mtx[1,])%*%solve(p2.mid)%*%x_mtx[1,]
  p1*p2
}
g3i = 2*shi_i^2/(n*(A.hat.pr+shi_i))
p1_pr = g1i(A.hat.pr) + g2i(A.hat.pr)
mse.pr = p1_pr + g3i
beta_ols = lm(hat.P~x1, data = baseball)$coeff
#write a function to provide PR estimates when u th area is deleted.
PR_JRR = function(dim, y,x_matrix){
  beta_u = solve(t(x_matrix)%*%x_matrix)%*%(t(x_matrix)%*%y)
  h_jj_mid.u = matrix(apply(sapply(1:dim, function(i) x_matrix[i,]%*%t(x_matrix[i,])), 1, sum), 2, 2)
  h_jj.u = sapply(1:dim, function(i)t(x_matrix[i,])%*%solve(h_jj_mid.u)%*%x_matrix[i,])
  A_tld.u = 1/(dim-1)*(sum(y-x_matrix%*%beta_u)^2)-sum(shi_i*(1-h_jj.u))
  A.u = max(0, A_tld.u)
  B.u = shi_i/(shi_i+A.u)
  return(list(A.u = A.u, beta_u = beta_u, B.u = B.u))
}
#estimate of A when u th area is deleted.
A_u = sapply(1:n, function(i) PR_JRR(n-1, hat.P[-i], x_mtx[-i,])$A.u)
#estimate of B when u th area is deleted.
B_u = sapply(1:n, function(i) PR_JRR(n-1, hat.P[-i], x_mtx[-i,])$B.u)
#estimate of beta when u th area is deleted.
beta_u = sapply(1:n, function(i) PR_JRR(n-1, hat.P[-i], x_mtx[-i,])$beta_u)
#clacualte mse_JLWp1_jlw = g1i(A.hat.pr)
p1_jlw = g1i(A.hat.pr)
p2_jlw = - (n-1)/n*sum(sapply(1:n, function(i)g1i(A_u[i])-g1i(A.hat.pr)))
theta.tld = sapply(1:n, function(u) B_u[u]*x_mtx[1,]%*%beta_u[,u]+(1-B_u[u])*hat.P[1])
p3_jlw = (n-1)/n*sum((theta.tld-ELUP.pr[1])^2)
mse.JLW = p1_jlw + p2_jlw + p3_jlw
#calcualte mse_CL using two versions of w_u
p2_CL = -(n-1)/n*sum(sapply(1:n, function(u) g1i(A_u[u])+g2i(A_u[u])))
mse.CL1 = n*p1_pr + p2_CL + p3_jlw
mse.CL2 = p1_pr - sum(h_jj*(sapply(1:n, function(u) g1i(A_u[u]) + g2i(A_u[u]) - g1i(A.hat.pr) - g2i(A.hat.pr))))+sum(h_jj*(theta.tld-ELUP.pr[1])^2)
#mse using boot strap
#calcualte estimated mean
mu_i = x_mtx%*%beta_ols
#claim variables
A.star = rep(0, 1000)
ELUP.star = rep(0, 1000)
beta.star = matrix(0, 1000, 2)
diff.theta = rep(0, 1000)
seed = runif(1000, -1000, 1000)
#bootstrap
set.seed(1)
bootrep<-
replicate(1000,(function(){
  #generate two levels of errors
  vb_i = rnorm(n, 0, A.hat.pr)
  eb_i = rnorm(n, 0, shi_i)
  #generate bootstrap sample
  y_i = mu_i + vb_i + eb_i
  #ols based on bootstrap sample
  beta.star_j = solve(t(x_mtx)%*%x_mtx)%*%(t(x_mtx)%*%y_i)
  beta.star[j,] = beta.star_j
  #estimate of A using PR method based on bootstrap sample
  A_tld_j = 1/(n-1)*(sum((y_i-x_mtx%*%beta.star_j)^2)-sum(shi_i*(1-h_jj)))
  A.star = max(0, A_tld_j)#estimate of B on bootstrap sample
  B.star_j = shi_i/(shi_i+A.star)
  #EBLUPs based on bootstrap sample
  ELUP.star = (1-B.star_j)*y_i[1] + B.star_j*x_mtx[1,]%*%beta.star_j
  #the difference between EBLUPs based on bootstrap sample and the generated theta_i
  diff.theta = ELUP.star - mu_i[1] - vb_i[1]
c(A_tld_j=A_tld_j,A.star=A.star,B.star_j=B.star_j,ELUP.star=ELUP.star,diff.theta=diff.theta)
  })())
mse.BL = mean(sapply(1:1000, function(i) g1i(bootrep["A.star",i])+g2i(bootrep["A.star",i]))) + mean((bootrep["ELUP.star",] - ELUP.pr[1])^2)
mse.boot = mean((bootrep["diff.theta",])^2)

