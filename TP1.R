#EXERCICE1:
#1
dbinom(0,5,0.6)
dbinom(1,5,0.6)
#2
x<-0:5
par(mfrow=c(1,2))
plot(x,dbinom(x,5,0.6),type="h",beside=TRUE,xlab="pk",ylab="xk",main = "B(5,0.6)")
#3
pbinom(1,5,0.6)
pbinom(4,5,0.6)
#4
plot(x,pbinom(x,5,0.6),type="s",beside=TRUE,xlab="pk",ylab="xk",main = "Fonction de repartition de B(5,0.6)")
#5
qbinom(0.25,5,0.6)
#6
rbinom(20,5,0.6)

#Exercice2:
#1
#a
x<-seq(from=-5,to=5,by=0.01)
par(mfrow=c(1,2))
plot(x,dnorm(x),xlab="t",ylab="f(t)",main = "Fonction de densité de N(0,1)")
#b
plot(x,pnorm(x),type="s",beside=TRUE,xlab="t",ylab="F(t)",type='l',main = "Fonction de repartition de N(0,1)")
#c
pnorm(0.11)
pnorm(-0.51)
pnorm(1.5)-pnorm(0.5)

qnorm(0.975)
qnorm(0.2358)
qnorm((1+0.33)/2)
#2
par(mfrow=c(1,1))
plot(x,dnorm(x),xlab="t",ylab="f(t)",type='l',main = "Fonction de densité de N(0,1) et N(3,0.2^2)")
points(x,dnorm(x,mean=3,sd=0.2),col="red",type='l')

pnorm(3.29,mean=3,sd=0.2)
qnorm(0.975,mean=3,sd=0.2)

#EXERCICE3:
#1
approx_bin_normale <- function(n,p)par(mfrow=c(1,3))
approx_bin_normale(10,3/4)
approx_bin_normale(50,3/4)
approx_bin_normale(100,3/4)

{
  x=0:n
  plot(x,dbinom(x,n,p),type="h")
  lines(x,dnorm(x,n*p,sqrt(n*p*(1-p))),col="red",type='l')
  return()
}
par(mfrow=c(2,3))
approx_bin_normale(10,3/4)
approx_bin_normale(50,3/4)
approx_bin_normale(100,3/4)

#2
approx_bin_pois <- function(n,p)
{
  M=matrix(0,nrow=2,ncol=n+1)
  k=0:n
  M[1,]=dbinom(k,n,p)
  M[2,]=dpois(k,n*p)
  barplot(M,beside=TRUE,col=c('red','blue'),legend=c('binomiale','poisson'))
  return()
}
approx_bin_pois(10,0.5)
approx_bin_pois(50,1/10)
approx_bin_pois(100,1/20)

