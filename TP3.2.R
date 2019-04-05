#EXERCICE3:

simulation <- function(n,lambda){
  U = runif(n)
  X = -log(1-U)/lambda
  return (X)
}

par(mfrow=c(1,2))
plot(density(simulation(1000,2)),beside=T)
lines(x,dexp(x,2)) #verifier 

hist(simulation(1000,2),prob = TRUE)
x<-seq(from=-1,to=5,by=0.01)
lines(x,dexp(x,2)) #verifier 

#EXERCICE4
f_densite <- function(x){
  y <-x *(x>=0)*(x<=1)+(2-x)*(x>1)*(x<=2)
  return (y)
}

x <- seq(from=-1,to=3,by=0.01)
y <- f_densite(x)
plot(x,y,type='l')

f_densite_approx <- function(n){
  x = runif(n)
  y = sqrt(2*x) *(x>=0)*(x<=0.5)+(2-sqrt(2-2*x))*(x>0.5)*(x<=1)
  return (y)
}

hist(f_densite_approx(1000),prob=TRUE)
lines(density(f_densite_approx(1000)))

#EXERCICE5

f <- function(x){
  y <- ((2/sqrt(2*pi))*exp(-(x*x)/2))*(x>=0)
  return(y)
}
x <- seq(from=-1,to=2,by=0.01)
plot(x,f(x),ylim=c(0,2),xlim=c(0,2),type="l")

g <- function(x){
  y <- exp(-x)*(x>=0)+0*(x<0)
  return(y)
}
lines(x,1.5*g(x),col="green")
legend("topleft",paste(c("g(x)","f(x)")),col=c("green","black"),lwd=1)

#c=1.5
h <- function(x){
  return(f(x)/g(x))
}
x <- seq(from=0,to=10,by=0.01)
plot(x,h(x),type='l')
legend("topright",paste("h(x)"),col="black",lwd=1)
#c=1.32

rejet <- function(n){
  vect=rep(NA,n)
  i=1
  while(i!=n+1){
    X=rexp(1,1)
    U=runif(1)
    if((sqrt(2*exp(1)/pi)*g(X)*U) <= f(X)){
      vect[i]=X
      i=i+1
    }
  }
  return(vect)
}
x <- seq(from=-1,to=5,by=0.01)
hist(rejet(100000))
plot(density(rejet(100000)),ylim=c(0,1))
lines(x,f(x),col="green")
legend("topleft",paste(c("f(x)","f(x) approx")),col=c("green","black"),lwd=1)

rejet2 <- function(n){
  vect=rep(NA,n)
  i=1
  Cpt=rep(0,n)
  while(i!=n+1){
    X=rexp(1,1)
    U=runif(1)
    Cpt[i]=Cpt[i]+1
    if((sqrt(2*exp(1)/pi)*g(X)*U) <= f(X)){
      vect[i]=X
      i=i+1
    }
  }
  return(list(s1=Cpt,s2=vect))
}
toto=rejet2(10000)
toto$s1
#loi geometrique
barplot(table(toto$s1))
mean(toto$s1) #proche de c

M=rbind(table(rgeom(10000,1/(sqrt(2*exp(1)/pi)))),table(toto$s1))
barplot(M,beside=T,col=1:2)
legend("topright", c("loi géometrique", "nb d'essais"),fill=1:2)

#4

rejet(1)$s2
U=runif(1)
eps=1*(U<=0.5)-1*(U>0.5)