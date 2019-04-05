#EXERCICE1

MCtraj = function(n){
  U=runif(n,0,2)
  h=2*U*U
  res=cumsum(h)/(1:n)  
  return(res)
}

y=MCtraj(1000)
x=seq(1,1000)
plot(x,y)

MCtraj2 = function(n){
  U=runif(n,0,2)
  h=(2*U*U)*(2*U*U)
  res=cumsum(h)/(1:n)  
  return(res)
}

IC = function(n){
  U=runif(n,0,2)
  h=2*U*U
  In=mean(h)
  sigma=sqrt(var(h))
  Icinf=In-1.96*sigma/sqrt(n)
  Icsup=In+1.96*sigma/sqrt(n)
  return(list(s1=Icinf,s2=Icsup))
}

n=1000
X=runif(n,0,2)
Ichap=rep(NA,n)
Imin=rep(NA,n)
Imax=rep(NA,n)
for(k in 1:n){
  Ichap[k]=mean(2*X[1:k]^2)
  Imin[k]=Ichap[k]-1.96*sqrt(var(2*X[1:k]^2)/k)
  Imax[k]=Ichap[k]+1.96*sqrt(var(2*X[1:k]^2)/k)
}
plot(1:n,Ichap)
lines(1:n,Imin)
lines(1:n,Imax)

#EXERCICE2

MCtraj.unif = function(n){
  U=runif(n,0,2)
  h=2*exp(-U*U)
  print(var(h))
  res=cumsum(h)/(1:n)  
  return(res)
}
y=MCtraj.unif(1000)
x=seq(1,1000)

MCtraj.norm = function(n){
  U=rnorm(n)
  h=sqrt(2*pi)*exp(-(U*U)/2)*(U>=0)*(U<=2)
  print(var(h))
  res=cumsum(h)/(1:n)  
  return(res)
}

a=MCtraj.norm(1000)

plot(x,y,t='l',ylim=c(0,2))
lines(x,a,t='l',col='red')
legend("topright", c("Uniforme", "Normale"),fill=1:2)

a=rep(NA,100)
b=rep(NA,100)
for (i in 1:100){
  a[i]=MCtraj.norm(1000)
  b[i]=MCtraj.unif(1000)
}
mean(a)
mean(b)

n=1000
X=runif(n,0,2)
Ichap=rep(NA,n)
Imin=rep(NA,n)
Imax=rep(NA,n)
for(k in 1:n){
  Ichap[k]=mean(sqrt(2*pi)*exp(-(X[1:k]^2)/2))
  Imin[k]=Ichap[k]-1.96*sqrt(var(sqrt(2*pi)*exp(-(X[1:k]^2)/2))/k)
  Imax[k]=Ichap[k]+1.96*sqrt(var(sqrt(2*pi)*exp(-(X[1:k]^2)/2))/k)
}
plot(1:n,Ichap)
lines(1:n,Imin)
lines(1:n,Imax)

