#Exercice1:
#2
x = rbinom(5,1,0.5)
xbar = sum(x)/5
#3
X = rep(1:100)
for(i in 1:100){
  X[i]=sum(rbinom(5,1,0.5))/5
}
#4
mean(abs(X-0.5)>=0.1)#estime P(|Xn-0.5|>=e)
#5
Rn=rep(1:5)
cpt=0
C=c(10,25,50,100,200)
X = rep(1:100)

for(n in C){

  for(i in 1:100){
    X[i]=sum(rbinom(n,1,0.5))/n
  }
  cpt=cpt+1
  Rn[cpt] = mean(abs(X-0.5)>=0.1)
}
Y=(0.5*0.5)/(C*0.1*0.1)
plot(C,Rn,type='l',beside=TRUE,main='bernoulli')
points(C,Y,type='l')

#EXERCICE2:
#1
seq_x=rexp(1000,2)
#2
par(mfrow=c(1,2))
plot(1:n,cumsum(seq_x)/1:n,type= 'l',ylab="moyenne empirique",col=1,cex.axis=2,cex.lab=2,ylim=c(0,1),main='Exponentielle')

#3
for(i in 2:20) {
  seq_x=rexp(n,2)
  lines(1:n,cumsum(seq_x)/1:n,type= 'l',col=i) 
}
abline(a=0.5,b=0)

#4
seq_x=rcauchy(1000)

plot(1:n,cumsum(seq_x)/1:n,type= 'l',ylab="moyenne empirique",col=1,cex.axis=2,cex.lab=2,ylim=c(0,1),main='Loi Cauchy')
for(i in 2:10) {
  seq_x=rcauchy(n,2)
  lines(1:n,cumsum(seq_x)/1:n,type= 'l',col=i) 
}

#EXERCICE3:
#1
seq_x=rbinom(1000,1,0.5)
plot(1:n,cumsum(seq_x)/1:n,type= 'l',ylab="moyenne empirique",col=1,cex.axis=2,cex.lab=2,ylim=c(0,1),main='Bernoulli')
for(i in 2:10) {
  seq_x=rbinom(1000,1,0.5)
  lines(1:n,cumsum(seq_x)/1:n,type= 'l',col=i) 
}

#2
seq_x=rbinom(1000,1,0.5)
par(mfrow=c(1,1))
plot(1:n,(cumsum(seq_x)/1:n)-0.5,type= 'l',ylab="moyenne empirique",col=1,cex.axis=2,cex.lab=2,ylim=c(-1,1),main='Bernoulli')
for(i in 2:10) {
  seq_x=rbinom(1000,1,0.5)
  lines(1:n,(cumsum(seq_x)/1:n)-0.5,type= 'l',col=i) 
}
n=1000
c=1:n
X=(1.96*sqrt(0.5*0.5))/sqrt(c)
Y=(-1.96*sqrt(0.5*0.5))/sqrt(c)
lines(1:n,Y,type= 'l',col='red')
lines(1:n,X,type= 'l',col='red')

Z=(1.96*sqrt(0.5*0.5))/c
A=(-1.96*sqrt(0.5*0.5))/c
lines(1:n,Z,type= 'l',col='green')
lines(1:n,A,type= 'l',col='green')

D=(1.96*sqrt(0.5*0.5))/log(c)
B=(-1.96*sqrt(0.5*0.5))/log(c)
lines(1:n,D,type= 'l',col='blue')
lines(1:n,B,type= 'l',col='blue')

#Exercice4:
tclexpo = function(n,a){
  for(i in 1:1000){
    seq_x=rexp(n,a)
    X[i]=(sqrt(n)*(cumsum(seq_x)/1:n)-(1/a))/(sqrt(1/(a*a)))
  }
  return(X)
}
C=c(2,10,50,100)
par(mfrow=c(2,2))
for(i in C){
  hist(tclexpo(i,2),probability =TRUE)
}
