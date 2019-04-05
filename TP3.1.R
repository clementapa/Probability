#Exercice 1
n=1000
p=0.3
U=runif(n) #renvoie n simulation d une loi uniforme
X=1*(U<=p)
barplot(table(X)/n)   #table compte le nomre de fois 1 et 0 apparaissent  # Barplot pour les variables discretes
table(X)

bernp=function(n,p){
  U=runif(n) #renvoie n simulation d une loi uniforme
  X=1*(U<=p)
  return (X)
}

barplot(table(bernp(1000,0.3))/1000)   #table compte le nomre de fois 1 et 0 apparaissent  # Barplot pour les variables discretes


binnp1=function(n,p){
  return(sum(bernp(n,p)))
}

binnpk=function(n,p,k){
  X=rep(0,k)
  for (i in 1:k)	{
    X[i]=binnp1(n,p)
  }
  return (X)
}

#barplot(table(binnpk(100,0.3,10000))/10000)
X=binnpk(100,0.3,10000)
barplot(rbind(table(X)/10000,dbinom(min(X):max(X),100,0.3)),beside=T)


#Exercice 2
a = c(0.5,1,1.5,2)
p = c(1/8,1/2,1/8,1/4) 
p_decale = c(0, p[1:3])
u =runif(1)
ind = (u > cumsum(p_decale)) & (u < cumsum(p))
X = a[which(ind==1)]

n=1000
X=rep(NA, n)
for (i in 1:n)
{ u =runif(1)
ind = (u > cumsum(p_decale)) & (u <=cumsum(p))
X[i] = a[which(ind==1)]
}

#> table(X)/n
#X
#0.5     1   1.5     2 
#0.128 0.493 0.131 0.248 
barplot(rbind(table(X)/n,p),beside=T,col=1:2)
legend("topright", c("simulation", "p"),fill=1:2)
#hist(X)  # mais hist pas vraiment fait pour représenter un diagramme en baton.
