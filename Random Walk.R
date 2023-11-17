library(ggplot2)

RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift

P1<-RW(100,0,0,0.007)
P2<-RW(100,0,0,0.007)
plot(P1, main="Random Walk without Drift", 
     xlab="T",ylab="S", ylim=c(-.5,.5),
     typ='l', col="red")

x <- seq(0, 100, by = 1)

P1 <- c(0, P1)
df <- data.frame(cbind(x, P1))


ggplot(data=df, aes(x=x, y=P1)) +
  geom_line(color="black") + xlab("T") + ylab("S") + theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) 


# Brownian Motion

BM <- function(N, x0, mu, variance) {
  
  t<-seq(0, N, by = .001)
  z<-cumsum(rnorm(n= length(t), mean=0, 
                  sd=sqrt(variance)))
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift

P1<-BM(100,0,0,0.007)

x <- seq(0, 100, by = .001)

df <- data.frame(cbind(x, P1))


ggplot(data=df, aes(x=x, y=P1)) +
  geom_line(color="black") + xlab("T") + ylab("S") + theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

# poisson Process

#set.seed(1998)
library(tidyverse)
#Definición de variables en nuestro modelo
u=100 #capital inicial
library(EnvStats)
#Simulamos 10 tiempos de arribo
lambda=.5 #tau~exp(1/2)
Tarribo<-NULL
Tarribo[1]<-rexp(1,lambda)
#Queremos un t hasta 10
t<-50
x<-seq(0,50,by=.05)# Definición del eje X
k=1
while(Tarribo[k]<t){
  Tarribo[k+1]<-Tarribo[k]+rexp(1,lambda)
  k<-k+1
}

Tarribo
TarriboP<-NULL
for (i in 1:length(Tarribo)){
  TarriboP[2*(i-1)+1]<-Tarribo[i]
  TarriboP[2*(i-1)+2]<-Tarribo[i]
}

Tarribo<-c(0,Tarribo)

TarriboP<-c(0,TarriboP)
length(TarriboP)
N<-NULL
for (i in 1:(length(Tarribo))){
  N[2*(i-1)+1]<-(i-1)
  N[2*(i-1)+2]<-i
}
N<-N[-length(Tarribo)*2]
N<-N[-(length(Tarribo)*2-1)]
N<-c(0,N)
N
plot(TarriboP,N,type="l")

Tarribo
df<-data.frame(TarriboP,N)
library(ggplot2)
ggplot(df,aes(x=TarriboP))+geom_line(aes(y=N))+theme(plot.title = element_text(hjust=.5))+
  xlab("T")+ylab("S") + xlim(0,50)+ theme_classic() +theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

