
#set.seed(1998)
library(tidyverse)

lambda=2 #tau~exp(1/2)
Tarribo<-NULL
Tarribo[1]<-rexp(1,lambda)
#Queremos un t hasta 10
t<-10
x<-seq(0,12,by=.05)# Definición del eje X
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

Tarribo2<- Tarribo[seq(2,length(Tarribo),by=1)]
Tarribo2<-c(Tarribo2,rep(-1,length(TarriboP)-length(Tarribo2)))
Ypoint<- seq(1,max(N), by=1)
Ypoint<-c(Ypoint,rep(-1,length(N)-length(Ypoint)))


df<-data.frame(TarriboP,N)
library(ggplot2)
ggplot(df,aes(x=TarriboP))+geom_line(aes(y=N))+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo de Arribo")+ylab("# de Arribo")+geom_point(aes(x=Tarribo2,y=Ypoint),colour="blue", size = 2) +xlim(0,t)+
  theme_classic() +theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
