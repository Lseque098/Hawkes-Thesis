#set.seed(1998)
library(tidyverse)

lambda=1#tau~exp(1)
t=10

Tarribo<-sort(Aceptar)

x<-seq(0,10,by=.05)


int<-sin(x)*sin(x) 


plot(x,int,type="l")

Tarribo
df<-data.frame(x,int)
library(ggplot2)
ggplot(df,aes(x))+geom_line(aes(y=int))+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo")+ ylab("intensidad") + xlim (c(0,10.5))

