#Gráfica Intensidad
library(ggplot2)
lambda=2
alpha= 5
beta=10
x<-NULL
df<-read.csv("D:/Hawkes Process/Tiempos De llegada Simulados parametros 1 alt 2 5 10.csv")
df<-data.frame(df$X0)
df$llegada<-1
colnames(df)<-c("Tiempo_llegada","llegada")
tiempos<-data.frame(df$Tiempo_llegada)
x$Tiempo_llegada<-data.frame(seq(0,10, by=.001))
x<-data.frame(x)
colnames(x)<-("Tiempo_llegada")
x$llegada<-0



df_final=rbind(x,df)  

df_final<-df_final[order(df_final$Tiempo_llegada),]


suma<-lambda
suma_xj<-0
vec<-NULL
for (i in c(1:length(df_final$Tiempo_llegada))){
  suma_xj<-0
  for (j in c (1:length(df$Tiempo_llegada))){
    z<-df$Tiempo_llegada[j]
    p<-df_final$Tiempo_llegada[i]
    if (p>=z){
      suma_xj<-suma_xj+alpha*exp(-1*beta*(p-z))
    }
    else{
      break
    }
  }
  suma<-lambda+suma_xj
  
  vec[i]<-suma
  
  
  
}

y<-data.frame(vec)
x<-data.frame(df_final$Tiempo_llegada)
df<-cbind(x,y)
colnames(df)=c("x","y")

ggplot(data=df, aes(x=x, y=y)) +
  geom_line()+geom_line(aes(y=lambda),linetype='dotted', col = 'red',linewidth=1)+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo")+ ylab("Intensidad")+ xlim(c(0,10))+ ylim (c(0, 20))


Tarribo<-tiempos$df.Tiempo_llegada

TarriboP<-NULL
for (i in 1:length(Tarribo)){
  TarriboP[2*(i-1)+1]<-Tarribo[i]
  TarriboP[2*(i-1)+2]<-Tarribo[i]
}
Tarribo<-c(0,Tarribo)
TarriboP<-c(0,TarriboP)

N<-NULL
for (i in 1:(length(Tarribo))){
  N[2*(i-1)+1]<-(i-1)
  N[2*(i-1)+2]<-i
}
N<-N[-length(Tarribo)*2]
N<-N[-(length(Tarribo)*2-1)]
N<-c(0,N)
df<-data.frame(TarriboP,N)
df<-df %>% filter(TarriboP<=10)
num<-max(df$N)
N<-c(N,num)
TarriboP<-c(TarriboP,10)
n_saltos<-max(N)
df<-data.frame(TarriboP,N)

df<-df %>% filter(TarriboP<=10)

ggplot(df,aes(x=TarriboP))+geom_line(aes(y=N))+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo de Arribo")+ylab("# de Arribos")

