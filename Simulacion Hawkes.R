#Gráfica Intensidad
library(ggplot2)
lambda=39.64153894
alpha= 3.72787277
beta=10.73261861
x<-NULL
df<-read.csv("D:/Hawkes Process/Tiempos de llegada intensidad 2020.csv")
df$llegada<-1
x$Tiempo_llegada<-data.frame(seq(0,max(df$Tiempo_llegada), by=.001))
x<-data.frame(x)
colnames(x)<-("Tiempo_llegada")
x$llegada<-0

 
df_final=rbind(x,df)  

df_final<-df_final[order(df_final$Tiempo_llegada),]
df_final$Tiempo_llegada[24]
df_final$Tiempo_llegada[25]


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
vec
lambda
alpha
beta


y<-data.frame(vec[1:1000])
x<-data.frame(df_final$Tiempo_llegada[1:1000])
df<-cbind(x,y)
colnames(df)=c("x","y")

ggplot(data=df, aes(x=x, y=y)) +
  geom_line()+geom_line(aes(y=lambda),linetype='dotted', col = 'red',linewidth=1)+ggtitle("Intensidad Proceso de Hawkes Sismos 1 de julio 2020")+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo")+ ylab("Intensidad")+ coord_cartesian(xlim =c(0, 1), ylim = c(0, 70))
