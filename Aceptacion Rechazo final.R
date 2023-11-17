
#M?todo utilizando ggplot2

library(ggplot2)

f2<-function(x){
  3*(x**(2))}

integrate(f2,0,1)

p<-ggplot(data.frame(x=c(0, 1)), aes(x)) + stat_function(fun=f2,color='blue',size=1)
prev<-p+geom_hline(yintercept=3, color='red',size=1.5)
c<-3
#Simulaci?n

set.seed(100)
n<-100
i<-1
rechazos<-0
Aceptados<-numeric(n)
Rechazados<-numeric(n)


u<-runif(n) #probabilidades 0,1
y<-runif(n,0,1) #realizaciones uniformes en el intervalo de inter?s (0,1)


for (i in 1:n){
  if(u[i]*c*1<=f2(y[i]))
  {
    Aceptados[i]<-y[i]
    
  }
  else 
    Rechazados[i]<-y[i]
  
}

aceptados_prev<-cbind(Aceptados,u)

aceptados_fin<-data.frame(aceptados_prev[Aceptados>0,])

rechazados_prev<-cbind(Rechazados,u)
rechazados_fin<-data.frame(rechazados_prev[Rechazados>0,])


y_aceptados_fin<-data.frame(cbind(aceptados_fin$Aceptados,c*aceptados_fin$u))
colnames(y_aceptados_fin)<-c("x","y")


y_rechazados_fin<-data.frame(cbind(rechazados_fin$Rechazados,c*rechazados_fin$u))
colnames(y_rechazados_fin)<-c("x","y")

prev
prev+geom_point(data=y_aceptados_fin, aes(x=x,y=y),colour='purple')+geom_point(data=y_rechazados_fin, aes(x=x,y=y),colour='red')+ theme_classic()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
