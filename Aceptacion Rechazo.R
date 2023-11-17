f<-function(x){
  x*exp(-x)/(1-exp(-2)-2*exp(-2))}
integrate(f,0,2)
curve(f,0,2,lwd=5,col=4)
abline(h=c(0,f(1)),v=c(0,1,2))
f(1)
g<-function(x){(1/2)*(x>0)*(x<2)}
c<-2*f(1)
curve(c*g(x),.01,1.99,lwd=5,col=2,add=T)


#Simulación

n<-100
i<-1
rechazos<-0
Y<-numeric(n)
while (i<n){
  u<-runif(1)
  y<-runif(1,0,2)
  if(u*c*g(y)<=f(y))
  {
    Y[i]<-y
    i<-i+1
    points(y,u*c*g(y),col='blue',pch=16)
    
  }
  else{
  rechazos<- rechazos+1
  
  
  points(y,u*c*g(y),col='red',pch=16)
  
  
  }
}

#Método utilizando ggplot2

library(ggplot2)

f2<-function(x){
  4*(x**(3))}

integrate(f2,0,1)

p<-ggplot(data.frame(x=c(0, 1)), aes(x)) + stat_function(fun=f2,color='blue',size=1)
prev<-p+geom_hline(yintercept=4, color='red',size=1.5)
c<-4
#Simulación

set.seed(100)
n<-100
i<-1
rechazos<-0
Aceptados<-numeric(n)
Rechazados<-numeric(n)


u<-runif(n) #probabilidades 0,1
y<-runif(n,0,1) #realizaciones uniformes en el intervalo de interés (0,1)


for (i in 1:n){
  if(u[i]*c*1<=f2(y[i]))
  {
    Aceptados[i]<-y[i]
    
  }
  else 
    Rechazados[i]<-y[i]
  
}

while (i<n){
  u<-runif(1)
  y<-runif(1,0,2)
  if(u*c*g(y)<=f(y))
  {
    Y[i]<-y
    i<-i+1
    points(y,u*c*g(y),col='blue',pch=16)
    
  }
  else{
    rechazos<- rechazos+1
    
    
    points(y,u*c*g(y),col='red',pch=16)
  }
}

aceptados_prev<-cbind(Aceptados,u)

aceptados_fin<-data.frame(aceptados_prev[Aceptados>0,])

rechazados_prev<-cbind(Rechazados,u)
rechazados_fin<-data.frame(rechazados_prev[Rechazados>0,])


y_aceptados_fin<-data.frame(cbind(aceptados_fin$Aceptados,c*aceptados_fin$u))
colnames(y_aceptados_fin)<-c("x","y")


y_rechazados_fin<-data.frame(cbind(rechazados_fin$Rechazados,c*rechazados_fin$u))
colnames(y_rechazados_fin)<-c("X_Rechazado","Y_Rechazado")

prev
prev+geom_point(data=y_aceptados_fin, aes(x=x,y=y),colour='purple')

