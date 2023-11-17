T_<-12
set.seed(100)
t<-0
P<-NULL
M<-1
Aceptados<-NULL
Rechazados<-NULL
n<-100

tiempos<- c(0,rexp(n,M))
tiempos_fin<-numeric(n)

f2<-function(x){
  sin(x)**2
}
for (i in 2:n){
  tiempos_fin[i]<- tiempos_fin[i-1]+tiempos[i]
}

a<-tiempos_fin[tiempos_fin<T_]

prob<-runif(length(a),0,M)

vec_fin<-data.frame(cbind(a,prob,f2(a)))

Aceptados<- vec_fin[vec_fin$prob<vec_fin$V3,]
Rechazados<- vec_fin[vec_fin$prob>=vec_fin$V3,]

Aceptados<-data.frame(Aceptados$a, Aceptados$prob)
Rechazados<-data.frame(Rechazados$a, Rechazados$prob)

p<-ggplot(data.frame(x=c(0, T_)), aes(x)) + stat_function(fun=f2,color='blue',size=1)
prev<-p+geom_hline(yintercept=1, color='red',size=1)

colnames(Aceptados)<-c("x","y")
colnames(Rechazados)<-c("x","y")

prev+geom_point(data=Aceptados, aes(x=x,y=y),colour='purple',size=2)+geom_point(data=Rechazados, aes(x=x,y=y),colour='red',size=2)+
  ggtitle("Algoritmo de Aceptación Rechazo para un proceso Poisson no Homogéneo")+theme(plot.title = element_text(hjust=.5))





