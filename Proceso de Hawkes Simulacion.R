# Euler scheme

sim_Hawkes_Euler<- function(lambda0, a, beta, alpha, fin_data, n)
{
  #Inputs 
  #- a parametro de reversion a la media
  # beta tasa de decaimiento exponencial
  # lambda0 intensidad inicial
  # alpha parametro de la dist exponencial
  # fin_data limite de la simulacion
  # n numero de escalones en el tiempo
  
  #Outputs 
  #lambda intensidad del salto
  #numero de saltos
  
  
  if (lambda0<=a | beta<=alpha){
    return('lambda0 must be greater than a and beta must be greater than alpha') 
    
  }
  if (fin_data<0){
    return('final date must be a positive number') }
  
  if (n<0){
    return('number of time steps must be a positive number') 
  }
  # incrementos de tiempo
  
  dt <- fin_data/n
  
  # Numeros pseudoniformes entre 0 y 1 
  U <- runif(n,0,1)
  
  # generar matriz para el decaimiento
  lambda = c(rep(0,n))
  
  dNt = lambda
  dLt = lambda
  
  
  #% lambda inicial para cada una de las simulaciones 
  
  lambda[1] = lambda0
  
  tiempos<-c(rep(0,n))
  
  # simulate trajectories
  for (j in c(2:n)){
    
    # Una indicadora que ve si hay salto o no
    dNt[j] = (U[j] < lambda[j-1]*dt)
    
    if(dNt[j]==1){
      tiempos[j]=1
    } else{
      tiempos[j]=0
    }
    
    # jump process
    
    dLt[j] = dNt[j]*alpha
    #  intensity process
    dlambda = beta*(a - lambda[j-1])*dt + dLt[j]
    lambda[j] = lambda[j-1] + dlambda
  }
  # Numero de saltos
  N_saltos  = sum(dNt)
  
  return(c(lambda,tiempos,N_saltos))
}
lambda0 = 11 #intensidad inicial
a = 1       # parametro de reversion
beta = 10    #tasa de decaimiento
alpha = 5   # magnitud del salto
Tiempo = 10      # fin_data
n = 250*Tiempo  

tot<-sim_Hawkes_Euler(lambda0,a,beta,alpha,Tiempo,n)
lambda<-tot[1:n]
llegadas<-tot[(n+1):(2*n)]

n_saltos<-tot[((2*n)+1)]
x<-seq(Tiempo/n,Tiempo, by= Tiempo/n)

df<-data.frame(x,lambda)

ggplot(df,aes(x))+geom_line(aes(y=lambda))+ggtitle("Intensidad Proceso de Hawkes")+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo")+ ylab("intensidad")+scale_x_continuous(breaks = seq(0,Tiempo,by=2))

x<-seq(Tiempo/n,Tiempo, by= Tiempo/n)

Tarribo<-x[llegadas==1]

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

df<-data.frame(TarriboP,N)
library(ggplot2)
ggplot(df,aes(x=TarriboP))+geom_line(aes(y=N))+ggtitle("Proceso Poisson No Homogeneo")+theme(plot.title = element_text(hjust=.5))+
  xlab("Tiempo de Arribo")+ylab("# de Arribos")+xlim(0,Tiempo)+ylim(0,n_saltos)+scale_y_continuous(breaks=seq(0,n_saltos, by=2))
