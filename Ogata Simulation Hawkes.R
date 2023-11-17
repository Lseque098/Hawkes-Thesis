sim_Hawkes_Euler<- function(lambda0, a, beta, alpha, fin_data, n)
{
  #Inputs 
  #- a parametro de reversion a la media
  # beta tasa de decaimiento exponencial
  # lambda0 intensidad inicial
  # alpha parametro de la dist exponencial
  # fin_data limite de la simulacion
  # n numero de escalones en el tiempo (número de llegadas)
  
  #Outputs 
  #lambda intensidad del salto
  #numero de saltos
  # incrementos de tiempo
  
  dt <- fin_data/n
  
  # Numeros  entre 0 y 1 
  U <- runif(n,0,1)
  
  # para generar matriz para el decaimiento
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
ogata<-function(lambda, alpha, beta, T_)
{
  lambda_aux<-lambda
  t<-0
  tiempos<-NULL
  var<-TRUE
  rechazos<-NULL
  y_rechazos<-NULL
  t_aceptados<-NULL
  y_aceptados<-NULL
  y_real<-NULL
  i_rech<-1
  i_acep<-1
  while(var==TRUE){
    M<-lambda_aux
    deltat<-rexp(1)/M
    t<-t+deltat
    if(t>T_){
      var<-FALSE
      break
    }
    lambda_aux<-lambda+(lambda_aux-lambda)*exp(-1*beta*deltat)
    u<-M*runif(1)
    if(u>lambda_aux){
      rechazos[i_rech]<-t 
      y_rechazos[i_rech]
      i_rech<-i_rech+1
    }
    else{
      
      t_aceptados[i_acep]<-t
      y_aceptados[i_acep]<-u
      y_real[i_acep]<-lambda_aux
      i_acep<-i_acep+1
    }
    
    lambda_aux<-lambda_aux+alpha
    
    
  }
  
  
 A<- data.frame(cbind(t_aceptados,y_aceptados,y_real))
  return(A)
    
}
t_arribos_fin<-ogata(1,1,1.5,10)

write.csv(t_arribos_fin,"D:/Hawkes Process/Tests output/Intensidad_Ogata.csv")
