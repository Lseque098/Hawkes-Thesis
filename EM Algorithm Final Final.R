intensity <- function(lambda, alpha, beta, t,t_interes){
  N<-length(t)
  i<-1
  f<-0
  lambda_aux<-0
  while (t[i]<t_interes && i<=N){
    lambda_aux<-lambda_aux+alpha*exp(-1*beta*(t_interes-t[i]))
    i<-i+1
  }
  
  lambda_fin<-lambda_aux+lambda
  return (lambda_fin)
}
mu<- function(t,lambda,alpha,beta){
  return(alpha*exp(-1*beta*t))
}
mu_alt<- function(a,lambda,alpha,beta){
  return(alpha*exp(-1*beta*(x-a)))
}

mu_alt

loglikelihood <-function(lambda,alpha,beta,t, z_0, z_ij){
  n<-length(t)
  integral<-NULL
  T_max<-max(t)
  for (i in 1:n){
   
    integral[i]<- (alpha*exp(-1*beta*t[i])/beta)*(exp(-1*beta*t[i])-exp(-1*beta*T_max))
    
  }
  s<-NULL
  for (j in 1 :)
  
  
  l<- -1*lambda*T_max-sum(integral)+log(lambda)*sum(z_0)
  
  
  
}
em <- function(lambda, alpha, beta, num_iteraciones,llegadas){
  a<-1
  r<-0
  lambda_inicial<-lambda
  alpha_inicial<-alpha
  beta_inicial<-beta
  n<-as.numeric(length(llegadas))
  intensidad_<-NULL
  T_<-max(llegadas)
  z_0<-NULL
  z_i<-NULL
  
  for(i in 1:num_iteraciones){
    
    for (j in 1: n){
      intensidad_[j]<-intensity(lambda,alpha,beta, llegadas,llegadas[j])
      
      z_0[j]<-lambda/(intensity(lambda,alpha,beta,llegadas,llegadas[j]))
      
    }
  
  
  y<-1
  llegadas
  int_<-NULL
  for (j in 2:n)
    {
      for (x in 1: (j-1)){
    
        int_[y]<-(intensity(lambda,alpha,beta,llegadas,(llegadas[j]-llegadas[x])))
        z_i[y]<-mu((llegadas[j]-llegadas[x]),lambda,alpha,beta)/(intensity(lambda,alpha,beta,llegadas,(llegadas[j]-llegadas[x])))
        y<-y+1
        
      }
      
  }
  }
  return(z_i)
  
  
}
t<-c(1,1.01,4,5.5)
   lambda<-1
   alpha<-1
   beta<-1.1
   n<-3 
   llegadas<-t
   num_iteraciones<-10
 em(lambda,alpha,beta,10,t)    
   
    
  
  