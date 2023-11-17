set.seed(200)
exact<-function(lambda,alpha,beta,N){
  lambda_aux<-lambda
  t_k<-0
  h<-numeric(N)
  for (i in 1:N)
  {
    
    u_1<-runif(1)
    u_2<-runif(1)
    t1<-t_k-log(u_1)/lambda
    t2<-t_k-log(1+beta/(lambda_aux+alpha-lambda)*log(u_2))/beta
    t2<-t_k-1/beta*log(1+beta*(lambda_aux+alpha-lambda)**(-1)*log(u_2))
    
    
    t_prev<-t_k
    t_k<-min(t1,t2)
    h[i]<-t_k
    lambda_aux<-lambda+(lambda_aux+alpha-lambda)*exp(-beta*(t_k-t_prev))
  }
  
  
  
  return(h)
    
  
}
a<-exact(1.2,1.7,1.8,10)
