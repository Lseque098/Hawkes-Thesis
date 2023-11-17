install.packages("Rmpfr")
library(Rmpfr)
responsibilities<-function(t,theta){
  lambda<-theta[1]
  alpha<-theta[2]
  beta<-theta[3]
  N<-length(t)
  Z<-matrix(0,N,N)

  for (i in 1:N){
    if (i==1){
      Z[i,1]<-1
    
    }
    else{
      Z[i,1]<-lambda
      suma<-lambda
      for (j in 2:i){
        Z[i, j] = alpha*exp(-1*beta*(t[i]-t[j-1]))
        suma = suma+ Z[i, j]
      }
      for (j in 1: i){
        Z[i, j] = Z[i, j]/suma
      }
    }
  }
  return (Z)
  
}

iter_em<-function(t,lambda,alpha,beta){
  t<-ts  
  N = length(t)

    dif<-T_ - t

    # E step
    resp = responsibilities(t,c(lambda,alpha,beta))
    T_<-max(t)

    # M step: actualizar lambda
    lambda = sum(resp[,1])/T_

    # M step: actualizar alpha
    numer = sum(resp[,2:length(resp[2,])])
    denom = sum(1 - exp(-1*beta*(dif)))
    alpha = beta*numer/denom

    # M step: actualizar beta
    
    
    sum(1 - exp(-1*beta*(dif)))/beta
    
    
    numer = sum(1 - exp(-1*beta*(dif)))/beta - sum((dif)*exp(-1*beta*(dif)))
    
    denom=0
    for (j in 2: (N)){
      denom = denom + sum((t[j] - t[1:(j-1)])*resp[j,2:j])
    }
    
    beta = alpha*numer/denom
    

  return(c(lambda,alpha,beta))
  
}
as.numeric(T_-t)


exp_em<-function(t,lambda_in,alpha_in,beta_in,iter){
  lambda<-lambda_in
  alpha<-alpha_in
  beta<-beta_in
  
  lambda_hist<-NULL
  alpha_hist<-NULL
  beta_hist<-NULL
  i<-1
  for (i in 1: iter){
    theta<- iter_em(t,lambda,alpha,beta)
    
    lambda_hist[i]<-lambda
    alpha_hist[i]<-alpha
    beta_hist[i]<-beta

    
    lambda<-theta[1]
    alpha<-theta[2]
    beta<-theta[3]
    
    i<-i+1

  }
  
  return(c(lambda,alpha,beta))
  
  
  
}
  
  
datos_sismo<-read.csv("D:/Hawkes Process/sismos japon.csv")
datos_sismo$fecha=paste(datos_sismo$Year,datos_sismo$Month,datos_sismo$Day, sep="-")
datos_sismo$fecha=as.Date(datos_sismo$fecha)

datos_sismo$fecha_hora <- as.POSIXct(paste(datos_sismo$fecha, datos_sismo$Time),  # Add hours, minutes & seconds
                            format = "%Y-%m-%d %H:%M:%S")
datos_sismo<-datos_sismo[order(datos_sismo$fecha_hora), ]

ts<- (datos_sismo$fecha_hora-as.POSIXct("2015-01-01"))/24
ts<-as.numeric(ts)

x<-responsibilities(ts,c(1,2,3))

exp_em(ts,1,2,3,100)

iter_em(ts,1.360298  ,1.501650  ,2.404193)
x<-as.data.frame(x)
data_in<-x$V1
z<-matrix(1,2,4)

length(z[1,])
sum(z[,2:length(z[1,])])

t<-c(1,2,3)
sum(t[1:2])
10-t
