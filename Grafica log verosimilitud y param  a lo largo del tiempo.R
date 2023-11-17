library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("kassambara/easyGgplot2",force=TRUE)
library(easyGgplot2)
library("tidyverse")
install.packages("reshape")
library(reshape)
datos<-read.csv("D:/Hawkes Process/log_likelihood_starting_parameters sim 2020 2s param 1 2000.csv")

df<-data.frame(cbind(datos$it,datos$log_verosimilitud,datos$log_verosimilitud_2,datos$log_verosimilitud_3,datos$log_verosimilitud_4))
colnames(df)<-c("it","log1","log2","log3","log4")

df <- melt(df, id.vars = "it")

ln<-ggplot(df, aes(x = it, y = value, color = variable),size=1) +
  geom_point(size=1.2) +ggtitle("Evolución log-verosimilitud")+theme(plot.title = element_text(hjust=.5)) + ylim (c(34000, 35000)) +xlim(c(0,2000))+ xlab("Iteración")+ylab("log-versosimilitud")

ln

df<-data.frame(cbind(datos$it,datos$lambda,datos$lambda_2,datos$lambda_3,datos$lambda_4))
colnames(df)<-c("it","l1","l2","l3","l4")

df <- melt(df, id.vars = "it")

lambda<- ggplot(df, aes(x = it, y = value, color = variable)) +
  geom_point(size=1.2) +ggtitle("Evolución lambda")+theme(plot.title = element_text(hjust=.5))+ ylim (c(0, 100))+ xlab("Iteración")+ ylab("lambda")

lambda
df<-data.frame(cbind(datos$it,datos$alpha,datos$alpha_2,datos$alpha_3,datos$alpha_4))
colnames(df)<-c("it","a1","a2","a3","a4")

df <- melt(df, id.vars = "it")

alpha<- ggplot(df, aes(x = it, y = value, color = variable)) +
  geom_point(size=1.2)+ xlab("Iteración")+ ylab("alpha")+ xlim(c(0,2000))+ ylim (c(0, 15))+ggtitle("Evolución alpha")+theme(plot.title = element_text(hjust=.5))

alpha
df<-data.frame(cbind(datos$it,datos$beta,datos$beta_2,datos$beta_3,datos$beta_4))
colnames(df)<-c("it","b1","b2","b3","b4")

df <- melt(df, id.vars = "it")

beta<- ggplot(df, aes(x = it, y = value, color = variable),size=2) +
  geom_point(size=1.2)+ xlab("Iteración")+ ylab("beta")+ xlim(c(0,2000))+ ylim (c(0, 15))+ggtitle("Evolución beta")+theme(plot.title = element_text(hjust=.5))

beta
ggplot2.multiplot(ln,lambda,alpha,beta,cols=2)

