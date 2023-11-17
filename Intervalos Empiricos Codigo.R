library(ggplot2)
library(devtools)
library("tidyverse")
library(reshape)
library(dplyr)
datos<-read.csv("D:/Hawkes Process/Intervalo 2017 2023 m 4 poisson homogeneo.csv")
datos<-data.frame(cbind(datos$Fecha,datos$Lim_sup,datos$Real,datos$Lim_inf))
colnames(datos)<-c("Fecha","Lim_sup","Real","Lim_inf")
datos$Fecha<-as.Date(datos$Fecha)
datos$Lim_sup<-as.integer(datos$Lim_sup)
datos$Lim_inf<-as.integer(datos$Lim_inf)
datos$Real<-as.integer(datos$Real)

datos2<-datos %>% filter(Real<Lim_inf | Real>Lim_sup)
df <- melt(datos, id.vars = "Fecha")
df$Fecha<-as.Date(df$Fecha)
df$value<-as.integer(df$value)

mean(as.numeric(datos$Lim_sup)) - mean(as.numeric(datos$Lim_inf))

ggplot(df, aes(x = Fecha, y = value, color = variable),size=1) +
  geom_point(size=.9)+theme(plot.title = element_text(hjust=.5))+xlab("Fecha")+ylab("# Sismos") +
  theme_classic() +theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.text = element_text(size = 14), legend.title = element_text(size = 15)) 


ggplot() + geom_pointrange(data=datos2, mapping=aes(x=Fecha, y=Real, ymin=Lim_inf, ymax=Lim_sup), lwd=1, size=1, color="red", fill="white", shape=22)+
  xlab("Fecha")+ylab("# Sismos") + ylim(c(0,500)) +theme_classic() +theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.text = element_text(size = 14))


