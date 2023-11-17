library(ggplot2)
library(dplyr)
datos1<-read.csv("D:/Hawkes Process/Sismo ago-10 oct 2017 todas magnitudes.csv")
df1<-data.frame(as.Date(datos1$Fecha))

colnames(df1)<-c("Fecha")

#df1<-datos1 %>% filter(Fecha<='2017-10-10')
s1<-df1 %>% group_by(Fecha) %>% summarise(n=n())
s1<-data.frame(s1)
s1$Fecha=as.Date(s1$Fecha)
ggplot(data=s1, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity")+theme(plot.title = element_text(hjust=.5))+ 
  theme(axis.text.x = element_text( vjust = 0.5)) +ylab("# Sismos") + theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))


datos<-read.csv("D:/Hawkes Process/Tiempos De llegada Simulados 4 grados.csv", fileEncoding="latin1")
df<-datos
df<-datos %>% filter(tiempos_sin_hora<='2018-01-01')
s<-df %>% group_by(tiempos_sin_hora) %>% summarise(n=n())
s<-data.frame(s)
s$Fecha=as.Date(s$tiempos_sin_hora)


ggplot(data=s, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity")+ ggtitle(label='Sismos Simulados México ago-dic 2017')+theme(plot.title = element_text(hjust=.5))+ 
  theme(axis.text.x = element_text( vjust = 0.5))+scale_y_continuous(limits = c(0, 150)) 




datos1<-read.csv("D:/Hawkes Process/Sismo ago-15 may 2023 magnitudes m o igual 4.csv")
df1<-datos1
df1<-datos1
df1$Fecha=as.Date(df1$Fecha)
s1<-df1 %>% group_by(Fecha) %>% summarise(n=n())
s1<-data.frame(s1)
s1$Fecha=as.Date(s1$Fecha)
ggplot(data=s1, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity")+theme(plot.title = element_text(hjust=.5))+ 
  theme(axis.text.x = element_text( vjust = 0.5)) +ylab("# Sismos") + theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))  


datos<-read.csv("D:/Hawkes Process/Sismo ago-10 oct 2017 todas magnitudes.csv")
df<-datos
df<-datos %>% filter(Fecha<='2018-10-10')
s<-df %>% group_by(Fecha) %>% summarise(n=n())
s<-data.frame(s)
s$Fecha<-as.Date(s$Fecha)

ggplot(data=s, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity")+ 
  theme(axis.text.x = element_text( vjust = 0.5))+ylab("# Sismos")



df<-datos
df<-datos %>% filter( between(as.Date(Fecha), as.Date('2020-01-01'), as.Date('2020-10-01')))
s<-df %>% group_by(Fecha) %>% summarise(n=n())
s<-data.frame(s)
s$Fecha=as.Date(s$Fecha)
p<-ggplot(data=s, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity",fill="black")+ ggtitle('Sismos México ago-dic 2017')+xlab('Mes')+ylab('Número de sismos')+ theme(panel.border = element_rect(linetype = "dashed", fill = NA))+ theme(panel.background = element_rect(fill = "white", colour = "grey50"))+theme(plot.title = element_text(hjust=.5))
p

#2020
datos1<-read.csv("D:/Hawkes Process/Base de datos Sismos 2020 2 S.csv")
df1<-datos1
s1<-df1 %>% group_by(Fecha_formato_R) %>% summarise(n=n())
s1<-data.frame(s1)
s1$Fecha=as.Date(s1$Fecha_formato_R)
ggplot(data=s1, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity")+theme(plot.title = element_text(hjust=.5))+ 
  theme(axis.text.x = element_text( vjust = 0.5)) +ylab("# Sismos") + theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))  

#Simulados

datos<-read.csv("D:/Hawkes Process/Tiempos De llegada Simulados 2020 1 2s.csv")
df<-datos
df$t_simulados_R
s<-df %>% group_by(t_simulados_R) %>% summarise(n=n())
s<-data.frame(s)
s$Fecha=as.Date(s$t_simulados_R)


ggplot(data=s, aes(x=Fecha, y=n)) +
  geom_bar(stat="identity")+ ggtitle(label='Sismos Simulados México 2020 2S')+theme(plot.title = element_text(hjust=.5))+ 
  theme(axis.text.x = element_text( vjust = 0.5))+scale_y_continuous(limits = c(0, 100)) 

citation("ggpl")
