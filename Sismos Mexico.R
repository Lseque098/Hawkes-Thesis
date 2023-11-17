
library(ggplot2)
datos_sismo<-data.frame(read.csv("D:/Hawkes Process/Sismo R.csv",header=TRUE))
ggplot(data=datos_sismo, aes(x=as.Date(Fecha,"%d/%m/%Y") , y=Conteo_Sismos)) +
  geom_bar(stat="identity",fill="blue")+xlab("Fecha")+ggtitle("Sismos Sur México")+theme(plot.title = element_text(hjust=.5))+
  scale_x_date(date_breaks = "15 day",date_labels = "%Y-%m-%d")+geom_vline(xintercept=as.Date("22/09/2022","%d/%m/%Y"),lwd=1,colour="red")+
  geom_vline(xintercept=as.Date("19/09/2022","%d/%m/%Y"),lwd=1,colour="green")


1.35**(1/12)
