library(ggplot2)
datos_covid<-data.frame(read.csv("D:/Hawkes Process/contagios covid.csv",header=TRUE))
ggplot(data=datos_covid, aes(x=as.Date(Fecha,"%d/%m/%Y") , y=Contagios)) +
  geom_bar(stat="identity",fill="blue")+xlab("Fecha")+ggtitle("Contagios Covid México")+theme(plot.title = element_text(hjust=.5))+
  scale_x_date(date_breaks = "4 month",date_labels = "%Y-%m-%d")
