
install.packages("ggmap")
library(ggplot2)
library(ggmap)
library(httr)    
set_config(use_proxy(url="10.3.100.207",port=8080))
options(timeout = max(100000, getOption("timeout")))

mexico_map<-get_stamenmap(bbox=c(left=-109.501, bottom=12.329, right=-89.121, top=22.573),maptype = "terrain",zoom=7)

df<-read.csv("D:/Hawkes Process/Sismo ago-10 oct 2017 todas magnitudes.csv")
df2<-NULL
df2<-df

ggmap(mexico_map)+geom_point(data=df2,aes(x=Longitud,y=Latitud,size=Magnitud),colour = "blue" ,alpha=.5)+xlab("Longitud")+ylab("Latitud")
colnames(df2) +geom_point(aes(x=-100,y=15),colour="red",alpha=.5)

df<-read.csv("D:/Hawkes Process/Sismos Mexico grafica limpio.csv")

df2<-df

ggmap(mexico_map)+geom_point(data=df2,aes(x=Longitud,y=Latitud,size=Magnitud),colour = "blue" ,alpha=.5)+xlab("Longitud")+ylab("Latitud")+ggtitle("Sismos México 2022")+theme(plot.title = element_text(hjust=.5))
colnames(df2)


mexico_map<-get_stamenmap(bbox=c(left=-109.501, bottom=12.329, right=-89.121, top=22.573),maptype = "terrain",zoom=7)
df<-read.csv("D:/Hawkes Process/Sismo ago-15 may 2023 magnitudes m o igual 4.csv")
df2<-NULL
df2<-df

ggmap(mexico_map)+geom_point(data=df2,aes(x=Longitud,y=Latitud,size=Magnitud),colour = "blue" ,alpha=.5)+xlab("Longitud")+ylab("Latitud")+geom_point(shape=17,aes(x=-100,y=15),colour="red",size=5,fill="red")


mexico_map<-get_stamenmap(bbox=c(left=-109.501, bottom=12.329, right=-89.121, top=22.573),maptype = "terrain",zoom=7)
df<-read.csv("D:/Hawkes Process/Base de datos Sismos 2020 2 S.csv")
df2<-NULL
df2<-df

ggmap(mexico_map)+geom_point(data=df2,aes(x=Longitud,y=Latitud,size=Magnitud),colour = "blue" ,alpha=.5)+xlab("Longitud")+ylab("Latitud")

#+geom_point(shape=17,aes(x=-100,y=15),colour="red",size=5,fill="red")
