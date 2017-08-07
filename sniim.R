#Paqueteria necesaria:
# rJava, xlsxjars, XML, ggplot2

library(rJava)
library(xlsxjars)
library(XML)
# library(xlsx)

#Al importar se crearan problemas de lectura en simbolos usados en idioma espa√±ol, como la "√±"
#usar fileEncoding === datos<-read.table("swirl_temp/inmigintnalpry.csv", header=TRUE, sep=",", fileEncoding = "latin1")

#Se genera el cÛdigo para el link de descarga de datos basado en fecha especÌfica
# setwd("C:/Users/Chris/Documents/Rtrabajos")
baseURL<-"http://www.economia-sniim.gob.mx/NUEVO/Consultas/"
suffixURL1<-"MercadosNacionales/PreciosDeMercado/Agricolas/ResultadosConsultaFechaFrutasYHortalizas.aspx?fechaInicio="
suffixURL2<-"&fechaFinal="
suffixURL3<-"&ProductoId=-1&OrigenId=-1&Origen=Todos&DestinoId=220&Destino=Quer%C3%A9taro:%20Mercado%20de%20Abasto%20de%20Quer%C3%A9taro&PreciosPorId=2&RegistrosPorPagina=10000"
# Dia <- readline(prompt="Fecha de consulta - dia en dd:")
# Mes <- readline(prompt="Fecha de consulta - mes en mm:")
# Ano <- readline(prompt="Fecha de consulta - a;o en aaaa:")

#Se obtiene el dÌa de la semana en cuestiÛn
d.semana<-toupper(weekdays(Sys.Date()))
Dia<-format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d")
Mes<-format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%m")
Ano<-format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%Y")

#Se genera la fecha buscada en el formato legible para la busqueda en el link de descarga de datos
Fecha <- paste0(Dia,"/",Mes,"/",Ano)
url<-paste0(baseURL,suffixURL1,Fecha,suffixURL2,Fecha,suffixURL3)

#Descarga de datos
#url <- readline(prompt="URL: ")
x = readHTMLTable(url,skip.rows = 19,header = TRUE, which=1,encoding="UTF-8")
nombres<-c("Producto", "Calidad", "Presentacion", "Origen", "Precio_min", "Precio_max", "Precio_frec", "Obs")
names(x)<-nombres
str(x)
sindato<-which(is.na(x$Precio_frec)==T)
y<- x[-c(sindato),]
y<- y[-c(1),]

#GeneraciÛn de dataframe con los datos descargados
fech<-as.factor(rep(Fecha,length(y$Producto)))
z<-data.frame(fech, y$Producto, y$Calidad, y$Presentacion, y$Origen, y$Precio_min, y$Precio_max, y$Precio_frec, y$Obs)
nombres<-c("Fecha","Producto", "Calidad", "Presentacion", "Origen", "Precio_min", "Precio_max", "Precio_frec", "Obs")
names(z)<-nombres
z$Producto<-gsub("#","no.",z$Producto)
# z

#GeneraciÛn de archivo CSV con nombre aÒo-mes-dia.csv para su trazabilidad posterior
name_file<-paste0(getwd(),"/",Ano,"-",Mes,"-",Dia,".csv")
write.csv(z,file=name_file)

#Resumen de productos para infografÌa
#Productos seleccionados de acuerdo a la encuesta de consumo
data_info<-data.frame(as.vector(c("Aguacate Hass",
                        "LimÛn c/semilla no. 5",
                        "Calabacita Italiana",
                        "Chile JalapeÒo",
                        "Chile Poblano",
                        "Chile Serrano",
                        "Pepino",
                        "Tomate Saladette",
                        "Tomate Verde",
                        "Lechuga Romanita grande")))
colnames(data_info)<-c("V1")

# data_info<-as.vector(read.table("data_infografia.txt",sep = "\t",quote=""))
# data_info
# summary(z)
# nrow(data_info)
infograf<-0

#InfografÌa
for( i in 1:nrow(data_info)){
  infograf[i]=which(z$Producto == as.character(data_info$V1[i]))
  i=i+1
  }
# i
z_infogrf<-z[infograf,]
# class(z_infogrf)
z_infogrf<-as.data.frame(z_infogrf)
z_infogrf$Precio_max = as.double(levels(z_infogrf$Precio_max))[z_infogrf$Precio_max] # <-- converting 
# z_infogrf

library(ggplot2)
png(file = paste0("pmercado","_",Ano,"-",Mes,"-",Dia,".png"), bg = "transparent")
p1 <- ggplot(data = z_infogrf, aes(x = Producto, y = Precio_max,label=Precio_max)) + 
  geom_bar(stat = "identity", fill = "#552683") +
  ylab("Precio m·ximo") + xlab("Producto") + 
  ggtitle(sprintf("Precios de mercado #QRO %s %s/%s/%s",d.semana,Dia,Mes,Ano))+
  geom_text(size = 5, position = position_stack(vjust = 0.5),color=3, 
            fontface = "bold")
p1
p1+ coord_flip()+ theme(axis.text=element_text(size=12),
                      axis.title=element_text(size=14,face="bold"))+
  scale_y_continuous(labels = scales::dollar)
dev.off()
