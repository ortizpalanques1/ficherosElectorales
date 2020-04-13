#Librerias
library(readr)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(ggplot2)
# Traer archivo de base de datos
DISTRITOS2019 <- sqlFetch(conn,"DISTRITOS2019")
#Al venir de la base de datos, la columna de ID (que está hecha de números) es cambiada a numérica.
#Por ello toca nuevamente volverla a caracter.
DISTRITOS2019$ID2019 <- as.character(DISTRITOS2019$ID2019)
#Crear columnas en el fichero 03
X03021911$partidos <- substr(X03021911$X1,9,14)
X03021911$nombreCorto <- substr(X03021911$X1,15,64)
X03021911$nombreLargo <- substr(X03021911$X1,65,214)
X03021911$nombreCorto <- gsub(" ","",X03021911$nombreCorto)
X03021911$nombreCorto <- gsub("-","",X03021911$nombreCorto)
#Crear columnas en el fichero 09
X09021911$localidad <- substr(X09021911$X1,10,16)
X09021911$distrito <- substr(X09021911$X1,17,18)
X09021911$seccion <- substr(X09021911$X1,19,22)
X09021911$mesa <- substr(X09021911$X1,23,23)
X09021911$censoINE <- substr(X09021911$X1,24,30)
X09021911$blancos <- substr(X09021911$X1,66,72)
X09021911$nulos <- substr(X09021911$X1,73,79)
X09021911$validos <- substr(X09021911$X1,80,86)
X09021911$censoINE <- as.numeric(X09021911$censoINE)
X09021911$blancos <- as.numeric(X09021911$blancos)
X09021911$nulos <- as.numeric(X09021911$nulos)
X09021911$validos <- as.numeric(X09021911$validos)
X09021911$ID2019Mesa <- paste0(substr(X09021911$distrito,2,2),substr(X09021911$seccion,2,3),X09021911$mesa)
#Crear columnas fichero 10
X10021911$localidad <- substr(X10021911$X1,10,16)
X10021911$distrito <- substr(X10021911$X1,17,18)
X10021911$seccion <- substr(X10021911$X1,19,22)
X10021911$mesa <- substr(X10021911$X1,23,23)
X10021911$partidos <- substr(X10021911$X1,24,29)
X10021911$votos <- substr(X10021911$X1,30,36)
X10021911$votos <- as.numeric(X10021911$votos)
#Poner los nombres de los partidos y arreglar data.frame
X10021911 <- left_join(X10021911,X03021911[,2:3],by="partidos")
X10021911 <- X10021911[,c(2,3,4,5,8,7)]
#Extraer el municipio de interés de archivo X09021911
getafeTotales <- X09021911 %>% 
  filter(localidad=="1228065") %>% 
  select(6:10)
#Extraer el municipio de interés de archivo X10021911
getafe20191110 <- X10021911 %>%
  filter(localidad=="1228065") %>%
  mutate(ID2019=paste0(distrito,seccion)) %>%
  pivot_wider(names_from=nombreCorto, values_from=votos) %>%
  group_by(ID2019)
#Homogeneizar el ID en getafe20191110 a DISTRITO2019
getafe20191110$ID2019Mesa <- paste0(substr(getafe20191110$ID2019,2,2),substr(getafe20191110$ID2019,4,5),getafe20191110$mesa)
getafe20191110$ID2019 <- paste0(substr(getafe20191110$ID2019,2,2),substr(getafe20191110$ID2019,4,5))
#Añadir los datos totales
getafe20191110 <- left_join(getafe20191110,getafeTotales,by="ID2019Mesa")
#Añadir los distritos a getafe20191110
getafe20191110 <- left_join(getafe20191110,DISTRITOS2019,by="ID2019")
#Agrupar por distrito
getafe20191110Distrito <- getafe20191110 %>% 
  select(5:24) %>% 
  group_by(DISTRITO) %>% 
  summarise(
    Cs=sum(Cs),
    VOX=sum(VOX),
    PP=sum(PP),
    PSOE=sum(PSOE),
    PODEMOSIU=sum(PODEMOSIU),
    `PUM+J`=sum(`PUM+J`),
    PACMA=sum(PACMA),
    MASM=sum(MASPAMSEQ),
    PH=sum(PH),
    PCTE=sum(PCTE),
    R0=sum(RECORTESCEROGVPCASTC),
    PCPE=sum(PCPE),
    censoINE=sum(censoINE),
    nulos=sum(nulos),
    blancos=sum(blancos),
    validos=sum(validos)
    ) 
#Agrupar por seccion
getafe20191110Seccion <- getafe20191110 %>% 
  select(5:24) %>% 
  group_by(ID2019) %>% 
  summarise(
    Cs=sum(Cs),
    VOX=sum(VOX),
    PP=sum(PP),
    PSOE=sum(PSOE),
    PODEMOSIU=sum(PODEMOSIU),
    `PUM+J`=sum(`PUM+J`),
    PACMA=sum(PACMA),
    MASM=sum(MASPAMSEQ),
    PH=sum(PH),
    PCTE=sum(PCTE),
    R0=sum(RECORTESCEROGVPCASTC),
    PCPE=sum(PCPE),
    censoINE=sum(censoINE),
    nulos=sum(nulos),
    blancos=sum(blancos),
    validos=sum(validos)
  ) 
# El gráfico
## Construir el data frame y las variables necesarias
### Seleccionamos la fila con el distrito municipal de nuestro interés
### Nombre del distrito
xx <- "Valle Arriba"
dataGraficante <- getafe20191110Distrito %>% 
  filter(DISTRITO==xx) %>% 
  select(2:13) 
dataGraficante <- as.data.frame(t(dataGraficante))
dataGraficante$nomina <- rownames(dataGraficante)
dataGraficante <- dataGraficante[order(dataGraficante$V1, decreasing = TRUE),]
dataGraficanteAdd <- data.frame("V1"=sum(dataGraficante$V1[9:nrow(dataGraficante)]), "nomina"="Otros")
dataGraficante <- rbind(dataGraficante[1:8,],dataGraficanteAdd[1,])
percenta <- round((dataGraficante$V1/sum(dataGraficante$V1))*100,2)
breaks <- cumsum(porcentaje)
semiSegmenta <- breaks-(percenta/2)
nomina <- c("apple","pear","orange","eggplant","lettuce","cherry","banana","carrot","mango")
colores <- c("coral","chartreuse3","darkorange","darkorchid","chartreuse1","darkred","darkorange1","darksalmon","yellow")

# Gráfico para ocho partidos
gg.gauge <- function(breaks,porcentaje) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(0,breaks[1]),aes(x,y),fill=colores[1])+
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill=colores[2])+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill=colores[3])+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill=colores[4])+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill=colores[5])+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill=colores[6])+
    geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill=colores[7])+
    geom_polygon(data=get.poly(breaks[7],breaks[8]),aes(x,y),fill=colores[8])+
    geom_polygon(data=get.poly(breaks[8],breaks[9]),aes(x,y),fill=colores[9])+
    geom_text(data=as.data.frame(porcentaje), size=ifelse(porcentaje>10,2,1), fontface="bold", vjust=0,
              aes(x=0.75*cos(pi*(1-semiSegmenta/100)),y=0.75*sin(pi*(1-semiSegmenta/100)),label=paste0(nomina,": ",porcentaje,"%")))+
    coord_fixed()+
    labs(title=paste0("Porcentaje por partidos: ",xx),
         caption = "Fuente: Min.In., Cálculos propios")+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
  glipho<-paste0(xx,"porcentaje",".png")
  ggsave(glipho,width=10, height=6,units="cm")
}
gg.gauge(breaks,percenta)