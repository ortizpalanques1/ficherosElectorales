library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(forcats)
library(rgdal)
load("huescar.RData")
mapaAndalucia <- readOGR(dsn = "mapas", layer = "da08_seccion_censal", use_iconv=TRUE, encoding = "UTF-8")
huescar04 <- mapaAndalucia[grep("Huéscar", mapaAndalucia@data$MUNICIPIO),]
save(huescar03,huescar04,partyColor,fuenteIngresoHuescar,fuenteIngresoS,ingresoTramos,ingresoTramosHuescar,rentaMediaHuescar,rentaMediaS,huescarM20190428,huescarM20191110,huescarS20190428,huescarS20191110,mapaAndalucia,file = "municipio/data/huescar.RData")

# INGRESO POR TRAMOS DE EUROS
colorTramos <- c("#024959","#bf5b04","#a66249","#8c0303")
tramosIngreso <- ingresoTramos %>% 
  dplyr::filter(id=="101") %>% 
  dplyr::select(ends_with("2017")) %>% 
  dplyr::select(starts_with("menos")) 
tramosIngresoHuescar <- ingresoTramosHuescar %>% 
  dplyr::select(ends_with("2017")) %>% 
  dplyr::select(starts_with("menos")) 
tramosText <- c("menos de\n5000€", "de 5001\na 7500€", "de 7501\na 10000€", "más de\n10001€")
tramosSecc <- c(tramosIngreso$menos50002017, tramosIngreso$menos75002017-tramosIngreso$menos50002017,tramosIngreso$menos100002017-tramosIngreso$menos75002017, 100-tramosIngreso$menos100002017)
tramosHues <- c(tramosIngresoHuescar$menos50002017, tramosIngresoHuescar$menos75002017-tramosIngresoHuescar$menos50002017,tramosIngresoHuescar$menos100002017-tramosIngresoHuescar$menos75002017, 100-tramosIngresoHuescar$menos100002017)
tramosGraph <- cbind("Indicador"=tramosText,"Seccion"=tramosSecc,"Huescar"=tramosHues)
tramosGraph <- as.data.frame(tramosGraph)
tramosGraph$Seccion <- as.numeric(as.character(tramosGraph$Seccion))
tramosGraph$Huescar <- as.numeric(as.character(tramosGraph$Huescar))
tramosGraph <- pivot_longer(tramosGraph,-Indicador,names_to = "Sector",values_to = "Tramos")
tramosColor <- tibble(tramosText,colorTramos)
tramosGraph$Indicador <- factor(tramosGraph$Indicador,levels=unique(tramosColor$tramosText))
tramosGraph$Indicador <- fct_rev(tramosGraph$Indicador)
tramosSeccCum <- cumsum(tramosSecc)
tramosHuesCum <- cumsum(tramosHues)
tramosCUMSUM <- c(matrix(c(tramosSeccCum, tramosHuesCum), 2, byrow = T))
tramosGraph$cumulative <- tramosCUMSUM
tramosGraph$Positio <- tramosGraph$cumulative-(tramosGraph$Tramos*0.5)
graphico06 <- ggplot(tramosGraph, aes(x=Sector,y=Tramos,fill=Indicador))+
  geom_col()+
  geom_text(aes(y=tramosGraph$Positio,label=Tramos), color="white", fontface="bold")+
  labs(
    title="Distribuión del Ingreso (Porcentaje)"
  )+
  scale_fill_manual(values = tramosColor$colorTramos)+
  coord_flip()+
  theme(
    plot.title = element_text(size = 14, face = "bold", color="black"),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid=element_blank(),
    panel.border=element_blank(),
    panel.background=element_rect(colour="white", fill="white"),
    plot.background = element_rect(colour="white", fill="white"),
    legend.title = element_blank()
  )



graphMun2019 <- huescarS20190428 %>%
  dplyr::filter(ID2019==101) %>% 
  dplyr::select(2:14) %>% 
  gather("Partido", "Votos", 1:13) %>% 
  mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
  arrange(desc(Votos)) %>% 
  mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
  mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
  group_by(Partido) %>% 
  summarise(Votos=sum(Votos),Porciento=sum(Porciento))
graphMun2019 <- left_join(graphMun2019,partyColor, by=c("Partido"="nominaColor"))
colorMun <- setNames(as.character(graphMun2019$ideaColor), graphMun2019$Partido)
graphico01 <- ggplot(graphMun2019, aes(x=reorder(Partido, -Porciento), y=Porciento, fill=Partido, label=Porciento)) +
  geom_col()+
  geom_text(aes(y=10),size=10, color="black",fontface="bold")+
  scale_fill_manual(values = colorMun)+
  theme(axis.text.x=element_text(face="bold", colour="black", size=12),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        panel.background=element_rect(colour="white", fill="white"),
        plot.background = element_rect(colour="white", fill="white"),
        legend.position = "none")



#############################################
output$comparo <- renderPlot({
  #AÑADIDO
  if(is.null(estaComunidad())){
    elLugarS3 <- "101"
  } else {
    elLugarS3 <- estaComunidad()
  }
  graphMun2019 <- huescarS20190428 %>%
    dplyr::filter(ID2019==elLugarS3) %>% 
    dplyr::select(2:14) %>% 
    gather("Partido", "Votos", 1:13) %>% 
    mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
    arrange(desc(Votos)) %>% 
    mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
    mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
    group_by(Partido) %>% 
    summarise(Votos=sum(Votos),Porciento=sum(Porciento))
  graphMun2019 <- left_join(graphMun2019,partyColor, by=c("Partido"="nominaColor"))
  graphGen2019 <- huescarS20191110 %>%
    dplyr::filter(ID2019==elLugarS3) %>% 
    dplyr::select(2:15) %>% 
    gather("Partido", "Votos", 1:14) %>% 
    mutate(Porciento=round((Votos/sum(Votos)*100),2)) %>% 
    arrange(desc(Votos)) %>% 
    mutate(Decisor=ifelse(as.numeric(row.names(.))<6,row.names(.),6)) %>% 
    mutate(Partido=ifelse(Decisor<6,Partido,"Otros")) %>% 
    group_by(Partido) %>% 
    summarise(Votos=sum(Votos),Porciento=sum(Porciento))
  graphGen2019 <- left_join(graphGen2019,partyColor, by=c("Partido"="nominaColor"))
  #FIN AÑADIDO
graphComparar01 <- graphMun2019
graphComparar01$Partido <- ifelse(graphComparar01$Partido=="PODEMOSIU","PODEMOS",graphComparar01$Partido)
graphComparar02 <- graphGen2019
graphComparar02$Partido <- ifelse(graphComparar02$Partido=="PODEMOSIULVCA","PODEMOS",graphComparar02$Partido)
graphComparar <- full_join(graphComparar01,graphComparar02,by="Partido")
graphComparar <- graphComparar %>% 
  mutate(VOTOS=Votos.y-Votos.x,
         PORCENT=Porciento.y-Porciento.x) 
colorGen <- setNames(as.character(graphComparar$ideaColor.y), graphComparar$Partido)
graphico03 <- ggplot(graphComparar)+
  geom_col(aes(x=reorder(Partido,-Porciento.x), y=PORCENT, fill=Partido), alpha=0.7)+
  geom_text(aes(x=Partido, y=-1, label=PORCENT),size=6, color="black",fontface="bold")+
  scale_fill_manual(values = colorGen)+
  theme(axis.text.x=element_text(face="bold", colour="black", size=12),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        panel.background=element_rect(colour="white", fill="white"),
        #plot.title = element_text(size=20,face="bold"),
        plot.background = element_rect(colour="white", fill="white"),
        legend.position = "none")
  graphico03
}, height = 250)




###################################################
diferencia <- partGenX-partMunX
output$PartDif <- renderValueBox({
  valueBox(
    subtitle = "DIFERENCIA EN LA PARTICIPACIÓN (Elección general - elección municipal)",
    paste0(diferencia, "%"), 
    icon = shiny::icon("bar-chart"),
    color = "green"
  )
})

library(sp)
mapaAndalucia <- readOGR(dsn = "mapas", layer = "da08_seccion_censal", use_iconv=TRUE, encoding = "UTF-8")
mapaAndalucia84 <- spTransform(mapaAndalucia,CRS("+init=epsg:4326"))
huescar04 <- mapaAndalucia84[grep("Huéscar", mapaAndalucia@data$MUNICIPIO),]
leaflet() %>%
  addTiles() %>%
  addPolygons(data = huescar04
  )
library(tmap)
qtm(huescar04)
load("municipio/data/huescar.RData")
rm(huescar04)
rm(mapaAndalucia)
save(huescar04,partyColor,fuenteIngresoHuescar,fuenteIngresoS,ingresoTramos,ingresoTramosHuescar,rentaMediaHuescar,rentaMediaS,huescarM20190428,huescarM20191110,huescarS20190428,huescarS20191110,file = "municipio/data/huescar.RData")
save(huescar04,partyColor,fuenteIngresoHuescar,fuenteIngresoS,ingresoTramos,ingresoTramosHuescar,rentaMediaHuescar,rentaMediaS,huescarM20190428,huescarM20191110,huescarS20190428,huescarS20191110,file = "huescar.RData")
huescar04@data$id <- as.character(100+as.numeric(as.character(huescar04@data$COD_SEC)))


# PROPROCIONAR EL PORCENTAJE DE ELECTORES POR SECCION
totalInscritos <- sum(huescarS20191110$censoINE, rm.na=TRUE)
proporcion <- huescarS20191110 %>% 
  dplyr::filter(ID2019==101) %>%
  mutate(Propor=round(censoINE/totalInscritos,2)) %>% 
  pull(Propor)
  
# 
test <- setNames(as.list(c(1: (ncol(huescarS20191110)-5))), c(colnames(huescarS20191110[2:(ncol(huescarS20191110)-4)])))
test1 <- c(colnames(huescarS20191110[2:(ncol(huescarS20191110)-4)]))

elPartido <- names(test)[as.integer(13)]
partidoIngreso <- huescarS20191110 %>% 
  dplyr::select(ID2019,elPartido,validos) %>% 
  mutate(estePartido=round(.[,2]/validos*100,2)) %>% 
  left_join(.,rentaMediaS[,c(1,5)], by=c("ID2019"="id"))
graphico07 <- ggplot(partidoIngreso,aes(x=RMH2017,y=estePartido[,1],size=(partidoIngreso[[2]])))+
  geom_point(colour="#8c0303", alpha=0.4)+
  xlim(min(partidoIngreso$RMH2017)-400, max(partidoIngreso$RMH2017)+400)+
  ylab("Porcentaje")+
  xlab("Renta media por Hogar 2017")+
  scale_size(range = c(6, 30))+
  theme(
    axis.text = element_text(color = "black"),
    legend.position="none",
    panel.grid.major =element_line(colour="#bf5b04"),
    #panel.border=element_rect(colour="#bf5b04"),
    panel.background=element_rect(colour="#bf5b04", fill="white"),
    plot.background = element_rect(colour="#bf5b04", fill="white"))+
  geom_text(label=paste0("Sec.",substring(partidoIngreso$ID2019,2,3),"\n Votos:",partidoIngreso[[2]]), size=4, fontface="bold")

as.name(elPartido)


library(mongolite)

options(mongodb = list(
  "host" = "cluster0.5jypi.mongodb.net",
  "username" = "marco",
  "password" = "Palanques13"
))
databaseName <- "aborrar"
collectionName <- "naves"
loadData <- function(thisName) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s?retryWrites=true&w=majority",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
  assign(deparse(substitute(thisName)), data, envir=.GlobalEnv)
}
loadData(parrot)
