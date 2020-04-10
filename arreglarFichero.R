#Librerias
library(readr)
library(dplyr)
library(tidyr)
#Tomar ficheros
X03021911 <- read_csv("C:/Users/ortiz/OneDrive/Escritorio/eleccion20191110/03021911.DAT", 
                      col_names = FALSE, locale = locale(encoding = "ASCII"))
X09021911 <- read_csv("C:/Users/ortiz/OneDrive/Escritorio/eleccion20191110/09021911.DAT", 
                      col_names = FALSE)
X10021911 <- read_csv("C:/Users/ortiz/OneDrive/Escritorio/eleccion20191110/10021911.DAT", 
                      col_names = FALSE)
# CARGA DE LIBRERIAS Y CONEXION----
library(RODBC)
dsn_driver <- "{IBM DB2 ODBC DRIVER}"
dsn_database <- "BLUDB"            
dsn_hostname <- "dashdb-txn-sbox-yp-lon02-02.services.eu-gb.bluemix.net" 
dsn_port <- "50000"                
dsn_protocol <- "TCPIP"            
dsn_uid <- "nkg63522"        
dsn_pwd <- "fsck70shc3-7n58l" 
conn_path <- paste("DRIVER=",dsn_driver,
                   ";DATABASE=",dsn_database,
                   ";HOSTNAME=",dsn_hostname,
                   ";PORT=",dsn_port,
                   ";PROTOCOL=",dsn_protocol,
                   ";UID=",dsn_uid,
                   ";PWD=",dsn_pwd,sep="")
conn <- odbcDriverConnect(conn_path)
conn
# Pedir las tablas del ESQUEMA
sqlTables(conn,schema="NKG63522")
DISTRITOS2019 <- sqlFetch(conn,"DISTRITOS2019")
#Al venir de la base de datos, la columna de ID (que está hecha de números) es cambiada a numérica.
#Por ello toca nuevamente volverla a carácter.
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
    MASPAMSEQ=sum(MASPAMSEQ),
    PH=sum(PH),
    PCTE=sum(PCTE),
    R0=sum(RECORTESCEROGVPCASTC),
    PCPE=sum(PCPE),
    censoINE=sum(censoINE),
    nulos=sum(nulos),
    blancos=sum(blancos),
    validos=sum(validos)
    ) 
