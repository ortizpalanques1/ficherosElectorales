library(xlsx)
library(dplyr)
library(ggplot2)
library(lmtest)
#Recoger datos de renta. Notese que estamos recogiendo del archivo .xlsx 
#Pero los datos también están en la base de IBM (ver archivo "PersonalNoGit.R")
RENTA2017MADRID <- read.xlsx("renta2017Madrid.xlsx", sheetName="tabla-31097", encoding = "UTF-8", header=FALSE)
str(RENTA2017MADRID)
RENTA2017MADRID$X2 <- as.numeric(as.character(RENTA2017MADRID$X2))
RENTA2017MADRID$MUNICIPIO <- substr(RENTA2017MADRID$X1,1,5)
RENTA2017MADRID$Sectio <- substr(RENTA2017MADRID$X1,7,7)
RENTA2017MADRID$subSectio <- substr(RENTA2017MADRID$X1,9,10)
RENTA2017MADRID$ID2019 <- paste0(RENTA2017MADRID$Sectio,RENTA2017MADRID$subSectio)
RENTA2017MADRID <- data.frame(RENTA2017MADRID[,c(1:3,6)])
# Recogemos las rentas de Getafe
renta2017Getafe <- RENTA2017MADRID %>% 
  filter(MUNICIPIO==28065)
# JOIN A LOS DATOS ELECTORALES
# ATENCIÓN: para esta parte se necesita haber ejecutado primero "PersonalNoGit.R","AbrilNoviembre.R"
# ABRIL
getafe20190428Seccion <- left_join(getafe20190428Seccion,renta2017Getafe,by="ID2019")
# NOVIEMBRE
getafe20191110Seccion <- left_join(getafe20191110Seccion,renta2017Getafe,by="ID2019")
# Variación Porcentual de Cs
variacion <- data.frame("ID2019"=getafe20190428Seccion$ID2019,
                        "VoxA"=round((getafe20190428Seccion$VOX/getafe20190428Seccion$validos)*100,2),
                        "EdadM"=getafe20190428Seccion$EdadmujerPro,
                        "Renta"=getafe20190428Seccion$X2)
variacion <- left_join(variacion,tabulaAetatis,by=c("ID2019"="ID"))
Auxiliar <- data.frame("ID2019"=getafe20191110Seccion$ID2019,
                       "VoxN"=round((getafe20191110Seccion$VOX/getafe20191110Seccion$validos)*100,2))
variacion <- left_join(variacion,Auxiliar,by="ID2019")
variacion$Diferencia <- variacion$VoxN-variacion$VoxA
str(variacion)
# Regresiones
## Diferencia
regee00 <- lm(Diferencia~Edad+Renta+Renta:Edad,data=variacion)
summary(regee00)
regee01 <- lm(Diferencia~Edad+Renta,data=variacion)
summary(regee01)
regee02 <- lm(Diferencia~Renta,data=variacion)
summary(regee02)
regee03 <- lm(Diferencia~Edad,data=variacion)
summary(regee03)
# Noviembre
regee04 <- lm(VoxN~Renta,data=variacion)
summary(regee04)
regee05 <- lm(VoxN~Edad,data=variacion)
summary(regee05)
regee06 <- lm(VoxN~Edad+Renta,data=variacion)
summary(regee06)
regee07 <- lm(VoxN~Edad+Renta+Renta:Edad,data=variacion)
summary(regee07)
regee08 <- lm(VoxN~EdadM,data=variacion)
summary(regee08)
regee09 <- lm(VoxN~EdadM+Renta,data=variacion)
summary(regee09)
regee10 <- lm(VoxN~EdadM+Renta+Renta:EdadM,data=variacion)
summary(regee10)
# Abril
regee11 <- lm(VoxA~Renta,data=variacion)
summary(regee11)
regee12 <- lm(VoxA~EdadH,data=variacion)
summary(regee12)
regee13 <- lm(VoxA~EdadH+Renta,data=variacion)
summary(regee13)
regee14 <- lm(VoxA~EdadH+Renta+Renta:EdadH,data=variacion)
summary(regee14)
regee15 <- lm(VoxA~EdadM,data=variacion)
summary(regee15)
regee16 <- lm(VoxA~EdadM+Renta,data=variacion)
summary(regee16)
regee17 <- lm(VoxA~EdadM+Renta+Renta:EdadM,data=variacion)
summary(regee17)
# Graph regression
grafico02 <- ggplot(variacion,aes(x=Renta,y=Diferencia))+
  geom_point(colour="#964b00")+
  geom_smooth(method="lm", colour="#c08457")+
  labs(title="Renta vs diferencia de votos de VOX (2019)",
       subtitle="Getafe, secciones electorales",
       caption="Fuente Min. In., cálculos propios")+
  theme(plot.background = element_rect(fill = "#b7c6db"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(colour = "black")
  )
ggsave("VoxRenta.png", grafico02)
grafico03 <- ggplot(variacion,aes(x=EdadH,y=Diferencia))+
  geom_point(colour="#964b00")+
  geom_smooth(method="lm", colour="#c08457")+
  labs(title="Edad de los Hombres vs diferencia de votos de VOX (2019)",
       subtitle="Getafe, secciones electorales",
       caption="Fuente Min. In., cálculos propios")+
  theme(plot.background = element_rect(fill = "#b7c6db"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.background = element_blank(),
        axis.text = element_text(colour = "black")
  )
ggsave("VoxEdad.png", grafico03)

par(mfrow=c(2,2))
plot(regee03)
# BreuschPagan test para heterocedasticidad
bptest(regee03)
