#Librerías
library(dplyr)
library(ggplot2)
#HACIENDO LA COMPARACIÓN
#Cargar los datos
load("dobelResultado.RData")
#Vector con los nombres de los datos
lista01 <- load("dobelResultado.RData")
#Crear la columna de abstención
for(i in 1:length(lista01)){ 
  abstantes <- (get(lista01[i])$censoINE-get(lista01[i])$validos)
  esteEs <- cbind(get(lista01[i]),abstantes)
  esteEs[,1] <- as.character(esteEs[,1])
  assign(as.character(lista01[i]),esteEs)
}
#Para todo el municipio
# Determinar los archivos de trabajo
lista02 <- lista01[seq(1, by = 2, len = 2)]
for(j in 1:length(lista02)){ 
  municipioTotal <- c("Total",apply(get(lista02[j])[,2:ncol(get(lista02[j]))],2,sum))
  esteEs02 <- rbind(get(lista02[j]),municipioTotal)
  esteEs02 <- esteEs02[nrow(esteEs02),]
  assign(paste0(as.character(lista02[j]),"Total"),esteEs02)
}
# Homogeneizar gráficos con los datos que se necesitan: Cs,PP,VOX,PSOE, PODEMOSIU y Abstención, total y válidos
# Abril
abril01 <- getafe20190428DistritoTotal %>% 
  select(c(2:6,17,18,14)) %>% 
  mutate(MASM=0)
abril01 <- abril01[,c(1:5,9,6,7,8)]
abril01 <- t(abril01)
abril01 <- data.frame(abril01,"Mes"= "Abril")
colnames(abril01)[1] <- "Votos"
abril01$Preferencias <- rownames(abril01)
abril01$Votos <- as.numeric(as.character(abril01$Votos))
abril01$Porciento <- round((abril01$Votos/abril01$Votos[nrow(abril01)])*100,2)
abril01 <- abril01[c(1:6,8),]
# Noviembre
noviembre01 <- getafe20191110DistritoTotal %>% 
  select(c(2:6,9,14,17,18))
noviembre01 <- noviembre01[,c(1:6,8,9,7)]
noviembre01 <- t(noviembre01)
noviembre01 <- data.frame(noviembre01,"Mes"="Noviembre")
colnames(noviembre01)[1] <- "Votos"
noviembre01$Preferencias <- rownames(noviembre01)
noviembre01$Votos <- as.numeric(as.character(noviembre01$Votos))
noviembre01$Porciento <- round((noviembre01$Votos/noviembre01$Votos[nrow(noviembre01)])*100,2)
noviembre01 <- noviembre01[c(1:6,8),]
# UNIR
totalTOTAL <- rbind(abril01,noviembre01)
# Gráfico
grafico01 <- ggplot(totalTOTAL,aes(x=Preferencias,y=Porciento,fill=Mes))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.5))
grafico01
# Bucle para distritos
