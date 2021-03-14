#Librerías
library(xlsx)
library(dplyr)
EDADT2013 <- read.xlsx("Edad2013Getafe.xlsx",sheetName = "Todos", header = TRUE, encoding="UTF-8")
# Obtener promedios 
ejemplo1000 <- function(x){ 
  edadPromedio <- matrix(nrow=nrow(x),ncol=2)
  for(i in 1:nrow(x)){
    edadPromedio[i,1] <- paste0(substr(x[i,1],11,11),substr(x[i,1],13,14))
    edadPromedio[i,2] <- sum(x[i,2:ncol(x)]*
                                   (2.5+(seq(0,100,5))))/
      sum(x[i,2:ncol(x)])
  }
  tabulaPromedio <<- as.data.frame(edadPromedio) 
}
colnames(tabulaPromedio) <- c("ID2013","PromedioEdad")
ejemplo1000(EDADT2013)
ID2019ID <- data.frame("ID2019"=getafe20191110Seccion$ID2019)
aqui <- anti_join(ID2019ID,tabulaPromedio,by=c("ID2019"="ID2013"))