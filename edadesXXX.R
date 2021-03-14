library(RODBC)
library(dplyr)
edades <- sqlFetch(conn,"EDADES")
secciones <- levels(as.factor(edades$IDSEC))
intervalos <- nrow(unaSeccion)%/%3

tabulaAetatis <- matrix(nrow=length(secciones),ncol=2)
for(j in 1:length(secciones)){
  unaSeccion <- edades %>% 
    filter(IDSEC==secciones[j])
  for(i in 7:intervalos){
    alfa <- ((i-1)*3)+1
    beta <- (i*3)
    gamma <- sum(unaSeccion[alfa:beta,4],na.rm=TRUE)
    delta <- alfa+1.5
    edadIntervalos <- c(edadIntervalos,delta)
    edadCantidad <- c(edadCantidad,gamma)
    epsilon <- which.max(edadCantidad)
    edadModal <- edadIntervalos[epsilon]
  }
  tabulaAetatis[j,1] <- secciones[j]
  tabulaAetatis[j,2] <- edadModal
  edadCantidad <- NULL
  edadIntervalos <- NULL
}
colnames(tabulaAetatis) <- c("ID","Edad")
tabulaAetatis <- data.frame(tabulaAetatis)
tabulaAetatis$Edad <- as.numeric(as.character(tabulaAetatis$Edad))
str(tabulaAetatis)
