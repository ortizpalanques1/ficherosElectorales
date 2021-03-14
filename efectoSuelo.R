library(dplyr)
porcentjeCs20190428 <- (getafe20190428Seccion$Cs/getafe20190428Seccion$validos)*100
porcentjeCs20190428 <- data.frame("ID2019"=getafe20190428Seccion$ID2019,"Abril"=porcentjeCs20190428)
porcentjeCs20191110 <- (getafe20191110Seccion$Cs/getafe20191110Seccion$validos)*100
porcentjeCs20191110 <- data.frame("ID2019"=getafe20191110Seccion$ID2019,"Noviembre"=porcentjeCs20191110)
porcentajesCd <- left_join(porcentjeCs20190428,porcentjeCs20191110,by="ID2019")
porcentajes=mutate(diferencia=100-((Noviembre/Abril)*100),porcentajesCd)
prueba33 <- cor(porcentajes$Abril, porcentajes$diferencia)
