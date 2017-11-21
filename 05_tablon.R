##Vamos a unir las tablas que proceden del INE con las que proceden de la página de tourspain.

##########################################################################################
###Tablas de visitantes según tipo de alojamiento
##########################################################################################
alojamiento
inealojamiento

setnames(alojamiento,"Total_Alojamiento", "Total")
#Cambiamos el orden de las columnas de la primera tabla
setcolorder(alojamiento, c("Fecha","Total","Hotelero","Vivienda en alquiler","Resto de mercado","No de mercado"))
#???Creamos un subset pues inealojamiento tiene más columnas
subinealojamiento <- inealojamiento[,c(1:2,4:7)]
#Fusionamos las dos tablas en 1 y las ordenamos por fecha
tipo_alojamiento <- bind_rows(alojamiento,subinealojamiento)
setorder(tipo_alojamiento,Fecha)  
#Creamos un objeto tipo serie temporal
tipo_alojamiento_ts <- ts(tipo_alojamiento[, 2:6], start = c(2000, 1),end = c(2017,7), frequency = 12)

#Representamos los distintos visitantes según su acceso
autoplot(tipo_alojamiento_ts)
autoplot(tipo_alojamiento_ts, facets = T)
plot(tipo_alojamiento[,3:6])
par(mfrow=c(2,2))
lapply(tipo_alojamiento[,3:6],boxplot)
write.xlsx(tipo_alojamiento, paste0(resultados,"/","tipo_alojamiento.xlsx"))
#rm(alojamiento, inealojamiento, tipo_alojamiento_ts, subinealojamiento)

#Creo el tablón
tablon <- tipo_alojamiento[,  ':='  (AÑO= year(Fecha), MES = month(Fecha))]
setnames(tablon, c("Fecha","Total","Hotelero","Vivienda en alquiler","Resto de mercado","No de mercado"),
                 c("FECHA","TOTAL","HOTEL", "VIV_ALQ", "REST_MERC", "NO_MERC"))

#Ordeno colocando las fechas primero
setcolorder(tablon, c("FECHA","AÑO","MES","TOTAL","HOTEL", "VIV_ALQ", "REST_MERC", "NO_MERC") )



#######################################################################################
###Tablas de visitantes según tipo de acceso
########################################################################################
acceso
ineacceso
#Cambiamos los nombrea a ineacceso para que coincidan
setnames(ineacceso,c("Total","Tren"), c("Total_Acceso","Ferrocarril"))

#Fusionamos las dos tablas en 1 y las ordenamos por fecha
tipo_acceso <- bind_rows(acceso,ineacceso)
setorder(tipo_acceso,Fecha)

plot(tipo_acceso[,3:6])
boxplot(tipo_acceso[,3:6])

#Gráficos para estudiar los datos
# a <- gather(tipo_acceso,"Acceso","Personas",2:6)
# ggplot(a,aes(Acceso, Personas)) + geom_boxplot()+facet_grid(.~ Acceso, scales = "free")
# 
# ggplot(tipo_acceso[Carretera>2000000,],aes(Fecha, Carretera))+geom_line()
# ggplot(tipo_acceso,aes(Fecha, Ferrocarril))+geom_line() 
# ggplot(tipo_acceso,aes(Fecha, Puerto))+geom_line()
# ggplot(tipo_acceso,aes(Fecha, Aeropuerto))+geom_line()
#  summary(tipo_acceso)

#Creamos un objeto tipo serie temporal
tipo_acceso_ts <- ts(tipo_acceso[, 2:6], start = c(2000, 1),end = c(2017,6), frequency = 12)

#Representamos los distintos visitantes según su acceso
autoplot(tipo_acceso_ts)
autoplot(tipo_acceso_ts, facets = T)

write.xlsx(tipo_acceso, paste0(resultados,"/","tipo_acceso.xlsx"))

#rm(acceso, ineacceso, tipo_acceso_ts)
setnames(tipo_acceso, names(tipo_acceso),toupper( names(tipo_acceso)))
  
tablon <- left_join(tablon,tipo_acceso[,-2], by = "FECHA")


##########################################################################################
###Tablas de visitantes según país de residencia
##########################################################################################

setnames(ineresidencia,c("Total","Estados Unidos de America","Resto America","Suiza ")
         ,c("Total_Paises","Estados Unidos","Resto de America","Suiza"))

#Fusionamos las dos tablas en 1 y las ordenamos por fecha
tipo_residencia <-bind_rows(residencia, ineresidencia)
setorder(tipo_residencia,Fecha)

#Creamos un objeto tipo serie temporal
tipo_residencia_ts <- ts(tipo_residencia[, 2:16], start = c(2000, 1),end = c(2015,9), frequency = 12)
tipo_residencia_ts2 <- ts(tipo_residencia[, 12:13], start = c(2000, 1),end = c(2015,9), frequency = 12)
#Representamos los distintos visitantes según su residencia
autoplot(tipo_residencia_ts2)
autoplot(residencia_ts, facets = T)

write.xlsx(tipo_residencia, paste0(resultados,"/","tipo_residencia.xlsx"))

#rm(residencia, ineresidencia, tipo_residencia_ts)
setnames(tipo_residencia, names(tipo_residencia),toupper( names(tipo_residencia)))
setnames(tipo_residencia,c("PAISES BAJOS", "REINO UNIDO"), c("PAISES_BAJOS", "REINO_UNIDO"))
setnames(tipo_residencia,c("PAISES NORDICOS", "RESTO DE EUROPA","ESTADOS UNIDOS","RESTO DE AMERICA","RESTO DEL MUNDO"),
  c("PAISES_NORDICOS", "RESTO_EUROPA","EEUU","RESTO_AMERICA","RESTO_MUNDO"))
summary(tipo_residencia)
#englobar paises nordicos en resto europa y rusia en resto mundo
tipo_residencia[,EUR := sum(PAISES_NORDICOS,RESTO_EUROPA, na.rm=T)]
tipo_residencia[is.na(tipo_residencia)] <- 0
tipo_residencia[,EUR:=PAISES_NORDICOS+RESTO_EUROPA]
tipo_residencia$RESTO_EUROPA<-NULL
setnames(tipo_residencia,"EUR", "RESTO_EUROPA")

tipo_residencia[,EUR:=RUSIA+RESTO_MUNDO]
tipo_residencia$RESTO_MUNDO<-NULL
setnames(tipo_residencia,"EUR", "RESTO_MUNDO")
tipo_residencia$RUSIA<-NULL
tipo_residencia$PAISES_NORDICOS<-NULL
tablon <- left_join(tablon,tipo_residencia, by = "FECHA")



##########################################################################################
###Tablas de visitantes según Comunidad Autónoma de destino principal
##########################################################################################
names(destino)
names(inedestino)

setnames(inedestino,"Total","Total_Destinos")

#Fusionamos las dos tablas en 1 y las ordenamos por fecha
tipo_destino <-bind_rows(destino, inedestino)
setorder(tipo_destino,Fecha)

#Creamos un objeto tipo serie temporal
tipo_destino_ts <- ts(tipo_destino[, 2:9], start = c(2000, 1),end = c(2015,9), frequency = 12)

#Representamos los distintos visitantes según su destino
autoplot(tipo_destino_ts)
autoplot(destino_ts, facets = T)

write.xlsx(tipo_destino, paste0(resultados,"/","tipo_destino.xlsx"))


setnames(tipo_destino, names(tipo_destino),toupper( names(tipo_destino)))
setnames(tipo_destino, "OTRAS COMUNIDADES AUTONOMAS","OTRAS_CCAA")
#rm(destino, inedestino, tipo_destino_ts)



tablon <- left_join(tablon,tipo_destino, by = "FECHA")

tablon$TOTAL_DESTINOS <- NULL
tablon$TOTAL_PAISES <- NULL







