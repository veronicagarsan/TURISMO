#Aquí tratamos los datos obtenidos del portal de frontipologia (movimientos en Fronteras)
#Leemos los archivos csv

file_names <- list.files(datosfrontur,pattern = "\\.csv$")
frontur <- lapply(paste0(datosfrontur,"/",file_names),read.csv,header = F, sep = ",", stringsAsFactors = F)
frontur <- lapply(frontur,as.data.table)


################################################################
#Primera tabla, tipología de visitante
###############################################################

#limpiamos todo lo innecesario
tipologia<-frontur[[1]][,-c(10:12),]
tipologia<-tipologia[,-c(0:2),]

#Transformamos fechas
#Cambiamos los nombres de los meses a valores numéricos
tipologia$V6<-gsub(c("Enero "),c("01-"),tipologia$V6)
tipologia$V6<-gsub(c("Febrero "),c("02-"),tipologia$V6)
tipologia$V6<-gsub(c("Marzo "),c("03-"),tipologia$V6)
tipologia$V6<-gsub(c("Abril "),c("04-"),tipologia$V6)
tipologia$V6<-gsub(c("Mayo "),c("05-"),tipologia$V6)
tipologia$V6<-gsub(c("Junio "),c("06-"),tipologia$V6)
tipologia$V6<-gsub(c("Julio "),c("07-"),tipologia$V6)
tipologia$V6<-gsub(c("Agosto "),c("08-"),tipologia$V6)
tipologia$V6<-gsub(c("Septiembre "),c("09-"),tipologia$V6)
tipologia$V6<-gsub(c("Octubre "),c("10-"),tipologia$V6)
tipologia$V6<-gsub(c("Noviembre "),c("11-"),tipologia$V6)
tipologia$V6<-gsub(c("Diciembre "),c("12-"),tipologia$V6)
tipologia

#Agregamos cabecera
colnames(tipologia) = c("1","2","3","Fecha", "Visitante", "Turista", "Excursionista")


#Filtramos tabla
tipologia = tipologia[-1, ] 
tipologia= tipologia[,4:7]

#Transformamos los datos
tipologia$Turista<-gsub(c("."),c(""),tipologia$Turista, fixed = TRUE)
tipologia$Excursionista<-gsub(c("."),c(""),tipologia$Excursionista, fixed = TRUE)
tipologia$Visitante<-gsub(c("."),c(""),tipologia$Visitante, fixed = TRUE)
tipologia
tipologia[,2:4]<-lapply(tipologia[,2:4],as.numeric)
#Quitamos las filas de total
#tipologia_total<-tipologia[Fecha=="Total",]
tipologia<-tipologia[Fecha!="Total",]
tipologia
#Transformamos a formato fecha
tipologia$Fecha<-as.Date(as.yearmon(tipologia$Fecha,"%m-%Y"))

#Creamos un objeto tipo serie temporal
tipologia_ts <- ts(tipologia[, 2:4], start = c(2000, 1),end = c(2015,9), frequency = 12)

#Representamos los distintos visitantes a lo largo del tiempo
autoplot(tipologia_ts)
autoplot(tipologia_ts, facets= TRUE)


#Observamos su estructura
str(tipologia)


#Examinamos datos
summary(tipologia)
plot(tipologia[,3:4])

write.xlsx(tipologia, paste0(resultados,"/","tipos_viajero.xlsx"))

################################################################
#Tabla de viajeros según tipo de alojamiento 
################################################################
alojamiento<-frontur[[2]][,-c(11:12),]
alojamiento<-alojamiento[,-1]
colnames(alojamiento)<-c("Fecha","Total_Alojamiento", "Hotelero", "Mercado y no mercado no hotelero", "No de mercado", "Vivienda en alquiler", "Resto de mercado",  "No Se Alojan", " Sin Especificar" ) 

#Filtramos tabla
alojamiento = alojamiento[-1, ] 
alojamiento= alojamiento[,c(1:3,5:7)]
#Ordenamos para que coincida con la tabla del INE.
setcolorder(alojamiento,c("Fecha","Total_Alojamiento","Hotelero","Vivienda en alquiler","Resto de mercado","No de mercado"))

#Transformamos fechas
#Cambiamos los nombres de los meses a valores numéricos
alojamiento$Fecha<-gsub(c("Enero "),c("01-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Febrero "),c("02-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Marzo "),c("03-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Abril "),c("04-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Mayo "),c("05-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Junio "),c("06-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Julio "),c("07-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Agosto "),c("08-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Septiembre "),c("09-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Octubre "),c("10-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Noviembre "),c("11-"),alojamiento$Fecha)
alojamiento$Fecha<-gsub(c("Diciembre "),c("12-"),alojamiento$Fecha)
alojamiento

#Transformamos los datos
alojamiento$Total_Alojamiento<-gsub(c("."),c(""),alojamiento$Total_Alojamiento, fixed = TRUE)
alojamiento$Hotelero<-gsub(c("."),c(""),alojamiento$Hotelero, fixed = TRUE)
alojamiento$`Vivienda en alquiler`<-gsub(c("."),c(""),alojamiento$`Vivienda en alquiler`, fixed = TRUE)
alojamiento$`Resto de mercado`<-gsub(c("."),c(""),alojamiento$`Resto de mercado`, fixed = TRUE)
alojamiento$`No de mercado`<-gsub(c("."),c(""),alojamiento$`No de mercado`, fixed = TRUE)

alojamiento[,2:6]<-lapply(alojamiento[,2:6],as.numeric)
#Quitamos las filas de total
alojamiento<-alojamiento[Fecha!="Total",]

#Transformamos a formato fecha
alojamiento$Fecha<-as.Date(as.yearmon(alojamiento$Fecha,"%m-%Y"))

#Creamos un objeto tipo serie temporal
alojamiento_ts <- ts(alojamiento[, 2:6], start = c(2000, 1),end = c(2015,9), frequency = 12)

#Representamos los distintos visitantes según su alojamiento
autoplot(alojamiento_ts)
autoplot(alojamiento_ts, facets = T)

#Observamos su estructura
str(alojamiento)


#Examinamos datos
summary(alojamiento)
plot(alojamiento[,3:6])
write.xlsx(alojamiento, paste0(resultados,"/","tipo_alojamiento.xlsx"))

######################################################
#Turistas según vía de acceso
#####################################################


acceso<-frontur[[3]][,-c(14:16),]
acceso<-acceso[,-c(1:2)]
colnames(acceso)<-c("1","2","3","4","5","Fecha","Total_Acceso", "Aeropuerto", "Carretera", "Puerto", "Ferrocarril" ) 

#Filtramos tabla
acceso = acceso[-1, ] 
acceso= acceso[,c(6:11)]


#Transformamos fechas
#Cambiamos los nombres de los meses a valores numéricos
acceso$Fecha<-gsub(c("Enero "),c("01-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Febrero "),c("02-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Marzo "),c("03-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Abril "),c("04-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Mayo "),c("05-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Junio "),c("06-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Julio "),c("07-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Agosto "),c("08-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Septiembre "),c("09-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Octubre "),c("10-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Noviembre "),c("11-"),acceso$Fecha)
acceso$Fecha<-gsub(c("Diciembre "),c("12-"),acceso$Fecha)


#Transformamos los datos a valores numéricos
df <- acceso[,2:length(acceso)]
df<-as.matrix(df)
df<-as.data.table(gsub(".","",df, fixed = T))
df<-as.data.table(sapply(df, as.numeric))
acceso[,2:6]<-df
rm(df)

#Quitamos las filas de total
acceso<-acceso[Fecha!="Total",]

#Transformamos a formato fecha
acceso$Fecha<-as.Date(as.yearmon(acceso$Fecha,"%m-%Y"))

#Creamos un objeto tipo serie temporal
acceso_ts <- ts(acceso[, 2:6], start = c(2000, 1),end = c(2015,9), frequency = 12)

#Representamos los distintos visitantes según su acceso
autoplot(acceso_ts)
autoplot(acceso_ts, facets = T)

#Observamos su estructura
str(acceso)

#Examinamos datos
summary(acceso)
plot(acceso[,3:6])
write.xlsx(acceso, paste0(resultados,"/","tipo_acceso.xlsx"))

######################################################
#Turistas según país de residencia
#####################################################


residencia<-frontur[[4]][,-c(20:22),]
residencia<-residencia[,-c(1:2)]
subparte1<-residencia[1:205]
subparte2<-residencia[206:length(residencia$V3)]
colnames(subparte1)<-c("1","2","3","4","5","6","7","8","Fecha","Total_Paises", "Alemania", "Belgica", "Francia", "Irlanda", "Italia","Paises Bajos","Portugal") 
subparte1= subparte1[,c(9:17)]
colnames(subparte2)<-c("1","2","3","4","5","6","7","8","Fecha","Total_Paises", "Reino Unido", "Suiza", "Paises Nordicos", "Resto de Europa", "Estados Unidos","Resto de America","Resto del Mundo") 
subparte2= subparte2[,c(9:17)]
residencia<-inner_join(subparte1,subparte2,by=c("Fecha", "Total_Paises"))
#Filtramos tabla
residencia = residencia[-1, ] 
rm(subparte1,subparte2, df)
#Transformamos fechas
#Cambiamos los nombres de los meses a valores numéricos
residencia$Fecha<-gsub(c("Enero "),c("01-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Febrero "),c("02-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Marzo "),c("03-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Abril "),c("04-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Mayo "),c("05-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Junio "),c("06-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Julio "),c("07-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Agosto "),c("08-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Septiembre "),c("09-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Octubre "),c("10-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Noviembre "),c("11-"),residencia$Fecha)
residencia$Fecha<-gsub(c("Diciembre "),c("12-"),residencia$Fecha)


#Transformamos los datos a valores numéricos
df <- residencia[,2:length(residencia)]
df<-as.matrix(df)
df<-as.data.table(gsub(".","",df, fixed = T))
df<-as.data.table(sapply(df, as.numeric))
residencia[,2:length(residencia)]<-df
residencia<-as.data.table(residencia)

#Quitamos las filas de total
residencia<-residencia[Fecha!="Total",]

#Transformamos a formato fecha
residencia$Fecha<-as.Date(as.yearmon(residencia$Fecha,"%m-%Y"))

#Creamos un objeto tipo serie temporal
residencia_ts <- ts(residencia[, 2:length(residencia)], start = c(2000, 1),end = c(2015,9), frequency = 12)

#Representamos los distintos visitantes según su residencia
autoplot(residencia_ts)
autoplot(residencia_ts, facets = T)

#Observamos su estructura
str(residencia)

#Examinamos datos
summary(residencia)
plot(residencia[,3:16], col="darkgray")
#write.xlsx(residencia, paste0(resultados,"/","pais_residencia.xlsx"))
boxplot(residencia)

######################################################
#Turistas según Comunidad Autónoma de destino vacacional
#####################################################

#Seleccionamos tabla
destino<-frontur[[5]]
#Quitamos columnas que no interesan
destino<- destino[,-c(20:22)]
destino<-destino[,-c(1:2)]
#Añadimos nombres a las columnas
colnames(destino)<-c("1","2","3","4","5","6","7","8","Fecha","Total_Destinos", "Andalucia", "Balears", 
                       "Canarias", "Cataluna", "Comunitat_Valenciana","Madrid","Otras Comunidades Autonomas") 

#Nos quedamos con las columnas que nos interesan
destino= destino[,c(9:17)]
destino = destino[-1, ] 

#Transformamos fechas
destino <- fechafrontur(destino)

#Transformamos los datos a valores numéricos

destino <- transfrontur(destino)


#Quitamos las filas de total
destino<-destino[Fecha!="Total",]

#Transformamos a formato fecha
destino$Fecha<-as.Date(as.yearmon(destino$Fecha,"%m-%Y"))

#Creamos un objeto tipo serie temporal
destino_ts <- ts(destino[, 2:length(destino)], start = c(2000, 1),end = c(2015,9), frequency = 12)

#Representamos los distintos visitantes según su destino
autoplot(destino_ts)
autoplot(destino_ts, facets = T)


#Observamos su estructura
str(destino)

#Examinamos datos
summary(destino)
plot(destino[,3:9])


