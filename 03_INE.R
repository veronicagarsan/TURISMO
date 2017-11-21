#Leemos las tablas de Frontur del INE

#Aquí tratamos los datos obtenidos del portal de INE, comenzamos con FRONTUR(movimientos en Fronteras)
#Leemos los archivos csv

file_names <- list.files(datosine,pattern = "\\.csv$")
inefrontur <- lapply(paste0(datosine,"/",file_names),read.csv,header = F, sep = ";", stringsAsFactors = F)
inefrontur <- lapply(inefrontur,as.data.table)

################################################################
#Primera tabla, tipología de visitante
###############################################################

#limpiamos todo lo innecesario
inetipologia<-inefrontur[[1]]
#Quitamos las primeras filas, pues son datos no válidos
inetipologia<-inetipologia[-c(0:6),]
inetipologia<-inetipologia[-c((length(inetipologia$V2)-2):length(inetipologia$V2)),]
colnames(inetipologia) =as.character(unlist(inetipologia[1, ]))  # la primera fila será la cabecera
setnames(inetipologia,"","Fecha")
inetipologia = inetipologia[-1, ] 
inetipologia = inetipologia[,1:4 ]

#Transformamos los datos
inetipologia[,2:length(inetipologia)]<-lapply(inetipologia[,2:length(inetipologia)],as.numeric)
inetipologia$Fecha<-gsub("M","-",inetipologia$Fecha)
inetipologia$Fecha <-as.Date(as.yearmon(inetipologia$Fecha))
##inetipologia$Fecha <-as.Date(inetipologia$Fecha, "%d/%m/%Y")

######===================================================================================== 
##Lo reciclaremos para las gráficas de Tableau
#long_DF <- inefront_duracion %>% gather(Estancias, Personas, 2:7)
#long_DF$Estancias<-as.factor(long_DF$Estancias)
# long_DF$V8<-NULL
# long_DF <- as.data.table(long_DF)
# long_DF[,Suma_fechas:=sum(Personas), by= Fecha]
# inefront_duracion<-as.data.table(long_DF)
######===================================================================================== 

#Observamos su estructura
str(inetipologia)


#Examinamos datos
summary(inetipologia)

#boxplot(inefront_duracion$Personas)

# #Quitamos outliers
# inefront_duracion[,PersoNOutlier:= fun( inefront_duracion$Personas )]
# 
# #Outliers que hemos quitado
# y<-inefront_duracion$Personas-inefront_duracion$PersoNOutlier
# table(y)
# boxplot(personas2)

#write.xlsx(inetipologia, paste0(resultados,"/","tipologia20015-2017.xlsx"))

#.Para usar este gráfico hay que transformar la tabla
# #Gráfico con serie temporal, total de personas por fecha
# p <- plot_ly(x = inefront_duracion$Fecha,y = inefront_duracion$`Ninguna noche`
#              mode = 'lines',
#              type = "scatter",
#              #color = color.brewer(21),
#              text = "personas")


##################################################################################################################
##Tabla de viajeros según tipo de alojamiento 
##################################################################################################################

inealojamiento<-inefrontur[[2]]

#limpiamos todo lo innecesario

#Quitamos las primeras filas, pues son datos no válidos
inealojamiento<-inealojamiento[-c(0:6),]
inealojamiento<-inealojamiento[-c((length(inealojamiento$V2)-2):length(inealojamiento$V2)),]
colnames(inealojamiento) =as.character(unlist(inealojamiento[1, ]))  # la primera fila será la cabecera
inealojamiento = inealojamiento[-1, ]
setnames(inealojamiento,"","Fecha")
inealojamiento = inealojamiento[,1:10 ]

#####Esto se puede encapsular en una función.
#Transformamos los datos
inealojamiento[,2:length(inealojamiento)]<-lapply(inealojamiento[,2:length(inealojamiento)],as.numeric)
inealojamiento$Fecha<-gsub("M","-",inealojamiento$Fecha)
inealojamiento$Fecha <-as.Date(as.yearmon(inealojamiento$Fecha))

#Observamos su estructura
str(inealojamiento)

#Examinamos datos
summary(inealojamiento)

##################################################################################################################
##Tabla de viajeros según tipo de acceso
##################################################################################################################

ineacceso<-inefrontur[[3]]

#limpiamos todo lo innecesario
ineacceso<-limpiarine(ineacceso)
ineacceso <-ineacceso[-1, ]

#Transformamos los datos
ineacceso<-transfine(ineacceso)

#Observamos su estructura
str(ineacceso)

#Examinamos datos
summary(ineacceso)



######################################################
#Turistas según país de residencia
#####################################################
ineresidencia<-inefrontur[[4]]
#limpiamos todo lo innecesario
ineresidencia<-limpiarine(ineresidencia)
#Transformamos los datos
ineresidencia<-transfine(ineresidencia)

# ineresidencia %>% ggplot( aes(Fecha, y= Alemania)) + geom_line()
             
#Observamos su estructura
str(ineresidencia)

#Examinamos datos
summary(ineresidencia)


######################################################
#Turistas según Comunidad Autónoma de destino vacacional
#####################################################

inedestino<-inefrontur[[5]]
#limpiamos todo lo innecesario
inedestino<-limpiarine(inedestino)
#Transformamos los datos
inedestino<-transfine(inedestino)

# inedestino %>% ggplot( aes(Fecha, y= Alemania)) + geom_line()

#Observamos su estructura
str(inedestino)

#Examinamos datos
summary(inedestino)



