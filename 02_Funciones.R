#Funciones

#Definimos una función para quitar los outliers

fun <- function(x){
  quantiles <- quantile( x, c(.001, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

#Función para limpiar las tablas del INE


#Quitamos las primeras filas, pues son datos no válidos
limpiarine<-function(dt) {
              dt<-dt[-c(0:6),]
              dt<-dt[-c((length(dt$V2)-2):length(dt$V2)),]
              colnames(dt) =as.character(unlist(dt[1, ]))  # la primera fila será la cabecera
              dt <-dt[-1, ]
              setnames(dt,"","Fecha")
              dt
}

#transformamos los datos al tipo que necesitamos
transfine<-function(dt) {
              dt[,2:length(dt)]<-lapply(dt[,2:length(dt)],as.numeric)
              dt$Fecha<-gsub("M","-",dt$Fecha)
              dt$Fecha <-as.Date(as.yearmon(dt$Fecha))
              dt
}       


#Transformamos fechas en FRONTUR (Cambiamos los nombres de los meses a valores numéricos)

fechafrontur <- function(dt){
              dt$Fecha<-gsub(c("Enero "),c("01-"),dt$Fecha)
              dt$Fecha<-gsub(c("Febrero "),c("02-"),dt$Fecha)
              dt$Fecha<-gsub(c("Marzo "),c("03-"),dt$Fecha)
              dt$Fecha<-gsub(c("Abril "),c("04-"),dt$Fecha)
              dt$Fecha<-gsub(c("Mayo "),c("05-"),dt$Fecha)
              dt$Fecha<-gsub(c("Junio "),c("06-"),dt$Fecha)
              dt$Fecha<-gsub(c("Julio "),c("07-"),dt$Fecha)
              dt$Fecha<-gsub(c("Agosto "),c("08-"),dt$Fecha)
              dt$Fecha<-gsub(c("Septiembre "),c("09-"),dt$Fecha)
              dt$Fecha<-gsub(c("Octubre "),c("10-"),dt$Fecha)
              dt$Fecha<-gsub(c("Noviembre "),c("11-"),dt$Fecha)
              dt$Fecha<-gsub(c("Diciembre "),c("12-"),dt$Fecha)
              dt
              }

transfrontur <- function(dt) {
              df <- dt[,2:length(dt)]
              df<-as.matrix(df)
              df<-as.data.table(gsub(".","",df, fixed = T))
              df<-as.data.table(sapply(df, as.numeric))
              dt[,2:length(dt)]<-df
              dt<-as.data.table(dt)
              dt
}

              