
summary(tablon)
tablon <- as.data.table(tablon)
tablon <-tablon [13:210,]
tablon <-tablon[,-1]
#tablon2 <-tablon
tablon <- tablon2

n_neuronas <- c(6,5,2)

#Comprobamos que no existan NA es el tablón
apply(tablon,2,function(x) sum(is.na(x)))
#Analizamos la estructura del tablón
str(tablon)
#Introducimos las fechas como factor
tablon$AÑO <-as.factor(tablon$AÑO)
tablon$MES <-as.factor(tablon$MES)
# #selecciono fecha test. Tomo el 0.75 del tablón como datos de entrenamiento y el 0.25 como test
round(nrow(tablon)*0.75)
rango_test  <- 148:186
rango_train <-  1:147

test<- tablon[rango_test,]
# #selecciono conjunto de entrenamiento
train<- tablon[rango_train,]
 
#Escalamos con el método min max
maxs <- apply(tablon[,3:ncol(tablon)], 2, max) 
mins <- apply(tablon[,3:ncol(tablon)], 2, min)

#Tenemos 4 tablas, por lo que dividimos entre 4 para calcular el total
tablon[,4:ncol(tablon)] <- tablon[,4:ncol(tablon)]/4

#Escalamos los datos a la unidad, ya que funcionan mejor las redes neuronales
scaled <- as.data.frame(scale(tablon[,3:ncol(tablon)], center = mins, scale = maxs - mins))

#Creamos conjunto de datos de entrenamiento y test escalado
test_<- scaled[rango_test,]
train_<- scaled[rango_train,]

#Elaboramos la fórmula
n <- names(train_)
length(n)
f <- as.formula(paste("TOTAL ~", paste(n[!n %in% "TOTAL"], collapse = " + ")))
#Ejecutamos la red neuronal
nn <- neuralnet(f,data=train_,hidden=n_neuronas,linear.output=T)


#Representamos red neuronal
par(mar=numeric(4),family='serif')
plot.nnet(nn)


pr.nn <- compute(nn,test_[,2:ncol(test_)])

pr.nn_ <- pr.nn$net.result*(max(tablon$TOTAL)-min(tablon$TOTAL))+min(tablon$TOTAL)
test.r <- (test_$TOTAL)*(max(tablon$TOTAL-min(tablon$TOTAL))+min(tablon$TOTAL))
test$TOTAL

#Error MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

test.r
par(mfrow=c(1,2))
plot(test$TOTAL,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.9)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')




set.seed(450)
cv.error <- NULL
k <- 100

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(tablon),round(0.9*nrow(tablon)))
  train.cv <- scaled[rango_train,]
  test.cv <- scaled[rango_test,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(n_neuronas),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,2:ncol(test.cv)])
  pr.nn <- pr.nn$net.result*(max(tablon$TOTAL)-min(tablon$TOTAL))+min(tablon$TOTAL)
  
  test.cv.r <- (test.cv$TOTAL)*(max(tablon$TOTAL)-min(tablon$TOTAL))+min(tablon$TOTAL)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

#MSE
mean(cv.error)


#Métricas de error
accuracy(test.r,test$TOTAL)


#Representación error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


#Representación de la predicción
x <- (test[,PREDICCIÓN := pr.nn_])
ncol(test)
Número_turistas <- ts(test[,c(3,33)],start = c(2013, 4),end = c(2016,6), frequency = 12)


autoplot(Número_turistas,size=10, xlab= "Año", ylab="Número de turistas")
