##------LIBRERIAS---------

library(neuralnet)

##------RED NEURONAL MLP------

##-------DATOS-------

mice_neurona <- mice_scale[, c(3:79, 83)]
mice_neurona$class <-as.factor(mice_neurona$class)

#Creamos los grupos train y test.

set.seed(123)
train <- sample(1:nrow(mice_neurona), size = nrow(mice_neurona)*0.7)
datos_train_mice <- mice_neurona[train,]
datos_test_mice  <- mice_neurona[-train,]


##------- BÚSQUEDA DEL NÚMERO DE NEURONAS Y CAPAS OCULTAS -----

#El bucle no abarca de 1 a 78 (que son las neuronas totales) para asegurar la convergencia del algoritmo.
error_red_mice <- matrix(0,nrow=67,ncol=2) 
for (j in 3:69){
  set.seed(123)
  red_n <- neuralnet(class~., data=datos_train_mice, hidden=j, algorithm="rprop+", linear.output = FALSE)
  error_red_mice[j-2,1] <- j 
  error_red_mice[j-2,2] <- red_n$result.matrix[1,]
}

error_red_mice[which.min(error_red_mice[,2]) ,] #El error más bajo se consigue utilizando 21 neuronas, y es de 0.5631096

plot(x=error_red_mice[,1], y=error_red_mice[,2], xlab="Número de neuronas", ylab="Error")


#Procedemos a comprobar si es mejor tener una o dos capas.

error_red2_mice <- matrix(0,nrow=67,ncol=2) 
for (j in 3:69){
  set.seed(123)
  red_n <- neuralnet(class~., data=datos_train_mice, hidden=c(21,j), algorithm="rprop+", linear.output = FALSE)
  error_red2_mice[j-2,1] <- j 
  error_red2_mice[j-2,2] <- red_n$result.matrix[1,]
}

error_red2_mice[which.min(error_red2_mice[,2]) ,] #El error con dos capas ocultas es mayor que con una. 

#Se entrena el modelo definitivo, que contendrá una única capa oculta con 21 neuronas.

set.seed(123)
red_mice <- neuralnet(class ~., data=datos_train_mice, hidden=21, algorithm="rprop+", linear.output = FALSE)
red_mice$result.matrix[1,]


##----PREDICCIÓN Y EVALUACIÓN DEL MODELO ------

mice_prediction <- neuralnet::compute(red_mice, datos_test_mice[,-78])
mice_idx <- apply(mice_prediction$net.result, 1, which.max)
mice_predicted <- c(levels(mice_neurona$class))[mice_idx]
table(mice_predicted, datos_test_mice$class)

#La red neuronal cuenta con una tasa de acierto de 99.375%.









