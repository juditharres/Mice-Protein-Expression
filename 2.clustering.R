##------LIBRERIAS---------

library(RColorBrewer)
library(ggplot2) 
library(cluster)

##-------PCA------

ACP <- princomp(mice_scale[, 3:79], scores=TRUE)
summary(ACP) #Necesitaríamos 9 componentes para explicar el menos el 80% de la varianza del banco de datos original.

#Graficamos las dos primeras componentes.

colores <- brewer.pal(9,"Set1")
length(colores)

qplot(ACP$scores[, 1], ACP$scores[, 2], data = mice_scale, color = class,xlab = "Componente 1", ylab = "Componente 2", alpha = I(0.7), main = "PCA")


##-------k-Means------

#Número de clusters a elegir.

sc <- c()
for (k in 1:15){
  kk <- kmeans(mice_scale[,3:79], centers=k, nstart = 50)
  sc[k] <- kk$tot.withinss
}

ggplot() + geom_point(aes(x = 1:15, y = sc[1:15]), color = 'blue') + 
  geom_line(aes(x = 1:15, y = sc[1:15]), color = 'blue') + 
  xlab('Cantidad de Centroides k') + 
  ylab('SCDG')



#Pintando los resultados - k=5.

set.seed(123)
km5 <- kmeans(mice_scale[,3:79], centers=5, nstart = 50)
mice_scale$km5 <- km5$cluster

qplot(ACP$scores[, 1], ACP$scores[, 2], data = mice_scale, color = factor(km5), xlab = "Comp. 1 (25.08%)", ylab = "Comp. 2 (18.02%)", alpha = I(0.7)) +
  stat_ellipse() + labs(color='Cluster')

#Pintando los resultados - k=8.

set.seed(123)
km8 <- kmeans(mice_scale[,3:79], centers=8, nstart = 50)
mice_scale$km8 <- km8$cluster

qplot(ACP$scores[, 1], ACP$scores[, 2], data = mice_scale, color = factor(km8), xlab = "Comp. 1 (25.08%)", ylab = "Comp. 2 (18.02%)", alpha = I(0.7)) +
  stat_ellipse() + labs(color='Cluster')


#Matrices de confusión.

matriz_confusion_general_km5 <- table(mice_scale$class, mice_scale$km5,  dnn = c("Observaciones", "Predicciones"))
matriz_confusion_general_km5

matriz_confusion_general_km8 <- table(mice_scale$class, mice_scale$km8,  dnn = c("Observaciones", "Predicciones"))
matriz_confusion_general_km8




