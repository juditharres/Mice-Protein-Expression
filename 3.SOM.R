##------LIBRERIAS---------

library(RColorBrewer)
library(ggplot2) 
library(kohonen)

##-------SOM----

#SOM de dimensiones 8x8 (38 ratones control + 34 trisómicos = 72 en total). 

set.seed(7)
mice.sc <- scale(mice_scale[,3:79]) #Creamos el banco de datos escalado en forma de matriz.
mice.som <- som(mice.sc, grid = somgrid(8,8,"rectangular"), rlen = 1000)
plot(mice.som, type="changes", main="") #Visualizamos si ha convergido.

#Mapa de densidad en tonos azules.

degrade.bleu <- function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))}

plot(mice.som, type="count", shape = "straight", palette.name = degrade.bleu, main="")

#Calcular el número de observaciones en cada nodo.
nb <- table(mice.som$unit.classif)
print(nb)


#Quality control plot.
plot(mice.som, type="quality", shape = "straight", palette.name = degrade.bleu, main="")

#Matriz de distancia unificada.
plot(mice.som, type="dist.neighbours", shape = "straight", palette.name = degrade.bleu, main="")

#Matriz de distancia entre nodos.
mice.nb <- table(mice.som$unit.classif)
mice.dc <- dist(getCodes(mice.som,1))

#HAC.
cah <- hclust(mice.dc,method="ward.D2",members=mice.nb)
plot(cah,hang=-1,labels=F, main="")


#Parece que una partición en 6,7 u 8 es lo más razonable. Probamos con 6:
rect.hclust(cah,k=6)

grupos_mice_6 <- cutree(cah, k=6)
print(grupos_mice_6)
table(grupos_mice_6)

#Asignamos a cada registro su clúster.
mice_scale$grupoSOM_6 <- grupos_mice_6[mice.som$unit.classif]

#Visualización de los clústers en el mapa.
colores_fondo_6 <- brewer.pal(6, "Accent")
length(colores_fondo_6)

plot(mice.som,type="mapping",bgcol=colores_fondo_6[grupos_mice_6], shape = "straight", main="")
add.cluster.boundaries(mice.som,clustering=grupos_mice_6)

#También probamos con la partición de 8. Dado que sabemos el número de grupos reales, esta debería de ser la más adecuada.

#HAC.
cah <- hclust(mice.dc,method="ward.D2",members=mice.nb)
plot(cah,hang=-1,labels=F, main="")

rect.hclust(cah,k=8)
grupos_mice_8 <- cutree(cah, k=8)
print(grupos_mice_8)
table(grupos_mice_8)

# Asignamos a cada registro su clúster.
mice_scale$grupoSOM_8 <- grupos_mice_8[mice.som$unit.classif]

# Visualización de los clústers en el mapa.
colores_fondo_8 <- brewer.pal(8, "Accent")
length(colores_fondo_8)

plot(mice.som,type="mapping",bgcol=colores_fondo_8[grupos_mice_8], shape = "straight", main="")
add.cluster.boundaries(mice.som,clustering=grupos_mice_8)


#Matriz de confusión - clasificación.
matriz_confusion_SOM_mice_8 <- table(mice_scale$class, mice_scale$grupoSOM_8,  dnn = c("Observaciones", "Predicciones"))
matriz_confusion_SOM_mice_8




