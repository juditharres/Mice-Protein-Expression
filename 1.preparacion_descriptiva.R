##------LIBRERIAS---------

library(readxl)
library(dplyr)
library(tidymodels)
library(corrplot)
library(ggplot2)
library(ggplot)
library(ggcorrplot)
library(reshape2)


##---LECTURA DE LA BBDD--------

mice <- read_xls("./Datos/Mice_expression.xls")

mice$raton <- rep(1:72, each=15) #Cada 15 medidas es un ratón.
mice <- mice[, c(83, 1:82)] 


##---DESCRIPTIVA VARIABLES CONTINUAS--------

#Descriptivos numéricos de algunas de las variables continuas.

round(summary(mice$ITSN1_N),3)
round(sd(mice$ITSN1_N, na.rm=TRUE),3)

round(summary(mice$S6_N),3)
round(sd(mice$S6_N, na.rm=TRUE),3)

round(summary(mice$H3AcK18_N),3)
round(sd(mice$H3AcK18_N, na.rm=TRUE),3)

round(summary(mice$H3MeK4_N),3)
round(sd(mice$H3MeK4_N, na.rm=TRUE),3)

round(summary(mice$pPKCAB_N),3)
round(sd(mice$pPKCAB_N, na.rm=TRUE),3)

round(summary(mice$pBRAF_N),3)
round(sd(mice$pBRAF_N, na.rm=TRUE),3)

round(summary(mice$BDNF_N),3)
round(sd(mice$BDNF_N, na.rm=TRUE),3)

round(summary(mice$GSK3B_N),3)
round(sd(mice$GSK3B_N, na.rm=TRUE),3)

round(summary(mice$ELK_N),3)
round(sd(mice$ELK_N, na.rm=TRUE),3)

round(summary(mice$pCFOS_N),3)
round(sd(mice$pCFOS_N, na.rm=TRUE),3)

round(summary(mice$SOD1_N),3)
round(sd(mice$SOD1_N, na.rm=TRUE),3)



#Descriptivos gráficos.

ggplot(mice, aes(y = pPKCAB_N)) + geom_boxplot(fill="salmon") + scale_y_continuous(NULL)
 
ggplot(mice, aes(y = pBRAF_N)) + geom_boxplot(fill="lightblue") + scale_y_continuous(NULL)

ggplot(mice, aes(y = SOD1_N)) + geom_boxplot(fill="pink2") + scale_y_continuous(NULL)



##---DESCRIPTIVA VARIABLES CATEGÓRICAS--------

#Frecuencias absolutas de las variables discretas.
table(mice$Genotype) 
table(mice$Treatment) 
table(mice$Behavior) 
table(mice$class) 

#Frecuencias relativas de las variables discretas.

round((table(mice$Genotype)/ margin.table(table(mice$Genotype))*100),3) 
round((table(mice$Treatment)/ margin.table(table(mice$Treatment))*100),3) 
round((table(mice$Behavior)/ margin.table(table(mice$Behavior))*100),3) 
round((table(mice$class)/ margin.table(table(mice$class))*100),3) 

#Diagrama de barras.


ggplot(data = mice) + stat_count(mapping = aes(x = Genotype), fill=c("hotpink1","aquamarine")) + theme(axis.title.x=element_blank()) + scale_y_continuous(NULL)

ggplot(data = mice) + stat_count(mapping = aes(x = Behavior), fill=c("hotpink1","aquamarine")) + theme(axis.title.x=element_blank()) + scale_y_continuous(NULL)

ggplot(data = mice) + stat_count(mapping = aes(x = Treatment), fill=c("hotpink1","aquamarine")) + theme(axis.title.x=element_blank()) + scale_y_continuous(NULL)



##----ESTUDIO DE OUTLIERS-------

options(max.print=2000)

apply(X = is.na(mice), MARGIN = 1, FUN = mean) #Las filas 988, 989 y 990 tiene muchos na. Concretamente un 51.81% de valores faltantes en todas las proteínas. Es el ratón 66, perteneciente al grupo t-SC-s.

mice_outliers <- subset(mice, mice$raton!=66)


#Ahora vamos a estudiar los outliers por columnas.

apply(X = is.na(mice_outliers), MARGIN = 2, FUN = sum)
apply(X = is.na(mice_outliers), MARGIN = 2, FUN = mean)


##------ ESTUDIO DE DATOS FALTANTES Y SUSTITUCIÓN POR LA MEDIA --------------

cCSm <- subset(mice_outliers, mice_outliers$class=="c-CS-m")
cCSs <- subset(mice_outliers, mice_outliers$class=="c-CS-s")
cSCm <- subset(mice_outliers, mice_outliers$class=="c-SC-m")
cSCs <- subset(mice_outliers, mice_outliers$class=="c-SC-s")
tCSm <- subset(mice_outliers, mice_outliers$class=="t-CS-m")
tCSs <- subset(mice_outliers, mice_outliers$class=="t-CS-s")
tSCm <- subset(mice_outliers, mice_outliers$class=="t-SC-m")
tSCs <- subset(mice_outliers, mice_outliers$class=="t-SC-s")


#Columnas que continen valores u observaciones faltantes.

mice_outliers %>% map_dbl(.f = function(x){sum(is.na(x))})


#Reemplazamos los valores faltantes por la media.

mice_impute_mean_cCSm <- data.frame(sapply(cCSm, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_cCSs <- data.frame(sapply(cCSs, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_cSCm <- data.frame(sapply(cSCm, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_cSCs <- data.frame(sapply(cSCs, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_tCSm <- data.frame(sapply(tCSm, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_tCSs <- data.frame(sapply(tCSs, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_tSCm <- data.frame(sapply(tSCm, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))
mice_impute_mean_tSCs <- data.frame(sapply(tSCs, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))


#Comprobamos que se han sustituido correctamente.

mice_impute_mean_cCSm %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_cCSs %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_cSCm %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_cSCs %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_tCSm %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_tCSs %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_tSCm %>% map_dbl(.f = function(x){sum(is.na(x))})
mice_impute_mean_tSCs %>% map_dbl(.f = function(x){sum(is.na(x))})

#Construimos la bbdd final, juntando todos los grupos que ya no contienen valores faltantes; y le damos formato.

mice_impute_mean <- bind_rows(mice_impute_mean_cCSm, mice_impute_mean_cCSs, mice_impute_mean_cSCm, mice_impute_mean_cSCs, mice_impute_mean_tCSm, mice_impute_mean_tCSs, mice_impute_mean_tSCm, mice_impute_mean_tSCs)

for (i in 3:79) {
  mice_impute_mean[,i] <- as.numeric(mice_impute_mean[,i])
  
}

rm(mice_impute_mean_cCSm, mice_impute_mean_cCSs, mice_impute_mean_cSCm, mice_impute_mean_cSCs, mice_impute_mean_tCSm, mice_impute_mean_tCSs, mice_impute_mean_tSCm, mice_impute_mean_tSCs)
rm(cCSm, cCSs, cSCm, cSCs, tCSm, tCSs, tSCm, tSCs)



##------ESCALAR LA BBDD---------

#Comprobamos si existe diferencia en la escala de las variables.

variables <- mice_outliers[, 3:7]
ggplot(data = melt(variables), aes(x = variable, y= value)) + geom_boxplot(aes(fill = variable)) + coord_flip() + theme(axis.title.x=element_blank()) +  theme(axis.title.y=element_blank()) + theme(legend.position='none')


#Creamos una función para escalar las variables.

escalar<-function(x){
  (x-min(x))/(max(x)-min(x))
}

mice_scale <- as.data.frame(apply(mice_impute_mean[,3:79], MARGIN=2, FUN=escalar))
mice_scale <- data.frame(mice_outliers[,1:2], mice_scale, mice_outliers[, 80:83])

rm(mice, mice_impute_mean, mice_outliers, variables)


