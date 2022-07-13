##------LIBRERIAS---------

library(ISLR)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(ggpubr)
library(tidymodels)
library(ranger)
library(doParallel)

##------RANDOM FOREST------
##-------DATOS-------

mice_random <- mice_scale[, c(3:79, 83)]
mice_random$class <-as.factor(mice_random$class)

#Dividimos el banco de datos en los grupos train y test.
set.seed(123)
train <- sample(1:nrow(mice_random), size = nrow(mice_random)*0.7)
datos_train <- mice_random[train,]
datos_test  <- mice_random[-train,]



##-------GRID SEARCH (búsqueda de los hiperparámetros correctos) -------

#Definición del modelo y de los hiperparámetros a optimizar.

modelo <- rand_forest(
  mode  = "classification",
  mtry  = tune(),
  trees = tune()
) %>%
  set_engine(
    engine     = "ranger",
    max.depth  = tune(),
    importance = "none",
    seed       = 123
  )

transformer <- recipe(
  formula = class ~ .,
  data    =  datos_train
)

set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 8,
  strata  = class)


#Establecemos el Workflow.

workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo)


#Grid de hiperparámetros.

hiperpar_grid <- expand_grid(
  'trees'     = c(50, 100, 500, 1000, 5000),
  'mtry'      = c(3, 5, 7, ncol(datos_train)-1),
  'max.depth' = c(1, 3, 10, 20)
)


#Optimización de hiperparámetros.

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(accuracy),
  grid      = hiperpar_grid
)

stopCluster(cl)


#Mejores hiperparámetros por CV

show_best(grid_fit, metric = "accuracy", n = 1) #Los mejores hiperparámetros son 7 precitores en cada división, 1000 árboles y una profundidad máxima de 20. .


##----PREDICCIÓN Y EVALUACIÓN DEL MODELO -------

#Entrenamiento final.

mejores_hiperpar <- select_best(grid_fit, metric = "accuracy")

modelo_final_fit <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
) %>%
  fit(
    data = datos_train
  ) 

#Error del test del modelo final.

predicciones <- modelo_final_fit %>%
  predict(new_data = datos_test)

predicciones <- predicciones %>% 
  bind_cols(datos_test %>% dplyr::select(class))

accuracy_test  <- accuracy(
  data     = predicciones,
  truth    = class,
  estimate = .pred_class,
  na_rm    = TRUE
)
accuracy_test


mat_confusion <- predicciones %>%
  conf_mat(
    truth     = class,
    estimate  = .pred_class
  )
mat_confusion

#Tras optimizar los hiperparámetros, se consigue un porcentaje de acierto del 97.19%



#Predicción de probabilidades.

predicciones <- modelo_final_fit %>%
  predict(new_data = datos_test, type = "prob")
head(predicciones, 4)

##------ IMPORTANCIA DE PREDICTORES -------

#Importancia por pureza de nodos.

# Entrenamiento modelo.
modelo <- rand_forest(
  mode  = "classification"
) %>%
  set_engine(
    engine     = "ranger",
    importance = "impurity",
    seed       = 123
  )

modelo <- modelo %>% finalize_model(mejores_hiperpar)
modelo <- modelo %>% fit(class ~., data = datos_train)

# Importancia.
importancia_pred <- modelo$fit$variable.importance %>%
  enframe(name = "predictor", value = "importancia")

# Gráfico.
ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor") +
  geom_col() +
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


#Importancia por permutación.

# Entrenamiento modelo.
modelo <- rand_forest(
  mode  = "classification"
) %>%
  set_engine(
    engine     = "ranger",
    importance = "permutation",
    seed       = 123
  )

modelo <- modelo %>% finalize_model(mejores_hiperpar)
modelo <- modelo %>% fit(class ~., data = datos_train)

# Importancia.
importancia_pred <- modelo$fit$variable.importance %>%
  enframe(name = "predictor", value = "importancia")

# Gráfico.
ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "predictor") +
  geom_col() +
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")


