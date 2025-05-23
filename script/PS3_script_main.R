#-------------------------------------------------------------------#
## --------------- Problem Set 3: Making Money with ML ------------- ##
## - Santiago Melo - Miguel Blanco - Diana Lopera -------------------##
#-------------------------------------------------------------------#


# ---------- LIBRERÍAS -------------

if (!requireNamespace("recipes", quietly = TRUE)) {
  install.packages("recipes")
}
library(recipes)

if (!require("pacman")) install.packages("pacman")
library(pacman)

pacman::p_load(
  readr,        # Importar datos
  labelled,     # Manejo de etiquetas
  naniar,       # Visualizar datos faltantes
  DataExplorer, # Gráficos de missing values
  psych,        # Estadísticas descriptivas
  rvest,        # Web scraping
  rio,          # Importar/exportar datos
  tidyverse,    # Conjunto de paquetes tidy data
  skimr,        # Resumen de datos
  visdat,       # Visualizar datos faltantes
  corrplot,     # Gráficos de correlación
  gridExtra,    # Organización de gráficos
  MASS,         # Funciones estadísticas diversas
  stargazer,    # Tablas para salida a TEX
  chromote,     # Automatización de navegador
  ggplot2,      # Gráficos
  boot,         # Funciones de bootstrap
  patchwork,    # Combinación de gráficos
  caret,        # Evaluación de modelos
  purrr,        # Funciones map_*
  kableExtra,   # Opciones de tablas
  dplyr,        # Manipulación de datos
  summarytools, # Descriptivos enriquecidos
  knitr,        # kable en LaTeX
  xtable,       # Tablas en LaTeX
  tidyr,        # Separar/unir columnas
  gmodels,      # CrossTable y más
  glmnet,       # Regularización Lasso/Ridge
  ranger,       # Random Forest rápido
  randomForest, # Random Forest clásico
  Metrics,      # Métricas de evaluación
  adabag,       # Bagging y boosting
  rsample,      # Remuestreo
  rpart,        # Árboles de decisión
  rpart.plot,   # Gráficos para árboles
  ipred,        # Modelos ensamblados
  gbm,          # Boosting
  stringi,      # Manipulación de texto
  sf,           # Datos espaciales
  tidymodels,   # Framework de modelado
  spatialsample,# Muestreo espacial
  plotly,       # Gráficos interactivos
  leaflet,      # Mapas interactivos
  tmaptools,    # Herramientas espaciales
  osmdata,      # Datos OpenStreetMap
  tm,           # Text Mining
  tidytext,     # Procesamiento de texto
  stopwords,    # Stopwords multilingües
  parsnip,      # Especificación de modelos
  dials,        # Hiperparámetros
  workflows,    # Pipelines
  tune,         # Tuning de modelos
  yardstick,    # Métricas
  udpipe,       # Procesamiento NLP
  stringr,      # Manipulación de cadenas
  nnet          # Redes neuronales simples
)


## --- Creacion de las particiones de datos para probar -------- 
#LLamamos la base desde GitHub, para no correr todo 
train <- read.csv("https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/trainfull.csv")
test <- read.csv("https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/testfull.csv")

# Variables en train (sin 'price' porque solo está en train)
vars_train <- setdiff(names(train), "price")

# Variables en test
vars_test <- names(test)

# Variables comunes
vars_comunes <- intersect(vars_train, vars_test)

# Variables que están en train pero no en test
vars_train_solo <- setdiff(vars_train, vars_test)

# Variables que están en test pero no en train
vars_test_solo <- setdiff(vars_test, vars_train)

# Mostrar resultados
list(
  comunes = vars_comunes,
  solo_en_train = vars_train_solo,
  solo_en_test = vars_test_solo
)

train <- train %>% select(-precio_por_mt2_sc, -distancia_red_vial, -precio_por_mt2)
test <- test %>% select(-price, -precio_por_mt2)

# Creamos índices para dividir 
index <- createDataPartition(train$price, p = 0.7, list = FALSE)

train_split <- train[index, ]  #Con esta se hace la estimación
test_split  <- train[-index, ] #Con esta se hace la prueba del MAE




#----- MODELOS --------------------

## ---------- OLS ----------------------
# Montamos la validación cruzada
LM_spec <- linear_reg() %>% #tune es para que escoga optimamente 
  set_engine("glm")

#grid_values_LM <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>% #estos dos ultimos codigos son para que haga la conbinacion optima entre lasso y ridge
 # expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

# price ~ bedrooms + bathrooms + property_type + distancia_parque + distancia_CC + distancia_tm + distancia_avenida

# Primera receta (recuerde poner los pasos en orden para el sistema le modele corectamente)
rec_1_LM <- recipe(price ~ bedrooms + bathrooms + property_type + distancia_parque + distancia_CC + distancia_tm + distancia_avenida , data = train_split) %>%
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_interact(terms = ~ distancia_parque:matches("property_type_") + distancia_CC:matches("property_type_") + distancia_tm:matches("property_type_") + distancia_avenida:matches("property_type_")) %>% #aca usa matches dado que en la parte anterior las variables property type se habian vuelot dummyes en la parte anterior 
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

# Segunda receta 
rec_2_LM <- recipe(price ~ bedrooms + bathrooms + property_type + distancia_parque + distancia_CC + distancia_tm + distancia_avenida
, data = train_split) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ distancia_parque:matches("property_type_")+ distancia_CC:matches("property_type_") + distancia_tm:matches("property_type_") + distancia_avenida:matches("property_type_")) %>% 
  step_interact(terms = ~ distancia_parque:bedrooms) %>% 
  step_interact(terms = ~ distancia_parque:distancia_avenida) %>% 
  step_poly(distancia_parque, distancia_avenida, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%   
  step_normalize(all_predictors())

workflow_1_LM <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1_LM) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(LM_spec)

## Lo mismo con la receta rec_2 

workflow_2_LM <- workflow() %>%
  add_recipe(rec_2_LM) %>%
  add_model(LM_spec)

## Entrenamiento de los hiperparametros: Validacion Cruzada Espacial en Bloques 

# definimos nuestra variable como sf
train_sf_LM <- st_as_sf(
  train_split,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# Definiendo la validacion cruzada en bloques 

set.seed(86936)
block_folds_LM <- spatial_block_cv(train_sf_LM, v = 5) #Esto es lo que monta la validacion cruzada 
block_folds_LM

autoplot(block_folds_LM) #esta grafica me saca los fold y me muestra que quedo donde

p_load("purrr")

walk(block_folds_LM$splits, function(x) print(autoplot(x))) #Aqui me muesta como seria cada una de las pruebas  

## Ahora procedemos a escoger y entrenar los hiperparametros para CV espacial, esto de aca en adelante es carptinteria en general

set.seed(86936)

#con el flujo de trabajo 1
tune_res1_LM <- tune_grid(
  workflow_1_LM,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds_LM,  # Folds de validación cruzada espacial,       
  metrics = metric_set(mae)  # metrica
)

collect_metrics(tune_res1_LM) #Esto te saca solo las primeras obsercacioens 
# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res1_LM<- select_best(tune_res1_LM, metric = "mae")
best_tune_res1_LM

set.seed(86936)

#Con el flujo de trabajo 2
tune_res2_LM <- tune_grid(
  workflow_2_LM,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds_LM,  # Folds de validación cruzada       
  metrics = metric_set(mae)  # metrica
)

collect_metrics(tune_res2_LM)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res2_LM<- select_best(tune_res2_LM, metric = "mae")
best_tune_res2_LM

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
res1_final_LM <- finalize_workflow(workflow_1_LM, best_tune_res1_LM)

# Ajustar el modelo  utilizando los datos de entrenamiento
res2_final_LM <- finalize_workflow(workflow_2_LM, best_tune_res2_LM)


# Aqui si vamos a entrenar el modelo con los datos finales 
EN_final1_fit_LM <- fit(res1_final_LM, data = train_split)
EN_final2_fit_LM <- fit(res2_final_LM, data = train_split)

augment(EN_final1_fit_LM, new_data = test_split) %>%
  mae(truth = price, estimate = .pred)

augment(EN_final2_fit_LM, new_data = test_split) %>%
  mae(truth = price, estimate = .pred)

## Finalmente revisando si el error es alto o bajo 

MAE_1_LM <- augment(EN_final1_fit_LM, new_data = test_split) %>%
  mae(truth = price, estimate = .pred) %>% select(.estimate) %>% pull()
nMAE1_LM<- MAE_1_LM/mean(train_split$price)*100 %>% round(.,2) #aca lo que hizo gustavo fue mirar el tamaño del error vs el promedio en general
nMAE1_LM

MAE_2_LM <- augment(EN_final2_fit_LM, new_data = test_split) %>%
  mae(truth = price, estimate = .pred) %>% select(.estimate) %>% pull()
nMAE2_LM<- MAE_2_LM/mean(train_split$price)*100 %>% round(.,2)
nMAE2_LM
# Ahora ya con los datos reales para presentar 
# ========================
# Con todos los datos 
# ========================

# Primera receta (recuerde poner los pasos en orden para el sistema le modele corectamente)
rec_1_LM <- recipe(price ~ bedrooms + bathrooms + property_type + distancia_parque + distancia_CC + distancia_tm + distancia_avenida , data = train) %>%
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_interact(terms = ~ distancia_parque:matches("property_type_") + distancia_CC:matches("property_type_") + distancia_tm:matches("property_type_") + distancia_avenida:matches("property_type_")) %>% #aca usa matches dado que en la parte anterior las variables property type se habian vuelot dummyes en la parte anterior 
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

# Segunda receta 
rec_2_LM <- recipe(price ~ bedrooms + bathrooms + property_type + distancia_parque + distancia_CC + distancia_tm + distancia_avenida
                   , data = train) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ distancia_parque:matches("property_type_")+ distancia_CC:matches("property_type_") + distancia_tm:matches("property_type_") + distancia_avenida:matches("property_type_")) %>% 
  step_interact(terms = ~ distancia_parque:bedrooms) %>% 
  step_interact(terms = ~ distancia_parque:distancia_avenida) %>% 
  step_poly(distancia_parque, distancia_avenida, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%   
  step_normalize(all_predictors())

workflow_1_LM <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1_LM) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(LM_spec)

## Lo mismo con la receta rec_2 

workflow_2_LM <- workflow() %>%
  add_recipe(rec_2_LM) %>%
  add_model(LM_spec)

## Entrenamiento de los hiperparametros: Validacion Cruzada Espacial en Bloques 

# definimos nuestra variable como sf
train_sf_LM <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# Definiendo la validacion cruzada en bloques 

set.seed(86936)
block_folds_LM <- spatial_block_cv(train_sf_LM, v = 5) #Esto es lo que monta la validacion cruzada 
block_folds_LM

autoplot(block_folds_LM) #esta grafica me saca los fold y me muestra que quedo donde

p_load("purrr")

walk(block_folds_LM$splits, function(x) print(autoplot(x))) #Aqui me muesta como seria cada una de las pruebas  

## Ahora procedemos a escoger y entrenar los hiperparametros para CV espacial, esto de aca en adelante es carptinteria en general

set.seed(86936)

#con el flujo de trabajo 1
tune_res1_LM <- tune_grid(
  workflow_1_LM,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds_LM,  # Folds de validación cruzada espacial,       
  metrics = metric_set(mae)  # metrica
)

collect_metrics(tune_res1_LM) #Esto te saca solo las primeras obsercacioens 
# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res1_LM<- select_best(tune_res1_LM, metric = "mae")
best_tune_res1_LM

set.seed(86936)

#Con el flujo de trabajo 2
tune_res2_LM <- tune_grid(
  workflow_2_LM,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds_LM,  # Folds de validación cruzada       
  metrics = metric_set(mae)  # metrica
)

collect_metrics(tune_res2_LM)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res2_LM<- select_best(tune_res2_LM, metric = "mae")
best_tune_res2_LM

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
res1_final_LM <- finalize_workflow(workflow_1_LM, best_tune_res1_LM)

# Ajustar el modelo  utilizando los datos de entrenamiento
res2_final_LM <- finalize_workflow(workflow_2_LM, best_tune_res2_LM)


# Aqui si vamos a entrenar el modelo con los datos finales 
EN_final1_fit_LM <- fit(res1_final_LM, data = train)
EN_final2_fit_LM <- fit(res2_final_LM, data = train)

augment(EN_final1_fit_LM, new_data = test) %>%
  mae(truth = price, estimate = .pred)

augment(EN_final2_fit_LM, new_data = test) %>%
  mae(truth = price, estimate = .pred)

## Finalmente sacando los valores

# Modelo 1
predicciones_1_LM <- augment(EN_final1_fit_LM, new_data = test) %>%
  dplyr::select(property_id, .pred) %>%
  dplyr::rename(price = .pred)

# Modelo 2
predicciones_2_LM<- augment(EN_final2_fit_LM, new_data = test) %>%
  dplyr::select(property_id, .pred) %>%
  dplyr::rename(price = .pred)

# Unir resultados en una sola tabla
predicciones_LM <- left_join(predicciones_1_LM, predicciones_2_LM, by = "property_id")

head(predicciones_1_LM)
head(predicciones_2_LM)
head(predicciones_LM)


# Guardamos el resultado en formato CSV para Kaggle
write.csv(predictSampleOLS, "/Users/miguelblanco/Library/CloudStorage/OneDrive-Personal/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/PS3_SM_MB_DL/stores/OLS.csv", row.names = FALSE)


##----- Elastic net--------------------

# Preprocesamiento & Feature Engineering

# Primera receta
rec_1_EN <- recipe(price ~ distancia_parque + distancia_CC + distancia_tm + 
                     distancia_avenida + rooms + bathrooms + surface_total + property_type, 
                   data = train_split) %>%
  step_interact(terms = ~ distancia_parque:property_type +
                  distancia_CC:property_type +
                  distancia_tm:property_type +
                  distancia_avenida:property_type +
                  surface_total:property_type) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Segunda receta 
rec_2_EN <- recipe(price ~ distancia_parque + distancia_CC + distancia_tm + 
                     distancia_avenida + rooms + bathrooms + surface_total + property_type, 
                   data = train_split) %>%
  step_interact(terms = ~ distancia_parque:property_type +
                  distancia_CC:property_type +
                  distancia_tm:property_type +
                  distancia_avenida:property_type +
                  surface_total:property_type) %>%
  step_poly(distancia_parque, degree = 2) %>%
  step_poly(distancia_CC, degree = 2) %>%
  step_poly(distancia_tm, degree = 2) %>%
  step_poly(distancia_avenida, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Elastic-Net 

elastic_net_spec <- parsnip::linear_reg(
  penalty = tune(), 
  mixture = tune()) %>%
  set_engine("glmnet")

grid_values_EN <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>%
  expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

set.seed(86936)
db_fold_EN <- rsample::vfold_cv(train_split, v = 5) 


# Iniciar un flujo de trabajo utilizando 'workflow()'
workflow_1_EN <- workflow() %>% 
  add_recipe(rec_1_EN) %>% 
  add_model(elastic_net_spec)

## Lo mismo con la receta rec_2 
workflow_2_EN <- workflow() %>%
  add_recipe(rec_2_EN) %>%
  add_model(elastic_net_spec)

#Fit y predict 

set.seed(86936)

tune_res1_EN  <- tune::tune_grid(
  workflow_1_EN,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = db_fold_EN,  # Folds de validación cruzada
  grid = grid_values_EN,        # Grilla de valores de penalización
  metrics = metric_set(rmse)  # metrica
)


#los resultados:

workflowsets::collect_metrics(tune_res1_EN)  

set.seed(86936)

tune_res2_EN <- tune_grid(
  workflow_2_EN,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = db_fold_EN,  # Folds de validación cruzada
  grid = grid_values_EN,        # Grilla de valores de penalización
  metrics = metric_set(rmse)  # metrica
)

collect_metrics(tune_res2_EN)


best_penalty_1_EN <- select_best(tune_res1_EN, metric = "rmse")
best_penalty_1_EN

best_penalty_2_EN <- select_best(tune_res2_EN, metric = "rmse")
best_penalty_2_EN

EN_final1 <- finalize_workflow(workflow_1_EN, best_penalty_1_EN)
EN_final2 <- finalize_workflow(workflow_2_EN, best_penalty_2_EN)

EN_final1_fit <- fit(EN_final1, data = train_split)
EN_final2_fit <- fit(EN_final2, data = train_split)

# Sacamos las predicciones sobre los datos de test 
predictiones_1_EN <- predict(EN_final1_fit , new_data = test_split)
predictiones_2_EN <- predict(EN_final2_fit , new_data = test_split)

#Evaluación del Modelo (aunque esto es adicional porque es lo que hace kaggle)

# Calculamos el MAE de las predicciones 
rmse_1_EN <- test_split %>%
  bind_cols(predictiones_1_EN) %>%
  yardstick::rmse(truth = price, estimate = .pred)

rmse_2_EN <- test_split %>%
  bind_cols(predictiones_2_EN) %>%
  yardstick::rmse(truth = price, estimate = .pred)


# Resultados finales Elastic Net
resultados_EN <- c(rmse_1_EN[[".estimate"]], rmse_2_EN[[".estimate"]])
resultados_EN

#Modelo final Elastic net con todos los datos 
EN_final_Kaggle <- finalize_workflow(workflow_2_EN, best_penalty_2_EN)
EN_fit_Kaggle <- fit(EN_final_Kaggle, data = train)

EN_final_predictions_Kaggle  <- predict(EN_fit_Kaggle, new_data = test)

# Crear dataframe con property_id + predicción
submission <- test %>%
  select(property_id) %>%               # selecciona solo el ID
  bind_cols(EN_final_predictions_Kaggle)  # une con predicciones

# Renombra columnas según lo que espera Kaggle
names(submission) <- c("property_id", "price")

# Guardar archivo final-revisar la ruta (pendiente!)
write.csv(submission, "D:/OneDrive - CGIAR/Pictures/Diplomado_BigData/Problem_set/ProblemSet3/Elastic_net.csv", row.names = FALSE)

##----- Redes neuronales--------------------


formula <- as.formula(
  paste("price ~ distancia_parque + distancia_CC + distancia_tm + 
  distancia_avenida + ascensor + parquedero + rooms + deposito + bedrooms + bathrooms + surface_total + property_type"
  )
)


recipe_nnet <- recipes::recipe(
  formula  , data = train_split) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

#Primera Aproximación

nnet_base <- 
  parsnip::mlp(
    hidden_units = 6,
    epochs = 100,
    engine = 'nnet' # valor por defecto
  ) %>% 
  parsnip::set_mode("regression") %>% 
  parsnip::set_engine("nnet")
nnet_base

#Flujo de trabajo

workflow_base <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_base) 

#entrenemos el modelo

base_final_fit <- fit(workflow_base, data = train_split)

#calcular el desempeño fuera de muestra usando nuestra base de datos test:

broom::augment(base_final_fit, new_data = test_split) %>%
  mae(truth = price, estimate = .pred)


## Selección de hiperparámetros: definir validación cruzada tradicional 

# Creamos folds de validación cruzada tradicional
set.seed(86936)
cv_folds <- vfold_cv(train_split, v = 5)

# Visualizamos la estructura de los folds
cv_folds

#definir grilla y hacer el ejercicio de validación cruzada para nuevo flujo de trabajo:

nnet_tune <- 
  parsnip::mlp(hidden_units =tune(), epochs = tune()) %>% 
  parsnip::set_mode("regression") %>% 
  parsnip::set_engine("nnet", trace = 0) #trace 0 previene la verbosidad del entrenamiento

#Definir la grilla de parámetros que vamos a usar en el ejercicio de validación cruzada:

grid_values <- tidyr::crossing(
  # `crossing` nos permite generar una grilla 
  # rectangular con la combinación de todos los hiperparámetros. 
  hidden_units = seq(from= 5, to=60, by = 5),
  epochs =  seq(from= 300, to=500, by = 100)
)
#Especificar nuevo flujo de trabajo

workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_tune) 

#realizar la validación cruzada

set.seed(86936)

tune_nnet <- tune_grid(
  workflow_tune,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = cv_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores 
  metrics = metric_set(mae)  # métrica
)

#mejores métricas utilizando select_best()

best_tune_nnet <- select_best(tune_nnet, metric = "mae")
best_tune_nnet

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parámetros
nnet_tuned_final <- finalize_workflow(workflow_tune, best_tune_nnet)

nnet_tuned_final_fit <- fit(nnet_tuned_final, data = train_split)

#evaluar su desempeño

## predicciones finales 
mae<- augment(nnet_tuned_final_fit, new_data = test_split) %>%
  mae(truth = price, estimate = .pred) 
mae[1,3]


###Entrenar con TODO train, predecir en test

# Reutilizar la receta pero ajustada a toda la data
recipe_final <- recipe(formula, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Flujo con todo el train
workflow_final <- workflow() %>%
  add_recipe(recipe_final) %>%
  add_model(finalize_model(nnet_tune, best_tune_nnet))  # Usa los mejores hiperparámetros

# Entrenar con todo el train
fit_final <- fit(workflow_final, data = train)

# Predecir sobre test (sin price si está presente)
test_input <- test %>% select(-price)  # O ignora esta línea si no tiene price

predicciones <- predict(fit_final, new_data = test_input)

# Resultado final para Kaggle
resultado_NN_kaggle <- tibble(
  property_id = test$property_id,
  price = predicciones$.pred
)

# Exportar CSV
write.csv(resultado_NN_kaggle, "red_neuronal.csv", row.names = FALSE)

## ARBOLES ----------

# Especificación del modelo CART
CART_spec <- decision_tree(cost_complexity = tune(), tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Grilla de hiperparámetros para CART
grid_values_CART <- grid_regular(
  cost_complexity(range = c(-4, -1)),
  tree_depth(range = c(2, 20)),
  levels = 5
)

# Primera receta
rec_1_CART <- recipe(price ~ distancia_parque + distancia_CC + distancia_tm + 
                       distancia_avenida + bedrooms + bathrooms + surface_3 + property_type + estrato, data = train_split) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ distancia_parque:matches("property_type") + distancia_tm:matches("property_type") + distancia_CC:matches("property_type") + distancia_avenida:matches("property_type") + estrato:matches("property_type"))

# Flujos de trabajo
workflow_1_CART <- workflow() %>%
  add_recipe(rec_1_CART) %>%
  add_model(CART_spec)

# Validación cruzada espacial
train_sf_CART <- st_as_sf(train_split, coords = c("lon", "lat"), crs = 4326)

set.seed(86936)
block_folds_CART <- spatial_block_cv(train_sf_CART, v = 5)
autoplot(block_folds_CART)
walk(block_folds_CART$splits, function(x) print(autoplot(x)))

# Entrenamiento y tuning
set.seed(86936)
tune_res1_CART <- tune_grid(
  workflow_1_CART,
  resamples = block_folds_CART,
  grid = grid_values_CART,
  metrics = metric_set(mae)
)

best_tune_res1_CART <- select_best(tune_res1_CART, metric = "mae")

# Finalización de workflows
res1_final_CART <- finalize_workflow(workflow_1_CART, best_tune_res1_CART)

# Entrenamiento final
EN_final1_fit_CART <- fit(res1_final_CART, data = train_split)

# Evaluación
mae(augment(EN_final1_fit_CART, new_data = test_split), truth = price, estimate = .pred)

# nMAE
MAE_1_CART <- augment(EN_final1_fit_CART, new_data = test_split) %>%
  mae(truth = price, estimate = .pred) %>%
  select(.estimate) %>% pull()
nMAE1_CART <- MAE_1_CART / mean(train_split$price) * 100 %>% round(2)


nMAE1_CART

#### Ahora con el modelo completo ---------------------
# Primera receta
rec_1_CART <- recipe(price ~ distancia_parque + distancia_CC + distancia_tm + 
                       distancia_avenida + bedrooms + bathrooms + surface_3 + property_type + estrato, data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ distancia_parque:matches("property_type") + distancia_tm:matches("property_type") + distancia_CC:matches("property_type") + distancia_avenida:matches("property_type") + estrato:matches("property_type"))

# Flujos de trabajo
workflow_1_CART <- workflow() %>%
  add_recipe(rec_1_CART) %>%
  add_model(CART_spec)

# Validación cruzada espacial
train_sf_CART <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

set.seed(86936)
block_folds_CART <- spatial_block_cv(train_sf_CART, v = 5)
autoplot(block_folds_CART)
walk(block_folds_CART$splits, function(x) print(autoplot(x)))

# Entrenamiento y tuning
set.seed(86936)
tune_res1_CART <- tune_grid(
  workflow_1_CART,
  resamples = block_folds_CART,
  grid = grid_values_CART,
  metrics = metric_set(mae)
)

best_tune_res1_CART <- select_best(tune_res1_CART, metric = "mae")
best_tune_res1_CART

# Finalización de workflows
res1_final_CART <- finalize_workflow(workflow_1_CART, best_tune_res1_CART)

# Entrenamiento final
EN_final1_fit_CART <- fit(res1_final_CART, data = train)

# Generando el CSV
# Obtener los valores de hiperparámetros
best_params_CART <- best_tune_res1_CART

# Extraer valores de cost_complexity y tree_depth
cc_val <- best_params_CART$cost_complexity
td_val <- best_params_CART$tree_depth

# Crear las predicciones
predicciones_CART <- augment(EN_final1_fit_CART, new_data = test) %>%
  select(property_id, .pred) %>%
  rename(price = .pred)

# Definir el nombre del archivo con formato solicitado
nombre_archivo <- sprintf(
  "CART_costcomplexity_%.4f_tree_depth_%d.csv",
  cc_val, td_val
)

# Definir la ruta completa
ruta_archivo <- file.path(
  "/Users/miguelblanco/Library/CloudStorage/OneDrive-Personal/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/PS3_SM_MB_DL/stores",
  nombre_archivo
)

# Guardar el archivo CSV
write.csv(predicciones_CART, file = ruta_archivo, row.names = FALSE)





## RANDOM FOREST 


## BOOSTING ----

# Formando los evaluadores
  fiveStats <- function(...) {
  c(
    caret::defaultSummary(...)
  )
}


ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    verbose=FALSE,
                    savePredictions = TRUE)


### Modelo Gradient Boosting ----

grid_gbm <- expand.grid(n.trees = c(50, 100, 150),
                        interaction.depth = c(1, 2),
                        shrinkage = c(0.01),
                        n.minobsinnode = c(5, 10))

set.seed(91519)
gbm_tree <- train(price~ distancia_parque + distancia_CC + distancia_tm + 
                    distancia_avenida + bedrooms + bathrooms + property_type + year + estrato,
                  data = train_split, 
                  method = "gbm", 
                  trControl = ctrl,
                  tuneGrid = grid_gbm,
                  metric = "RMSE",
                  verbose = FALSE
)

vars_modelo <- c("price", "distancia_parque", "distancia_CC", "distancia_tm", 
                 "distancia_avenida", "bedrooms", "bathrooms", 
                 "property_type", "year", "estrato")

train_split_clean <- train_split %>%
  drop_na(all_of(vars_modelo))

gbm_tree <- train(price ~ distancia_parque + distancia_CC + distancia_tm + 
                    distancia_avenida + bedrooms + bathrooms + property_type + year + estrato,
                  data = train_split_clean, 
                  method = "gbm", 
                  trControl = ctrl,
                  tuneGrid = grid_gbm,
                  metric = "RMSE",
                  verbose = FALSE
)

predictSample_gbm <- test_split %>%
  mutate(price = predict(gbm_tree, newdata = test_split)) %>%
  dplyr::select(property_id, price)

head(predictSample_gbm)

library(yardstick)

# Unimos los datos reales y predichos
evaluacion_gbm <- test_split %>%
  dplyr::select(property_id, price_real = price) %>%
  left_join(predictSample_gbm, by = "property_id")

# Calculamos el MAE
MAE_1_GBM <- mae(data = evaluacion_gbm, truth = price_real, estimate = price)
MAE_1_GBM

nMAE1_GBM <- MAE_1_GBM / mean(train_split$price) * 100 %>% round(2)


### Modelo XGBoost ----

p_load(xgboost)

grid_xgboost <- expand.grid(nrounds = c(250, 500),
                            max_depth = c(1, 2),
                            eta = c(0.1, 0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7), 
                            subsample = c(0.7))

set.seed(91519)

Xgboost_tree <- train(price ~ lat + lon + bedrooms + year + month,
                      data = train_split, 
                      method = "xgbTree", 
                      trControl = ctrl,
                      tuneGrid = grid_xgboost,
                      metric = "RMSE",
                      verbosity = 0
)

predictSample_Xgboost <- test_split %>%
  mutate(Price = predict(Xgboost_tree, newdata = test_split)) %>%
  dplyr::select(property_id, price)

head(predictSample_Xgboost)


# Formato específico Kaggle para XGBoost
nrounds_str <- gsub("\\.", "_", as.character(round(Xgboost_tree$bestTune$nrounds, 4)))
max_depth_str <- gsub("\\.", "_", as.character(Xgboost_tree$bestTune$max_depth))
eta_str <- gsub("\\.", "_", as.character(Xgboost_tree$bestTune$eta))
gamma_str <- gsub("\\.", "_", as.character(Xgboost_tree$bestTune$gamma))
colsample_bytree_str <- gsub("\\.", "_", as.character(Xgboost_tree$bestTune$colsample_bytree))
min_child_weight_str <- gsub("\\.", "_", as.character(Xgboost_tree$bestTune$min_child_weight))
subsample_str <- gsub("\\.", "_", as.character(Xgboost_tree$bestTune$subsample))

name <- paste0(
  "XTBoost_",
  "nrounds_", nrounds_str, 
  "_depth_", max_depth_str,
  "_eta_", eta_str,
  "_gamma_", gamma_str,
  "_colsample_", colsample_bytree_str,
  "_minchild_", min_child_weight_str,
  "_subsample_", subsample_str,
  ".csv"
)

write.csv(predictSample_Xgboost, name, row.names = FALSE)


## RANDOM FOREST ----
  # Con train_split y test_split ----
# Recetas
# Primera receta

rec_1_RF <- recipe(price ~ distancia_parque + distancia_CC + distancia_tm + 
                     distancia_avenida + rooms + bathrooms + surface_total + property_type, 
                   data = train_split) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%  # Primero dummies
  step_interact(terms = ~ starts_with("property_type"):all_numeric_predictors()) %>%  # Luego interacciones
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Segunda receta
rec_2_RF <- recipe(price ~ distancia_parque + distancia_CC + distancia_tm + 
                     distancia_avenida + rooms + bathrooms + surface_total + property_type, 
                   data = train_split) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%  # Primero dummies
  step_poly(distancia_parque, degree = 2) %>%
  step_poly(distancia_CC, degree = 2) %>%
  step_poly(distancia_tm, degree = 2) %>%
  step_poly(distancia_avenida, degree = 2) %>%
  step_interact(terms = ~ starts_with("property_type"):all_numeric_predictors()) %>%  # Luego interacciones
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Probamos parallel para optimizar
library(doParallel)
cores <- parallel::detectCores()
cl <- makePSOCKcluster(cores - 1)
registerDoParallel(cl)

# 1. Definir la especificación del modelo con 'ranger' para regresión
rf_spec <- rand_forest(
  mtry = tune(),
  min_n = tune(),     # <- coincide con el nombre en la grilla
  trees = 1000
) %>%
  set_engine("ranger", splitrule = "variance") %>%
  set_mode("regression")

# 2. Crear la grilla de hiperparámetros con expand.grid()
mtry_grid <- expand.grid(
  mtry = c(2, 4, 6, 8),
  min_n = c(1, 5, 10, 20, 35, 50)  # este es el nombre correcto
)

# 3. Crear workflows
workflow_1_RF <- workflow() %>%
  add_recipe(rec_1_RF) %>%
  add_model(rf_spec)

workflow_2_RF <- workflow() %>%
  add_recipe(rec_2_RF) %>%
  add_model(rf_spec)

# 4. Tuning con validación cruzada
set.seed(86936)
db_fold_EN <- vfold_cv(train_split, v = 5)

tune_res1_RF <- tune_grid(
  workflow_1_RF,
  resamples = db_fold_EN,     # folds de validación cruzada ya definidos
  grid = mtry_grid,
  metrics = metric_set(rmse)
)

tune_res2_RF <- tune_grid(
  workflow_2_RF,
  resamples = db_fold_EN,
  grid = mtry_grid,
  metrics = metric_set(rmse)
)

stopCluster(cl)
registerDoSEQ()

# 5. Elegir los mejores hiperparámetros 
best_rf_1 <- select_best(tune_res1_RF, metric = "rmse")
best_rf_2 <- select_best(tune_res2_RF, metric = "rmse")

# 6. Finalizar workflows
final_rf_1 <- finalize_workflow(workflow_1_RF, best_rf_1)
final_rf_2 <- finalize_workflow(workflow_2_RF, best_rf_2)

# 7. Ajustar sobre datos de entrenamiento
fit_rf_1 <- fit(final_rf_1, data = train_split)
fit_rf_2 <- fit(final_rf_2, data = train_split)

# 8. Predecir sobre test_split y calcular RMSE
pred_rf_1 <- predict(fit_rf_1, new_data = test_split)
pred_rf_2 <- predict(fit_rf_2, new_data = test_split)

# RMSE
rmse_rf_1 <- rmse(bind_cols(test_split, pred_rf_1), truth = price, estimate = .pred)
rmse_rf_2 <- rmse(bind_cols(test_split, pred_rf_2), truth = price, estimate = .pred)

# MAE
mae_rf_1 <- yardstick::mae(bind_cols(test_split, pred_rf_1), truth = price, estimate = .pred)
mae_rf_2 <- yardstick::mae(bind_cols(test_split, pred_rf_2), truth = price, estimate = .pred)

# Guardamos resultados
resultados_rmse <- c(rmse_rf_1[[".estimate"]], rmse_rf_2[[".estimate"]])
resultados_mae <- c(mae_rf_1[[".estimate"]], mae_rf_2[[".estimate"]])

# Dataframe resumen
resultados_finales_rf <- tibble(
  modelo = c("Random Forest 1", "Random Forest 2"),
  RMSE = resultados_rmse,
  MAE = resultados_mae
)

resultados_finales_rf

  # Entrenamiento Kaggle con todos los datos train y test ----
# 9. Entrenamiento final para Kaggle
final_rf_kaggle <- finalize_workflow(workflow_2_RF, best_rf_2)  # Asumiendo que rec_2 fue mejor
fit_rf_kaggle <- fit(final_rf_kaggle, data = train)

# Predicciones sobre test
pred_rf_kaggle <- predict(fit_rf_kaggle, new_data = test)

# Crear submission
submission_rf <- test %>%
  select(property_id) %>%
  bind_cols(pred_rf_kaggle)

names(submission_rf) <- c("property_id", "price")

# Guardar archivo final-revisar la ruta (pendiente!)
write.csv(submission_rf, "C:/Users/samel/OneDrive/Datos adjuntos/Universidad de los Andes/IV/Big Data - Machine Learning/GitHub/PS3_SM_MB_DL/stores/RF_V2.csv", row.names = FALSE)