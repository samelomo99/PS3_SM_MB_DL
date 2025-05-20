#-------------------------------------------------------------------#
## --------------- Problem Set 3: Making Money with ML ------------- ##
## - Santiago Melo - Miguel Blanco - Diana Lopera -------------------##
#-------------------------------------------------------------------#


# ---------- LIBRERÍAS -------------

if (!require("pacman")) install.packages("pacman")
library(pacman)

pacman::p_load(
  readr,        # Importar datos (ya incluido en tidyverse)
  labelled,     # Manejo de etiquetas
  naniar,       # Visualizar datos faltantes
  DataExplorer, # Gráficos de missing values
  psych,        # Estadísticas descriptivas
  rvest,        # Web scraping
  rio,          # Importar/exportar datos
  tidyverse,    # Conjunto de paquetes para tidy data (incluye dplyr, ggplot2, etc.)
  skimr,        # Resumen de datos
  visdat,       # Visualizar datos faltantes
  corrplot,     # Gráficos de correlación
  gridExtra,    # Organización de gráficos
  MASS,         # Funciones estadísticas diversas
  stargazer,    # Tablas para salida a TEX
  chromote,     # Automatización de navegador (útil para scraping avanzado)
  ggplot2,      # Gráficos (ya incluido en tidyverse)
  boot,         # Funciones de bootstrap
  patchwork,    # Combinación de gráficos
  caret,         # For predictive model assessment
  purrr,
  kableExtra,   # Opciones adicionales para kable()
  dplyr,          # Manipulación de datos
  summarytools,
  knitr,          # kable() para generar tablas en LaTeX
  xtable,
  tidyr,
  gmodels,
  glmnet,
  ranger, # Para bagging y random forest
  randomForest, # Para random forest
  tidyverse, # tidy-data
  caret ,  # for model training and tunning
  Metrics, ## Evaluation Metrics for ML
  adabag,
  rsample,#para remuestreos y dividir la muestra
  rpart, 
  rpart.plot, 
  ipred,
  gbm,
  visdat,
  stringi,
  sf, # Leer/escribir/manipular datos espaciales
  tidymodels,  # entrenamiento de modelos
  spatialsample # Muestreo espacial para modelos de aprendizaje automático
  )




# ---------- BASE DE DATOS --------------
train_basica <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/train.csv"
) #Esta es la base de train que esta en kaggle, en base a esta se deben crear las nuevas variables 

test_basica <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/test.csv"
) # La base de test original 

# --- Modificaicones a la base de datos ------#
#Aca van las modificaciones a la base de datos original para lograr que el modelo funcione.

train_basica %>%
  count(property_type) #aca miramos cuantos tipos de pripiedad hay, tenemos dos Apartamentos y Casas

#para ver las missings
missing_info <- data.frame(
  variable = names(train_basica),
  missing_count = sapply(train_basica, function(x) sum(is.na(x))),
  missing_pct = sapply(train_basica, function(x) mean(is.na(x))) * 100
)

skim(train_basica)
#y graficamente 
vis_dat(train_basica)

#ahora vamos a imputar las variabes con muchos missings 
train_basica %>%
  count(rooms) #valor mas comun es 3

train_basica %>%
  count(bathrooms) #valor mas comun es 2

# Calcular la mediana
mediana_sup_cubierta <- median(train_basica$surface_covered, na.rm = TRUE)
mediana_sup_total<- median(train_basica$surface_total, na.rm = TRUE)

# Imputar datos faltantes
train <- train_basica %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total),)

# Revision de la nueva base

skim(train)

# volviendo a revisar graficamente 
vis_dat(train)

# --- Ahora con las variables test --------3
skim(test_basica)
#y graficamente 
vis_dat(test_basica)

#ahora vamos a imputar las variabes con muchos missings 
test_basica %>%
  count(rooms) #valor mas comun es 3

test_basica %>%
  count(bathrooms) #valor mas comun es 2

# Calcular la mediana
#mediana_sup_cubierta_test <- median(test_basica$surface_covered, na.rm = TRUE)
#mediana_sup_total_test<- median(test_basica$surface_total, na.rm = TRUE)

# Imputar datos faltantes
test <- test_basica %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, mediana_sup_cubierta),
         surface_total = replace_na(surface_total, mediana_sup_total))

# Revision de la nueva base

skim(test)

# volviendo a revisar graficamente 
vis_dat(test)

# ------- Agregamos las nuevas variables --------------#







# --- Creacion de las particiones de datos para probar -------- #
# Creamos índices para dividir (Recuerde cambiarlos una vez este la base de datos final)
index <- createDataPartition(train$price, p = 0.7, list = FALSE)

train_split <- train[index, ]  #Con esta se hace la estimación
test_split  <- train[-index, ] #Con esta se hace la prueba del F1




#----- MODELOS --------------------

## ---------- OLS ----------------------
# Montamos la validación cruzada
set.seed(10101)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     savePredictions = TRUE)

# Usamos un modelo con todas las variables

model_ols1 <- train(price ~ lat + lon + bedrooms + year + month + bathrooms + property_type,
                    data = train,
                    method = "glm",
                    trControl = ctrl) 

model_ols1

# Realizamos la predicción
predictSampleOLS <- test %>% 
  mutate(price = predict(model_ols1, newdata = test)) %>% 
  dplyr::select(property_id, price)

head(predictSampleOLS)

# Guardamos el resultado en formato CSV para Kaggle
write.csv(predictSampleOLS, "/Users/miguelblanco/Library/CloudStorage/OneDrive-Personal/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/PS3_SM_MB_DL/stores/OLS.csv", row.names = FALSE)

## ---------- ELASTIC NET  ----
set.seed(1410)

# Grilla para glmnet
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>%
  expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

### Modelo con Accuracy ----
ctrl_acc <- trainControl(method = "cv",
                         number = 5,
                         savePredictions = T)

model_acc <- train(price ~ lat + lon + bedrooms + year + month + bathrooms + property_type,
                   data = train_split, method = "glmnet",
                   metric = "RMSE", trControl = ctrl_acc, 
                   tuneGrid = grid)

### Modelo con RMSE ----
ctrl_rmse <- trainControl(method = "cv", number = 5,
                          savePredictions = T)

model_rmse <- train(price ~ lat + lon + bedrooms + year + month + bathrooms + property_type,
                    data = train_split, method = "glmnet",
                    metric = "RMSE", trControl = ctrl_rmse, 
                    tuneGrid = grid)


# Probamos el mejor modelo
# Obtener las predicciones de cada modelo
pred_acc <- predict(model_acc, newdata = test_split)
pred_rmse  <- predict(model_rmse,  newdata = test_split)

head(pred_acc)
head(pred_rmse)

# Evaluar cada uno
RMSE(pred_acc, test_split$price)
RMSE(pred_rmse, test_split$price)


## Envío Kaggle Elastic Net ## 
# Poner el modelo que en efecto funcione

ctrl_rmse_f <- trainControl(method = "cv",
                           number = 5,
                           savePredictions = T)

model_rmse_f <- train(price ~ lat + lon + bedrooms + year + month,
                     data = train_basica, method = "glmnet",
                     metric = "RMSE", trControl = ctrl_acc, 
                     tuneGrid = grid)


# Hacemos la predicción

predictSample_en <- test_basica %>%
  mutate(price= predict(model_rmse_f, newdata = test_basica)) %>%
  dplyr::select(property_id, price)

head(predictSample_en)

# Generando el archivo que lo saque

lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model_rsme_f$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model_rmse_f$bestTune$alpha))

name<- paste0(
  "EN_VF_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample_en, name, row.names = FALSE)

## ARBOLES ----------

# Version 1

complex_tree_1 <- rpart(price ~ lat + lon + bedrooms + year + month, 
                        data = train_split,
                        method = "anova",
                        cp = 0)

prp(complex_tree_1)


# Poda del árbol
arbol_1 <- rpart(price ~ lat + lon + bedrooms + year + month, 
                 data = train_split,
                 method = "anova")

# AUC para el árbol
pred_prob_1 <- predict(arbol_1, newdata = test_split)

# Evaluación con RMSE
rmse_arbol_1 <- Metrics::rmse(test_split$price, pred_prob_1)
print(rmse_arbol_1)


# Control para cross-validation
ctrl <- trainControl(method = "cv",
                     number = 5,
                     savePredictions = TRUE)

# Grilla de cp
grid <- expand.grid(cp = seq(0, 0.03, 0.001))

# Modelo con cross-validation
cv_tree_1 <- train(price ~ lat + lon + bedrooms + year + month,
                   data = train_split,
                   method = "rpart", 
                   trControl = ctrl, 
                   tuneGrid = grid,
                   metric = "RMSE")

# Predicción para Kaggle
predictSample_CART <- test_split %>% 
  mutate(price = predict(cv_tree_1, newdata = test_split)) %>%
  dplyr::select(property_id, price)

head(predictSample_CART)

name <- "CART_alpha0.csv"
write.csv(predictSample_CART, name, row.names = FALSE)

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
gbm_tree <- train(price~lat + lon + bedrooms + year + month,
                  data = train_split, 
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



