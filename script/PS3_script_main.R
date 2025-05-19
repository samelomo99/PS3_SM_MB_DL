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
  ipred
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

# --- Creacion de las particiones de datos para probar -------- #
# Creamos índices para dividir (Recuerde cambiarlos una vez este la base de datos final)
index <- createDataPartition(train_basica$price, p = 0.7, list = FALSE)

train_split <- train_basica[index, ]  #Con esta se hace la estimación
test_split  <- train_basica[-index, ] #Con esta se hace la prueba del F1




#----- MODELOS --------------------

## ---------- OLS ----------------------
# Montamos la validación cruzada
set.seed(10101)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     savePredictions = TRUE)

# Usamos un modelo con todas las variables

model_ols1 <- train(price ~ lat + lon + bedrooms + year,
                    data = train_basica,
                    method = "glm",
                    trControl = ctrl) 

model_ols1

# Realizamos la predicción
predictSampleOLS <- test_basica %>% 
  mutate(price = predict(model_ols1, newdata = test_basica)) %>% 
  dplyr::select(property_id, price)

head(predictSampleOLS)

# Guardamos el resultado en formato CSV para Kaggle
write.csv(predictSampleOLS, "OLS.csv", row.names = FALSE)

## ---------- ELASTIC NET  ----
set.seed(1410)


# Grilla para glmnet
grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),
  lambda = 10^seq(10, -2, length = 10)
)


### Modelo con Accuracy ----
ctrl_acc <- trainControl(method = "cv",
                         number = 5,
                         savePredictions = T)

model_acc <- train(price ~ lat + lon + bedrooms + year + month,
                   data = train_split, method = "glmnet",
                   metric = "RMSE", trControl = ctrl_acc, 
                   tuneGrid = grid)

### Modelo con RMSE ----
ctrl_rmse <- trainControl(method = "cv", number = 5,
                          savePredictions = T)

model_rmse <- train(price ~ lat + lon + bedrooms + year + month,
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

ctrl_acc_f <- trainControl(method = "cv",
                           number = 5,
                           savePredictions = T)

model_acc_f <- train(price ~ lat + lon + bedrooms + year + month,
                     data = train, method = "glmnet",
                     metric = "RMSE", trControl = ctrl_acc, 
                     tuneGrid = grid)


# Hacemos la predicción

predictSample_en <- test %>%
  mutate(price= predict(model_acc_f, newdata = test)) %>%
  dplyr::select(property_id, price)

head(predictSample_en)


lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model_acc_f$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model_acc_f$bestTune$alpha))

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
  dplyr::select(property.id, price)

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

### -----Modelo Adaboost -----------

#Formando la grilla de valores a usar en el proceso de entrenamiento

adagrid <- expand.grid(
  mfinal = c(50, 100), 
  maxdepth = c(1, 2), 
  coeflearn = c('Breiman')
)

#Creando el modelo

set.seed(10101)

adaboost_tree <- train(price~lat + lon + bedrooms + year + month,
                       data = train_split, 
                       method = "AdaBoost.M1",
                       trControl = ctrl,
                       metric = "RMSE",
                       tuneGrid = adagrid
)

#ahora predecimos el funcionamiento del modelo
predictSample_adaboost <- test_split   %>% 
  mutate(price = predict(adaboost_tree, newdata = test_split)) %>%
  dplyr::select(property.id, price)

head(predictSample_adaboost)

# Formato específico Kaggle
lambda_str <- gsub("\\.", "_", as.character(round(adaboost_tree$bestTune$mfinal, 4)))
alpha_str <- gsub("\\.", "_", as.character(adaboost_tree$bestTune$maxdepth))
al_str <- gsub("\\.", "_", as.character(adaboost_tree$bestTune$coeflearn))

name<- paste0(
  "Adaboost_", lambda_str,
  "_mfinal_" , alpha_str, 
  "_coeflearn_", al_str,
  ".csv")

write.csv(predictSample_adaboost, name, row.names = FALSE)


### Modelo Gradient Boosting ----

p_load(gbm)

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
  dplyr::select(property.id, price)

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
  dplyr::select(property.id, Price)

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
