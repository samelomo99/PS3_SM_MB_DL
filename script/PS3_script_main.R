#-------------------------------------------------------------------#
## --------------- Problem Set 3: Making Money with ML ------------- ##
## - Santiago Melo - Miguel Blanco - Diana Lopera -------------------##
#-------------------------------------------------------------------#


# ---------- LIBRERÍAS ---------- # ----

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
  adabag
)


# ---------- BASE DE DATOS ---------- # ----
train_basica <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/train.csv"
) #Esta es la base de train que esta en kaggle, en base a esta se deben crear las nuevas variables 

test_basica <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/test.csv"
) # La base de test original 

# --- Modificaicones a la base de datos ------#
#Aca van las modificaciones a la base de datos original para lograr que el modelo funcione.



#----- MODELOS ----------------------# --------

# ---------- OLS ---------- # ----
#Montamos la validacion cruzada
set.seed(10101)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     savePredictions = T)

#Usamos un modelo con todas las varibles

model_ols1 <- train(precio ~ lat + lot + bedroom + year + month,
                    data = train,
                    metric = "Accuracy",
                    method = "glm",
                    trControl = ctrl) 

model_ols1

#Obtenemos la matriz de confusion 

y_pred <- predict(model_ols1, newdata = train)
y_true <- factor(train$Pobre, levels = c("No", "Si"))

confusionMatrix(data = y_pred, reference = y_true, positive = "Si")


#Haciendo la prediccion 
predictSample <- test   %>% 
  mutate(pobre_lab = predict(model_ols1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% dplyr::select(id,pobre_lab)

head(predictSample)

# Transformamos variable pobre para que cumpla con la especificación de la competencia
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  dplyr::select(id,pobre)
head(predictSample)

# Formato específico Kaggle 

name<- paste0(
  "OLS.csv") #Dado que el modelo no tiene hiperparametros no es necesario usar algo mas sofisticado

write.csv(predictSample,name, row.names = FALSE)


# ---------- ELASTIC NET ---------- # ----
set.seed(1410)
library(caret)

# Creamos índices para dividir
index <- createDataPartition(train$Pobre, p = 0.7, list = FALSE)

train_split <- train[index, ]
test_split  <- train[-index, ]

# Grilla para glmnet
grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),
  lambda = 10^seq(10, -2, length = 10)
)

# Función F1 personalizada
f1_summary <- function(data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = lev[1])
  recall <- sensitivity(data$pred, data$obs, positive = lev[1])
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  out <- c(F1 = f1)
  return(out)
}

# Modelo con Accuracy ----
ctrl_acc <- trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE,
                         savePredictions = T)

model_acc <- train(Pobre ~ Nper + edad_jefe + nocupados + nmujeres + nmenores + 
                     H_Head_mujer +  H_Head_Educ_level,
                   data = train_split, method = "glmnet",
                   metric = "Accuracy", trControl = ctrl_acc, 
                   tuneGrid = grid)

# Modelo con ROC ----
ctrl_roc <- trainControl(method = "cv", number = 5,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         savePredictions = T)

model_roc <- train(Pobre ~ Nper + edad_jefe + nocupados + nmujeres + nmenores + 
                     H_Head_mujer +  H_Head_Educ_level,
                   data = train_split, method = "glmnet",
                   metric = "ROC", trControl = ctrl_roc, 
                   tuneGrid = grid)


# Probamos el mejor modelo # ----
# Obtener las probabilidades de cada modelo
probs_acc <- predict(model_acc, newdata = test_split, type = "prob")
probs_roc  <- predict(model_roc,  newdata = test_split, type = "prob")


# Aplicar el umbral personalizado: probabilidad de clase "Si" > 0.2
pred_acc_02 <- ifelse(probs_acc$Si < 0.2, "Si", "No")
pred_roc_01  <- ifelse(probs_roc$Si  < 0.3, "Si", "No")
pred_roc_02 <- ifelse(probs_roc$Si < 0.1, "Si", "No")

# Convertir a factor con los mismos niveles que test_split$Pobre
pred_acc_02 <- factor(pred_acc_02, levels = levels(test_split$Pobre))
pred_roc_01  <- factor(pred_f1_02,  levels = levels(test_split$Pobre))
pred_roc_02 <- factor(pred_roc_02, levels = levels(test_split$Pobre))

# Evaluar cada uno
confusionMatrix(pred_acc_02, test_split$Pobre)
confusionMatrix(pred_f1_02,  test_split$Pobre)
confusionMatrix(pred_roc_02, test_split$Pobre)

# Ver f1
f1_ <- function(pred, ref) {
  cm <- confusionMatrix(pred, ref, positive = "Si")
  data.frame(
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"],
    F1 = cm$byClass["F1"]
  )
}

f1_(pred_acc_02, test_split$Pobre)
f1_(pred_roc_01, test_split$Pobre)
f1_(pred_roc_02, test_split$Pobre)

# Vemos gráficamente cual puede ser el mejor umbral

library(ggplot2)
library(caret)
library(dplyr)

# Vector real
y_real <- test_split$Pobre

# Probabilidades del modelo (usa prob_f1 o prob_roc si preferís)
probs <- probs_acc[, "Si"]

# Crear función para calcular métricas por umbral
metricas_umbral <- function(threshold, y_real, probs) {
  pred <- factor(ifelse(probs > threshold, "Si", "No"), levels = c("No", "Si"))
  cm <- confusionMatrix(pred, y_real, positive = "Si")
  
  data.frame(
    threshold = threshold,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"],
    F1 = cm$byClass["F1"]
  )
}

# Calcular para umbrales entre 0 y 1
umbrales <- seq(0, 1, by = 0.05)
resultados <- bind_rows(lapply(umbrales, metricas_umbral, y_real = y_real, probs = probs))

# Reorganizar para ggplot
resultados_largos <- resultados %>%
  pivot_longer(cols = -threshold, names_to = "Metrica", values_to = "Valor")

# Graficar
ggplot(resultados_largos, aes(x = threshold, y = Valor, color = Metrica)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0.2, linetype = "dashed", color = "gray40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Evaluación de métricas según el umbral de corte",
       x = "Umbral de corte (probabilidad para 'Pobre = Sí')",
       y = "Valor de la métrica",
       color = "Métrica") +
  theme_minimal()

# Nos quedamos con model_acc

# Envío Kaggle Elastic Net # ---- 
# Modelo ACC

ctrl_acc_f <- trainControl(method = "cv",
                           number = 5,
                           classProbs = TRUE,
                           savePredictions = T)

model_acc_f <- train(Pobre ~ Nper + edad_jefe + nocupados + nmujeres + nmenores + 
                       H_Head_mujer +  H_Head_Educ_level,
                     data = train, method = "glmnet",
                     metric = "Accuracy", trControl = ctrl_acc, 
                     tuneGrid = grid)


#- Hacemos la predicción

predictSample_en <- test %>%
  mutate(prob_Si = predict(model_acc_f, newdata = test, type = "prob")[, "Si"],
         pobre_lab = ifelse(prob_Si > 0.2, "Si", "No")) %>%
  dplyr::select(id, pobre_lab)

head(predictSample_en)

#- Transformamos variable pobre para que cumpla con la especificación de la competencia

predictSample_en <- predictSample_en %>%
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>%
  dplyr::select(id, pobre)

head(predictSample_en)


lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model_acc_f$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model_acc_f$bestTune$alpha))

name<- paste0(
  "EN_VF_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample_en,name, row.names = FALSE)





