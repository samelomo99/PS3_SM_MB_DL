
# PS3_SM_MB_DL  
## Taller 3 - Problem Set 3: Predicción del Precio de Vivienda  

### Santiago Melo, Miguel Blanco, Diana Lopera  

---

## 📂 Estructura del repositorio PS3_SM_MB_DL  

El repositorio está organizado en las siguientes carpetas:  

### 📄 `document`  
Contiene el documento final en formato PDF del Problem Set 3. En este archivo se resumen los objetivos del proyecto, la metodología utilizada y los principales hallazgos del análisis predictivo aplicado al mercado inmobiliario de Chapinero (Bogotá).  

### 📜 `scripts`  
Incluye los scripts en R empleados en el proceso de análisis, desde el preprocesamiento de datos hasta el entrenamiento de modelos y la evaluación de resultados:  
1. **PS3_database_variables_creation.R**: Carga, limpieza e imputación de variables, enriquecimiento con información geoespacial y creación de nuevas variables derivadas de texto.  
2. **PS3_script_main.R**: Implementación de distintos modelos de regresión y aprendizaje automático, incluyendo redes neuronales profundas (Keras), XGBoost, Random Forest y regresión lineal. 

### 📊 `stores`  
Contiene las bases de datos finales utilizadas en el análisis:  
- `train_chapinero.csv`  
- `test_chapinero.csv`  
Estas bases están enriquecidas con variables derivadas de procesamiento de lenguaje natural y fuentes abiertas (OpenStreetMap, Planeación Distrital, TransMilenio).  

### 📈 `views`  
Agrupa los gráficos y cuadros en los formatos requeridos para su inclusión en LaTeX:  
- **Gráficas**: `.jpg` y `.pdf`  
- **Cuadros**: Archivos `.tex` listos para Overleaf.  

---

## 📌 Resumen del ejercicio  

Este trabajo tuvo como propósito construir un modelo predictivo del precio de oferta de viviendas en la localidad de Chapinero, Bogotá, a partir de información estructural, geoespacial y textual. Para ello se utilizaron técnicas de machine learning y procesamiento de lenguaje natural aplicadas a datos extraídos de Properati y fuentes abiertas.  

Se evaluaron múltiples modelos de regresión, destacándose la red neuronal profunda implementada en `Keras`, la cual logró el menor MAE en la competencia de Kaggle asociada al taller.  

El análisis incorporó transformaciones logarítmicas, imputación de datos, codificación de variables categóricas y estandarización, bajo el ecosistema `tidymodels`.  

---

### 🔍 Principales hallazgos  

- El mejor modelo fue una **red neuronal profunda con arquitectura simple** (cuatro capas ocultas con 32 neuronas), logrando un **MAE de $277.389.181** en el leaderboard privado de Kaggle.  
- La transformación logarítmica del precio mejoró sustancialmente el desempeño predictivo.  
- Modelos más complejos (con más capas, imputaciones agresivas o más épocas de entrenamiento) presentaron **mayor sobreajuste o ruido**, lo cual redujo la precisión.  
- Modelos como **XGBoost** y **SuperLearner** mostraron buen desempeño, pero fueron superados por la red neuronal simple en términos de MAE.  
- Modelos lineales como OLS y Elastic Net fueron superados ampliamente, lo que reafirma la necesidad de métodos no lineales para capturar las complejidades del mercado inmobiliario.  
- Las variables más relevantes en la predicción del precio fueron las distancias a infraestructuras clave (parques, centros comerciales, troncales de TransMilenio), el número de cuartos y baños, y características como parqueadero, depósito y estrato.  
- Se evidencia que **la calidad del preprocesamiento y la selección de variables es tan o más importante que la complejidad del modelo**.  

---

📌 **Este repositorio corresponde a un ejercicio académico desarrollado en el curso de Big Data y Machine Learning de la Universidad de los Andes, bajo la guía del profesor Ignacio Sarmiento.**  
