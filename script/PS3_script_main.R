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




# ---------- BASE DE DATOS --------------
train_basica <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/train.csv"
) #Esta es la base de train que esta en kaggle, en base a esta se deben crear las nuevas variables 

test_basica <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/test.csv"
) # La base de test original 

## --- Modificaicones a la base de datos ------
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

# Calculamos valor del metro cuadrado 
train <- train %>%
  mutate(precio_por_mt2 = round(price / surface_total, 0))%>%
  mutate(precio_por_mt2  =precio_por_mt2/1000000 )  ## precio x Mt2 en millones. 

stargazer(train["precio_por_mt2"],type="text")
hist(train$precio_por_mt2)
#Manejando los valores atipicos
low <- round(mean(train$precio_por_mt2) - 2*sd(train$precio_por_mt2),2)
up <- round(mean(train$precio_por_mt2) + 2*sd(train$precio_por_mt2))
perc1 <- unname(round(quantile(train$precio_por_mt2, probs = c(.01)),2))

#
p1 <- train %>%
  ggplot(aes(y = precio_por_mt2)) +
  geom_boxplot(fill = "darkblue", alpha = 0.4) +
  labs(
    title = "Muestra completa",
    y = "Precio por metro cuadrado (millones)", x = "") +
  theme_bw()
p2 <- train %>%
  filter(between(precio_por_mt2, perc1,  up)) %>% 
  ggplot(aes(y = precio_por_mt2)) +
  geom_boxplot(fill = "darkblue", alpha = 0.4) +
  labs(
    title = "Muestra filtrada",
    y = "Precio por metro cuadrado (millones)", x = "") +
  theme_bw()
grid.arrange(p1, p2, ncol = 2)

# Ahora si generamos el filtro 

#Filtramos outliers
train <- train %>%
  filter(between(precio_por_mt2, perc1, up))

# Visualicemos la distribución de nuestra variable de interés
p <- ggplot(train, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

ggplotly(p)

## --- Haciendo Graficas de mapas --------------

# Creamos un mapa base
leaflet() %>% 
  addTiles() 
# Eliminamos las observaciones que no tienen información de latitud o longitud (que creo no es nninguna)
train <- train %>%
  filter(!is.na(lat) & !is.na(lon))

# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

## Escalamos para que se pueda graficar
train <- train %>%
  mutate(precio_por_mt2_sc =( (precio_por_mt2 - min(precio_por_mt2)) / (max(precio_por_mt2) - min(precio_por_mt2))))

train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

# Vamos a crear un mensaje en popup con html
html <- paste0("<b>Precio:</b> ",
               scales::dollar(train$price),
               "<br> <b>Area:</b> ",
               as.integer(train$surface_total), " mt2",
               "<br> <b>Tipo de immueble:</b> ",
               train$property_type,
               "<br> <b>Numero de alcobas:</b> ",
               as.integer(train$rooms),
               "<br> <b>Numero de baños:</b> ",
               as.integer(train$bathrooms))

# Eliminamos los immuebles con área menor a 20
train <- train %>% filter( surface_covered > 20)

# Encontramos el queremos que sea el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Creamos el plot
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             radius = train$precio_por_mt2_sc*10,
             popup = html)
#Transmormamos los datos de precios de vivienda a SF para graficar en ggplot
sf_train<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)

# Creamos un mapa con los estratos 
 #descargamos
estratos <- st_read("https://datosabiertos.bogota.gov.co/dataset/55467552-0af4-4524-a390-a2956035744e/resource/29f2d770-bd5d-4450-9e95-8737167ba12f/download/manzanaestratificacion.json")
 #vemos mapa

estratos <-st_transform(estratos,4326)
ggplot()+
  geom_sf(data=estratos, aes(fill = as.factor(ESTRATO)), color = "black", lwd = 0) +
  scale_fill_brewer(palette = "Set1", name = "Estrato") +  # o usa otra como "Paired", "Set3", etc.
  theme_minimal() +
  labs(title = "Mapa por estrato", subtitle = "Estratos del 1 al 6") + 
  geom_sf(data=sf_train %>% filter(property_type == c("Apartamento", "Casa")),aes(color = precio_por_mt2) ,shape=15, size=0.3) +
  theme_bw()

# Creamos un mapa con las avenidas mas cercanas 

red_vial <- st_read("https://datosabiertos.bogota.gov.co/dataset/0e2bdaed-eb3c-4b14-90b4-44454013bbef/resource/c2966db7-eb06-4a79-931c-0661d790d03d/download/redinfraestructuravialarterial.json")

red_vial<-st_transform(red_vial,4326)
ggplot()+
  geom_sf(data=red_vial, color = "blue") + 
  geom_sf(data=sf_train %>% filter(property_type == c("Apartamento", "Casa")),aes(color = precio_por_mt2) ,shape=15, size=0.3) +
  theme_bw()

#Creamos un mapa con los centros comerciales

localidades <- st_read("https://datosabiertos.bogota.gov.co/dataset/856cb657-8ca3-4ee8-857f-37211173b1f8/resource/497b8756-0927-4aee-8da9-ca4e32ca3a8a/download/loca.json")

localidades<-st_transform(localidades,4326)
ggplot()+
  geom_sf(data=localidades, color = "red") + 
  geom_sf(data=sf_train %>% filter(property_type == c("Apartamento", "Casa")),aes(color = precio_por_mt2) ,shape=15, size=0.3) +
  theme_bw()
  

# Creamos un mapa para los parques 

parques <- st_read("https://datosabiertos.bogota.gov.co/dataset/1ca41514-3671-41d6-8c3b-a970dc8c24a7/resource/16288e7f-0345-4680-84aa-40e987706ea8/download/parque.json")
  
parques <- st_transform(parques,4326)
ggplot()+
  geom_sf(data = parques, color = "green") +
  geom_sf(data=sf_train %>% filter(property_type == c("Apartamento", "Casa")),aes(color = precio_por_mt2) ,shape=15, size=0.3) +
  theme_bw()

# Creamos mapa para Centros Comerciales (CC)

    # 1. Cargar el archivo GeoJSON original desde Datos Abiertos
    CC <- st_read("https://datosabiertos.bogota.gov.co/dataset/ce479dd9-7d54-4400-a05d-8df538c43e29/resource/c91f8dbd-f0a4-4fe1-83be-935a2de908da/download/gran_centro_comercial.geojson")
    
    # 2. Definir el CRS ESRI:102771 manualmente (MAGNA-SIRGAS Cartesianas Origen Bogotá)
    bog_crs <- "+proj=tmerc +lat_0=4.599047222222222 +lon_0=-74.08091666666666 +k=1 +x_0=100000 +y_0=100000 +ellps=GRS80 +units=m +no_defs"
    
    # 3. Asignar el CRS correcto al objeto (sin transformar aún)
    st_crs(CC) <- bog_crs
    
    # 4. Transformar a WGS 84 (EPSG:4326) para trabajar en grados
    CC <- st_transform(CC, 4326)
    
    # 5. Verificación opcional
    print(st_crs(CC))
    print(st_bbox(CC))  # Debe dar xmin ≈ -74.1, ymin ≈ 4.5
    
    # 6. Graficar con ggplot2
    ggplot() +
      geom_sf(data = CC, fill = "orange", color = "red", alpha = 0.6) +
      coord_sf(lims_method = "geometry_bbox") +
      theme_minimal() +
      labs(title = "Centros Comerciales de Bogotá", subtitle = "Fuente: Datos Abiertos Bogotá")

# Creo grafica con troncales de transmilenio

tm <- st_read("https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/Trazados_Troncales_de_TRANSMILENIO.geojson")

tm <- st_transform(tm,4326)
ggplot()+
  geom_sf(data = tm, color = "yellow") +
  geom_sf(data=sf_train %>% filter(property_type == c("Apartamento", "Casa")),aes(color = precio_por_mt2) ,shape=15, size=0.3) +
  theme_bw()

## Creando las variables adicionales ----------------------
### Distancia a parque mas cercano ------------------------
    # Extraemos la info de todos los parques de Bogotá
    parques <- opq(bbox = getbb("Bogota Colombia")) %>%
      add_osm_feature(key = "leisure" , value = "park") 
    # Cambiamos el formato para que sea un objeto sf (simple features)
    parques_sf <- osmdata_sf(parques)
    
    
    # De las features del parque nos interesa su geomoetría y donde estan ubicados 
    parques_geometria <- parques_sf$osm_polygons %>% 
      dplyr::select(osm_id, name) 
    
    
    # Guardemos los poligonos de los parques 
    parques_geometria <- st_as_sf(parques_sf$osm_polygons)
    
    # Calculamos el centroide de cada parque para aproximar su ubciacion como un solo punto 
    centroides_parques <- st_centroid(parques_geometria, byid = T)
    
   
    centroides_parques <- centroides_parques %>%
      mutate(x=st_coordinates(centroides_parques)[, "X"]) %>%
      mutate(y=st_coordinates(centroides_parques)[, "Y"]) 
    
    # Creamos una grafica 
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
      addPolygons(data = parques_geometria, col = "red",weight = 10,
                  opacity = 0.8, popup = parques_geometria$name) %>%
      addCircles(lng = centroides_parques$x, 
                 lat = centroides_parques$y, 
                 col = "darkblue", opacity = 0.5, radius = 1)   
    
    # Calculamos la distancia
    centroides_parques_sf <- st_as_sf(centroides_parques, coords = c("x", "y"), crs=4326)
   
    dist_matrix_parques <- st_distance(x = sf_train, y = centroides_parques_sf)
    dim(dist_matrix_parques)
    
    dist_min_parque <- apply(dist_matrix_parques, 1, min)  
    
    
    # La agregamos como variable a nuestra base de datos original 
    train <- train %>% mutate(distancia_parque = dist_min_parque)
    
    #Finalmente la distribucion
    
    p_parques <- ggplot(train, aes(x = distancia_parque)) +
      geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
      labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
           title = "Distribución de la distancia a los parques") +
      theme_bw()
    ggplotly(p_parques)



### Distancia a CC mas cercano --------------------------

    # Asumimos que ya tienes el objeto CC cargado, transformado a EPSG:4326 y listo.
    
    # 1. Seleccionamos geometría y nombre del centro comercial
    # Reemplazo de la parte que selecciona y asigna nombre si está vacío
    CC_geometria <- CC %>%
      mutate(name = paste("Centro Comercial", row_number())) %>%
      dplyr::select(name)
    
    # 2. Calculamos los centroides de los centros comerciales
    centroides_CC <- st_centroid(CC_geometria, byid = TRUE)
    
    # 3. Extraemos coordenadas para visualización
    centroides_CC <- centroides_CC %>%
      mutate(x = st_coordinates(centroides_CC)[, "X"],
             y = st_coordinates(centroides_CC)[, "Y"])
    
    # 4. Creamos un mapa interactivo con leaflet
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
      addPolygons(data = CC_geometria, col = "red", weight = 2,
                  opacity = 0.8, popup = CC_geometria$name) %>%
      addCircles(lng = centroides_CC$x, 
                 lat = centroides_CC$y, 
                 col = "darkblue", opacity = 0.5, radius = 1)
    
    # 5. Convertimos los centroides a sf
    centroides_CC_sf <- st_as_sf(centroides_CC, coords = c("x", "y"), crs = 4326)
    
    # 6. Calculamos matriz de distancias entre viviendas y centros comerciales
    dist_matrix_CC <- st_distance(x = sf_train, y = centroides_CC_sf)
    dim(dist_matrix_CC)
    
    # 7. Extraemos la distancia mínima para cada vivienda
    dist_min_CC <- apply(dist_matrix_CC, 1, min)
    
    # 8. Agregamos a la base de entrenamiento
    train <- train %>% mutate(distancia_CC = dist_min_CC)
    
    # 9. Visualizamos la distribución
    p_CC <- ggplot(train, aes(x = distancia_CC)) +
      geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
      labs(x = "Distancia mínima a un centro comercial (m)", y = "Cantidad",
           title = "Distribución de la distancia a centros comerciales") +
      theme_bw()
    
    ggplotly(p_CC)
    



### Distancia a TM ---------------------------

    # 1. Seleccionar geometría y nombre de estaciones de TM
    tm_geometria <- tm %>%
      dplyr::select(nombre_trazado_troncal)  # Usa directamente el nombre si ya existe
    
    # 2. Calcular centroides de las estaciones
    centroides_tm <- st_centroid(tm_geometria, byid = TRUE)
    
    # 3. Extraer coordenadas para visualización
    centroides_tm <- centroides_tm %>%
      mutate(x = st_coordinates(centroides_tm)[, "X"],
             y = st_coordinates(centroides_tm)[, "Y"])
    
    centroides_tm <- centroides_tm %>%
      filter(!is.na(x), !is.na(y))
    
    # 4. Crear el mapa interactivo
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
      addPolygons(data = tm_geometria, col = "red", weight = 2,
                  opacity = 0.8) %>%
      addCircles(lng = centroides_tm$x,
                 lat = centroides_tm$y,
                 color = "darkblue", opacity = 0.5, radius = 1)
    
    # 5. Convertir los centroides a objeto sf
    centroides_tm_sf <- st_as_sf(centroides_tm, coords = c("x", "y"), crs = 4326)
    
    # 6. Calcular matriz de distancias entre viviendas y estaciones TM
    dist_matrix_tm <- st_distance(x = sf_train, y = centroides_tm_sf)
    
    # 7. Extraer la distancia mínima
    dist_min_tm <- apply(dist_matrix_tm, 1, min)
    
    # 8. Agregar la variable a la base de entrenamiento
    train <- train %>% mutate(distancia_tm = dist_min_tm)
    
    # 9. Visualizar la distribución
    p_tm <- ggplot(train, aes(x = distancia_tm)) +
      geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
      labs(x = "Distancia mínima a TransMilenio (m)", y = "Cantidad",
           title = "Distribución de la distancia a estaciones TM") +
      theme_bw()
    
    ggplotly(p_tm)
    
### Distancia a TM en lineas ---------
    # 1. Seleccionar solo la geometría (opcionalmente puedes conservar el nombre del trazado si quieres visualizar)
    tm_lineas <- tm %>%
      dplyr::select(nombre_trazado_troncal)  # Asegúrate de que esta sea la columna correcta
    
    # 2. Asegurarse de que está en el CRS adecuado (EPSG 4326)
    tm_lineas <- st_transform(tm_lineas, 4326)
    
    # 3. Visualización opcional del trazado TM + puntos de vivienda
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
      addPolylines(data = tm_lineas, color = "red", weight = 2, opacity = 0.8) %>%
      addCircles(data = sf_train, color = "darkblue", radius = 1, opacity = 0.5)
    
    # 4. Calcular matriz de distancias desde cada vivienda a los tramos de TransMilenio
    dist_matrix_tm <- st_distance(x = sf_train, y = tm_lineas)
    
    # 5. Extraer la distancia mínima por vivienda
    dist_min_tm <- apply(dist_matrix_tm, 1, min)
    
    # 6. Agregar la distancia como nueva variable al dataset de entrenamiento
    train <- train %>% mutate(distancia_tm = dist_min_tm)
    
    # 7. Visualizar la distribución de las distancias
    p_tm <- ggplot(train, aes(x = distancia_tm)) +
      geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
      labs(x = "Distancia mínima al trazado de TransMilenio (m)", y = "Cantidad",
           title = "Distribución de la distancia a TransMilenio") +
      theme_bw()
    
    ggplotly(p_tm)
    

### Distancia a Avenida principal mas cercana ----------------------
    # 1. Seleccionamos geometría y nombre de los tramos viales
    red_vial_geometria <- red_vial %>%
      dplyr::select(name = NOMBRE)  # Ajusta si el nombre real de la columna es diferente
    
    # Si la columna `name` tiene NAs, puedes limpiar:
    red_vial_geometria <- red_vial_geometria %>%
      mutate(name = ifelse(is.na(name), "Sin nombre", name))
    
    # 2. Calculamos los centroides de cada tramo vial
    library(sf)
    
    # Reparar geometrías defectuosas
    red_vial_geometria <- st_make_valid(red_vial_geometria)
    
    centroides_red_vial <- st_centroid(red_vial_geometria, byid = TRUE)
    
    # 3. Extraemos coordenadas
    centroides_red_vial <- centroides_red_vial %>%
      mutate(x = st_coordinates(centroides_red_vial)[, "X"],
             y = st_coordinates(centroides_red_vial)[, "Y"])
    
    # 4. Crear el mapa interactivo
    leaflet() %>%
      addTiles() %>%
      setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
      addPolylines(data = red_vial_geometria, color = "red", weight = 2,
                   opacity = 0.8, popup = red_vial_geometria$name) %>%
      addCircles(lng = centroides_red_vial$x,
                 lat = centroides_red_vial$y,
                 color = "darkblue", opacity = 0.5, radius = 1)
    
    # 5. Convertimos a sf con geometría
    centroides_red_vial_sf <- st_as_sf(centroides_red_vial, coords = c("x", "y"), crs = 4326)
    
    # 6. Calculamos la matriz de distancias entre viviendas y red vial
    dist_matrix_red_vial <- st_distance(x = sf_train, y = centroides_red_vial_sf)
    
    # 7. Distancia mínima
    dist_min_red_vial <- apply(dist_matrix_red_vial, 1, min)
    
    # 8. Agregar al dataset original
    train <- train %>% mutate(distancia_red_vial = dist_min_red_vial)
    
    # 9. Visualización
    p_red_vial <- ggplot(train, aes(x = distancia_red_vial)) +
      geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
      labs(x = "Distancia mínima a red vial (m)", y = "Cantidad",
           title = "Distribución de la distancia a tramos de la red vial") +
      theme_bw()
    
    ggplotly(p_red_vial)
    
### Distancia a red vial minima version 2.0 ---------
    # 1. Consultar avenidas de Bogotá desde OSM
    AV <- opq(bbox = getbb("Bogota Colombia")) %>%
      add_osm_feature(key = "highway", value = c("trunk", "primary"))
    
    # 2. Convertir a sf
    AV_sf <- osmdata_sf(AV)
    
    # 3. Extraer líneas de la red vial (evita polígonos, puntos, etc.)
    AV_lineas <- AV_sf$osm_lines  # Este es el que contiene las LINESTRING
    
    # 4. Asegúrate de que todo esté en EPSG:4326
    AV_lineas <- st_transform(AV_lineas, 4326)
    
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = AV_lineas, color = "red", weight = 2, opacity = 0.7) %>%
      addCircles(data = sf_train, color = "blue", radius = 1, opacity = 0.5)
    
    
    # 5. Calcular distancia directa a los segmentos más cercanos
    dist_matrix_AV <- st_distance(x = sf_train, y = AV_lineas)
    
    # 6. Extraer distancia mínima por vivienda
    dist_min_AV <- apply(dist_matrix_AV, 1, min)
    
    # 7. Agregar a la base
    train <- train %>% mutate(distancia_avenida = dist_min_AV)
    
    # 8. Visualizar
    p_avenida <- ggplot(train, aes(x = distancia_avenida)) +
      geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.5) +
      labs(x = "Distancia mínima a avenidas (trunk/primary)", y = "Cantidad",
           title = "Distribución de la distancia a avenidas principales") +
      theme_minimal()
    
    ggplotly(p_avenida)
    

    
   ###----- Crear variables a partir de texto: Train--------------------
    
    nuevo_df_train <- select(train, property_id, description)
    
    #Definir la función de limpieza
    limpiar_texto <- function(texto) {
      if (is.na(texto)) return(texto)                      # conserva NA
      str_replace_all(texto, "[^[:alnum:]\\s]", "")        # elimina todo salvo letras, números y espacios
    }
    
    #Aplicar la función y crear la nueva columna
    nuevo_df_train <- nuevo_df_train %>% 
      mutate(
        description_limpia = map_chr(description, limpiar_texto)  # devuelve vector tipo character
      )
    
    p_load(udpipe)                       # instala y carga 'udpipe' si falta
    
    #Descargar y cargar el modelo español (una sola vez)
    modelo_info <- udpipe_download_model(language = "spanish")
    nlp <- udpipe_load_model(modelo_info$file_model)
    
    # Función: tokenizar + lematizar + quitar stop-words
    tokenizar_lemmatizar <- function(texto, modelo) {
      # Devuelve character(0) si el texto es NA o vacío
      if (is.na(texto) || str_trim(texto) == "") return(character(0))
      
      # Anotar con udpipe
      anno <- udpipe_annotate(modelo, x = texto)
      tokens <- as.data.frame(anno)
      
      # Lista de stop-words en español
      sw <- stopwords::stopwords("es")
      
      # Filtrar: sin puntuación ni espacios, quitar stop-words
      lemmas <- tokens$lemma[
        tokens$upos != "PUNCT" &
          !(tolower(tokens$lemma) %in% sw)
      ]
      lemmas <- lemmas[lemmas != ""]
      lemmas
    }
    
    # Crear la nueva columna list-column `lemmas`
    nuevo_df_train <- nuevo_df_train %>%
      mutate(
        lemmas = map(description_limpia, ~ tokenizar_lemmatizar(.x, nlp))
      )
    
    #Crear la nueva columna `parqueadero`, 'deposito', 'ascensor'
    
    palabras_clave <- c("parqueadero", "garaje")
    nuevo_df_train$parquedero <- vapply(
      nuevo_df_train$lemmas,
      function(x) as.integer(any(x %in% palabras_clave)),
      FUN.VALUE = integer(1)
    )
    
    palabras_deposito <- c("deposito", "depósito")
    
    nuevo_df_train$deposito <- vapply(
      nuevo_df_train$lemmas,
      function(x) as.integer(any(x %in% palabras_deposito)),
      FUN.VALUE = integer(1)
    )
    
    palabras_deposito <- c("deposito", "depósito")
    
    nuevo_df_train$deposito <- vapply(
      nuevo_df_train$lemmas,
      function(x) as.integer(any(x %in% palabras_deposito)),
      FUN.VALUE = integer(1)
    )
    
    palabras_ascensor <- "ascensor"
    
    nuevo_df_train$ascensor <- vapply(
      nuevo_df_train$lemmas,
      function(x) as.integer(any(x %in% palabras_ascensor)),
      FUN.VALUE = integer(1)
    )
    
    nuevo_df_train <- nuevo_df_train %>%
      select(property_id, description_limpia, lemmas, parquedero, deposito, ascensor)
    train <- train %>%
      left_join(nuevo_df_train, by = "property_id")
    
    
    
### Guardar la base de datos para usarla ya de manera clara
    
    write.csv(train, file = "/Users/miguelblanco/Library/CloudStorage/OneDrive-Personal/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/PS3_SM_MB_DL/Stores/trainfull.csv", row.names = FALSE)

### Estrato 
    ## En esta seccion 
## --- Ahora con las variables test --------------------------------------------------------------
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


# 2. Mapa base
leaflet() %>% addTiles()

# 3. Filtramos observaciones sin lat/lon (esto elimina datos → ¡ADVERTENCIA!)
# A PETICIÓN TUYA: No se puede eliminar observaciones
# Verificamos si hay NA y solo avisamos
sum_na_latlon <- sum(is.na(test$lat) | is.na(test$lon))
print(paste("Observaciones con NA en lat/lon:", sum_na_latlon))
# Si hay alguna, puedes imputarlas o visualizarlas aparte



# Nota: Copiado directamente en test
# 5. Colores

test <- test %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

# 6. HTML popup
html_test <- paste0("<br> <b>Area:</b> ",
                    as.integer(test$surface_total), " mt2",
                    "<br> <b>Tipo de inmueble:</b> ",
                    test$property_type,
                    "<br> <b>Numero de alcobas:</b> ",
                    as.integer(test$rooms),
                    "<br> <b>Numero de baños:</b> ",
                    as.integer(test$bathrooms))

# 7. Coordenadas centrales (usando solo las que no son NA)
latitud_central_test <- mean(test$lat, na.rm = TRUE)
longitud_central_test <- mean(test$lon, na.rm = TRUE)

# 8. Visualización en Leaflet
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addCircles(lng = test$lon,
             lat = test$lat,
             col = test$color,
             fillOpacity = 1,
             opacity = 1,
             popup = html_test)

# 9. Transformar test a objeto sf
sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# ========================
# DISTANCIA A PARQUE MÁS CERCANO (TEST)
# ========================

parques_test <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park")
parques_sf_test <- osmdata_sf(parques_test)
parques_geometria_test <- parques_sf_test$osm_polygons %>%
  dplyr::select(osm_id, name)
parques_geometria_test <- st_as_sf(parques_sf_test$osm_polygons)
centroides_parques_test <- st_centroid(parques_geometria_test, byid = TRUE)
centroides_parques_test <- centroides_parques_test %>%
  mutate(x = st_coordinates(centroides_parques_test)[, "X"],
         y = st_coordinates(centroides_parques_test)[, "Y"])

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addPolygons(data = parques_geometria_test, col = "red", weight = 10,
              opacity = 0.8, popup = parques_geometria_test$name) %>%
  addCircles(lng = centroides_parques_test$x,
             lat = centroides_parques_test$y,
             col = "darkblue", opacity = 0.5, radius = 1)

centroides_parques_sf_test <- st_as_sf(centroides_parques_test, coords = c("x", "y"), crs = 4326)
dist_matrix_parques_test <- st_distance(x = sf_test, y = centroides_parques_sf_test)
dist_min_parque_test <- apply(dist_matrix_parques_test, 1, min)
test <- test %>% mutate(distancia_parque = dist_min_parque_test)

p_parques_test <- ggplot(test, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()

ggplotly(p_parques_test)

# ========================
# DISTANCIA A CENTRO COMERCIAL MÁS CERCANO (TEST)
# ========================

CC_geometria_test <- CC %>%
  mutate(name = paste("Centro Comercial", row_number())) %>%
  dplyr::select(name)

centroides_CC_test <- st_centroid(CC_geometria_test, byid = TRUE)
centroides_CC_test <- centroides_CC_test %>%
  mutate(x = st_coordinates(centroides_CC_test)[, "X"],
         y = st_coordinates(centroides_CC_test)[, "Y"])

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addPolygons(data = CC_geometria_test, col = "red", weight = 2,
              opacity = 0.8, popup = CC_geometria_test$name) %>%
  addCircles(lng = centroides_CC_test$x,
             lat = centroides_CC_test$y,
             col = "darkblue", opacity = 0.5, radius = 1)

centroides_CC_sf_test <- st_as_sf(centroides_CC_test, coords = c("x", "y"), crs = 4326)
dist_matrix_CC_test <- st_distance(x = sf_test, y = centroides_CC_sf_test)
dist_min_CC_test <- apply(dist_matrix_CC_test, 1, min)
test <- test %>% mutate(distancia_CC = dist_min_CC_test)

p_CC_test <- ggplot(test, aes(x = distancia_CC)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un centro comercial (m)", y = "Cantidad",
       title = "Distribución de la distancia a centros comerciales") +
  theme_bw()

ggplotly(p_CC_test)


# ========================
# DISTANCIA A TRONCAL TM MÁS CERCANA (TEST)
# ========================

tm_lineas_test <- tm %>% dplyr::select(nombre_trazado_troncal)
tm_lineas_test <- st_transform(tm_lineas_test, 4326)

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central_test, lat = latitud_central_test, zoom = 12) %>%
  addPolylines(data = tm_lineas_test, color = "red", weight = 2, opacity = 0.8) %>%
  addCircles(data = sf_test, color = "darkblue", radius = 1, opacity = 0.5)

dist_matrix_tm_test <- st_distance(x = sf_test, y = tm_lineas_test)
dist_min_tm_test <- apply(dist_matrix_tm_test, 1, min)
test <- test %>% mutate(distancia_tm = dist_min_tm_test)

p_tm_test <- ggplot(test, aes(x = distancia_tm)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima al trazado de TransMilenio (m)", y = "Cantidad",
       title = "Distribución de la distancia a TransMilenio") +
  theme_bw()

ggplotly(p_tm_test)

# ========================
# DISTANCIA A AVENIDAS PRINCIPALES MÁS CERCANAS (TEST)
# ========================

AV_test <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "highway", value = c("trunk", "primary"))
AV_sf_test <- osmdata_sf(AV_test)
AV_lineas_test <- AV_sf_test$osm_lines
AV_lineas_test <- st_transform(AV_lineas_test, 4326)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = AV_lineas_test, color = "red", weight = 2, opacity = 0.7) %>%
  addCircles(data = sf_test, color = "blue", radius = 1, opacity = 0.5)

dist_matrix_AV_test <- st_distance(x = sf_test, y = AV_lineas_test)
dist_min_AV_test <- apply(dist_matrix_AV_test, 1, min)
test <- test %>% mutate(distancia_avenida = dist_min_AV_test)

p_avenida_test <- ggplot(test, aes(x = distancia_avenida)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.5) +
  labs(x = "Distancia mínima a avenidas (trunk/primary)", y = "Cantidad",
       title = "Distribución de la distancia a avenidas principales") +
  theme_minimal()

ggplotly(p_avenida_test)



#----- Crear variables a partir de texto: Test--------------------

nuevo_df_test <- select(train, property_id, description)

#Definir la función de limpieza
limpiar_texto <- function(texto) {
  if (is.na(texto)) return(texto)                      # conserva NA
  str_replace_all(texto, "[^[:alnum:]\\s]", "")        # elimina todo salvo letras, números y espacios
}

#Aplicar la función y crear la nueva columna
nuevo_df_test <- nuevo_df_test %>% 
  mutate(
    description_limpia = map_chr(description, limpiar_texto)  # devuelve vector tipo character
  )

p_load(udpipe)                       # instala y carga 'udpipe' si falta

#Descargar y cargar el modelo español (una sola vez)
modelo_info <- udpipe_download_model(language = "spanish")
nlp <- udpipe_load_model(modelo_info$file_model)

# Función: tokenizar + lematizar + quitar stop-words
tokenizar_lemmatizar <- function(texto, modelo) {
  # Devuelve character(0) si el texto es NA o vacío
  if (is.na(texto) || str_trim(texto) == "") return(character(0))
  
  # Anotar con udpipe
  anno <- udpipe_annotate(modelo, x = texto)
  tokens <- as.data.frame(anno)
  
  # Lista de stop-words en español
  sw <- stopwords::stopwords("es")
  
  # Filtrar: sin puntuación ni espacios, quitar stop-words
  lemmas <- tokens$lemma[
    tokens$upos != "PUNCT" &
      !(tolower(tokens$lemma) %in% sw)
  ]
  lemmas <- lemmas[lemmas != ""]
  lemmas
}

# Crear la nueva columna list-column `lemmas`
nuevo_df_test <- nuevo_df_test %>%
  mutate(
    lemmas = map(description_limpia, ~ tokenizar_lemmatizar(.x, nlp))
  )

#Crear la nueva columna `parqueadero`, 'deposito', 'ascensor'

palabras_clave <- c("parqueadero", "garaje")
nuevo_df_test$parquedero <- vapply(
  nuevo_df_test$lemmas,
  function(x) as.integer(any(x %in% palabras_clave)),
  FUN.VALUE = integer(1)
)

palabras_deposito <- c("deposito", "depósito")

nuevo_df_test$deposito <- vapply(
  nuevo_df_test$lemmas,
  function(x) as.integer(any(x %in% palabras_deposito)),
  FUN.VALUE = integer(1)
)

palabras_deposito <- c("deposito", "depósito")

nuevo_df_test$deposito <- vapply(
  nuevo_df_test$lemmas,
  function(x) as.integer(any(x %in% palabras_deposito)),
  FUN.VALUE = integer(1)
)

palabras_ascensor <- "ascensor"

nuevo_df_test$ascensor <- vapply(
  nuevo_df_test$lemmas,
  function(x) as.integer(any(x %in% palabras_ascensor)),
  FUN.VALUE = integer(1)
)

nuevo_df_test <- nuevo_df_test %>%
  select(property_id, description_limpia, lemmas, parquedero, deposito, ascensor)
test <- test %>%
  left_join(nuevo_df_test, by = "property_id")


# Finalmente guardamos la base de datos en formato CSV para poderla usar al final cuando la necesitemos 

write.csv(test, file = "/Users/miguelblanco/Library/CloudStorage/OneDrive-Personal/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/PS3_SM_MB_DL/Stores/testfull.csv", row.names = FALSE)


## --- Creacion de las particiones de datos para probar -------- 
#LLamamos la base desde GitHub, para no correr todo 
train <- read.csv("https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/trainfull.csv")
test <- read.csv("https://raw.githubusercontent.com/samelomo99/PS3_SM_MB_DL/refs/heads/main/stores/testfull.csv")

<<<<<<< Updated upstream
=======
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

>>>>>>> Stashed changes
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



<<<<<<< Updated upstream
=======

## Random Forest
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


## SuperLearner ----

# Eliminar filas con NA en train (en X y en y)
train <- train %>% drop_na()

# No quitamos NAs en test porque normalmente el test no debería tener, 
# pero si quieres hacerlo, puedes descomentar la siguiente línea:
test <- test %>% drop_na()

# 3. Separar variables predictoras y respuesta
y_full <- train$price
x_full <- train %>% select(-price, -property_id)

# 4. Convertir character a factor en ambos conjuntos
x_full <- x_full %>% mutate(across(where(is.character), as.factor))
test <- test %>% mutate(across(where(is.character), as.factor))

# 5. Eliminar variables con un solo nivel en x_full
cols_to_keep <- sapply(x_full, function(col) {
  if (is.factor(col)) {
    length(levels(col)) > 1
  } else {
    TRUE
  }
})
x_full <- x_full[, cols_to_keep]

# 6. Sincronizar niveles en test según los de x_full
for (col_name in names(x_full)) {
  if (is.factor(x_full[[col_name]]) && is.factor(test[[col_name]])) {
    levels(test[[col_name]]) <- levels(x_full[[col_name]])
  }
}

# 7. Entrenar SuperLearner
set.seed(123)
sl_fit_kaggle <- SuperLearner(
  Y = y_full,
  X = x_full,
  family = gaussian(),
  SL.library = c("SL.mean", "SL.rpart", "SL.ranger"),
  method = "method.NNLS"
)


# 8. Predecir en test usando solo las columnas que quedaron en x_full
pred_kaggle <- predict(sl_fit_kaggle, newdata = test %>% select(names(x_full)))$pred

# 9. Crear archivo submission
submission_sl <- test %>%
  select(property_id) %>%
  mutate(price = as.numeric(pred_kaggle))


# 10. Guardar CSV para Kaggle
write_csv(submission_sl, "C:/Users/samel/OneDrive/Datos adjuntos/Universidad de los Andes/IV/Big Data - Machine Learning/GitHub/PS3_SM_MB_DL/stores/submission_superlearner.csv, row.names = FALSE")
>>>>>>> Stashed changes
