#' @title Entrena el modelo de metadatos de AREA para Acuacar y genera el binario,
#' la ficha con las medidas de rendimiento del modelo y la matriz de confusión 
#' 
#' @description ESte modelo es el codificado como 1.1. Los datos necesarios para 
#' correr el modelo se encuentran aquí: 
#' metadatos: "datos/ACUACAR_Activo_2020_06_01_A_2021_02_10_V1.xlsx"
#' 
#' 


## ----setup, include=FALSE-----------------------------------------------------------------
library(dplyr)
library(janitor)
library(readxl)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(tidymodels)
library(tidytext)
library(stopwords)
library(textrecipes)
library(discrim)
library(themis)
library(ranger)
library(forcats)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores - 2)

seed <- 4321
set.seed(seed)
quiebre = 1
#source(here("R","tablas_resumen.R"))



## ----leer datos---------------------------------------------------------------------------
crudos <- read_excel("datos/ACUACAR_Activo_2020_06_01_A_2021_02_10_V1.xlsx") %>%
  clean_names()

datos <- crudos %>%
  select(code,
         modo,
         name,
         date_created,
         date_modified,
         author,
         org_interesada_remitente,
         org_interesada_destinatario,
         nombre_fichero,
         mime_type,
         size,
         path,
         area,
         nombre_tipo_documental,
         codigo_tipo_documental,
         nombre_padre,
         codigo_padre,
         nombre_abuelo,
         codigo_abuelo,
         en_expte,
         nombre_serie_documental,
         codigo_serie_documental,
         dependencia_responsable) %>%
  unite(org_interesada,org_interesada_remitente:org_interesada_destinatario,
        remove = TRUE,
        na.rm = TRUE) %>%
  mutate(
    date_created = ymd_hms(date_created,tz = "UTC",locale = "es_CO.utf8")  - hours(5),
    date_modified = ymd_hms(date_modified,tz = "UTC", locale = "es_CO.utf8") - hours(5)
    ) %>%
  mutate(
    mes_creacion = month(date_created,label = FALSE,abbr = FALSE),
    dia_semana_creacion = wday(date_created,label = FALSE,
                               week_start = 1,abbr = FALSE),
    dia_creacion = day(date_created),
    hora_creacion = hour(date_created),
    mes_modificacion = month(date_modified,label = FALSE,abbr = FALSE),
    dia_semana_modificacion = wday(date_modified,label = FALSE,
                                   week_start = 1,abbr = FALSE),
    dia_modificacion = day(date_modified),
    hora_modificacion = hour(date_modified))




## -----------------------------------------------------------------------------------------
autores_automaticos <- c("acuacarweb","pqrweb","admin", "partner")

datos <- datos %>%
  filter(modo != "S") %>%
  filter(!author %in% autores_automaticos)

# Estos fueron los datos que le pase a Jose
# write_csv(datos, file = "datos_area.csv")


## ----mostrar tipos de areas---------------------------------------------------------------
tabla <- datos %>%
  group_by(area) %>%
  summarize(frecuencia = n()) %>%
  arrange(desc(frecuencia)) %>%
  mutate(
    total = sum(frecuencia),
    porcentaje = (frecuencia/total)*100,
    acumulado = cumsum(porcentaje),
    posicion = 1:n()
    ) %>%
  select(-total) %>%
  ungroup()

#hacer_tabla(datos,area)



## -----------------------------------------------------------------------------------------
otros <- tabla %>%
  filter(porcentaje < quiebre) %>%
  pull(area)



## -----------------------------------------------------------------------------------------

datos <- datos %>%
  mutate(
    tipo = factor(if_else(
      area %in% otros,"otros",area))
  )


#hacer_tabla(datos,area)

## -----------------------------------------------------------------------------------------
datos_sin_otros <- datos %>%
  filter(tipo != "otros") %>%
  mutate(tipo = fct_drop(tipo))  # Importante: eliminar este nivel del factor


n_clases_sin_otros <- datos_sin_otros %>%
  distinct(tipo) %>%
  tally() %>%
  pull()


## ----division-----------------------------------------------------------------------------
division <- initial_split(datos, strata = tipo)
entrenamiento <- training(division)
prueba <- testing(division)
pliegos <- vfold_cv(entrenamiento,v = 10,strata = tipo)


## ----random forest------------------------------------------------------------------------
rf_spec <- rand_forest() %>%
  set_args(trees = 600) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")


## ----receta-------------------------------------------------------------------------------
receta <- recipe(tipo ~
                   mime_type +
                   size +
                   name +
                   mes_creacion +
                   dia_semana_creacion +
                   dia_creacion +
                   hora_creacion +
                   mes_modificacion +
                   dia_semana_modificacion +
                   dia_modificacion +
                   hora_modificacion,
                   data = entrenamiento)



## ----receta no----------------------------------------------------------------------------
receta <- receta %>%
  step_upsample(tipo, over_ratio = 1) %>%
  step_normalize(size) %>%
  step_other(mime_type,
             threshold = 0.01,
             other = "otro") %>%
  step_tokenize(c(name)) %>%
  step_stopwords(c(name),language = "es") %>%
  step_tokenfilter(c(name),max_tokens = 100) %>%
  step_tfidf(c(name))




## ----flujo, eval = TRUE-------------------------------------------------------------------
flujo <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(receta)



## ----evaluacion prueba--------------------------------------------------------------------
final_rf <- flujo %>%
  last_fit(split = division)



## ----metricas prueba----------------------------------------------------------------------
final_res_metrics <- collect_metrics(final_rf)
final_res_predictions <- collect_predictions(final_rf)
correlacion_matthews <- mcc(final_res_predictions,truth = tipo, estimate = .pred_class)
final_res_metrics <- bind_rows(final_res_metrics, correlacion_matthews) %>%
  select(-.config)

matriz_confusion <- final_res_predictions %>%
  conf_mat(truth = tipo, estimate = .pred_class)

write.table(matriz_confusion$table,"matriz_confusion_mod_1_1.csv", sep = ",")

write_csv(final_res_metrics,"ficha_mod_1_1.csv")



## ---- eval = TRUE-------------------------------------------------------------------------
modelo <- final_rf$.workflow[[1]]

saveRDS(modelo, file = "modelo_1_1.rds")



