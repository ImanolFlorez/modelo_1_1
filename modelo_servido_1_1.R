args <- commandArgs(trailingOnly = TRUE)

#' Predice con el modelo de metadatos de acuacar (modelo 1.1)
#'
#' Asume que el modelo fue creado previamente y esta almacenado en un archivo
#' "modelo_1_1.rds"
#'
#' @param datos nombre del archivo .csv con los datos a predecir
#' @param resultados nombre del archivo .csv con los resultados de las predicciones
#'
#' @return Escribe archivo con los resultados de las predicciones
#'
predecir_metadatos <- function(datos_file,resultados_file) {
  library(readr)
  library(janitor)
  library(tidymodels)
  library(textrecipes)
  library(readxl)
  library(dplyr)
  library(lubridate)


  # Se excluyen los autores automáticos
  #autores_automaticos <- c("acuacarweb","pqrweb","admin", "partner")

  # se excluyen las areas con representación menor al 1%
  # otros <- c("1803", "1721", "1304", "1800", "1502", "1602", "1820",
  #                  "1505", "1600", "1200", "1202", "1501", "1210","1701",
  #                  "1702", "1401", "1402", "1711", "1801", "1900")

  muestra <- read_csv(datos_file) %>%
    clean_names() %>%
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
      hora_modificacion = hour(date_modified)) %>%

    # Crea tipo como area y una categoria para otros
    mutate(
      tipo = factor(area))


  ### Obtener modelo -------------------------------------------------------------
  modelo <- readRDS("modelo_1_1.rds")

  ### Obtener predicciones -------------------------------------------------------
  clase <- predict(modelo,new_data = muestra, type = "class")
  probabilidad <- predict(modelo,new_data = muestra, type = "prob")

  ### Calcular y escribir resultados ---------------------------------------------
  resultados <- cbind.data.frame(clase,probabilidad)
  write_csv(resultados,file = resultados_file)

  }

### LLamar a la funcion con el nombre de los parametros dados ------------------
entrada <- args[1]
salida <- args[2]

predecir_metadatos(entrada,salida)
