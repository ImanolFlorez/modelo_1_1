#' @title Genera datos de ejemplo de metadatos para ACUACAR
#' 
#' @description Utilizando como base los datos con los que se entreno el modelo,
#' toma una muestra de datos que sirven como ejemplo de un nuevo conjunto de 
#' datos con el cual se van a generar predicciones. Asume que los datos se
#' encuentran aquí: "datos/ACUACAR_Activo_2020_06_01_A_2021_02_10_V1.xlsx"
#' 
#' @param numero es el número de muestras a generar
#'

args <- commandArgs(trailingOnly = TRUE)

numero <- as.numeric(args[1])

library(readxl)
library(readr)
library(dplyr)

autores_automaticos <- c("acuacarweb","pqrweb","admin", "partner")


read_excel("datos/ACUACAR_Activo_2020_06_01_A_2021_02_10_V1.xlsx") %>%
  # AREA es NA, pues no se tiene al inicio y es lo que se quiere predecir
  mutate(AREA = NA) %>%
  # Filtro 1, entrada y excluye autores automáticos
  filter(Modo != "S") %>%
  filter(!author %in% autores_automaticos) %>%
  slice_sample(n = numero) %>%
  write_csv("muestra_mod_1.1.csv")


