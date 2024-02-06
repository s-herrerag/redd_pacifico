###########################################################
# Datos de deforestacion: Unión con Consejos Comunitarios
###########################################################


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(arrow)

# Union con datos de consejos comunitarios --------------------------------

panel_deforestacion <- read_parquet("/Users/santiagoherreragarcia/Downloads/panel_Defo_90_sintetico.parquet")
base_info <- readRDS("/Users/santiagoherreragarcia/Downloads/proyectos_by_CC.rds")

#Bases pueden contener espacios
base_info <- base_info %>%
  mutate(nombre_com = str_squish(nombre_com))

panel_deforestacion <- panel_deforestacion %>%
  mutate(nombre_com = str_squish(nombre_com))
  
# Arreglo de nombres

nom_com_correct <- unique(base_info$nombre_com)
nom_com_deforestacion <- unique(panel_deforestacion$nombre_com) |>
  na.omit()

# Non-matching
non_matching <- nom_com_correct[!nom_com_correct%in%nom_com_deforestacion]

# Reemplazar en non_com_deforestacion
panel_deforestacion <- panel_deforestacion %>%
  mutate(nombre_com = case_when(nombre_com == "La Cuenca Del Río Tolo Y Zona Costera Sur - Cocomasur" ~ "La Cuenca Del Río Tolo Y Zona Costera Sur",
                                .default = nombre_com))


# Merge ###
panel_deforestacion_consejos <- panel_deforestacion %>%
  left_join(base_info, by = "nombre_com")

# Exportar ----------------------------------------------------------------
write_parquet(panel_deforestacion_consejos, "/Users/santiagoherreragarcia/Downloads/panel_deforestacion_consejos.parquet")



