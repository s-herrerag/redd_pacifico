# Cargar la librería terra
library(terra)
library(raster)
library(rgdal)  # Para trabajar con shapefiles
library(sf)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  prism, # download PRISM data
  tictoc, # timing codes
  tigris, # to get county sf
  tmap # for mapping
)

#Rutas
ruta_pacifico <- "/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Inputs/pacifico_disuelto/pacifico_disuelto.shp"
ruta_defo <- "/Users/carolinacastroosorio/Library/CloudStorage/OneDrive-UniversidaddelosAndes/REDD+ bases/Inputs/Deforesta_IDEAM/masdetalle/2002_2004.tif"

# Cargar el raster y el shapefile
mi_raster <- raster(ruta_defo) #OJO ajustar a llamarlo de una vez SpatRaster
poligonopacifico <- st_read(dsn = ruta_pacifico)

#Recortar raster al poligono pacifico y convertirlo en SpatRaster
raster_recortado_sr <- terra::crop(mi_raster, poligonopacifico)
raster_recortado_sr <- rast(raster_recortado_sr)

#-convierto el poligono a spatvector-#
poligono_spat <- vect(poligonopacifico)

#-Extraer datos del raster recortado-#OJOJO falta dejar geometria y que cree un ID
df2000 <- terra::extract(raster_recortado_sr, poligono_spat)

#--- check the class ---#
class(df2000)
valores_unicos <- unique(df2000$X2002_2004)
print(valores_unicos)
na_count <- sum(is.na(df2000$X2002_2004))
print(na_count)

