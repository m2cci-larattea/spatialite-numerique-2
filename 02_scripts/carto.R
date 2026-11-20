library(sf)
library(ggplot2)
library(readr)
library(readxl)

library(dplyr)
library(ggplot2)

# choisir feuille, ligne d'en-tête, types
isere_tourisme <- read_csv2(
  "00_data_init/isere_tourisme_insee.csv",
)

isere_tourisme$INSEE_COM <- as.character(isere_tourisme$INSEE_COM)

communes <- st_read("00_data_init/communes-isere/communes-de-lisere.shp")

centroid_commune <- st_centroid(communes)

isere_tourisme_sf <- left_join(isere_tourisme, centroid_commune, c("INSEE_COM" = "code_insee"))

isere_tourisme_sf <- isere_tourisme_sf[!is.na(isere_tourisme_sf$nom.y),]

isere_tourisme_sf <- st_as_sf(isere_tourisme_sf)

communes_l93 <- st_transform(communes, 2154)
cercles <- communes_l93[match(isere_tourisme_sf$INSEE_COM, communes_l93$code_insee), ]
stopifnot(!any(st_is_empty(cercles)))
nouvelle_geom <- st_sample(cercles, size = rep(1, nrow(cercles)), exact = TRUE)
isere_tourisme_pts <- st_set_geometry(isere_tourisme_sf, nouvelle_geom)

st_write(isere_tourisme_pts, "01_data_preparee/isere_tourisme_pts.gpkg",
         layer = "Isère tourisme", delete_dsn = TRUE)
