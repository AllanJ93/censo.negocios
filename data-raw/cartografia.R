## code to prepare `cartografia` dataset goes here

library(dplyr)
library(sf)
library(leaflet)

shp_galeriasHipodromo <-
  tibble::tibble(latitude = 32.5083855,
                 longitude = -116.9925142) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326)

punto_utm <- sf::st_transform(shp_galeriasHipodromo, 32611)

# Crear una circunferencia de radio 1000 metros (1 km)
circulo_utm <- sf::st_buffer(punto_utm, dist = 1000)

# Transformar la circunferencia de vuelta a WGS84 (coordenadas geográficas)
circunferencia_1km <- sf::st_transform(circulo_utm, 4326)


usethis::use_data(shp_galeriasHipodromo, overwrite = TRUE)
usethis::use_data(circunferencia_1km, overwrite = TRUE)
