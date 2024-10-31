## code to prepare `poligono` dataset goes here

library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)

# Poligono delimitad
puntos <- list(coords_punto_a = c(32.507543, -116.980988),
               coords_punto_b = c(32.509905, -116.979942),
               coords_punto_c = c(32.510114, -116.979866),
               coords_punto_d = c(32.510216, -116.979830),
               coords_punto_e = c(32.511769, -116.978642),
               coords_punto_f = c(32.512259, -116.978119),
               coords_punto_g = c(32.512658, -116.977703),
               coords_punto_h = c(32.513202, -116.977415),
               coords_punto_i = c(32.513487, -116.977206),
               coords_punto_j = c(32.513928, -116.976968),
               coords_punto_k = c(32.514841, -116.976748),
               coords_punto_l = c(32.514992, -116.978630),
               coords_punto_m = c(32.516252, -116.994806),
               coords_punto_n = c(32.516259, -116.994901),
               coords_punto_o = c(32.516386, -116.996715),
               coords_punto_p = c(32.516479, -116.997721),
               coords_punto_q = c(32.516564, -116.998511),
               coords_punto_r = c(32.516852, -116.999738),
               coords_punto_s = c(32.517211, -117.000761),
               coords_punto_t = c(32.517537, -117.001909),
               coords_punto_u = c(32.517714, -117.004444),
               coords_punto_v = c(32.517389, -117.005050),
               coords_punto_w = c(32.516932, -117.005531),
               coords_punto_x = c(32.516770, -117.005826),
               coords_punto_y = c(32.516640, -117.006123),
               coords_punto_z = c(32.516558, -117.006542),
               coords_punto_aa = c(32.516568, -117.007068),
               coords_punto_ab = c(32.512856, -117.009141),
               coords_punto_ac = c(32.512140, -117.006070),
               coords_punto_ad = c(32.511618, -117.003009),
               coords_punto_ae = c(32.512131, -117.002813),
               coords_punto_af = c(32.511770, -117.000673),
               coords_punto_ag = c(32.511459, -117.000608),
               coords_punto_ah = c(32.511333, -116.999322),
               coords_punto_ai = c(32.511019, -116.999347),
               coords_punto_aj = c(32.510779, -116.998577),
               coords_punto_ak = c(32.510872, -116.998549),
               coords_punto_al = c(32.510787, -116.998297),
               coords_punto_am = c(32.511040, -116.998160),
               coords_punto_an = c(32.510735, -116.997209),
               coords_punto_ao = c(32.510422, -116.997008),
               coords_punto_ap = c(32.510065, -116.996845),
               coords_punto_aq = c(32.509861, -116.997269),
               coords_punto_ar = c(32.509762, -116.997605),
               coords_punto_as = c(32.509671, -116.997598),
               coords_punto_at = c(32.509604, -116.998292),
               coords_punto_au = c(32.509482, -116.998914),
               coords_punto_av = c(32.509864, -117.000595),
               coords_punto_aw = c(32.509078, -117.001558),
               coords_punto_ax = c(32.508320, -117.001725),
               coords_punto_ay = c(32.507506, -117.000923),
               coords_punto_az = c(32.505553, -117.002582),
               coords_punto_ba = c(32.505096, -117.003686),
               coords_punto_bb = c(32.504762, -117.004348),
               coords_punto_bc = c(32.504064, -117.004353),
               coords_punto_bd = c(32.503596, -117.004623),
               coords_punto_be = c(32.503549, -117.004737),
               coords_punto_bf = c(32.503380, -117.005136),
               coords_punto_bg = c(32.503249, -117.005419),
               coords_punto_bh = c(32.503094, -117.005523),
               coords_punto_bi = c(32.503250, -117.002988),
               coords_punto_bj = c(32.501327, -117.003148),
               coords_punto_bk = c(32.500099, -117.003040),
               coords_punto_bl = c(32.496644, -116.995695),
               coords_punto_bm = c(32.497589, -116.981560),
               coords_punto_bn = c(32.498563, -116.980793),
               coords_punto_bo = c(32.499064, -116.981654),
               coords_punto_bp = c(32.498744, -116.982103),
               coords_punto_bq = c(32.500119, -116.983204),
               coords_punto_br = c(32.501186, -116.982595),
               coords_punto_bs = c(32.502872, -116.981179),
               coords_punto_bt = c(32.506029, -116.978440)
               )


bd_puntos <- tibble(punto = c())
for(i in names(puntos)){

  nombre_punto <-
    i |>
    gsub(pattern = "coords_punto_",
         replacement = '') |>
    stringr::str_to_upper()

  bd_puntos <-
    bd_puntos |>
    bind_rows(tibble::tibble(punto = nombre_punto,
                             latitude = (puntos |>
                                           purrr::pluck(i))[1],
                             longitude = (puntos |>
                                            purrr::pluck(i))[2]))
}

shp_puntos <-
  bd_puntos |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326)

coordenadas <- st_coordinates(shp_puntos)
poligono <- st_polygon(list(rbind(coordenadas, coordenadas[1, ])))

shp_poligono <-
  st_sf(geometry = st_sfc(poligono), crs = 4326)

shp_galeriasHipodromo |>
  st_bbox(obj = shp_galeriasHipodromo) |>
  st_as_sfc() |>
  leaflet(options = leafletOptions(zoomControl = FALSE)) |>
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}",
           attribution = '© Google') |>
  addMarkers(lng = -116.9925142,
             lat = 32.5083855) |>
  addPolygons(data = circunferencia_1km,
              stroke = TRUE,
              weight = 2,
              fillOpacity = 0.2) |>
  addMarkers(data = shp_puntos,
             label = ~ punto) |>
  addPolygons(data = shp_poligono) |>
  addCircleMarkers(data = datos_censo,
                   opacity = 1,
                   radius = 10,
                   fillOpacity = 1,
                   stroke = F,
                   # fillColor = ~pal_tipo_negocios(P5_O1),
                   # fillColor = ~color,
                   # color = ~pal_tipo_negocios(P5_O1),
                   # color = ~color,
                   # clusterOptions = markerClusterOptions(),
                   popup = ~ glue::glue("<span style='font-size:20px;'>Nombre: {P2} <br>
                                              Tipo de cocina: {tipos_cocina} <br>
                                              Horario de apertura: {P6} <br>
                                              Horario de cierre: {P7} <br>
                                              Música en vivo: {P8} <br>
                                              Menús de temporada: {P9} <br>
                                              Promociones especiales: {P10} <br>
                                              Pedidos en línea o en app: {P11} <br>
                                              Áreas al aire libre o terrazas: {P12} <br>
                                              Área de juego para niños: {P13}</span>"),
                   group = "Negocios")

usethis::use_data(shp_poligono, overwrite = TRUE)
