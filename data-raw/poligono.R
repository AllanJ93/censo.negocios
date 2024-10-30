## code to prepare `poligono` dataset goes here

library(dplyr)
library(sf)
library(ggplot2)

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
               coords_punto_s = c(32.516653, -116.999839),
               coords_punto_t = c(32.515748, -116.999372),
               coords_punto_u = c(32.514509, -116.998677),
               coords_punto_v = c(32.514021, -116.998411),
               coords_punto_w = c(32.513260, -116.997934),
               coords_punto_x = c(32.513118, -116.997859),
               coords_punto_y = c(32.512542, -116.997363),
               coords_punto_z = c(32.512539, -116.997359),
               coords_punto_aa = c(32.512331, -116.997137),
               coords_punto_ab = c(32.511895, -116.996544),
               coords_punto_ac = c(32.511881, -116.996793),
               coords_punto_ad = c(32.511952, -116.997120),
               coords_punto_ae = c(32.512084, -116.997483),
               coords_punto_af = c(32.512168, -116.997782),
               coords_punto_ag = c(32.512168, -116.997782),
               coords_punto_ah = c(32.512203, -116.997994),
               coords_punto_ai = c(32.512200, -116.998082),
               coords_punto_aj = c(32.512221, -116.998426),
               coords_punto_ak = c(32.512221, -116.998426),
               coords_punto_al = c(32.512209, -116.998628),
               coords_punto_am = c(32.512126, -116.998724),
               coords_punto_an = c(32.511914, -116.998330),
               coords_punto_ao = c(32.511800, -116.998159),
               coords_punto_ap = c(32.511302, -116.997488),
               coords_punto_aq = c(32.510921, -116.997040),
               coords_punto_ar = c(32.510076, -116.996841),
               coords_punto_as = c(32.509675, -116.997603),
               coords_punto_at = c(32.509573, -116.998283),
               coords_punto_au = c(32.509481, -116.998913),
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
               coords_punto_bk = c(32.500023, -117.001076),
               coords_punto_bl = c(32.499856, -117.000546),
               coords_punto_bm = c(32.498884, -116.998935),
               coords_punto_bn = c(32.498452, -116.997900),
               coords_punto_bo = c(32.498105, -116.996859),
               coords_punto_bp = c(32.498064, -116.996156),
               coords_punto_bq = c(32.498103, -116.995473),
               coords_punto_br = c(32.498186, -116.994157),
               coords_punto_bs = c(32.498243, -116.993808),
               coords_punto_bt = c(32.500220, -116.989952),
               coords_punto_bu = c(32.500784, -116.989456),
               coords_punto_bv = c(32.500431, -116.988892),
               coords_punto_bw = c(32.500216, -116.986391),
               coords_punto_bx = c(32.500074, -116.985678),
               coords_punto_by = c(32.498678, -116.985881),
               coords_punto_bz = c(32.498754, -116.985307),
               coords_punto_ca = c(32.499037, -116.984860),
               coords_punto_cb = c(32.499508, -116.985129),
               coords_punto_cc = c(32.499843, -116.984559),
               coords_punto_cd = c(32.500030, -116.984667),
               coords_punto_ce = c(32.506338, -116.979526)
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
  addPolygons(data = shp_poligono)

usethis::use_data(shp_poligono, overwrite = TRUE)
