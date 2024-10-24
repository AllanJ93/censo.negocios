## code to prepare `datos_censo` dataset goes here

set.seed(123)

datos_censo <-
  tibble::tibble("negocio" = paste0("negocio_", LETTERS[1:10]),
                 "giro" = sample(x = letters[1:4],
                                 size = 10, replace = TRUE),
                 "lng" = c(-117.0027573, -116.9824742, -116.9854113, -116.9986603, -116.9993123,
                           -116.9992785, -116.9966967, -116.9919853, -116.9939682, -116.9969747),
                 "lat" = c(32.5061250, 32.5165065, 32.5125656, 32.5101631, 32.5021876,
                           32.5021872, 32.5004230, 32.5149833, 32.5102074, 32.5121346)) |>
  sf::st_as_sf(coords = c("lng", "lat"),
               crs = 4326)

usethis::use_data(datos_censo, overwrite = TRUE)
